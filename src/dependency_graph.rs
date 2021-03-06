use std::collections::HashMap;

use petgraph;
use serde::{Deserialize, Serialize};
use serde_json::json;

use crate::parser::{Expression, RIdentifier, Statement, StatementId, Statements};

/// A graph modelling dependencies between statement as a graph of `StatementId`s with the variable names as edges.
#[derive(Debug, Serialize, Deserialize, Default)]
pub struct DependencyGraph {
    graph: Graph,
    variables: VariableMap,
}

// Helper type definitions
type NodeIndexType = petgraph::graph::DefaultIx;
pub type NodeIndex = petgraph::graph::NodeIndex<NodeIndexType>;
#[derive(Serialize, Default, Deserialize, Debug)]
struct Graph {
    graph: petgraph::Graph<StatementId, Variable, petgraph::Directed, NodeIndexType>,
    ids: HashMap<StatementId, NodeIndex>,
}
type Variable = String;

// We use a custom Graph type, since petgraph::GraphMap is not serializable out of the box.
// This structure manages the converting between `StatementId`s and internal `NodexIndex`.
impl Graph {
    fn new() -> Self {
        Graph {
            graph: petgraph::Graph::new(),
            ids: HashMap::new(),
        }
    }

    fn add_node(&mut self, content: StatementId) {
        let node_id = self.graph.add_node(content);
        self.ids.insert(content, node_id);
    }

    fn add_edge(&mut self, from: StatementId, to: StatementId, content: Variable) {
        let from_idx = self.ids[&from];
        let to_idx = self.ids[&to];
        self.graph.add_edge(from_idx, to_idx, content);
    }

    fn neighbors_directed(
        &self,
        id: StatementId,
        direction: petgraph::Direction,
    ) -> Vec<StatementId> {
        let node_id = self.ids[&id];
        self.graph
            .neighbors_directed(node_id, direction)
            .map(|node_id| self.graph.node_weight(node_id).unwrap())
            .cloned()
            .collect()
    }

    fn serialize_nodes(&self) -> Vec<StatementId> {
        self.graph.raw_nodes().iter().map(|n| n.weight).collect()
    }

    fn serialize_edges(&self) -> Vec<(StatementId, StatementId, Variable)> {
        self.graph
            .raw_edges()
            .iter()
            .map(|e| {
                (
                    *self.graph.node_weight(e.source()).unwrap(),
                    *self.graph.node_weight(e.target()).unwrap(),
                    e.weight.clone(),
                )
            })
            .collect()
    }
}

impl DependencyGraph {
    pub fn new() -> Self {
        DependencyGraph {
            graph: Graph::new(),
            variables: VariableMap::new(),
        }
    }

    /// Constructs a new `DependencyGraph` from a collection of statements.
    pub fn from_input<M>(stmts: &Statements<M>) -> Self {
        let mut graph = Self::new();
        for (id, stmt, _) in stmts.iter() {
            graph.insert(id, stmt);
        }

        graph
    }

    /// Inserts all `StatementId`s from input into the graph.
    ///
    /// Requires that all statements corresponding to the ids can be looked up in `stmts`.
    pub fn batch_insert<M>(
        &mut self,
        input: impl Iterator<Item = StatementId>,
        stmts: &Statements<M>,
    ) {
        for id in input {
            self.insert(id, &stmts[id].0)
        }
    }

    /// Inserts a single `StatementId` into the graph.
    ///
    /// Requires that the statment corresponding to the id can be looked up in `stmts`.
    pub fn insert(&mut self, id: StatementId, statement: &Statement) {
        use Statement::*;
        match statement {
            Expression(expression) => {
                self.register_dependencies(id, expression);
            }
            Assignment(left, additional, right) => {
                let first = [left.clone()];
                let rest = additional.iter();
                let assigned = first.iter().chain(rest);
                self.register_dependencies(id, right);
                for variable in assigned {
                    match variable.extract_variable_name() {
                        Some(name) => self.variables.push(name, id),
                        None => panic!(
                        "Could not find a variable in {}, in the left side of the assignment {}.",
                        variable, statement
                    ),
                    };
                }
            }
            TailComment(statement, _) => self.insert(id, statement),
            // The following cannot have dependencies
            Empty => self.graph.add_node(id),
            Comment(_) => self.graph.add_node(id),
            If(_, _, _) => self.graph.add_node(id),
            While(_, _) => self.graph.add_node(id),
            For(_, _, _) => self.graph.add_node(id),
            Library(_) => self.graph.add_node(id),
        }
    }

    /// Analyzes the expression for dependencies and adds the `StatementId` into the graph along with edges for the dependencies.
    ///
    /// Assumes that `expression` is from a statement with the passed `id`.
    fn register_dependencies(&mut self, id: StatementId, expression: &Expression) {
        let dependencies = extract_dependencies(&expression);
        self.graph.add_node(id);
        for dependency in dependencies {
            if let Some(parent) = self.variables.get(&dependency) {
                self.graph.add_edge(*parent, id, dependency);
            }
            // Else, we do not know the variable. But this might still be valid,
            // e. g. if it is a library function that wasn't explicitly declared in the code.
            // Therefore, we simply ignore this in the dependency graph.
        }
    }

    /// Returns all `StatementId`s that assign to a variable used by the statement with `id`.
    pub fn parents(&self, id: StatementId) -> Vec<StatementId> {
        self.graph
            .neighbors_directed(id, petgraph::Direction::Incoming)
    }

    /// If the statement behind `id` contains an expression,
    /// constructs a new `Expression` where all variables used by that expression are inlined.
    /// Otherwise returns `None`.
    ///
    /// Requires that the statment corresponding to the id can be looked up in `stmts`.
    pub fn inline_id<M>(&self, id: StatementId, stmts: &Statements<M>) -> Option<Expression> {
        let statement = &stmts[id].0;
        statement
            .expression()
            .map(|exp| self.inline_exp(exp, id, stmts))
    }

    /// Constructs a new `Expression` where all variables used are inlined.
    ///
    /// Requires that the expression comes from a statement that can be looked up with `stmt_id` in `stmts`.
    fn inline_exp<M>(
        &self,
        exp: &Expression,
        stmt_id: StatementId,
        stmts: &Statements<M>,
    ) -> Expression {
        use Expression::*;
        match exp {
            Constant(constant) => Constant(constant.clone()),
            Variable(name) => {
                if let Some(exps) = self.variables.get_all(name) {
                    if let Some(replacement) = exps.iter().filter(|other| **other < stmt_id).last()
                    {
                        return self.inline_id(*replacement, stmts).unwrap();
                    }
                }
                Variable(name.clone())
            }
            Call(exp, args) => Call(
                Box::new(self.inline_exp(exp, stmt_id, stmts)),
                args.iter()
                    .map(|(name, exp)| (name.clone(), self.inline_exp(exp, stmt_id, stmts)))
                    .collect(),
            ),
            Column(left, right) => Column(
                Box::new(self.inline_exp(left, stmt_id, stmts)),
                Box::new(self.inline_exp(right, stmt_id, stmts)),
            ),
            Index(left, right) => Index(
                Box::new(self.inline_exp(left, stmt_id, stmts)),
                right
                    .iter()
                    .map(|maybe_exp| {
                        maybe_exp
                            .as_ref()
                            .map(|exp| self.inline_exp(exp, stmt_id, stmts))
                    })
                    .collect(),
            ),
            ListIndex(left, right) => ListIndex(
                Box::new(self.inline_exp(left, stmt_id, stmts)),
                right
                    .iter()
                    .map(|maybe_exp| {
                        maybe_exp
                            .as_ref()
                            .map(|exp| self.inline_exp(exp, stmt_id, stmts))
                    })
                    .collect(),
            ),
            OneSidedFormula(formula) => {
                OneSidedFormula(Box::new(self.inline_exp(formula, stmt_id, stmts)))
            }
            TwoSidedFormula(left, right) => TwoSidedFormula(
                Box::new(self.inline_exp(left, stmt_id, stmts)),
                Box::new(self.inline_exp(right, stmt_id, stmts)),
            ),
            Function(args, body) => Function(
                args.iter()
                    .map(|(name, maybe_expression)| {
                        (
                            name.clone(),
                            maybe_expression
                                .clone()
                                .map(|expression| self.inline_exp(&expression, stmt_id, stmts)),
                        )
                    })
                    .collect(),
                body.to_vec(), // TODO: Investigate whether statements can be analyzed.
            ),
            Prefix(operator, exp) => Prefix(
                operator.clone(),
                Box::new(self.inline_exp(exp, stmt_id, stmts)),
            ),
            Infix(operator, left, right) => Infix(
                operator.clone(),
                Box::new(self.inline_exp(left, stmt_id, stmts)),
                Box::new(self.inline_exp(right, stmt_id, stmts)),
            ),
        }
    }

    pub fn as_json(&self) -> serde_json::Value {
        json!({
            "nodes": self.graph.serialize_nodes(),
            "edges": self.graph.serialize_edges(),
            "variables": self.variables
        })
    }
}

/// Returns the names of the variables used in the `expression`.
fn extract_dependencies(expression: &Expression) -> Vec<RIdentifier> {
    use Expression::*;
    match expression {
        Variable(name) => vec![name.clone()],
        Call(_, arguments) => arguments
            .iter()
            .flat_map(|(_, exp)| extract_dependencies(exp))
            .collect(),
        Column(left, _) => extract_dependencies(left),
        Index(left, _) => extract_dependencies(left),
        ListIndex(left, _) => extract_dependencies(left),
        Prefix(_, exp) => extract_dependencies(exp),
        Infix(_, left, right) => {
            let mut deps = extract_dependencies(left);
            deps.append(&mut extract_dependencies(right));
            deps
        }
        OneSidedFormula(exp) => extract_dependencies(exp),
        TwoSidedFormula(left, right) => {
            let mut deps = extract_dependencies(left);
            deps.append(&mut extract_dependencies(right));
            deps
        }
        Function(_, _) | Constant(_) => Vec::new(),
    }
}

/// A map that keeps track of what variables are defined in what statements.
/// The variable names are used as keys and each value contains the defining `StatementId` in source code order.
#[derive(Default, Debug, PartialEq, Eq, Deserialize, Serialize)]
struct VariableMap(HashMap<String, Vec<StatementId>>);

impl VariableMap {
    fn new() -> Self {
        VariableMap(HashMap::new())
    }

    /// Add a new `StatementId` for the `variable`.
    fn push(&mut self, variable: String, index: StatementId) {
        self.0.entry(variable).or_insert_with(|| vec![]).push(index);
    }

    /// Returns the last `StatementId` that defined the `variable`, or returns `None` if the variable is undefined.
    fn get(&self, variable: &str) -> Option<&StatementId> {
        self.get_all(variable).and_then(|v| v.last())
    }

    /// Returns all `StatementId`s that defined the `variable`.
    fn get_all(&self, variable: &str) -> Option<&Vec<StatementId>> {
        self.0.get(variable)
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;
    use std::collections::HashSet;
    use std::iter::FromIterator;

    use petgraph::visit::Walker;

    use super::*;
    use crate::parser::{Expression, Statement};
    use crate::{assignment, call, column, constant, expression, infix, tail_comment, variable};

    impl Graph {
        fn from_ids(ids: impl Iterator<Item = StatementId>) -> Self {
            let mut graph = petgraph::Graph::new();
            let mut id_map = HashMap::new();

            for id in ids {
                let node_id = graph.add_node(id);
                id_map.insert(id, node_id);
            }

            Graph { graph, ids: id_map }
        }

        fn extend_with_edges(
            &mut self,
            edges: impl Iterator<Item = (StatementId, StatementId, Variable)>,
        ) {
            let edges: Vec<(NodeIndex, NodeIndex, Variable)> = edges
                .map(|(from, to, v)| (self.ids[&from], self.ids[&to], v))
                .collect();
            self.graph.extend_with_edges(edges);
        }
    }

    fn compare_graphs(expected: DependencyGraph, actual: DependencyGraph) {
        assert_eq!(expected.variables, actual.variables);

        let expected = &expected.graph.graph;
        let actual = &actual.graph.graph;
        let walk_expected = petgraph::visit::Topo::new(expected);
        let walk_actual = petgraph::visit::Topo::new(actual);
        for (expected_id, actual_id) in walk_expected.iter(expected).zip(walk_actual.iter(actual)) {
            assert_eq!(
                expected
                    .neighbors(expected_id)
                    .map(|n_id| expected.node_weight(n_id).unwrap())
                    .cloned()
                    .collect::<HashSet<StatementId>>(),
                actual
                    .neighbors(actual_id)
                    .map(|n_id| actual.node_weight(n_id).unwrap())
                    .cloned()
                    .collect::<HashSet<StatementId>>(),
                "Nodes {:?} and {:?} have different neighbors.",
                expected.node_weight(expected_id),
                actual.node_weight(actual_id)
            );
        }
    }

    #[test]
    fn detects_linear_graph() {
        let input = Statements::from_iter(vec![
            assignment!(variable!("x"), vec![], constant!("1")),
            assignment!(
                variable!("y"),
                vec![],
                call!(variable!("transform"), vec![(None, variable!("x"))])
            ),
            tail_comment!(
                expression!(call!(variable!("modify"), vec![(None, variable!("y"))])),
                "# modifies y"
            ),
            expression!(call!(variable!("change"), vec![(None, variable!("y"))])),
        ]);
        let ids: Vec<StatementId> = input.iter().map(|(id, _, _)| id).collect();
        let mut actual = DependencyGraph::new();
        actual.batch_insert(ids.iter().cloned(), &input);

        let variables: HashMap<String, Vec<StatementId>> = HashMap::from_iter(
            vec![("x".into(), vec![ids[0]]), ("y".into(), vec![ids[1]])].into_iter(),
        );
        let mut graph = Graph::from_ids(ids.iter().cloned());
        graph.extend_with_edges(
            vec![
                (ids[0], ids[1], "x".into()),
                (ids[1], ids[2], "y".into()),
                (ids[1], ids[3], "y".into()),
            ]
            .into_iter(),
        );
        let expected = DependencyGraph {
            graph,
            variables: VariableMap(variables),
        };

        compare_graphs(expected, actual);
    }

    #[test]
    fn detects_mutations() {
        let input = Statements::from_iter(vec![
            assignment!(variable!("x"), vec![], constant!("data frame")),
            assignment!(
                column!(variable!("x"), constant!("column")),
                vec![],
                call!(
                    variable!("factor"),
                    vec![(None, column!(variable!("x"), constant!("column")))]
                )
            ),
            expression!(call!(variable!("summary"), vec![(None, variable!("x"))])),
        ]);
        let ids: Vec<StatementId> = input.iter().map(|(id, _, _)| id).collect();
        let mut actual = DependencyGraph::new();
        actual.batch_insert(ids.iter().cloned(), &input);

        let variables: HashMap<String, Vec<StatementId>> =
            HashMap::from_iter(vec![("x".into(), vec![ids[0], ids[1]])].into_iter());
        let mut graph = Graph::from_ids(ids.iter().cloned());
        graph.extend_with_edges(
            vec![(ids[0], ids[1], "x".into()), (ids[1], ids[2], "x".into())].into_iter(),
        );
        let expected = DependencyGraph {
            graph,
            variables: VariableMap(variables),
        };

        compare_graphs(expected, actual);
    }

    #[test]
    fn detects_sibling_dependencies() {
        let input = Statements::from_iter(vec![
            assignment!(variable!("x"), vec![], constant!("data frame")),
            expression!(call!(
                variable!("factor"),
                vec![(None, column!(variable!("x"), constant!("column")))]
            )),
            expression!(call!(variable!("summary"), vec![(None, variable!("x"))])),
        ]);
        let ids: Vec<StatementId> = input.iter().map(|(id, _, _)| id).collect();
        let mut actual = DependencyGraph::new();
        actual.batch_insert(ids.iter().cloned(), &input);

        let variables: HashMap<String, Vec<StatementId>> =
            HashMap::from_iter(vec![("x".into(), vec![ids[0]])].into_iter());
        let mut graph = Graph::from_ids(ids.iter().cloned());
        graph.extend_with_edges(
            vec![(ids[0], ids[1], "x".into()), (ids[0], ids[2], "x".into())].into_iter(),
        );
        let expected = DependencyGraph {
            graph,
            variables: VariableMap(variables),
        };

        compare_graphs(expected, actual);
    }

    mod dependencies {
        use super::*;
        use crate::{column, index};
        use pretty_assertions::assert_eq;

        #[test]
        fn finds_variables() {
            let expression = variable!("x");
            let result = extract_dependencies(&expression);
            assert_eq!(vec!["x".to_string()], result);
        }

        #[test]
        fn finds_call_args() {
            let expression = call!(
                variable!("func"),
                vec![
                    (None, constant!("1")),
                    (None, variable!("x")),
                    (None, variable!("y")),
                ]
            );
            let result = extract_dependencies(&expression);
            assert_eq!(vec!["x".to_string(), "y".to_string()], result);
        }

        #[test]
        fn finds_column() {
            let expression = column!(variable!("x"), constant!("test"));
            let result = extract_dependencies(&expression);
            assert_eq!(vec!["x".to_string()], result);
        }

        #[test]
        fn finds_column_in_call() {
            let expression = call!(
                variable!("factor"),
                vec![(None, column!(variable!("x"), constant!("column")),)]
            );
            let result = extract_dependencies(&expression);
            assert_eq!(vec!["x".to_string()], result);
        }

        #[test]
        fn finds_index() {
            let expression = index!(variable!("x"), vec![Some(constant!("1"))]);
            let result = extract_dependencies(&expression);
            assert_eq!(vec!["x".to_string()], result);
        }

        #[test]
        fn finds_infix() {
            let expression = infix!("+", variable!("x"), constant!("10"));
            let result = extract_dependencies(&expression);
            assert_eq!(vec!["x".to_string()], result);
        }
    }

    mod inlining {
        use super::*;
        use pretty_assertions::assert_eq;

        #[test]
        fn inlines_simple_shadowing() {
            let input = Statements::from_iter(vec![
                assignment!(variable!("x"), vec![], constant!("old value")),
                expression!(call!(variable!("print"), vec![(None, variable!("x"))])),
                assignment!(variable!("x"), vec![], constant!("new value")),
                expression!(call!(variable!("print"), vec![(None, variable!("x"))])),
            ]);

            let ids: Vec<StatementId> = input.iter().map(|(id, _, _)| id).collect();
            let mut graph = DependencyGraph::new();
            graph.batch_insert(ids.iter().cloned(), &input);

            let result = graph.inline_id(ids[1], &input).unwrap();
            let expected: Expression =
                call!(variable!("print"), vec![(None, constant!("old value"))]);
            assert_eq!(expected, result);
            let result = graph.inline_id(ids[3], &input).unwrap();
            let expected: Expression =
                call!(variable!("print"), vec![(None, constant!("new value"))]);
            assert_eq!(expected, result);
        }
    }
}

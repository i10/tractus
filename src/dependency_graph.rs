use std::collections::HashMap;
use std::ops::Deref;
use std::rc::Rc;

use petgraph;

use crate::parser::{LineDisplay, RExpression, RIdentifier, RStatement, Span};

type NodeIndexType = petgraph::graph::DefaultIx;
pub type NodeIndex = petgraph::graph::NodeIndex<NodeIndexType>;
type Graph<T> = petgraph::Graph<Rc<RExpression<T>>, String, petgraph::Directed, NodeIndexType>;

#[derive(Default, Debug)]
pub struct DependencyGraph<T: Eq> {
    graph: Graph<T>,
    map: HashMap<Rc<RExpression<T>>, NodeIndex>,
    variables: VariableMap,
}

#[derive(Default, Debug)]
struct VariableMap(HashMap<String, Vec<NodeIndex>>);

impl VariableMap {
    fn new() -> Self {
        VariableMap(HashMap::new())
    }

    fn push(&mut self, variable: String, index: NodeIndex) {
        self.0.entry(variable).or_insert_with(|| vec![]).push(index);
    }

    fn get(&mut self, index: &str) -> Option<&NodeIndex> {
        self.get_all(index).and_then(|v| v.last())
    }

    fn get_all(&mut self, index: &str) -> Option<&Vec<NodeIndex>> {
        self.0.get(index)
    }
}

impl<T: Eq> DependencyGraph<T> {
    pub fn new() -> Self {
        DependencyGraph {
            graph: Graph::new(),
            map: HashMap::new(),
            variables: VariableMap::new(),
        }
    }

    pub fn from_input(input: impl Iterator<Item = Rc<RStatement<T>>>) -> Self {
        let mut graph = Self::new();
        graph.batch_insert(input);
        graph
    }

    pub fn batch_insert(&mut self, input: impl Iterator<Item = Rc<RStatement<T>>>) {
        for statement in input {
            self.insert(statement)
        }
    }

    fn insert(&mut self, statement: Rc<RStatement<T>>) {
        use RStatement::*;
        match &*statement {
            Expression(expression, _) => {
                register_dependencies(expression, self);
            }
            Assignment(left, additional, right, _) => {
                let mut assigned = vec![left];
                assigned.append(&mut additional.iter().collect());
                for variable in assigned {
                    let node_id = register_dependencies(right, self);
                    match variable.extract_variable_name() {
                        Some(name) => self.variables.push(name, node_id),
                        None => panic!(
                        "Could not find a variable in {}, in the left side of the assignment {}.",
                        variable, statement
                    ),
                    };
                }
            }
            TailComment(statement, _, _) => self.insert(statement.clone()),
            _ => (),
        };
    }

    pub fn graph(&self) -> &Graph<T> {
        &self.graph
    }

    pub fn graph_mut(&mut self) -> &mut Graph<T> {
        &mut self.graph
    }

    pub fn id(&self, expression: &RExpression<T>) -> Option<NodeIndex> {
        self.map.get(expression).cloned()
    }
}

pub struct GraphLineDisplay<'a>(&'a DependencyGraph<Span>);
impl<'a> From<&'a DependencyGraph<Span>> for GraphLineDisplay<'a> {
    fn from(other: &'a DependencyGraph<Span>) -> Self {
        GraphLineDisplay(other)
    }
}
impl<'a> std::fmt::Display for GraphLineDisplay<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let dependency_graph = self.0;
        let mapped = dependency_graph
            .graph()
            .map(|_, n| LineDisplay::from(n), |_, e| e);
        let display_graph = petgraph::dot::Dot::new(&mapped);
        write!(f, "{}", display_graph)
    }
}

fn register_dependencies<T: Eq>(
    expression: &Rc<RExpression<T>>,
    dependency_graph: &mut DependencyGraph<T>,
) -> NodeIndex {
    let dependencies = extract_dependencies(&expression);
    let node_id = dependency_graph.graph.add_node(expression.clone());
    dependency_graph.map.insert(expression.clone(), node_id);
    for dependency in dependencies {
        if let Some(parent) = dependency_graph.variables.get(&dependency) {
            dependency_graph
                .graph
                .add_edge(*parent, node_id, dependency);
        }
        // Else, we do not know the variable. But this might still be valid,
        // e. g. if it is a library function that wasn't explicitly declared in the code.
        // Therefore, we simply ignore this in the dependency graph.
    }
    node_id
}

fn extract_dependencies<T>(expression: &Rc<RExpression<T>>) -> Vec<RIdentifier> {
    use RExpression::*;
    match expression.deref() {
        Variable(name, _) => vec![name.clone()],
        Call(_, arguments, _) => arguments
            .iter()
            .flat_map(|(_, exp)| extract_dependencies(exp))
            .collect(),
        Column(left, _, _) => extract_dependencies(left),
        Index(left, _, _) => extract_dependencies(left),
        _ => vec![],
    }
}
#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;
    use std::collections::HashSet;

    use petgraph::visit::Walker;

    use super::*;
    use crate::parser::{RExpression, RStatement};
    use crate::{assignment, call, column, constant, expression, tail_comment, variable};

    fn compare_graphs(expected: DependencyGraph<()>, actual: DependencyGraph<()>) {
        let expected = &expected.graph();
        let actual = &actual.graph();
        let walk_expected = petgraph::visit::Topo::new(expected);
        let walk_actual = petgraph::visit::Topo::new(actual);
        for (expected_id, actual_id) in walk_expected.iter(expected).zip(walk_actual.iter(actual)) {
            assert_eq!(
                expected
                    .neighbors(expected_id)
                    .map(|n_id| expected.node_weight(n_id).unwrap())
                    .cloned()
                    .collect::<HashSet<Rc<RExpression<()>>>>(),
                actual
                    .neighbors(actual_id)
                    .map(|n_id| actual.node_weight(n_id).unwrap())
                    .cloned()
                    .collect::<HashSet<Rc<RExpression<()>>>>(),
                "Nodes {:?} and {:?} have different neighbors.",
                expected.node_weight(expected_id),
                actual.node_weight(actual_id)
            );
        }
    }

    #[test]
    fn detects_linear_graph() {
        let input = vec![
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
        ];
        let graph = DependencyGraph::from_input(input.into_iter());

        let mut expected = DependencyGraph::new();
        let expected_graph = expected.graph_mut();
        let e1 = constant!("1");
        let n1 = expected_graph.add_node(e1);
        let e2 = call!(variable!("transform"), vec![(None, variable!("x"))]);
        let n2 = expected_graph.add_node(e2);
        expected_graph.add_edge(n1, n2, "x".into());
        let e3 = call!(variable!("modify"), vec![(None, variable!("y"))]);
        let n3 = expected_graph.add_node(e3);
        expected_graph.add_edge(n2, n3, "y".into());
        let e4 = call!(variable!("change"), vec![(None, variable!("y"))]);
        let n4 = expected_graph.add_node(e4);
        expected_graph.add_edge(n2, n4, "y".into());

        compare_graphs(expected, graph);
    }
    #[test]
    fn detects_mutations() {
        let input = vec![
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
        ];
        let graph = DependencyGraph::from_input(input.into_iter());

        let mut expected = DependencyGraph::new();
        let expected_graph = expected.graph_mut();
        let e1 = constant!("data frame");
        let n1 = expected_graph.add_node(e1);
        let e2 = call!(
            variable!("factor"),
            vec![(None, column!(variable!("x"), constant!("column")))]
        );
        let n2 = expected_graph.add_node(e2);
        expected_graph.add_edge(n1, n2, "x".into());
        let e3 = call!(variable!("summary"), vec![(None, variable!("x"))]);
        let n3 = expected_graph.add_node(e3);
        expected_graph.add_edge(n2, n3, "x".into());

        compare_graphs(expected, graph);
    }

    #[test]
    fn detects_sibling_dependencies() {
        let input = vec![
            assignment!(variable!("x"), vec![], constant!("data frame")),
            expression!(call!(
                variable!("factor"),
                vec![(None, column!(variable!("x"), constant!("column")))]
            )),
            expression!(call!(variable!("summary"), vec![(None, variable!("x"))])),
        ];
        let graph = DependencyGraph::from_input(input.into_iter());

        let mut expected = DependencyGraph::new();
        let expected_graph = expected.graph_mut();
        let e1 = constant!("data frame");
        let n1 = expected_graph.add_node(e1);
        let e2 = call!(
            variable!("factor"),
            vec![(None, column!(variable!("x"), constant!("column")))]
        );
        let n2 = expected_graph.add_node(e2);
        expected_graph.add_edge(n1, n2, "x".into());
        let e3 = call!(variable!("summary"), vec![(None, variable!("x"))]);
        let n3 = expected_graph.add_node(e3);
        expected_graph.add_edge(n1, n3, "x".into());

        compare_graphs(expected, graph);
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
    }
}

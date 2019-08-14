use std::collections::HashMap;

use petgraph;

use crate::parser::{RExp, RIdentifier, RStmt};

type NodeIndexType = petgraph::graph::DefaultIx;
pub type NodeIndex = petgraph::graph::NodeIndex<NodeIndexType>;
type Graph<'a> = petgraph::Graph<&'a RExp, String, petgraph::Directed, NodeIndexType>;

#[derive(Default, Debug)]
pub struct DependencyGraph<'a> {
    graph: Graph<'a>,
    map: HashMap<&'a RExp, NodeIndex>,
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

impl<'a> DependencyGraph<'a> {
    pub fn new() -> Self {
        DependencyGraph {
            graph: Graph::new(),
            map: HashMap::new(),
            variables: VariableMap::new(),
        }
    }

    pub fn parse(input: &'a[RStmt]) -> Self {
        let mut dependency_graph: DependencyGraph = DependencyGraph::new();

        for statement in input.iter() {
            parse_statement(statement, &mut dependency_graph);
        }

        dependency_graph
    }


    pub fn graph(&self) -> &Graph<'a> {
        &self.graph
    }

    pub fn graph_mut(&mut self) -> &mut Graph<'a> {
        &mut self.graph
    }

    pub fn id(&self, expression: &RExp) -> Option<NodeIndex> {
        self.map.get(expression).cloned()
    }

    pub fn graphviz(&self) -> petgraph::dot::Dot<&Graph<'a>>{
            petgraph::dot::Dot::new(&self.graph)
    }
}

fn parse_statement<'a>(
    statement: &'a RStmt,
    dependency_graph: &mut DependencyGraph<'a>,
) {
    use RStmt::*;
    match statement {
        Expression(expression) => {
            register_dependencies(expression, dependency_graph);
        }
        Assignment(left, additional, right) => {
            let mut assigned = vec![left];
            assigned.append(&mut additional.iter().collect());
            for variable in assigned {
                let node_id = register_dependencies(right, dependency_graph);
                match variable.extract_variable_name() {
                    Some(name) => dependency_graph.variables.push(name, node_id),
                    None => panic!(
                        "Could not find a variable in {}, in the left side of the assignment {}.",
                        variable, statement
                    ),
                };
            }
        }
        TailComment(statement, _) => parse_statement(statement, dependency_graph),
        _ => (),
    };
}

fn register_dependencies<'a>(
    expression: &'a RExp,
    dependency_graph: &mut DependencyGraph<'a>,
) -> NodeIndex {
    let dependencies = extract_dependencies(&expression);
    let node_id = dependency_graph.graph.add_node(expression);
    dependency_graph.map.insert(expression, node_id);
    for dependency in dependencies {
        if let Some(parent) = dependency_graph.variables.get(&dependency) {
            dependency_graph.graph.add_edge(*parent, node_id, dependency);
        }
        // Else, we do not know the variable. But this might still be valid,
        // e. g. if it is a library function that wasn't explicitly declared in the code.
        // Therefore, we simply ignore this in the dependency graph.
    }
    node_id
}

fn extract_dependencies(expression: &RExp) -> Vec<RIdentifier> {
    use RExp::*;
    match expression {
        Variable(name) => vec![name.clone()],
        Call(_, arguments) => arguments
            .iter()
            .flat_map(|(_, exp)| extract_dependencies(exp))
            .collect(),
        Column(left, _) => extract_dependencies(left),
        Index(left, _) => extract_dependencies(left),
        _ => vec![],
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;
    use std::collections::HashSet;

    use petgraph::visit::Walker;

    use super::*;
    use crate::parser::{RExp, RStmt};


    fn compare_graphs(expected: &DependencyGraph, actual: &DependencyGraph) {
        let expected = &expected.graph();
        let actual = &actual.graph();
        let walk_expected = petgraph::visit::Topo::new(expected);
        let walk_actual = petgraph::visit::Topo::new(actual);
        for (expected_id, actual_id) in walk_expected.iter(expected).zip(walk_actual.iter(actual)) {
            assert_eq!(
                expected
                    .neighbors(expected_id)
                    .flat_map(|n_id| expected.node_weight(n_id))
                    .cloned()
                    .collect::<HashSet<&RExp>>(),
                actual
                    .neighbors(actual_id)
                    .flat_map(|n_id| actual.node_weight(n_id))
                    .cloned()
                    .collect::<HashSet<&RExp>>(),
                "Nodes {:?} and {:?} have different neighbors.",
                expected.node_weight(expected_id),
                actual.node_weight(actual_id)
            );
        }
    }

    #[test]
    fn detects_linear_graph() {
        let input = vec![
            RStmt::Assignment(RExp::variable("x"), vec![], RExp::constant("1")),
            RStmt::Assignment(
                RExp::variable("y"),
                vec![],
                RExp::Call(
                    RExp::boxed_variable("transform"),
                    vec![(None, RExp::variable("x"))],
                ),
            ),
            RStmt::TailComment(
                Box::new(RStmt::Expression(RExp::Call(
                    RExp::boxed_variable("modify"),
                    vec![(None, RExp::variable("y"))],
                ))),
                "# modifies y".into(),
            ),
            RStmt::Expression(RExp::Call(
                RExp::boxed_variable("change"),
                vec![(None, RExp::variable("y"))],
            )),
        ];
        let graph = DependencyGraph::parse(&input);

        let mut expected = DependencyGraph::new();
        let expected_graph = expected.graph_mut();
        let e1 = RExp::constant("1");
        let n1 = expected_graph.add_node(&e1);
        let e2 = RExp::Call(
            RExp::boxed_variable("transform"),
            vec![(None, RExp::variable("x"))],
        );
        let n2 = expected_graph.add_node(&e2);
        expected_graph.add_edge(n1, n2, "x".into());
        let e3 = RExp::Call(
            RExp::boxed_variable("modify"),
            vec![(None, RExp::variable("y"))],
        );
        let n3 = expected_graph.add_node(&e3);
        expected_graph.add_edge(n2, n3, "y".into());
        let e4 = RExp::Call(
            RExp::boxed_variable("change"),
            vec![(None, RExp::variable("y"))],
        );
        let n4 = expected_graph.add_node(&e4);
        expected_graph.add_edge(n2, n4, "y".into());

        compare_graphs(&expected, &graph);
    }
    #[test]
    fn detects_mutations() {
        let input = vec![
            RStmt::Assignment(RExp::variable("x"), vec![], RExp::constant("data frame")),
            RStmt::Assignment(
                RExp::Column(
                    Box::new(RExp::variable("x")),
                    Box::new(RExp::constant("column")),
                ),
                vec![],
                RExp::Call(
                    RExp::boxed_variable("factor"),
                    vec![(
                        None,
                        RExp::Column(
                            Box::new(RExp::variable("x")),
                            Box::new(RExp::constant("column")),
                        ),
                    )],
                ),
            ),
            RStmt::Expression(RExp::Call(
                RExp::boxed_variable("summary"),
                vec![(None, RExp::variable("x"))],
            )),
        ];
        let graph = DependencyGraph::parse(&input);

        let mut expected = DependencyGraph::new();
        let expected_graph = expected.graph_mut();
        let e1 = RExp::constant("data frame");
        let n1 = expected_graph.add_node(&e1);
        let e2 = RExp::Call(
            RExp::boxed_variable("factor"),
            vec![(
                None,
                RExp::Column(
                    Box::new(RExp::variable("x")),
                    Box::new(RExp::constant("column")),
                ),
            )],
        );
        let n2 = expected_graph.add_node(&e2);
        expected_graph.add_edge(n1, n2, "x".into());
        let e3 = RExp::Call(
            RExp::boxed_variable("summary"),
            vec![(None, RExp::variable("x"))],
        );
        let n3 = expected_graph.add_node(&e3);
        expected_graph.add_edge(n2, n3, "x".into());

        compare_graphs(&expected, &graph);
    }

    #[test]
    fn detects_sibling_dependencies() {
        let input = vec![
            RStmt::Assignment(RExp::variable("x"), vec![], RExp::constant("data frame")),
            RStmt::Expression(RExp::Call(
                RExp::boxed_variable("factor"),
                vec![(
                    None,
                    RExp::Column(
                        Box::new(RExp::variable("x")),
                        Box::new(RExp::constant("column")),
                    ),
                )],
            )),
            RStmt::Expression(RExp::Call(
                RExp::boxed_variable("summary"),
                vec![(None, RExp::variable("x"))],
            )),
        ];
        let graph = DependencyGraph::parse(&input);

        let mut expected = DependencyGraph::new();
        let expected_graph = expected.graph_mut();
        let e1 = RExp::constant("data frame");
        let n1 = expected_graph.add_node(&e1);
        let e2 = RExp::Call(
            RExp::boxed_variable("factor"),
            vec![(
                None,
                RExp::Column(
                    Box::new(RExp::variable("x")),
                    Box::new(RExp::constant("column")),
                ),
            )],
        );
        let n2 = expected_graph.add_node(&e2);
        expected_graph.add_edge(n1, n2, "x".into());
        let e3 = RExp::Call(
            RExp::boxed_variable("summary"),
            vec![(None, RExp::variable("x"))],
        );
        let n3 = expected_graph.add_node(&e3);
        expected_graph.add_edge(n1, n3, "x".into());

        compare_graphs(&expected, &graph);
    }

    mod dependencies {
        use crate::parser::RExp;

        use super::super::extract_dependencies;

        #[test]
        fn finds_variables() {
            let expression = RExp::variable("x");
            let result = extract_dependencies(&expression);
            assert_eq!(vec!["x".to_string()], result);
        }

        #[test]
        fn finds_call_args() {
            let expression = RExp::Call(
                RExp::boxed_variable("func"),
                vec![
                    (None, RExp::constant("1")),
                    (None, RExp::variable("x")),
                    (None, RExp::variable("y")),
                ],
            );
            let result = extract_dependencies(&expression);
            assert_eq!(vec!["x".to_string(), "y".to_string()], result);
        }

        #[test]
        fn finds_column() {
            let expression = RExp::Column(
                Box::new(RExp::variable("x")),
                Box::new(RExp::constant("test")),
            );
            let result = extract_dependencies(&expression);
            assert_eq!(vec!["x".to_string()], result);
        }

        #[test]
        fn finds_column_in_call() {
            let expression = RExp::Call(
                RExp::boxed_variable("factor"),
                vec![(
                    None,
                    RExp::Column(
                        Box::new(RExp::variable("x")),
                        Box::new(RExp::constant("column")),
                    ),
                )],
            );
            let result = extract_dependencies(&expression);
            assert_eq!(vec!["x".to_string()], result);
        }

        #[test]
        fn finds_index() {
            let expression = RExp::Index(
                Box::new(RExp::variable("x")),
                vec![Some(RExp::constant("1"))],
            );
            let result = extract_dependencies(&expression);
            assert_eq!(vec!["x".to_string()], result);
        }
    }

    #[test]
    fn gives_index_for_expression() {
        let input = vec![
            RStmt::Assignment(RExp::variable("x"), vec![], RExp::constant("data frame")),
            RStmt::Assignment(
                RExp::Column(
                    Box::new(RExp::variable("x")),
                    Box::new(RExp::constant("column")),
                ),
                vec![],
                RExp::Call(
                    RExp::boxed_variable("factor"),
                    vec![(
                        None,
                        RExp::Column(
                            Box::new(RExp::variable("x")),
                            Box::new(RExp::constant("column")),
                        ),
                    )],
                ),
            ),
            RStmt::Expression(RExp::Call(
                RExp::boxed_variable("summary"),
                vec![(None, RExp::variable("x"))],
            )),
        ];
        let graph = DependencyGraph::parse(&input);

        let assert_expression_is_found = |index: usize| {
            let e = input[index].expression().unwrap();
            assert_eq!(Some(e), graph.id(e).map(|id| graph.graph[id]));
        };
        assert_expression_is_found(0);
        assert_expression_is_found(1);
        assert_expression_is_found(2);
        assert_eq!(None, graph.id(&RExp::variable("nonexistant")));
    }
}

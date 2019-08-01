use std::collections::HashMap;

use petgraph::Graph;

use crate::parser::{RExp, RIdentifier, RStmt};

type NodeIndexType = petgraph::graph::DefaultIx;
pub type NodeIndex = petgraph::graph::NodeIndex<NodeIndexType>;

#[derive(Default, Debug)]
pub struct DependencyGraph<'a> {
    graph: Graph<&'a RExp, (), petgraph::Directed, NodeIndexType>,
    map: HashMap<&'a RExp, NodeIndex>,
}

impl<'a> DependencyGraph<'a> {
    pub fn new() -> Self {
        DependencyGraph {
            graph: Graph::new(),
            map: HashMap::new(),
        }
    }

    pub fn graph(&self) -> &Graph<&'a RExp, (), petgraph::Directed, NodeIndexType> {
        &self.graph
    }

    pub fn graph_mut(&mut self) -> &mut Graph<&'a RExp, (), petgraph::Directed, NodeIndexType> {
        &mut self.graph
    }

    pub fn id(&self, expression: &RExp) -> Option<NodeIndex> {
        self.map.get(expression).cloned()
    }
}

pub fn parse_dependency_graph(input: &[RStmt]) -> DependencyGraph {
    let mut dependency_graph: DependencyGraph = DependencyGraph::new();
    let mut variables: HashMap<String, NodeIndex> = HashMap::new();

    for statement in input.into_iter() {
        use RStmt::*;
        match statement {
            Expression(expression) => {
                register_dependencies(expression, &mut dependency_graph, &mut variables);
            }
            Assignment(left, right) => {
                let node_id = register_dependencies(right, &mut dependency_graph, &mut variables);
                match extract_variable_name(left) {
                    Some(name) => variables.insert(name, node_id),
                    None => panic!(
                        "Could not find a variable in the left side of the assignment {:?}.",
                        statement
                    ),
                };
            }
            _ => (),
        };
    }

    dependency_graph
}

fn register_dependencies<'a>(
    expression: &'a RExp,
    dependency_graph: &mut DependencyGraph<'a>,
    variables: &mut HashMap<String, NodeIndex>,
) -> NodeIndex {
    let dependencies = extract_dependencies(&expression);
    let node_id = dependency_graph.graph.add_node(expression);
    dependency_graph.map.insert(expression, node_id);
    for dependency in dependencies {
        if let Some(parent) = variables.get(&dependency) {
            dependency_graph.graph.add_edge(*parent, node_id, ());
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

fn extract_variable_name(exp: &RExp) -> Option<RIdentifier> {
    use RExp::*;
    match exp {
        Variable(name) => Some(name.to_string()),
        Column(left, _) => extract_variable_name(&*left),
        Index(left, _) => extract_variable_name(&*left),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use petgraph::visit::Walker;

    use super::{parse_dependency_graph, DependencyGraph};
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
            RStmt::Assignment(RExp::variable("x"), RExp::constant("1")),
            RStmt::Assignment(
                RExp::variable("y"),
                RExp::Call("transform".into(), vec![(None, RExp::variable("x"))]),
            ),
            RStmt::Expression(RExp::Call(
                "modify".into(),
                vec![(None, RExp::variable("y"))],
            )),
            RStmt::Expression(RExp::Call(
                "change".into(),
                vec![(None, RExp::variable("y"))],
            )),
        ];
        let graph = parse_dependency_graph(&input);

        let mut expected = DependencyGraph::new();
        let expected_graph = expected.graph_mut();
        let e1 = RExp::constant("1");
        let n1 = expected_graph.add_node(&e1);
        let e2 = RExp::Call("transform".into(), vec![(None, RExp::variable("x"))]);
        let n2 = expected_graph.add_node(&e2);
        expected_graph.add_edge(n1, n2, ());
        let e3 = RExp::Call("modify".into(), vec![(None, RExp::variable("y"))]);
        let n3 = expected_graph.add_node(&e3);
        expected_graph.add_edge(n2, n3, ());
        let e4 = RExp::Call("change".into(), vec![(None, RExp::variable("y"))]);
        let n4 = expected_graph.add_node(&e4);
        expected_graph.add_edge(n2, n4, ());

        compare_graphs(&expected, &graph);
    }

    #[test]
    fn detects_mutations() {
        let input = vec![
            RStmt::Assignment(RExp::variable("x"), RExp::constant("data frame")),
            RStmt::Assignment(
                RExp::Column(
                    Box::new(RExp::variable("x")),
                    Box::new(RExp::constant("column")),
                ),
                RExp::Call(
                    "factor".into(),
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
                "summary".into(),
                vec![(None, RExp::variable("x"))],
            )),
        ];
        let graph = parse_dependency_graph(&input);

        let mut expected = DependencyGraph::new();
        let expected_graph = expected.graph_mut();
        let e1 = RExp::constant("data frame");
        let n1 = expected_graph.add_node(&e1);
        let e2 = RExp::Call(
            "factor".into(),
            vec![(
                None,
                RExp::Column(
                    Box::new(RExp::variable("x")),
                    Box::new(RExp::constant("column")),
                ),
            )],
        );
        let n2 = expected_graph.add_node(&e2);
        expected_graph.add_edge(n1, n2, ());
        let e3 = RExp::Call("summary".into(), vec![(None, RExp::variable("x"))]);
        let n3 = expected_graph.add_node(&e3);
        expected_graph.add_edge(n2, n3, ());

        compare_graphs(&expected, &graph);
    }

    #[test]
    fn detects_sibling_dependencies() {
        let input = vec![
            RStmt::Assignment(RExp::variable("x"), RExp::constant("data frame")),
            RStmt::Expression(RExp::Call(
                "factor".into(),
                vec![(
                    None,
                    RExp::Column(
                        Box::new(RExp::variable("x")),
                        Box::new(RExp::constant("column")),
                    ),
                )],
            )),
            RStmt::Expression(RExp::Call(
                "summary".into(),
                vec![(None, RExp::variable("x"))],
            )),
        ];
        let graph = parse_dependency_graph(&input);

        let mut expected = DependencyGraph::new();
        let expected_graph = expected.graph_mut();
        let e1 = RExp::constant("data frame");
        let n1 = expected_graph.add_node(&e1);
        let e2 = RExp::Call(
            "factor".into(),
            vec![(
                None,
                RExp::Column(
                    Box::new(RExp::variable("x")),
                    Box::new(RExp::constant("column")),
                ),
            )],
        );
        let n2 = expected_graph.add_node(&e2);
        expected_graph.add_edge(n1, n2, ());
        let e3 = RExp::Call("summary".into(), vec![(None, RExp::variable("x"))]);
        let n3 = expected_graph.add_node(&e3);
        expected_graph.add_edge(n1, n3, ());

        compare_graphs(&expected, &graph);
    }

    mod extracts_variable_name {
        use crate::parser::RExp;

        use super::super::extract_variable_name;

        #[test]
        fn from_variable() {
            let name = extract_variable_name(&RExp::variable("x"));
            assert_eq!(Some("x".to_string()), name);
        }

        #[test]
        fn from_column() {
            let name = extract_variable_name(&RExp::Column(
                Box::new(RExp::variable("x")),
                Box::new(RExp::variable("a")),
            ));
            assert_eq!(Some("x".to_string()), name);
        }

        #[test]
        fn from_index() {
            let name = extract_variable_name(&RExp::Index(
                Box::new(RExp::variable("x")),
                vec![Some(RExp::variable("a"))],
            ));
            assert_eq!(Some("x".to_string()), name);
        }

        #[test]
        fn rejects_constants() {
            let name = extract_variable_name(&RExp::constant("x"));
            assert_eq!(None, name);
        }

        #[test]
        fn rejects_constant_in_column() {
            let name = extract_variable_name(&RExp::Column(
                Box::new(RExp::constant("x")),
                Box::new(RExp::variable("a")),
            ));
            assert_eq!(None, name);
        }
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
                "func".into(),
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
                "factor".into(),
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
            RStmt::Assignment(RExp::variable("x"), RExp::constant("data frame")),
            RStmt::Assignment(
                RExp::Column(
                    Box::new(RExp::variable("x")),
                    Box::new(RExp::constant("column")),
                ),
                RExp::Call(
                    "factor".into(),
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
                "summary".into(),
                vec![(None, RExp::variable("x"))],
            )),
        ];
        let graph = parse_dependency_graph(&input);

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

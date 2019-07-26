use std::collections::HashMap;

use petgraph::Graph;

use crate::parser::{RExp, RIdentifier, RStmt};

type NodeIndexType = petgraph::graph::DefaultIx;
type NodeIndex = petgraph::graph::NodeIndex<NodeIndexType>;

pub type DependencyGraph = Graph<RExp, (), petgraph::Directed, NodeIndexType>;

pub fn parse_dependency_graph(input: Vec<RStmt>) -> DependencyGraph {
    let mut dependency_graph: DependencyGraph = Graph::new();
    let mut variables: HashMap<String, NodeIndex> = HashMap::new();

    for statement in input.into_iter() {
        use RStmt::*;
        match statement {
            Expression(expression) => {
                register_dependencies(expression, &mut dependency_graph, &mut variables);
            }
            Assignment(left, right) => {
                let node_id = register_dependencies(right, &mut dependency_graph, &mut variables);
                extract_variable_name(left).map(|name| {
                    variables.insert(name, node_id);
                    node_id
                });
            }
            _ => (),
        };

    }

    dependency_graph
}

fn register_dependencies(
    expression: RExp,
    dependency_graph: &mut DependencyGraph,
    variables: &mut HashMap<String, NodeIndex>,
) -> NodeIndex {
    let dependencies = extract_dependencies(&expression);
    let node_id = dependency_graph.add_node(expression);
    for dependency in dependencies {
        let parent = variables
            .get(&dependency)
            .unwrap_or_else(|| panic!("Use of undeclared variable {}.", dependency));
        dependency_graph.add_edge(*parent, node_id, ());
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

fn extract_variable_name(exp: RExp) -> Option<RIdentifier> {
    use RExp::*;
    match exp {
        Variable(name) => Some(name),
        Column(left, _) => extract_variable_name(*left),
        Index(left, _) => extract_variable_name(*left),
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
        let walk_expected = petgraph::visit::Topo::new(expected);
        let walk_actual = petgraph::visit::Topo::new(actual);
        for (expected_id, actual_id) in walk_expected.iter(expected).zip(walk_actual.iter(actual)) {
            assert_eq!(
                expected
                    .neighbors(expected_id)
                    .flat_map(|n_id| expected.node_weight(n_id))
                    .collect::<HashSet<&RExp>>(),
                actual
                    .neighbors(actual_id)
                    .flat_map(|n_id| actual.node_weight(n_id))
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
        let graph = parse_dependency_graph(input);

        let mut expected = DependencyGraph::new();
        let n1 = expected.add_node(RExp::constant("1"));
        let n2 = expected.add_node(RExp::Call(
            "transform".into(),
            vec![(None, RExp::variable("x"))],
        ));
        expected.add_edge(n1, n2, ());
        let n3 = expected.add_node(RExp::Call(
            "modify".into(),
            vec![(None, RExp::variable("y"))],
        ));
        expected.add_edge(n2, n3, ());
        let n4 = expected.add_node(RExp::Call(
            "change".into(),
            vec![(None, RExp::variable("y"))],
        ));
        expected.add_edge(n2, n4, ());

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
        let graph = parse_dependency_graph(input);

        let mut expected = DependencyGraph::new();
        let n1 = expected.add_node(RExp::constant("data frame"));
        let n2 = expected.add_node(RExp::Call(
            "factor".into(),
            vec![(
                None,
                RExp::Column(
                    Box::new(RExp::variable("x")),
                    Box::new(RExp::constant("column")),
                ),
            )],
        ));
        expected.add_edge(n1, n2, ());
        let n3 = expected.add_node(RExp::Call(
            "summary".into(),
            vec![(None, RExp::variable("x"))],
        ));
        expected.add_edge(n2, n3, ());

        compare_graphs(&expected, &graph);
    }

    mod extracts_variable_name {
        use crate::parser::RExp;

        use super::super::extract_variable_name;

        #[test]
        fn from_variable() {
            let name = extract_variable_name(RExp::variable("x"));
            assert_eq!(Some("x".to_string()), name);
        }

        #[test]
        fn from_column() {
            let name = extract_variable_name(RExp::Column(
                Box::new(RExp::variable("x")),
                Box::new(RExp::variable("a")),
            ));
            assert_eq!(Some("x".to_string()), name);
        }

        #[test]
        fn from_index() {
            let name = extract_variable_name(RExp::Index(
                Box::new(RExp::variable("x")),
                vec![Some(RExp::variable("a"))],
            ));
            assert_eq!(Some("x".to_string()), name);
        }

        #[test]
        fn rejects_constants() {
            let name = extract_variable_name(RExp::constant("x"));
            assert_eq!(None, name);
        }

        #[test]
        fn rejects_constant_in_column() {
            let name = extract_variable_name(RExp::Column(
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
        fn finds_index() {
            let expression = RExp::Index(
                Box::new(RExp::variable("x")),
                vec![Some(RExp::constant("1"))],
            );
            let result = extract_dependencies(&expression);
            assert_eq!(vec!["x".to_string()], result);
        }
    }
}

use std::collections::HashMap;

use crate::parser::{RExp, RIdentifier, RStmt};

#[derive(PartialEq, Debug, Clone)]
pub enum DependencyGraph {
    Node(RExp, Vec<DependencyGraph>),
}

pub fn parse_dependency_graph<'a>(input: Vec<RStmt>) -> Vec<DependencyGraph> {
    let mut result = vec![];
    let mut variables: HashMap<String, Box<DependencyGraph>> = HashMap::new();
    for statement in input.into_iter() {
        use RStmt::*;
        match statement {
            Assignment(left, right) => {
                let maybe_name = extract_variable_name(left);
                if let Some(name) = maybe_name {
                    let node = Box::new(DependencyGraph::Node(right, vec![]));
                    result.push(node.clone());
                    variables.insert(name, node);
                }
            }
            Expression(expression) => {
                result.push(Box::new(DependencyGraph::Node(expression, vec![])))
            }
            _ => (),
        }
    }
    result.into_iter().map(|graph_box| *graph_box).collect()
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
    use crate::parser::{RExp, RStmt};

    use super::{parse_dependency_graph, DependencyGraph};
    use DependencyGraph::*;

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
        let expected = vec![Node(
            RExp::constant("1"),
            vec![Node(
                RExp::Call("transform".into(), vec![(None, RExp::variable("x"))]),
                vec![
                    Node(
                        RExp::Call("modify".into(), vec![(None, RExp::variable("y"))]),
                        vec![],
                    ),
                    Node(
                        RExp::Call("change".into(), vec![(None, RExp::variable("y"))]),
                        vec![],
                    ),
                ],
            )],
        )];
        assert_eq!(expected, graph);
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
}

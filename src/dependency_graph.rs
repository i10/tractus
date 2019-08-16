use std::collections::HashMap;

use petgraph;

use crate::parser::{LineDisplay, RExpression, RIdentifier, RStatement, Span};

type NodeIndexType = petgraph::graph::DefaultIx;
pub type NodeIndex = petgraph::graph::NodeIndex<NodeIndexType>;
type Graph<'a, T> = petgraph::Graph<&'a RExpression<T>, String, petgraph::Directed, NodeIndexType>;

#[derive(Default, Debug)]
pub struct DependencyGraph<'a, T: Eq> {
    graph: Graph<'a, T>,
    map: HashMap<&'a RExpression<T>, NodeIndex>,
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

impl<'a, T: Eq> DependencyGraph<'a, T> {
    pub fn new() -> Self {
        DependencyGraph {
            graph: Graph::new(),
            map: HashMap::new(),
            variables: VariableMap::new(),
        }
    }

    pub fn from_input(input: impl Iterator<Item = &'a RStatement<T>>) -> Self {
        let mut graph = Self::new();
        graph.batch_insert(input);
        graph
    }

    pub fn batch_insert(&mut self, input: impl Iterator<Item = &'a RStatement<T>>) {
        for statement in input {
            self.insert(statement)
        }
    }

    fn insert(&mut self, statement: &'a RStatement<T>) {
        use RStatement::*;
        match statement {
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
            TailComment(statement, _, _) => self.insert(statement),
            _ => (),
        };
    }

    pub fn graph(&self) -> &Graph<'a, T> {
        &self.graph
    }

    pub fn graph_mut(&mut self) -> &mut Graph<'a, T> {
        &mut self.graph
    }

    pub fn id(&self, expression: &RExpression<T>) -> Option<NodeIndex> {
        self.map.get(expression).cloned()
    }
}

pub struct GraphLineDisplay<'a>(&'a DependencyGraph<'a, Span>);
impl<'a> From<&'a DependencyGraph<'a, Span>> for GraphLineDisplay<'a> {
    fn from(other: &'a DependencyGraph<'a, Span>) -> Self {
        GraphLineDisplay(other)
    }
}
impl<'a> std::fmt::Display for GraphLineDisplay<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let dependency_graph = self.0;
        let mapped = dependency_graph
            .graph()
            .map(|_, n| LineDisplay::from(*n), |_, e| e);
        let display_graph = petgraph::dot::Dot::new(&mapped);
        write!(f, "{}", display_graph)
    }
}

fn register_dependencies<'a, T: Eq>(
    expression: &'a RExpression<T>,
    dependency_graph: &mut DependencyGraph<'a, T>,
) -> NodeIndex {
    let dependencies = extract_dependencies(expression);
    let node_id = dependency_graph.graph.add_node(expression);
    dependency_graph.map.insert(expression, node_id);
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

fn extract_dependencies<T>(expression: &RExpression<T>) -> Vec<RIdentifier> {
    use RExpression::*;
    match expression {
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
    use std::fmt::Debug;

    use petgraph::visit::Walker;

    use super::*;

    fn compare_graphs<T: Eq + Debug>(expected: &DependencyGraph<T>, actual: &DependencyGraph<T>) {
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
                    .collect::<HashSet<&RExpression<T>>>(),
                actual
                    .neighbors(actual_id)
                    .flat_map(|n_id| actual.node_weight(n_id))
                    .cloned()
                    .collect::<HashSet<&RExpression<T>>>(),
                "Nodes {:?} and {:?} have different neighbors.",
                expected.node_weight(expected_id),
                actual.node_weight(actual_id)
            );
        }
    }

    #[test]
    fn detects_linear_graph() {
        let input = vec![
            RStatement::Assignment(
                RExpression::variable("x"),
                vec![],
                RExpression::constant("1"),
                (),
            ),
            RStatement::Assignment(
                RExpression::variable("y"),
                vec![],
                RExpression::Call(
                    RExpression::boxed_variable("transform"),
                    vec![(None, RExpression::variable("x"))],
                    (),
                ),
                (),
            ),
            RStatement::TailComment(
                Box::new(RStatement::Expression(
                    RExpression::Call(
                        RExpression::boxed_variable("modify"),
                        vec![(None, RExpression::variable("y"))],
                        (),
                    ),
                    (),
                )),
                "# modifies y".into(),
                (),
            ),
            RStatement::Expression(
                RExpression::Call(
                    RExpression::boxed_variable("change"),
                    vec![(None, RExpression::variable("y"))],
                    (),
                ),
                (),
            ),
        ];
        let graph = DependencyGraph::from_input(input.iter());

        let mut expected = DependencyGraph::new();
        let expected_graph = expected.graph_mut();
        let e1 = RExpression::constant("1");
        let n1 = expected_graph.add_node(&e1);
        let e2 = RExpression::Call(
            RExpression::boxed_variable("transform"),
            vec![(None, RExpression::variable("x"))],
            (),
        );
        let n2 = expected_graph.add_node(&e2);
        expected_graph.add_edge(n1, n2, "x".into());
        let e3 = RExpression::Call(
            RExpression::boxed_variable("modify"),
            vec![(None, RExpression::variable("y"))],
            (),
        );
        let n3 = expected_graph.add_node(&e3);
        expected_graph.add_edge(n2, n3, "y".into());
        let e4 = RExpression::Call(
            RExpression::boxed_variable("change"),
            vec![(None, RExpression::variable("y"))],
            (),
        );
        let n4 = expected_graph.add_node(&e4);
        expected_graph.add_edge(n2, n4, "y".into());

        compare_graphs(&expected, &graph);
    }
    #[test]
    fn detects_mutations() {
        let input = vec![
            RStatement::Assignment(
                RExpression::variable("x"),
                vec![],
                RExpression::constant("data frame"),
                (),
            ),
            RStatement::Assignment(
                RExpression::Column(
                    Box::new(RExpression::variable("x")),
                    Box::new(RExpression::constant("column")),
                    (),
                ),
                vec![],
                RExpression::Call(
                    RExpression::boxed_variable("factor"),
                    vec![(
                        None,
                        RExpression::Column(
                            Box::new(RExpression::variable("x")),
                            Box::new(RExpression::constant("column")),
                            (),
                        ),
                    )],
                    (),
                ),
                (),
            ),
            RStatement::Expression(
                RExpression::Call(
                    RExpression::boxed_variable("summary"),
                    vec![(None, RExpression::variable("x"))],
                    (),
                ),
                (),
            ),
        ];
        let graph = DependencyGraph::from_input(input.iter());

        let mut expected = DependencyGraph::new();
        let expected_graph = expected.graph_mut();
        let e1 = RExpression::constant("data frame");
        let n1 = expected_graph.add_node(&e1);
        let e2 = RExpression::Call(
            RExpression::boxed_variable("factor"),
            vec![(
                None,
                RExpression::Column(
                    Box::new(RExpression::variable("x")),
                    Box::new(RExpression::constant("column")),
                    (),
                ),
            )],
            (),
        );
        let n2 = expected_graph.add_node(&e2);
        expected_graph.add_edge(n1, n2, "x".into());
        let e3 = RExpression::Call(
            RExpression::boxed_variable("summary"),
            vec![(None, RExpression::variable("x"))],
            (),
        );
        let n3 = expected_graph.add_node(&e3);
        expected_graph.add_edge(n2, n3, "x".into());

        compare_graphs(&expected, &graph);
    }

    #[test]
    fn detects_sibling_dependencies() {
        let input = vec![
            RStatement::Assignment(
                RExpression::variable("x"),
                vec![],
                RExpression::constant("data frame"),
                (),
            ),
            RStatement::Expression(
                RExpression::Call(
                    RExpression::boxed_variable("factor"),
                    vec![(
                        None,
                        RExpression::Column(
                            Box::new(RExpression::variable("x")),
                            Box::new(RExpression::constant("column")),
                            (),
                        ),
                    )],
                    (),
                ),
                (),
            ),
            RStatement::Expression(
                RExpression::Call(
                    RExpression::boxed_variable("summary"),
                    vec![(None, RExpression::variable("x"))],
                    (),
                ),
                (),
            ),
        ];
        let graph = DependencyGraph::from_input(input.iter());

        let mut expected = DependencyGraph::new();
        let expected_graph = expected.graph_mut();
        let e1 = RExpression::constant("data frame");
        let n1 = expected_graph.add_node(&e1);
        let e2 = RExpression::Call(
            RExpression::boxed_variable("factor"),
            vec![(
                None,
                RExpression::Column(
                    Box::new(RExpression::variable("x")),
                    Box::new(RExpression::constant("column")),
                    (),
                ),
            )],
            (),
        );
        let n2 = expected_graph.add_node(&e2);
        expected_graph.add_edge(n1, n2, "x".into());
        let e3 = RExpression::Call(
            RExpression::boxed_variable("summary"),
            vec![(None, RExpression::variable("x"))],
            (),
        );
        let n3 = expected_graph.add_node(&e3);
        expected_graph.add_edge(n1, n3, "x".into());

        compare_graphs(&expected, &graph);
    }

    mod dependencies {
        use super::*;
        use pretty_assertions::assert_eq;

        #[test]
        fn finds_variables() {
            let expression = RExpression::variable("x");
            let result = extract_dependencies(&expression);
            assert_eq!(vec!["x".to_string()], result);
        }

        #[test]
        fn finds_call_args() {
            let expression = RExpression::Call(
                RExpression::boxed_variable("func"),
                vec![
                    (None, RExpression::constant("1")),
                    (None, RExpression::variable("x")),
                    (None, RExpression::variable("y")),
                ],
                (),
            );
            let result = extract_dependencies(&expression);
            assert_eq!(vec!["x".to_string(), "y".to_string()], result);
        }

        #[test]
        fn finds_column() {
            let expression = RExpression::Column(
                Box::new(RExpression::variable("x")),
                Box::new(RExpression::constant("test")),
                (),
            );
            let result = extract_dependencies(&expression);
            assert_eq!(vec!["x".to_string()], result);
        }

        #[test]
        fn finds_column_in_call() {
            let expression = RExpression::Call(
                RExpression::boxed_variable("factor"),
                vec![(
                    None,
                    RExpression::Column(
                        Box::new(RExpression::variable("x")),
                        Box::new(RExpression::constant("column")),
                        (),
                    ),
                )],
                (),
            );
            let result = extract_dependencies(&expression);
            assert_eq!(vec!["x".to_string()], result);
        }

        #[test]
        fn finds_index() {
            let expression = RExpression::Index(
                Box::new(RExpression::variable("x")),
                vec![Some(RExpression::constant("1"))],
                (),
            );
            let result = extract_dependencies(&expression);
            assert_eq!(vec!["x".to_string()], result);
        }
    }

    #[test]
    fn gives_index_for_expression() {
        let input = vec![
            RStatement::Assignment(
                RExpression::variable("x"),
                vec![],
                RExpression::constant("data frame"),
                (),
            ),
            RStatement::Assignment(
                RExpression::Column(
                    Box::new(RExpression::variable("x")),
                    Box::new(RExpression::constant("column")),
                    (),
                ),
                vec![],
                RExpression::Call(
                    RExpression::boxed_variable("factor"),
                    vec![(
                        None,
                        RExpression::Column(
                            Box::new(RExpression::variable("x")),
                            Box::new(RExpression::constant("column")),
                            (),
                        ),
                    )],
                    (),
                ),
                (),
            ),
            RStatement::Expression(
                RExpression::Call(
                    RExpression::boxed_variable("summary"),
                    vec![(None, RExpression::variable("x"))],
                    (),
                ),
                (),
            ),
        ];
        let graph = DependencyGraph::from_input(input.iter());

        let assert_expression_is_found = |index: usize| {
            let e = input[index].expression().unwrap();
            assert_eq!(Some(e), graph.id(e).map(|id| graph.graph[id]));
        };
        assert_expression_is_found(0);
        assert_expression_is_found(1);
        assert_expression_is_found(2);
        assert_eq!(None, graph.id(&RExpression::variable("nonexistant")));
    }
}

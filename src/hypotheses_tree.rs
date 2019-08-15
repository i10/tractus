use std::collections::{BTreeMap, HashMap, HashSet};
use std::hash::Hash;

use crate::dependency_graph;
use crate::hypotheses::{detect_hypotheses, Hypothesis};
use crate::parser::{RExpression, RStatement};
use dependency_graph::DependencyGraph;

type HypothesesMap<
    'a,
    T,
    HSet = std::collections::hash_map::RandomState,
    HMap = std::collections::hash_map::RandomState,
> = HashMap<&'a RExpression<T>, HashSet<Hypothesis<'a, T>, HSet>, HMap>;

pub fn parse_hypotheses_map<'a, T: Eq + Hash>(
    input: impl Iterator<Item = &'a RStatement<T>>,
) -> HypothesesMap<'a, T> {
    input
        .filter_map(|statement| statement.expression())
        .map(|expression| (expression, detect_hypotheses(expression)))
        .collect()
}

pub type HypothesisTree<'a, T> = BTreeMap<Option<&'a Hypothesis<'a, T>>, Vec<Node<'a, T>>>;

#[derive(Debug, PartialEq)]
pub struct Node<'a, T: Eq + Hash> {
    pub expression: &'a RExpression<T>,
    pub children: HypothesisTree<'a, T>,
}

pub fn parse_hypothesis_tree<'a, T: Eq + Hash, HSet, HMap>(
    hypotheses_map: &'a HypothesesMap<'a, T, HSet, HMap>,
    dependency_graph: &DependencyGraph<'a, T>,
) -> HypothesisTree<'a, T>
where
    HMap: std::hash::BuildHasher,
    HSet: std::hash::BuildHasher,
{
    let mut sources: Vec<dependency_graph::NodeIndex> = dependency_graph
        .graph()
        .externals(petgraph::Direction::Incoming)
        .collect();
    sources.sort_unstable(); // Node indices are ascending in source code order. I. e., this sorts by line number.
    hypothesis_tree_from_nodes(sources, &dependency_graph, &hypotheses_map)
}

pub fn hypothesis_tree_from_nodes<'a, T: Eq + Hash, SSet, SMap>(
    nodes: Vec<dependency_graph::NodeIndex>,
    dependency_graph: &DependencyGraph<'a, T>,
    hypotheses_map: &'a HashMap<&'a RExpression<T>, HashSet<Hypothesis<T>, SSet>, SMap>,
) -> HypothesisTree<'a, T>
where
    SSet: std::hash::BuildHasher,
    SMap: std::hash::BuildHasher,
{
    let mut tree = BTreeMap::new();
    for node_id in nodes.iter() {
        let graph = dependency_graph.graph();
        let expression = graph.node_weight(*node_id).unwrap(); // Node id is valid.
        let mut dependents: Vec<dependency_graph::NodeIndex> = graph.neighbors(*node_id).collect();
        dependents.sort_unstable(); // Sort by id, which sorts by line number.

        let hypothesis = hypotheses_map
            .get(expression)
            .and_then(|hypotheses| hypotheses.iter().next()); // Ignore all but first hypotheses.
                                                              // TODO: Add all hypotheses to tree.
        tree.entry(hypothesis).or_insert_with(|| vec![]).push(Node {
            expression,
            children: hypothesis_tree_from_nodes(dependents, dependency_graph, hypotheses_map),
        });
    }
    tree
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;
    use std::iter::FromIterator;

    use super::*;

    #[test]
    fn simple_hypothesis_tree() {
        let input = vec![
            RStatement::Assignment(
                RExpression::variable("kbd"),
                vec![],
                RExpression::constant("data frame"),
                (),
            ),
            RStatement::Assignment(
                RExpression::Column(
                    Box::new(RExpression::variable("kbd")),
                    Box::new(RExpression::constant("ParticipantID")),
                    (),
                ),
                vec![],
                RExpression::Call(
                    RExpression::boxed_variable("factor"),
                    vec![(
                        None,
                        RExpression::Column(
                            Box::new(RExpression::variable("kbd")),
                            Box::new(RExpression::constant("ParticipantID")),
                            (),
                        ),
                    )],
                    (),
                ),
                (),
            ),
            RStatement::Expression(
                RExpression::Call(
                    RExpression::boxed_variable("plot"),
                    vec![
                        (
                            None,
                            RExpression::TwoSidedFormula(
                                Box::new(RExpression::variable("Speed")),
                                Box::new(RExpression::variable("Layout")),
                                (),
                            ),
                        ),
                        (Some("data".into()), RExpression::variable("kbd")),
                    ],
                    (),
                ),
                (),
            ),
            RStatement::Expression(
                RExpression::Call(
                    RExpression::boxed_variable("summary"),
                    vec![(None, RExpression::variable("kbd"))],
                    (),
                ),
                (),
            ),
        ];

        let hypotheses_map = parse_hypotheses_map(input.iter());
        let dependency_graph = DependencyGraph::parse(&input);
        let tree = parse_hypothesis_tree(&hypotheses_map, &dependency_graph);
        // Need to build from the inside out.
        let n4 = Node {
            expression: input[3].expression().unwrap(),
            children: BTreeMap::new(),
        };
        let n3 = Node {
            expression: input[2].expression().unwrap(),
            children: BTreeMap::new(),
        };
        let speed = RExpression::variable("Speed");
        let layout = RExpression::variable("Layout");
        let hyp = Hypothesis {
            left: &speed,
            right: &layout,
        };
        let n2 = Node {
            expression: input[1].expression().unwrap(),
            children: BTreeMap::from_iter(vec![(Some(&hyp), vec![n3]), (None, vec![n4])]),
        };
        let n1 = Node {
            expression: input[0].expression().unwrap(),
            children: BTreeMap::from_iter(vec![(None, vec![n2])]),
        };
        let mut expected = BTreeMap::new();
        expected.insert(None, vec![n1]);

        assert_eq!(expected, tree);
    }
}

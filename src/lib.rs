#[macro_use]
extern crate pest_derive;

mod dependency_graph;
mod hypotheses;
mod parser;

use std::collections::{BTreeMap, HashMap, HashSet};

pub use crate::dependency_graph::DependencyGraph;
use crate::hypotheses::{detect_hypotheses, Hypothesis};
pub use crate::parser::{parse, RExp, RFormula, RStmt};

pub fn parse_hypotheses_map(input: &[RStmt]) -> HashMap<&RExp, HashSet<Hypothesis>> {
    input
        .iter()
        .filter_map(|statement| statement.expression())
        .map(|expression| (expression, detect_hypotheses(expression)))
        .collect()
}

pub fn parse_hypothesis_tree<'a, HSet, HMap>(
    hypotheses_map: &'a HashMap<&RExp, HashSet<Hypothesis<'a>, HSet>, HMap>,
    dependency_graph: &DependencyGraph<'a>,
) -> HypothesisTree<'a>
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

pub type HypothesisTree<'a> = BTreeMap<Option<Hypothesis<'a>>, Vec<Node<'a>>>;

#[derive(Debug, PartialEq)]
pub struct Node<'a> {
    pub expression: &'a RExp,
    pub children: HypothesisTree<'a>,
}

pub fn hypothesis_tree_from_nodes<'a, S, T>(
    nodes: Vec<dependency_graph::NodeIndex>,
    dependency_graph: &DependencyGraph<'a>,
    hypotheses_map: &'a HashMap<&'a RExp, HashSet<Hypothesis, T>, S>,
) -> HypothesisTree<'a>
where
    S: std::hash::BuildHasher,
    T: std::hash::BuildHasher,
{
    let mut tree = BTreeMap::new();
    for node_id in nodes.iter() {
        let graph = dependency_graph.graph();
        let expression = graph.node_weight(*node_id).unwrap(); // Node id is valid.
        let mut dependents: Vec<dependency_graph::NodeIndex> = graph.neighbors(*node_id).collect();
        dependents.sort_unstable(); // Sort by id, which sorts by line number.

        let hypothesis = hypotheses_map
            .get(expression)
            .and_then(|hypotheses| hypotheses.iter().cloned().next()); // Ignore all but first hypotheses.
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

    use crate::parser::RFormula;

    use super::*;

    #[test]
    fn simple_hypothesis_tree() {
        let input = vec![
            RStmt::Assignment(RExp::variable("kbd"), vec![], RExp::constant("data frame")),
            RStmt::Assignment(
                RExp::Column(
                    Box::new(RExp::variable("kbd")),
                    Box::new(RExp::constant("ParticipantID")),
                ),
                vec![],
                RExp::Call(
                    RExp::boxed_variable("factor"),
                    vec![(
                        None,
                        RExp::Column(
                            Box::new(RExp::variable("kbd")),
                            Box::new(RExp::constant("ParticipantID")),
                        ),
                    )],
                ),
            ),
            RStmt::Expression(RExp::Call(
                RExp::boxed_variable("plot"),
                vec![
                    (
                        None,
                        RExp::Formula(RFormula::TwoSided(
                            Box::new(RExp::variable("Speed")),
                            Box::new(RExp::variable("Layout")),
                        )),
                    ),
                    (Some("data".into()), RExp::variable("kbd")),
                ],
            )),
            RStmt::Expression(RExp::Call(
                RExp::boxed_variable("summary"),
                vec![(None, RExp::variable("kbd"))],
            )),
        ];

        let hypotheses_map = parse_hypotheses_map(&input);
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
        let speed = RExp::variable("Speed");
        let layout = RExp::variable("Layout");
        let hyp = Hypothesis {
            left: &speed,
            right: &layout,
        };
        let n2 = Node {
            expression: input[1].expression().unwrap(),
            children: BTreeMap::from_iter(vec![(Some(hyp), vec![n3]), (None, vec![n4])]),
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

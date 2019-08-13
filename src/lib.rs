#[macro_use]
extern crate pest_derive;

mod dependency_graph;
mod hypotheses;
mod parser;

use std::collections::{BTreeMap, HashMap, HashSet};

use crate::dependency_graph::{parse_dependency_graph, DependencyGraph};

use crate::hypotheses::{detect_hypotheses, Hypothesis};
pub use crate::parser::{RExp, RFormula, RStmt};

#[derive(Debug)]
pub struct Tractus(Vec<RStmt>);

impl Tractus {
    pub fn parse(code: &str) -> Result<Self, parser::Error> {
        parser::parse(code).map(Tractus)
    }

    pub fn from(parsed: Vec<RStmt>) -> Self {
        Tractus(parsed)
    }

    pub fn get_statement(&self, index: usize) -> Option<&RStmt> {
        self.0.get(index)
    }

    pub fn generate_hypothesis_tree(&self) -> HypothesisTree {
        let hypotheses_map: HashMap<&RExp, HashSet<Hypothesis>> = self
            .0
            .iter()
            .filter_map(|statement| statement.expression())
            .map(|expression| (expression, detect_hypotheses(expression)))
            .collect();
        let dependency_graph = parse_dependency_graph(&self.0);
        let mut sources: Vec<dependency_graph::NodeIndex> = dependency_graph
            .graph()
            .externals(petgraph::Direction::Incoming)
            .collect();
        sources.sort_unstable(); // Node indices are ascending in source code order. I. e., this sorts by line number.
        hypothesis_tree_from_nodes(sources, &dependency_graph, &hypotheses_map)
    }
}

pub type HypothesisTree<'a> = BTreeMap<Option<Hypothesis>, Vec<Node<'a>>>;

#[derive(Debug, PartialEq)]
pub struct Node<'a> {
    pub expression: &'a RExp,
    pub children: HypothesisTree<'a>,
}

pub fn hypothesis_tree_from_nodes<'a, S, T>(
    nodes: Vec<dependency_graph::NodeIndex>,
    dependency_graph: &DependencyGraph<'a>,
    hypotheses_map: &HashMap<&'a RExp, HashSet<Hypothesis, T>, S>,
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

        let register_under = |hypothesis: Option<Hypothesis>| {
            tree.entry(hypothesis).or_insert_with(|| vec![]).push(Node {
                expression,
                children: hypothesis_tree_from_nodes(dependents, dependency_graph, hypotheses_map),
            });
        };
        match hypotheses_map.get(expression) {
            None => register_under(None),
            Some(hypotheses) => register_under(hypotheses.iter().cloned().next()), // Ignore all but first hypotheses.
                                                                                   // TODO: Add all hypotheses to tree.
        }
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
                    "factor".into(),
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
                "plot".into(),
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
                "summary".into(),
                vec![(None, RExp::variable("kbd"))],
            )),
        ];

        let tractus = Tractus::from(input);
        let tree = tractus.generate_hypothesis_tree();
        // Need to build from the inside out.
        let n4 = Node {
            expression: tractus.get_statement(3).unwrap().expression().unwrap(),
            children: BTreeMap::new(),
        };
        let n3 = Node {
            expression: tractus.get_statement(2).unwrap().expression().unwrap(),
            children: BTreeMap::new(),
        };
        let hyp = Hypothesis {
            left: "Speed".into(),
            right: RExp::variable("Layout"),
        };
        let n2 = Node {
            expression: tractus.get_statement(1).unwrap().expression().unwrap(),
            children: BTreeMap::from_iter(vec![(Some(hyp), vec![n3]), (None, vec![n4])]),
        };
        let n1 = Node {
            expression: tractus.get_statement(0).unwrap().expression().unwrap(),
            children: BTreeMap::from_iter(vec![(None, vec![n2])]),
        };
        let mut expected = BTreeMap::new();
        expected.insert(None, vec![n1]);

        assert_eq!(expected, tree);
    }
}

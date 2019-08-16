use std::cell::RefCell;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::rc::Rc;

use petgraph::Direction;
use serde::{Serialize};

use crate::dependency_graph;
use crate::hypotheses::{detect_hypotheses, Hypothesis};
use crate::parser::{RExpression, RStatement};
use dependency_graph::{DependencyGraph, NodeIndex};

#[derive(Debug, Serialize)]
pub struct HypothesisTree<'a, T: Eq> {
    root: Branches<'a, T>,
    hypotheses: HashMap<HypothesesId, Hypotheses>,
}

pub struct HypothesesMap(Vec<(Hypotheses)>);
impl HypothesesMap {
    pub fn new() -> Self {
        HypothesesMap(Vec::new())
    }

    pub fn insert(&mut self, item: Hypotheses) -> HypothesesId {
        match self.0.iter().position(|hyp| hyp == &item) {
            Some(index) => index,
            None => {
                self.0.push(item);
                self.0.len() - 1 // The id of the just inserted item.
            }
        }
    }

    pub fn get(&mut self, id: HypothesesId) -> Option<&mut Hypotheses> {
        if id < self.0.len() {
            Some(&mut self.0[id])
        } else {
            None
        }
    }

    pub fn into_map(self) -> HashMap<HypothesesId, Hypotheses> {
        self.0.into_iter().enumerate().collect()
    }
}

pub type HypothesesId = usize;

pub type Branches<'a, T> = BTreeMap<HypothesesId, Vec<Node<'a, T>>>;

#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct Node<'a, T: Eq> {
    pub expression: &'a RExpression<T>,
    pub children: Branches<'a, T>,
}

#[derive(Debug)]
struct RefNode {
    id: NodeIndex,
    children: BTreeMap<HypothesesId, Vec<Rc<RefCell<RefNode>>>>,
}

fn convert<'a, T: Eq>(ref_node: RefNode, dependency_graph: &DependencyGraph<'a, T>) -> Node<'a, T> {
    Node {
        expression: dependency_graph.graph()[ref_node.id],
        children: ref_node
            .children
            .into_iter()
            .map(|(h, r)| {
                (
                    h,
                    r.into_iter()
                        .map(|n| convert(Rc::try_unwrap(n).unwrap().into_inner(), dependency_graph))
                        .collect(),
                )
            })
            .collect(),
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Serialize)]
pub struct Hypotheses(HashSet<Hypothesis>);

impl std::cmp::Ord for Hypotheses {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self.0.len().cmp(&other.0.len()) {
            std::cmp::Ordering::Equal => {
                let mut mine: Vec<&String> = self.0.iter().collect();
                mine.sort_unstable();
                let mut others: Vec<&String> = other.0.iter().collect();
                others.sort_unstable();
                mine.cmp(&others)
            }
            ord => ord,
        }
    }
}

impl PartialOrd for Hypotheses {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

pub fn parse_hypothesis_tree<'a, T: Eq>(
    input: impl Iterator<Item = &'a RStatement<T>>,
    dependency_graph: &DependencyGraph<'a, T>,
) -> HypothesisTree<'a, T> {
    let mut root: BTreeMap<HypothesesId, Vec<Rc<RefCell<RefNode>>>> = BTreeMap::new();
    let mut expression_map: HashMap<&'a RExpression<T>, HypothesesId> = HashMap::new();
    let mut hypotheses_map: HypothesesMap = HypothesesMap::new();
    let mut node_map: HashMap<NodeIndex, Rc<RefCell<RefNode>>> = HashMap::new();

    let expressions = input.filter_map(|statement| statement.expression());
    for expression in expressions {
        collect_hypotheses(
            expression,
            &mut hypotheses_map,
            &mut expression_map,
            &dependency_graph,
        );
        let hypotheses_id = expression_map.get(expression).unwrap(); // This expression's hypotheses were just collected.
        let node_id = dependency_graph.id(expression).unwrap(); // Expression must be inside dependency graph.
        let ref_node = Rc::new(RefCell::new(RefNode {
            id: node_id,
            children: BTreeMap::new(),
        }));
        let mut parents: Vec<NodeIndex> = dependency_graph
            .graph()
            .neighbors_directed(node_id, Direction::Incoming)
            .collect();
        parents.sort_unstable();
        match parents.last() {
            Some(id) => {
                let parent_ref = Rc::clone(node_map.get(&id).unwrap()); // Parent must be in node_map.
                let mut parent = parent_ref.borrow_mut();
                parent
                    .children
                    .entry(*hypotheses_id)
                    .or_insert_with(|| vec![])
                    .push(ref_node.clone());
                node_map.insert(node_id, ref_node);
            }
            None => {
                root.entry(*hypotheses_id)
                    .or_insert_with(|| vec![])
                    .push(ref_node.clone());
                node_map.insert(node_id, ref_node);
            }
        }
    }

    drop(node_map);

    HypothesisTree {
        root: root
            .into_iter()
            .map(|(h, ids)| {
                (
                    h,
                    ids.into_iter()
                        .map(|id| {
                            convert(Rc::try_unwrap(id).unwrap().into_inner(), dependency_graph)
                        })
                        .collect(),
                )
            })
            .collect(),
        hypotheses: hypotheses_map.into_map(),
    }
}

fn collect_hypotheses<'a, T: Eq>(
    expression: &'a RExpression<T>,
    hypotheses_map: &mut HypothesesMap,
    expression_map: &mut HashMap<&'a RExpression<T>, HypothesesId>,
    dependency_graph: &DependencyGraph<'a, T>,
) {
    let node_id = dependency_graph.id(expression).unwrap(); // Expression must be inside dependency graph.
    let inherited_hypotheses: Vec<Hypothesis> = dependency_graph
        .graph()
        .neighbors_directed(node_id, Direction::Incoming)
        .map(|id| {
            let exp = dependency_graph.graph()[id];
            expression_map
                .get(exp)
                .map(|id| {
                    hypotheses_map
                        .get(*id)
                        .unwrap()
                        .0
                        .iter()
                        .cloned()
                        .collect::<Vec<Hypothesis>>()
                })
                .unwrap_or_else(|| Hypotheses(detect_hypotheses(exp)).0.into_iter().collect())
        })
        .flatten()
        .collect();
    let hypotheses_id = match expression_map.get(expression) {
        Some(id) => {
            let id = *id;
            let hypotheses = hypotheses_map.get(id).unwrap();
            for hyp in inherited_hypotheses {
                hypotheses.0.insert(hyp);
            }
            id
        }
        None => {
            let mut hypotheses = Hypotheses(detect_hypotheses(expression));
            for hyp in inherited_hypotheses {
                hypotheses.0.insert(hyp);
            }
            hypotheses_map.insert(hypotheses)
        }
    };
    expression_map.insert(expression, hypotheses_id);
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

        let dependency_graph = DependencyGraph::parse(&input);
        let tree = parse_hypothesis_tree(input.iter(), &dependency_graph);

        // Need to build from the inside out.
        let n4 = Node {
            expression: input[3].expression().unwrap(),
            children: BTreeMap::new(),
        };
        let n3 = Node {
            expression: input[2].expression().unwrap(),
            children: BTreeMap::new(),
        };
        let n2 = Node {
            expression: input[1].expression().unwrap(),
            children: BTreeMap::from_iter(vec![
                (find_hyp(&["Speed ~ Layout"], &tree), vec![n3]),
                (find_hyp(&[] ,&tree), vec![n4]),
            ]),
        };
        let n1 = Node {
            expression: input[0].expression().unwrap(),
            children: BTreeMap::from_iter(vec![(find_hyp(&[], &tree), vec![n2])]),
        };
        let mut expected = BTreeMap::new();
        expected.insert(find_hyp(&[], &tree), vec![n1]);

        assert_eq!(expected, tree.root);
    }

    fn find_hyp<'a,T:Eq>(hyp: &[&'static str], tree: &HypothesisTree<'a, T>) -> HypothesesId {
        let hypotheses = hyp.iter().map(|h| h.to_string()).collect::<HashSet<String>>();
        *tree.hypotheses.iter().find(|(_, other)| other.0 == hypotheses).expect("Could not find hypotheses.").0
    }
}

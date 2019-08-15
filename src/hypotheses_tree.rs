use std::cell::RefCell;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::rc::Rc;

use petgraph::Direction;

use crate::dependency_graph;
use crate::hypotheses::{detect_hypotheses, Hypothesis};
use crate::parser::{RExpression, RStatement};
use dependency_graph::{DependencyGraph, NodeIndex};

pub type HypothesisTree<'a, T> = BTreeMap<Hypotheses, Vec<Node<'a, T>>>;

#[derive(Debug, PartialEq, Eq)]
pub struct Node<'a, T: Eq> {
    pub expression: &'a RExpression<T>,
    pub children: HypothesisTree<'a, T>,
}

#[derive(Debug)]
struct RefNode {
    id: NodeIndex,
    children: BTreeMap<Hypotheses, Vec<Rc<RefCell<RefNode>>>>,
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

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Hypotheses(HashSet<Hypothesis>);

impl Hypotheses {
    pub fn set(&self) -> &HashSet<Hypothesis> {
        &self.0
    }
}

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
    let mut root: BTreeMap<Hypotheses, Vec<Rc<RefCell<RefNode>>>> = BTreeMap::new();
    let mut hypotheses_map = HashMap::new();
    let mut node_map: HashMap<NodeIndex, Rc<RefCell<RefNode>>> = HashMap::new();

    let expressions = input.filter_map(|statement| statement.expression());
    for expression in expressions {
        collect_hypotheses(expression, &mut hypotheses_map, &dependency_graph);
        let hypotheses = hypotheses_map.get(expression).unwrap(); // This expression's hypotheses were just collected.
        let node_id = dependency_graph.id(expression).unwrap(); // Expression must be inside dependency graph.
        let ref_node = Rc::new(RefCell::new(RefNode {
            id: node_id,
            children: BTreeMap::new(),
        }));
        let mut parents: Vec<NodeIndex> = dependency_graph
            .graph()
            .neighbors_directed(node_id, Direction::Incoming).collect();
        parents.sort_unstable();
        match parents.last() {
            Some(id) => {
                let parent_ref = Rc::clone(node_map.get(&id).unwrap()); // Parent must be in node_map.
                let mut parent = parent_ref.borrow_mut();
                parent
                    .children
                    .entry(hypotheses.clone())
                    .or_insert_with(|| vec![])
                    .push(ref_node.clone());
                node_map.insert(node_id, ref_node);
            }
            None => {
                root.entry(hypotheses.clone())
                    .or_insert_with(|| vec![])
                    .push(ref_node.clone());
                node_map.insert(node_id, ref_node);
            }
        }
    }

    drop(node_map);

    root.into_iter()
        .map(|(h, ids)| {
            (
                h,
                ids.into_iter()
                    .map(|id| convert(Rc::try_unwrap(id).unwrap().into_inner(), dependency_graph))
                    .collect(),
            )
        })
        .collect()
}

fn collect_hypotheses<'a, T: Eq>(
    expression: &'a RExpression<T>,
    hypotheses_map: &mut HashMap<&'a RExpression<T>, Hypotheses>,
    dependency_graph: &DependencyGraph<'a, T>,
) {
    let node_id = dependency_graph.id(expression).unwrap(); // Expression must be inside dependency graph.
    let inherited_hypotheses: Vec<Hypothesis> = dependency_graph
        .graph()
        .neighbors_directed(node_id, Direction::Incoming)
        .map(|id| {
            let exp = dependency_graph.graph()[id];
            let inherited_hypotheses = hypotheses_map
                .entry(exp)
                .or_insert_with(|| Hypotheses(detect_hypotheses(exp)));
            inherited_hypotheses
                .0
                .iter()
                .cloned()
                .collect::<Vec<Hypothesis>>()
        })
        .flatten()
        .collect();
    let own_hypotheses = hypotheses_map
        .entry(expression)
        .or_insert_with(|| Hypotheses(detect_hypotheses(expression)));
    for hyp in inherited_hypotheses {
        own_hypotheses.0.insert(hyp);
    }
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
        let mut hyp = HashSet::new();
        hyp.insert("Speed ~ Layout".to_string());
        let n2 = Node {
            expression: input[1].expression().unwrap(),
            children: BTreeMap::from_iter(vec![
                (Hypotheses(hyp), vec![n3]),
                (Hypotheses(HashSet::new()), vec![n4]),
            ]),
        };
        let n1 = Node {
            expression: input[0].expression().unwrap(),
            children: BTreeMap::from_iter(vec![(Hypotheses(HashSet::new()), vec![n2])]),
        };
        let mut expected = BTreeMap::new();
        expected.insert(Hypotheses(HashSet::new()), vec![n1]);

        assert_eq!(expected, tree);
    }
}

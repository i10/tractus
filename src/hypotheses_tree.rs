use std::cell::RefCell;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::fmt::Debug;
use std::rc::Rc;

use petgraph::Direction;
use serde::Serialize;

use crate::dependency_graph;
use crate::hypotheses::{detect_hypotheses, Hypothesis};
use crate::parser::{LineDisplay, RExpression, Span};
use dependency_graph::{DependencyGraph, NodeIndex};

#[derive(Debug)]
pub struct HypothesisTree<T> {
    root: Branches<Rc<RExpression<T>>>,
    hypotheses: BTreeMap<HypothesesId, Hypotheses>,
}

#[derive(Serialize)]
pub struct LineTree<'a, T: Serialize> {
    root: Branches<T>,
    hypotheses: &'a BTreeMap<HypothesesId, Hypotheses>,
}

impl<'a> LineTree<'a, String> {
    pub fn with_span(other: &'a HypothesisTree<Span>) -> Self {
        LineTree {
            root: map_branches(&other.root, &mut |e| format!("{}", LineDisplay::from(e))),
            hypotheses: &other.hypotheses,
        }
    }
}

impl<'a, T: Serialize> LineTree<'a, T> {
    pub fn with<U, F>(other: &'a HypothesisTree<U>, mut mapping: &mut F) -> Self
    where
        F: FnMut(&Rc<RExpression<U>>) -> T,
    {
        LineTree {
            root: map_branches(&other.root, &mut mapping),
            hypotheses: &other.hypotheses,
        }
    }
}

#[derive(Default)]
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

    pub fn into_map(self) -> BTreeMap<HypothesesId, Hypotheses> {
        self.0.into_iter().enumerate().collect()
    }
}

pub type HypothesesId = usize;

pub type Branches<C> = BTreeMap<HypothesesId, Vec<Node<C>>>;

#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct Node<C> {
    #[serde(rename = "expression")]
    pub content: C,
    #[serde(rename = "hypotheses")]
    pub children: Branches<C>,
}

fn map_branches<C, N, F: FnMut(&C) -> N>(branches: &Branches<C>, mapping: &mut F) -> Branches<N> {
    branches
        .iter()
        .map(|(id, children)| (*id, children.iter().map(|n| map_node(n, mapping)).collect()))
        .collect()
}

fn map_node<C, N, F: FnMut(&C) -> N>(node: &Node<C>, mapping: &mut F) -> Node<N> {
    Node {
        content: mapping(&node.content),
        children: map_branches(&node.children, mapping),
    }
}

#[derive(Debug)]
struct RefNode {
    id: NodeIndex,
    children: BTreeMap<HypothesesId, Vec<Rc<RefCell<RefNode>>>>,
}

fn convert<T: Eq>(
    ref_node: RefNode,
    dependency_graph: &DependencyGraph<T>,
) -> Node<Rc<RExpression<T>>> {
    Node {
        content: dependency_graph.graph()[ref_node.id].clone(),
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
pub struct Hypotheses(BTreeSet<Hypothesis>);

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

pub fn parse_hypothesis_tree<T: Eq + Clone>(
    dependency_graph: &DependencyGraph<T>,
) -> HypothesisTree<T> {
    let mut root: BTreeMap<HypothesesId, Vec<Rc<RefCell<RefNode>>>> = BTreeMap::new();
    let mut expression_map: HashMap<NodeIndex, HypothesesId> = HashMap::new();
    let mut hypotheses_map: HypothesesMap = HypothesesMap::new();
    let mut node_map: HashMap<NodeIndex, Rc<RefCell<RefNode>>> = HashMap::new();

    let ids = dependency_graph.lines();
    for id in ids {
        let expression = dependency_graph.id(id).unwrap();
        collect_hypotheses(
            id,
            &mut hypotheses_map,
            &mut expression_map,
            &dependency_graph,
        );
        let hypotheses_id = expression_map.get(&id).unwrap(); // This expression's hypotheses were just collected.
        let ref_node = Rc::new(RefCell::new(RefNode {
            id,
            children: BTreeMap::new(),
        }));
        let mut parents: Vec<NodeIndex> = dependency_graph
            .graph()
            .neighbors_directed(id, Direction::Incoming)
            .collect();
        parents.sort_unstable();
        match parents.last() {
            Some(parent_id) => {
                let parent_ref = Rc::clone(node_map.get(&parent_id).unwrap()); // Parent must be in node_map.
                let mut parent = parent_ref.borrow_mut();
                parent
                    .children
                    .entry(*hypotheses_id)
                    .or_insert_with(|| vec![])
                    .push(ref_node.clone());
            }
            None => {
                root.entry(*hypotheses_id)
                    .or_insert_with(|| vec![])
                    .push(ref_node.clone());
            }
        }
        node_map.insert(id, ref_node);
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

fn collect_hypotheses<T: Eq + Clone>(
    id: NodeIndex,
    hypotheses_map: &mut HypothesesMap,
    expression_map: &mut HashMap<NodeIndex, HypothesesId>,
    dependency_graph: &DependencyGraph<T>,
) {
    let inherited_hypotheses: Vec<Hypothesis> = dependency_graph
        .graph()
        .neighbors_directed(id, Direction::Incoming)
        .map(|id| {
            expression_map
                .get(&id)
                .map(|id| {
                    hypotheses_map
                        .get(*id)
                        .unwrap()
                        .0
                        .iter()
                        .cloned()
                        .collect::<Vec<Hypothesis>>()
                })
                .unwrap_or_else(|| {
                    let inlined_exp = dependency_graph.inline_id(id).unwrap();
                    Hypotheses(detect_hypotheses(&inlined_exp))
                        .0
                        .into_iter()
                        .collect()
                })
        })
        .flatten()
        .collect();
    let hypotheses_id = match expression_map.get(&id) {
        Some(id) => {
            let id = *id;
            let hypotheses = hypotheses_map.get(id).unwrap();
            for hyp in inherited_hypotheses {
                hypotheses.0.insert(hyp);
            }
            id
        }
        None => {
            let inlined_exp = dependency_graph.inline_id(id).unwrap();
            let mut hypotheses = Hypotheses(detect_hypotheses(&inlined_exp));
            for hyp in inherited_hypotheses {
                hypotheses.0.insert(hyp);
            }
            hypotheses_map.insert(hypotheses)
        }
    };
    expression_map.insert(id, hypotheses_id);
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;
    use std::iter::FromIterator;

    use super::*;
    use crate::parser::{RExpression, RStatement};
    use crate::{
        assignment, call, column, constant, expression, index, infix, two_sided_formula, variable,
    };

    #[test]
    fn simple_hypothesis_tree() {
        let input: Vec<Rc<RStatement<()>>> = vec![
            assignment!(variable!("kbd"), vec![], constant!("data frame")),
            assignment!(
                column!(variable!("kbd"), constant!("ParticipantID")),
                vec![],
                call!(
                    variable!("factor"),
                    vec![(None, column!(variable!("kbd"), constant!("ParticipantID")))]
                )
            ),
            expression!(call!(
                variable!("plot"),
                vec![
                    (
                        None,
                        two_sided_formula!(variable!("Speed"), variable!("Layout")),
                    ),
                    (Some("data".into()), variable!("kbd")),
                ]
            )),
            expression!(call!(variable!("summary"), vec![(None, variable!("kbd"))])),
        ];

        let dependency_graph = DependencyGraph::from_input(input.iter().cloned());
        let tree = parse_hypothesis_tree(&dependency_graph);

        // Need to build from the inside out.
        let n4 = Node {
            content: input[3].expression().unwrap().clone(),
            children: BTreeMap::new(),
        };
        let n3 = Node {
            content: input[2].expression().unwrap().clone(),
            children: BTreeMap::new(),
        };
        let n2 = Node {
            content: input[1].expression().unwrap().clone(),
            children: BTreeMap::from_iter(vec![
                (find_hyp(&["Speed ~ Layout"], &tree), vec![n3]),
                (find_hyp(&[], &tree), vec![n4]),
            ]),
        };
        let n1 = Node {
            content: input[0].expression().unwrap().clone(),
            children: BTreeMap::from_iter(vec![(find_hyp(&[], &tree), vec![n2])]),
        };
        let mut expected = BTreeMap::new();
        expected.insert(find_hyp(&[], &tree), vec![n1]);

        assert_eq!(expected, tree.root);
    }

    #[test]
    fn referred_hypothesis() {
        let input: Vec<Rc<RStatement<()>>> = vec![
            //kbd = var[var$independent == "level",]
            assignment!(
                variable!("kbd"),
                vec![],
                index!(
                    variable!("var"),
                    vec![
                        Some(infix!(
                            "==",
                            column!(variable!("var"), variable!("independent")),
                            constant!("\"level\"")
                        )),
                        None
                    ]
                )
            ),
            // kbd$dependent
            expression!(column!(variable!("kbd"), variable!("dependent"))),
        ];

        let dependency_graph = DependencyGraph::from_input(input.iter().cloned());
        let tree = parse_hypothesis_tree(&dependency_graph);

        // Need to build from the inside out.
        let n2 = Node {
            content: input[1].expression().unwrap().clone(),
            children: BTreeMap::from_iter(vec![]),
        };
        let n1 = Node {
            content: input[0].expression().unwrap().clone(),
            children: BTreeMap::from_iter(vec![(
                find_hyp(&["dependent ~ independent"], &tree),
                vec![n2],
            )]),
        };
        let mut expected = BTreeMap::new();
        expected.insert(find_hyp(&[], &tree), vec![n1]);

        assert_eq!(expected, tree.root);
    }

    fn find_hyp<E: Eq>(hyp: &[&'static str], tree: &HypothesisTree<E>) -> HypothesesId {
        let hypotheses = hyp
            .iter()
            .map(|h| h.to_string())
            .collect::<BTreeSet<String>>();
        *tree
            .hypotheses
            .iter()
            .find(|(_, other)| other.0 == hypotheses)
            .unwrap_or_else(|| panic!("Could not find hypotheses {:?} in actual tree.", hyp))
            .0
    }
}

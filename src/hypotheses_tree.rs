use std::collections::{BTreeMap, BTreeSet, HashMap};

use serde::{Deserialize, Serialize};

use crate::dependency_graph;
use crate::hypotheses::{detect_hypotheses, Hypothesis};
use crate::parser::{StatementId, Statements};
use dependency_graph::DependencyGraph;

#[derive(Debug, Serialize, Deserialize)]
pub struct HypothesisTree {
    root: Branches<StatementId>,
    hypotheses: BTreeMap<HypothesesId, Hypotheses>,
}

#[derive(Serialize, Debug)]
pub struct DisplayTree<T> {
    root: Branches<T>,
    hypotheses: BTreeMap<HypothesesId, Hypotheses>,
}

impl<T> DisplayTree<T>
where
    T: Serialize,
{
    pub fn with<F>(other: &HypothesisTree, mut mapping: &mut F) -> Self
    where
        F: FnMut(&StatementId) -> T,
    {
        Self {
            root: map_branches(&other.root, &mut mapping),
            hypotheses: other.hypotheses.clone(),
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

    pub fn get(&self, id: HypothesesId) -> Option<&Hypotheses> {
        if id < self.0.len() {
            Some(&self.0[id])
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

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Node<C> {
    pub content: C,
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

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
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

type RefBranches = BTreeMap<HypothesesId, Vec<StatementId>>;
type NodeMap = HashMap<StatementId, (HypothesesId, RefBranches)>;

pub fn parse_hypothesis_tree<T>(
    stmts: &Statements<T>,
    dependency_graph: &DependencyGraph,
) -> HypothesisTree {
    let mut roots: RefBranches = BTreeMap::new();
    let mut hypotheses_map: HypothesesMap = HypothesesMap::new();
    let mut node_map: NodeMap = HashMap::new();

    for (stmt_id, stmt, _) in stmts.iter() {
        if let Some(_expression) = stmt.expression() {
            let hyp_id = collect_hypotheses(
                stmt_id,
                &node_map,
                &mut hypotheses_map,
                &dependency_graph,
                &stmts,
            );
            node_map.insert(stmt_id, (hyp_id, BTreeMap::new()));

            let mut parents: Vec<StatementId> = dependency_graph.parents(stmt_id);
            parents.sort_unstable();
            match parents.last() {
                Some(parent_id) => {
                    let parent = &mut node_map.get_mut(&parent_id).unwrap().1; // Parent has already been analyzed and is thus in node map.
                    parent.entry(hyp_id).or_insert_with(Vec::new).push(stmt_id);
                }
                None => {
                    roots.entry(hyp_id).or_insert_with(Vec::new).push(stmt_id);
                }
            }
        }
    }

    HypothesisTree {
        root: roots
            .iter()
            .map(|(hyp_id, stmts)| {
                (
                    *hyp_id,
                    stmts.iter().map(|id| flatten(*id, &node_map)).collect(),
                )
            })
            .collect(),
        hypotheses: hypotheses_map.into_map(),
    }
}

fn flatten(id: StatementId, node_map: &NodeMap) -> Node<StatementId> {
    let children = node_map[&id]
        .1
        .iter()
        .map(|(hyp_id, subs)| {
            (
                *hyp_id,
                subs.iter()
                    .map(|child_id| flatten(*child_id, &node_map))
                    .collect(),
            )
        })
        .collect();
    Node {
        content: id,
        children,
    }
}

fn collect_hypotheses<T>(
    id: StatementId,
    node_map: &NodeMap,
    hypotheses_map: &mut HypothesesMap,
    dependency_graph: &DependencyGraph,
    stmts: &Statements<T>,
) -> HypothesesId {
    let inherited_hypotheses: Vec<Hypothesis> = dependency_graph
        .parents(id)
        .iter()
        .map(|id| {
            let hypotheses_id = node_map[&id].0; // Parents are already analyzed and thus in the node map.
            hypotheses_map
                .get(hypotheses_id)
                .unwrap()
                .0
                .iter()
                .cloned()
                .collect::<Vec<Hypothesis>>()
        })
        .flatten()
        .collect();

    let inlined_exp = dependency_graph.inline_id(id, stmts).unwrap();
    let mut hypotheses = Hypotheses(detect_hypotheses(&inlined_exp));
    for hyp in inherited_hypotheses {
        hypotheses.0.insert(hyp);
    }
    hypotheses_map.insert(hypotheses)
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;
    use std::iter::FromIterator;

    use super::*;
    use crate::parser::{Expression, Statement};
    use crate::{
        assignment, call, column, constant, expression, index, infix, two_sided_formula, variable,
    };

    #[test]
    fn simple_hypothesis_tree() {
        let input = Statements::from_iter(vec![
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
        ]);

        let ids: Vec<StatementId> = input.iter().map(|(id, _, _)| id).collect();
        let dependency_graph = DependencyGraph::from_input(&input);
        let tree = parse_hypothesis_tree(&input, &dependency_graph);

        // Need to build from the inside out.
        let n4 = Node {
            content: ids[3],
            children: BTreeMap::new(),
        };
        let n3 = Node {
            content: ids[2],
            children: BTreeMap::new(),
        };
        let n2 = Node {
            content: ids[1],
            children: BTreeMap::from_iter(vec![
                (find_hyp(&["Speed ~ Layout"], &tree), vec![n3]),
                (find_hyp(&[], &tree), vec![n4]),
            ]),
        };
        let n1 = Node {
            content: ids[0],
            children: BTreeMap::from_iter(vec![(find_hyp(&[], &tree), vec![n2])]),
        };
        let mut expected = BTreeMap::new();
        expected.insert(find_hyp(&[], &tree), vec![n1]);

        assert_eq!(expected, tree.root);
    }

    #[test]
    fn referred_hypothesis() {
        let input = Statements::from_iter(vec![
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
        ]);

        let ids: Vec<StatementId> = input.iter().map(|(id, _, _)| id).collect();
        let dependency_graph = DependencyGraph::from_input(&input);
        let tree = parse_hypothesis_tree(&input, &dependency_graph);

        // Need to build from the inside out.
        let n2 = Node {
            content: ids[1],
            children: BTreeMap::from_iter(vec![]),
        };
        let n1 = Node {
            content: ids[0],
            children: BTreeMap::from_iter(vec![(
                find_hyp(&["dependent ~ independent"], &tree),
                vec![n2],
            )]),
        };
        let mut expected = BTreeMap::new();
        expected.insert(find_hyp(&[], &tree), vec![n1]);

        assert_eq!(expected, tree.root);
    }

    fn find_hyp(hyp: &[&'static str], tree: &HypothesisTree) -> HypothesesId {
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

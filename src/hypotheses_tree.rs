use std::collections::{BTreeMap, HashMap};
use std::iter::FromIterator;

use serde::{Deserialize, Serialize};

use crate::dependency_graph;
use crate::hypotheses::{detect_hypotheses, Hypotheses, Hypothesis};
use crate::parser::{StatementId, Statements};
use dependency_graph::DependencyGraph;

/// A tree grouping `Statement`s by their hypotheses.
#[derive(Debug, Serialize, Deserialize)]
pub struct HypothesisTree<T> {
    root: Branches<T>,
    hypotheses: BTreeMap<HypothesesId, Hypotheses>,
}

/// The branches of a `HypothesisTree`, grouped by hypotheses.
#[derive(Debug, Serialize, PartialEq, Eq, Deserialize)]
pub struct Branches<C>(pub BTreeMap<HypothesesId, Vec<Node<C>>>);

impl<C> Branches<C> {
    /// Consume the `Branches` and return them with mapping applied to all `Node` contents.
    fn into_map<N, F>(self, mapping: &mut F) -> Branches<N>
    where
        F: FnMut(C) -> N,
    {
        self.0
            .into_iter()
            .map(|(id, children)| {
                (
                    id,
                    children.into_iter().map(|n| n.into_map(mapping)).collect(),
                )
            })
            .collect()
    }
}

impl<C> FromIterator<(HypothesesId, Vec<Node<C>>)> for Branches<C> {
    fn from_iter<I: IntoIterator<Item = (HypothesesId, Vec<Node<C>>)>>(other: I) -> Self {
        Branches(other.into_iter().collect())
    }
}

pub type HypothesesId = usize;

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Node<C> {
    pub content: C,
    pub children: Branches<C>,
}

impl<C> Node<C> {
    fn into_map<N, F>(self, mapping: &mut F) -> Node<N>
    where
        F: FnMut(C) -> N,
    {
        Node {
            content: mapping(self.content),
            children: self.children.into_map(mapping),
        }
    }
}

impl<T> HypothesisTree<T> {
    pub fn map<F, M>(self, mut mapping: &mut F) -> HypothesisTree<M>
    where
        F: FnMut(T) -> M,
    {
        HypothesisTree {
            root: self.root.into_map(&mut mapping),
            hypotheses: self.hypotheses,
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

type NodeMap = HashMap<StatementId, (HypothesesId, HashMap<HypothesesId, Vec<StatementId>>)>;

impl HypothesisTree<StatementId> {
    pub fn new<T>(stmts: &Statements<T>, dependency_graph: &DependencyGraph) -> Self {
        let mut roots: HashMap<HypothesesId, Vec<StatementId>> = HashMap::new();
        let mut hypotheses_map: HypothesesMap = HypothesesMap::new();
        let mut node_map: NodeMap = HashMap::new();

        for (stmt_id, stmt, _) in stmts.iter() {
            if let Some(_expression) = stmt.expression() {
                let hyp_id = Self::collect_hypotheses(
                    stmt_id,
                    &node_map,
                    &mut hypotheses_map,
                    &dependency_graph,
                    &stmts,
                );
                node_map.insert(stmt_id, (hyp_id, HashMap::new()));

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
                .into_iter()
                .map(|(hyp_id, children)| {
                    (
                        hyp_id,
                        children
                            .into_iter()
                            .map(|stmt_id| Self::flatten(stmt_id, &mut node_map))
                            .collect(),
                    )
                })
                .collect(),
            hypotheses: hypotheses_map.into_map(),
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
                    .iter()
                    .cloned()
                    .collect::<Vec<Hypothesis>>()
            })
            .flatten()
            .collect();

        let inlined_exp = dependency_graph.inline_id(id, stmts).unwrap();
        let mut hypotheses = detect_hypotheses(&inlined_exp);
        for hyp in inherited_hypotheses {
            hypotheses.insert(hyp);
        }
        hypotheses_map.insert(hypotheses)
    }

    fn flatten(id: StatementId, node_map: &mut NodeMap) -> Node<StatementId> {
        let branches = node_map.remove(&id).unwrap().1; // id has to be in map.
        let children = branches
            .into_iter()
            .map(|(hyp_id, subs)| {
                (
                    hyp_id,
                    subs.into_iter()
                        .map(|child_id| Self::flatten(child_id, node_map))
                        .collect(),
                )
            })
            .collect();
        Node {
            content: id,
            children,
        }
    }
}

impl<T> HypothesisTree<T> {
    pub fn into_map<F, N>(self, mut mapping: F) -> HypothesisTree<N>
    where
        F: FnMut(T) -> N,
    {
        HypothesisTree {
            root: self.root.into_map(&mut mapping),
            hypotheses: self.hypotheses,
        }
    }
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
        let tree = HypothesisTree::new(&input, &dependency_graph);

        // Need to build from the inside out.
        let n4 = Node {
            content: ids[3],
            children: Branches::from_iter(vec![]),
        };
        let n3 = Node {
            content: ids[2],
            children: Branches::from_iter(vec![]),
        };
        let n2 = Node {
            content: ids[1],
            children: Branches::from_iter(vec![
                (find_hyp(&["Speed ~ Layout"], &tree), vec![n3]),
                (find_hyp(&[], &tree), vec![n4]),
            ]),
        };
        let n1 = Node {
            content: ids[0],
            children: Branches::from_iter(vec![(find_hyp(&[], &tree), vec![n2])]),
        };
        let expected = Branches::from_iter(vec![(find_hyp(&[], &tree), vec![n1])]);

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
        let tree = HypothesisTree::new(&input, &dependency_graph);

        // Need to build from the inside out.
        let n2 = Node {
            content: ids[1],
            children: Branches::from_iter(vec![]),
        };
        let n1 = Node {
            content: ids[0],
            children: Branches::from_iter(vec![(
                find_hyp(&["dependent ~ independent"], &tree),
                vec![n2],
            )]),
        };
        let expected = Branches::from_iter(vec![(find_hyp(&[], &tree), vec![n1])]);

        assert_eq!(expected, tree.root);
    }

    fn find_hyp(hyp: &[&'static str], tree: &HypothesisTree<StatementId>) -> HypothesesId {
        let hypotheses = hyp.iter().map(|h| h.to_string()).collect::<Hypotheses>();
        *tree
            .hypotheses
            .iter()
            .find(|(_, other)| other == &&hypotheses)
            .unwrap_or_else(|| panic!("Could not find hypotheses {:?} in actual tree.", hyp))
            .0
    }
}

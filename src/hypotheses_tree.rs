use std::collections::{BTreeMap, HashMap};
use std::iter::FromIterator;

use serde::{Deserialize, Serialize};

use crate::dependency_graph;
use crate::hypotheses::{detect_hypotheses, Hypotheses, Hypothesis};
use crate::parser::{Statement, StatementId, Statements};
use dependency_graph::DependencyGraph;

/// A tree grouping `Statement`s by their hypotheses.
#[derive(Debug, Serialize, Deserialize)]
pub struct HypothesisTree<T> {
    root: Branches<T, BlockId>,
    hypotheses: BTreeMap<HypothesesId, Hypotheses>,
    blocks: Vec<Vec<StatementId>>,
}

/// The branches of a `HypothesisTree`, grouped by hypotheses.
#[derive(Debug, Serialize, PartialEq, Eq, Deserialize)]
pub struct Branches<C, H>(pub BTreeMap<HypothesesId, Vec<Node<C, H>>>);

impl<C, H> Branches<C, H> {
    fn new() -> Self {
        Branches(BTreeMap::new())
    }

    /// Consume the `Branches` and return them with mapping applied to all `Node` contents.
    fn into_map<N, F>(self, mapping: &mut F) -> Branches<N, H>
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

impl<C, H> FromIterator<(HypothesesId, Vec<Node<C, H>>)> for Branches<C, H> {
    fn from_iter<I: IntoIterator<Item = (HypothesesId, Vec<Node<C, H>>)>>(other: I) -> Self {
        Branches(other.into_iter().collect())
    }
}

pub type HypothesesId = usize;

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum Node<C, H> {
    Single {
        content: C,
        children: Branches<C, H>,
    },
    Group {
        header: H,
        elements: Vec<Node<C, H>>,
    },
}

impl<C, H> Node<C, H> {
    fn new(content: C) -> Self {
        Node::Single {
            content,
            children: Branches::new(),
        }
    }

    fn into_map<N, F>(self, mapping: &mut F) -> Node<N, H>
    where
        F: FnMut(C) -> N,
    {
        use Node::*;
        match self {
            Single { content, children } => Single {
                content: mapping(content),
                children: children.into_map(mapping),
            },
            Group { header, elements } => Group {
                header,
                elements: elements.into_iter().map(|n| n.into_map(mapping)).collect(),
            },
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

type NodeMap = HashMap<
    StatementId,
    (
        HypothesesId,
        HashMap<HypothesesId, Vec<RefNode>>,
        Option<BlockId>,
    ),
>;

type BlockId = usize;

#[derive(Clone, Debug)]
enum RefNode {
    Statement(StatementId),
    Group(BlockId, Vec<RefNode>),
}

impl HypothesisTree<StatementId> {
    /// Creates a new `HypothesisTree` based on the passed information.
    ///
    /// Requires all elements of `stmts` to be tracked in the `dependency_graph`.
    pub fn new<T>(stmts: &Statements<T>, dependency_graph: &DependencyGraph) -> Self {
        let mut roots: HashMap<HypothesesId, Vec<RefNode>> = HashMap::new();
        let mut hypotheses_map: HypothesesMap = HypothesesMap::new();
        let mut node_map: NodeMap = HashMap::new();

        let mut blocks: Vec<Vec<StatementId>> = Vec::new();
        let mut next_block_comment: Option<Vec<StatementId>> = None;
        let mut block_index: Option<BlockId> = None;

        for (stmt_id, stmt, _) in stmts.iter() {
            if let Statement::Comment(_) = stmt {
                next_block_comment
                    .get_or_insert_with(Vec::new)
                    .push(stmt_id);
            } else if let Statement::Empty = stmt {
                match &mut next_block_comment {
                    Some(block) => block.push(stmt_id),
                    None => block_index = None, // Prevent new statements from being added to block.
                }
            } else if let Some(_expression) = stmt.expression() {
                if let Some(next) = next_block_comment.take() {
                    blocks.push(next);
                    block_index = Some(blocks.len() - 1);
                }

                let hyp_id = Self::collect_hypotheses(
                    stmt_id,
                    &node_map,
                    &mut hypotheses_map,
                    &dependency_graph,
                    &stmts,
                );
                node_map.insert(stmt_id, (hyp_id, HashMap::new(), block_index));

                let mut parents: Vec<StatementId> = dependency_graph.parents(stmt_id);
                parents.sort_unstable();
                match parents.last() {
                    Some(parent_id) => {
                        let (_, parent_children, parent_block) =
                            &mut node_map.get_mut(&parent_id).unwrap(); // Parent has already been analyzed and is thus in node map.

                        let siblings = parent_children.entry(hyp_id).or_insert_with(Vec::new);
                        if parent_block != &block_index {
                            // If parent is in same block, we do not create a new one.
                            if let Some(b_idx) = block_index {
                                if !siblings.is_empty() {
                                    let last_index = siblings.len() - 1;
                                    let predecessor = &mut siblings[last_index];
                                    if let RefNode::Group(b, others) = predecessor {
                                        if b == &b_idx {
                                            others.push(RefNode::Statement(stmt_id));
                                        } else {
                                            siblings.push(RefNode::Group(
                                                b_idx,
                                                vec![RefNode::Statement(stmt_id)],
                                            ));
                                        }
                                    } else {
                                        siblings.push(RefNode::Group(
                                            b_idx,
                                            vec![RefNode::Statement(stmt_id)],
                                        ));
                                    }
                                } else {
                                    siblings.push(RefNode::Group(
                                        b_idx,
                                        vec![RefNode::Statement(stmt_id)],
                                    ));
                                }
                            }
                        } else {
                            siblings.push(RefNode::Statement(stmt_id));
                        }
                    }
                    None => {
                        let node_id = match block_index {
                            Some(block_idx) => {
                                RefNode::Group(block_idx, vec![RefNode::Statement(stmt_id)])
                            }
                            None => RefNode::Statement(stmt_id),
                        };
                        roots.entry(hyp_id).or_insert_with(Vec::new).push(node_id);
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
                            .map(|node| Self::flatten(node, &mut node_map))
                            .collect(),
                    )
                })
                .collect(),
            hypotheses: hypotheses_map.into_map(),
            blocks,
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

    fn flatten(node: RefNode, node_map: &mut NodeMap) -> Node<StatementId, BlockId> {
        use RefNode::*;
        match node {
            Statement(stmt_id) => {
                let branches = node_map.remove(&stmt_id).unwrap().1; // id has to be in map.
                let children = branches
                    .into_iter()
                    .map(|(hyp_id, subs)| {
                        (
                            hyp_id,
                            subs.into_iter()
                                .map(|child| Self::flatten(child, node_map))
                                .collect(),
                        )
                    })
                    .collect();
                Node::Single {
                    content: stmt_id,
                    children,
                }
            }
            Group(block_id, elements) => Node::Group {
                header: block_id,
                elements: elements
                    .into_iter()
                    .map(|n| Self::flatten(n, node_map))
                    .collect(),
            },
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
            blocks: self.blocks,
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
        let n4 = Node::Single {
            content: ids[3],
            children: Branches::from_iter(vec![]),
        };
        let n3 = Node::Single {
            content: ids[2],
            children: Branches::from_iter(vec![]),
        };
        let n2 = Node::Single {
            content: ids[1],
            children: Branches::from_iter(vec![
                (find_hyp(&["Speed ~ Layout"], &tree), vec![n3]),
                (find_hyp(&[], &tree), vec![n4]),
            ]),
        };
        let n1 = Node::Single {
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
        let n2 = Node::Single {
            content: ids[1],
            children: Branches::from_iter(vec![]),
        };
        let n1 = Node::Single {
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

    // TODO: Test blocks/groups
}

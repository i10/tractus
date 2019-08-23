#[macro_use]
extern crate pest_derive;

use std::rc::Rc;

pub mod dependency_graph;
pub mod hypotheses;
pub mod hypotheses_tree;
pub mod parser;

pub use crate::dependency_graph::{DependencyGraph, GraphLineDisplay};
pub use crate::hypotheses_tree::{parse_hypothesis_tree, HypothesisTree, LineTree};
pub use crate::parser::{
    Expression, LineDisplay, Parsed, RExpression, RStatement, Span, Statement,
};

pub struct Tractus {
    dependency_graph: DependencyGraph<Span>,
    result: HypothesisTree<Span>,
}

impl Tractus {
    pub fn new() -> Self {
        let dependency_graph = DependencyGraph::new();
        let tree = parse_hypothesis_tree(&dependency_graph);
        Tractus {
            dependency_graph,
            result: tree,
        }
    }

    pub fn parse_lines<S: AsRef<str>>(&mut self, lines: Vec<S>) -> Result<Vec<Rc<Statement>>, parser::Error> {
        let mut parsed = Parsed::new();
        for line in lines {
            // TODO: Does not retry lines.
            parsed.append(line.as_ref())?;
        }

        self.dependency_graph
            .batch_insert(parsed.clone().into_iter());

        Ok(parsed.into_iter().collect())
    }

    pub fn hypotheses_tree(&mut self) -> LineTree<String> {
        self.result = parse_hypothesis_tree(&self.dependency_graph);
        LineTree::with_span(&self.result)
    }
}

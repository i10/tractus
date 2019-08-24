#[macro_use]
extern crate pest_derive;

use std::rc::Rc;

use log::{debug, trace};
use serde::{Deserialize, Serialize};

pub mod dependency_graph;
pub mod hypotheses;
pub mod hypotheses_tree;
pub mod parser;

pub use crate::dependency_graph::{DependencyGraph, GraphLineDisplay};
pub use crate::hypotheses_tree::{parse_hypothesis_tree, HypothesisTree, LineTree};
pub use crate::parser::{
    Expression, LineDisplay, Parsed, RExpression, RStatement, Span, Statement,
};

#[derive(Serialize, Deserialize)]
pub struct Tractus {
    line_count: usize,
    unparsed: Vec<String>,
    dependency_graph: DependencyGraph<Span>,
    result: HypothesisTree<Span>,
}

impl Tractus {
    pub fn new() -> Self {
        let dependency_graph = DependencyGraph::new();
        let tree = parse_hypothesis_tree(&dependency_graph);
        Tractus {
            line_count: 0,
            unparsed: Vec::new(),
            dependency_graph,
            result: tree,
        }
    }

    pub fn parse_lines<S: AsRef<str>>(
        &mut self,
        lines: Vec<S>,
    ) -> Result<Vec<Rc<Statement>>, parser::Error> {
        let mut parsed = vec![];
        for line in lines.iter() {
            self.line_count += 1;
            self.unparsed.push(line.as_ref().to_string()); // Push to unparsed, such that all currently unparsed lines are treated together.
            let parse_result = Parsed::parse_stmts(&self.unparsed.join("\n"));
            match parse_result {
                Ok(stmts) => {
                    parsed.append(
                        &mut stmts
                            .into_iter()
                            .map(|stmt| {
                                stmt.map(&mut |span| {
                                    let mut span = span.clone();
                                    span.shift_line(self.line_count);
                                    span
                                })
                            })
                            .collect(),
                    );
                    self.unparsed.clear(); // We have parsed everything successfully.
                }
                Err(e) => {
                    // If the parsing error occurred at the very last symbol,
                    // we assume that it is simply incomplete and will try again when we have more input.
                    if let pest::error::InputLocation::Pos(pos) = e.location {
                        trace!(
                            "Error position is {}, last position is {}.",
                            pos,
                            line.as_ref().len()
                        );
                        if pos == line.as_ref().len() {
                            debug!("Will retry with more input.");
                            // Current line is already pushed to self.unparsed, so it will be retried on next iteration.
                        }
                    }
                    debug!("Skipping this input.");
                    self.unparsed.clear(); // We determined that there is an error in the currently unparsed code.
                }
            }
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

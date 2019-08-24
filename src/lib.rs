#[macro_use]
extern crate pest_derive;

use std::rc::Rc;
use std::ops::Deref;

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
    dependency_graph: DependencyGraph<(Span, String, Vec<String>)>,
    result: HypothesisTree<(Span, String, Vec<String>)>,
}

#[derive(Serialize, Deserialize)]
pub struct ExpressionMeta {
    expression: String,
    span: Span,
    statement: String,
    assigned_variables: Vec<String>,
    function_name: Option<String>,
}

impl From<&Rc<RExpression<(Span, String, Vec<String>)>>> for ExpressionMeta {
    fn from(other: &Rc<RExpression<(Span, String, Vec<String>)>>) -> Self {
        let meta = other.get_meta();
        let function_name = extract_function_name(&other);
        ExpressionMeta {
            expression: format!("{}", other),
            span: meta.0.clone(),
            statement: meta.1.clone(),
            assigned_variables: meta.2.clone(),
            function_name,
        }
    }
}

fn extract_function_name<T>(expression: &Rc<parser::RExpression<T>>) -> Option<String> {
    use parser::RExpression::*;
    match expression.deref() {
        Call(name, _, _) => name.extract_variable_name(),
        Column(left, _, _) => extract_function_name(&left),
        _ => None,
    }
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
    ) -> Result<Vec<Rc<RStatement<(Span, String, Vec<String>)>>>, parser::Error>{
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
                                let stmt_display = format!("{}", stmt);
                                let assigned_variables =
                                    if let parser::RStatement::Assignment(left, add, _, _) =
                                        stmt.deref()
                                    {
                                        let mut vs = vec![format!("{}", left)];
                                        let mut addition: Vec<String> =
                                            add.iter().map(|v| format!("{}", v)).collect();
                                        vs.append(&mut addition);
                                        vs
                                    } else {
                                        vec![]
                                    };
                                stmt.map(&mut |span| {
                                    let mut span = span.clone();
                                    span.shift_line(self.line_count);
                                    (span, stmt_display.clone(), assigned_variables.clone())
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

    pub fn hypotheses_tree(&mut self) -> LineTree<ExpressionMeta> {
        self.result = parse_hypothesis_tree(&self.dependency_graph);
        LineTree::with(&self.result, &mut |exp| {
            ExpressionMeta::from(exp)
        })
    }
}

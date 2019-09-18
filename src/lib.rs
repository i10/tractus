#[macro_use]
extern crate pest_derive;

use std::collections::HashMap;

use serde::{Deserialize, Serialize};
use serde_json::json;

pub mod dependency_graph;
pub mod hypotheses;
pub mod hypotheses_tree;
pub mod parser;

pub use crate::dependency_graph::DependencyGraph;
pub use crate::hypotheses_tree::HypothesisTree;
pub use crate::parser::{Expression, LineSpan, Parsed, RIdentifier, Statement, StatementId};

#[derive(Serialize, Deserialize, Default)]
pub struct Tractus {
    parsed: Parsed<(LineSpan, serde_json::Value)>,
    dependency_graph: DependencyGraph,
}

#[derive(Serialize, Deserialize)]
pub struct StatementMeta {
    statement: String,
    ast: serde_json::Value,
    expression: Option<String>,
    span: LineSpan,
    assignment: Option<(Vec<RIdentifier>, String)>,
    function_call: Option<(String, Vec<RIdentifier>)>,
    meta: serde_json::Value,
}

impl StatementMeta {
    fn with(stmt: &Statement, span: LineSpan, meta: serde_json::Value) -> Self {
        let assignment = break_down_assignment(stmt);
        let expression = stmt.expression();
        let function_call = expression.and_then(|exp| extract_function_name(exp));
        StatementMeta {
            expression: expression.map(|exp| format!("{}", exp)),
            ast: serde_json::to_value(stmt).unwrap(),
            span,
            statement: format!("{}", stmt),
            assignment,
            function_call,
            meta,
        }
    }
}

fn break_down_assignment(stmt: &Statement) -> Option<(Vec<RIdentifier>, String)> {
    use Statement::*;
    match stmt {
        Assignment(left, add, expression) => {
            let mut vs = vec![left
                .extract_variable_name()
                .unwrap_or_else(|| left.to_string())];
            let mut addition: Vec<String> = add
                .iter()
                .map(|v| v.extract_variable_name().unwrap_or_else(|| v.to_string()))
                .collect();
            vs.append(&mut addition);
            Some((vs, expression.to_string()))
        }
        TailComment(inner, _) => break_down_assignment(inner),
        Empty
        | Comment(_)
        | If(_, _, _)
        | While(_, _)
        | For(_, _, _)
        | Library(_)
        | Expression(_) => None,
    }
}

fn extract_function_name(expression: &parser::Expression) -> Option<(String, Vec<RIdentifier>)> {
    use parser::Expression::*;
    match expression {
        Call(name, args) => name.extract_variable_name().map(|name| {
            let arg_vars = args.iter().flat_map(|(_, exp)| exp.extract_variable_name()).collect();
            (name, arg_vars)
        }),
        Column(left, _) => extract_function_name(&left),
        Index(left, _) => extract_function_name(&left),
        _ => None,
    }
}

impl Tractus {
    pub fn new() -> Self {
        let parsed = Parsed::new();
        let dependency_graph = DependencyGraph::new();
        Tractus {
            parsed,
            dependency_graph,
        }
    }

    pub fn parse_lines<S: AsRef<str>>(&mut self, lines: Vec<S>) -> Result<(), parser::Error> {
        self.parse_lines_with_meta(lines, serde_json::Value::Null)?;
        Ok(())
    }

    pub fn parse_lines_with_meta<S: AsRef<str>>(
        &mut self,
        lines: Vec<S>,
        meta: serde_json::Value,
    ) -> Result<(), parser::Error> {
        let inserted = self
            .parsed
            .append_with_meta(lines, &mut |_, span| (span, meta.clone()));
        self.dependency_graph
            .batch_insert(inserted.into_iter(), self.parsed.statements());
        Ok(())
    }

    pub fn hypotheses_tree(&self) -> HypothesisTree<StatementId> {
        HypothesisTree::new(self.parsed.statements(), &self.dependency_graph)
    }

    pub fn serialize(&self) -> serde_json::Value {
        json!({
            "statements": self.parsed.statements().as_map(
                    &mut |id, stmt, (span, meta)| (id, StatementMeta::with(&stmt, span.clone(), meta.clone()))
                ).into_iter().collect::<HashMap<StatementId, StatementMeta>>(),
            "dependencies": self.dependency_graph.as_json(),
            "hypothesis_tree": self.hypotheses_tree()
        })
    }
}

#[macro_use]
extern crate pest_derive;

use serde::{Deserialize, Serialize};

pub mod dependency_graph;
pub mod hypotheses;
pub mod hypotheses_tree;
pub mod parser;

pub use crate::dependency_graph::DependencyGraph;
pub use crate::hypotheses_tree::HypothesisTree;
pub use crate::parser::{Expression, LineSpan, Parsed, RIdentifier, Statement};

#[derive(Serialize, Deserialize, Default)]
pub struct Tractus {
    parsed: Parsed<(LineSpan, serde_json::Value)>,
    dependency_graph: DependencyGraph,
}

#[derive(Serialize, Deserialize)]
pub struct StatementMeta {
    expression: Option<String>,
    span: LineSpan,
    statement: String,
    assigned_variables: Vec<String>,
    function_name: Option<String>,
    meta: serde_json::Value,
}

impl StatementMeta {
    fn with(stmt: &Statement, span: LineSpan, meta: serde_json::Value) -> Self {
        let assigned_variables = extract_assigned_variables(stmt);
        let expression = stmt.expression();
        let function_name = expression.and_then(|exp| extract_function_name(exp));
        StatementMeta {
            expression: expression.map(|exp| format!("{}", exp)),
            span,
            statement: format!("{}", stmt),
            assigned_variables,
            function_name,
            meta,
        }
    }
}

fn extract_assigned_variables(stmt: &Statement) -> Vec<RIdentifier> {
    use Statement::*;
    match stmt {
        Assignment(left, add, _) => {
            let mut vs = vec![format!("{}", left)];
            let mut addition: Vec<String> = add.iter().map(|v| format!("{}", v)).collect();
            vs.append(&mut addition);
            vs
        }
        TailComment(inner, _) => extract_assigned_variables(inner),
        Empty
        | Comment(_)
        | If(_, _, _)
        | While(_, _)
        | For(_, _, _)
        | Library(_)
        | Expression(_) => Vec::new(),
    }
}

fn extract_function_name(expression: &parser::Expression) -> Option<String> {
    use parser::Expression::*;
    match expression {
        Call(name, _) => name.extract_variable_name(),
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

    pub fn hypotheses_tree(&self) -> HypothesisTree<StatementMeta> {
        let tree = HypothesisTree::new(self.parsed.statements(), &self.dependency_graph);
        tree.into_map(|stmt_id| {
            let (statement, (span, meta)) = &self.parsed.statements()[stmt_id];
            StatementMeta::with(&statement, span.clone(), meta.clone())
        })
    }
}

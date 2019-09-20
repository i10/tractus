use std::borrow::Borrow;
use std::fmt::{Display, Write};
use std::iter::FromIterator;
use std::ops::Index;

use itertools::Itertools;
use log::{debug, trace};
use pest::Parser;
use serde::{Deserialize, Serialize};

#[derive(Parser)]
#[grammar = "r.pest"]
struct RParser;

/// An AST statement.
#[derive(PartialEq, Eq, Debug, Clone, Serialize, Deserialize)]
pub enum Statement {
    Empty,
    Comment(String),
    TailComment(Box<Statement>, String),
    Assignment(Expression, Vec<Expression>, Expression),
    If(Expression, Vec<Statement>, Option<Vec<Statement>>),
    While(Expression, Vec<Statement>),
    For(Expression, Expression, Vec<Statement>),
    Library(RIdentifier),
    Expression(Expression),
}

impl Statement {
    /// Returns the expression contained in the statement, if it exists.
    pub fn expression(&self) -> Option<&Expression> {
        use Statement::*;
        match self {
            Assignment(_, _, expression) => Some(expression),
            Expression(expression) => Some(expression),
            TailComment(statement, _) => statement.expression(),
            // TODO: Check how to handle if, while, and for.
            If(_, _, _) => None,
            For(_, _, _) => None,
            While(_, _) => None,
            Empty => None,
            Comment(_) => None,
            Library(_) => None,
        }
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Statement::*;
        match self {
            Empty => writeln!(f),
            Comment(text) => write!(f, "{}", text),
            TailComment(expression, text) => write!(f, "{} {}", expression, text),
            Assignment(left, additional, right) => {
                let mut assigned = vec![left];
                assigned.append(&mut additional.iter().collect());
                for variable in assigned.iter() {
                    write!(f, "{} <- ", variable)?
                }
                write!(f, "{}", right)
            }
            If(condition, body, maybe_else_body) => {
                write!(f, "if ({}) {{\n{}\n}}", condition, display_lines(body))?;
                if let Some(else_body) = maybe_else_body {
                    write!(f, "\nelse {{\n{}\n}}", display_lines(else_body))?;
                }
                Ok(())
            }
            While(condition, body) => {
                write!(f, "while ({}) {{\n{}\n}}", condition, display_lines(body))
            }
            For(variable, range, body) => write!(
                f,
                "for ({} in {}) {{\n{}\n}}",
                variable,
                range,
                display_lines(body)
            ),
            Library(name) => write!(f, "library({})", name),
            Expression(exp) => write!(f, "{}", exp),
        }
    }
}

/// Render lines with line breaks.
fn display_lines(lines: &[Statement]) -> String {
    lines.iter().map(|line| line.to_string()).join("\n")
}

/// An AST expression.
#[derive(PartialEq, Debug, Eq, Clone, Serialize, Deserialize)]
pub enum Expression {
    Constant(String),
    Variable(RIdentifier),
    Call(Box<Expression>, Vec<(Option<RIdentifier>, Expression)>),
    Column(Box<Expression>, Box<Expression>),
    Index(Box<Expression>, Vec<Option<Expression>>),
    ListIndex(Box<Expression>, Vec<Option<Expression>>),
    OneSidedFormula(Box<Expression>),
    TwoSidedFormula(Box<Expression>, Box<Expression>),
    Function(Vec<(RIdentifier, Option<Expression>)>, Vec<Statement>),
    Prefix(String, Box<Expression>),
    Infix(String, Box<Expression>, Box<Expression>),
}

impl Expression {
    /// If the expression contains a unique variable, return its name.
    pub fn extract_variable_name(&self) -> Option<RIdentifier> {
        use Expression::*;
        match self {
            Variable(name) => Some(name.to_string()),
            Column(left, _) => left.extract_variable_name(),
            Index(left, _) => left.extract_variable_name(),
            ListIndex(left, _) => left.extract_variable_name(),
            Call(_, args) => {
                // `colnames(variable) <- c("a", "b", "c")` is valid R.
                if args.len() == 1 {
                    let (_, exp) = &args[0];
                    exp.extract_variable_name()
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Expression::*;
        match self {
            Constant(constant) => write!(f, "{}", constant),
            Variable(name) => write!(f, "{}", name),
            Call(name, args) => {
                let arguments = args
                    .iter()
                    .map(|(maybe_name, expression)| {
                        let mut s = String::new();
                        if let Some(name) = maybe_name {
                            write!(s, "{} = ", name)?;
                        }
                        write!(s, "{}", expression)?;
                        Ok(s)
                    })
                    .collect::<Result<Vec<String>, std::fmt::Error>>()?
                    .join(", ");
                write!(f, "{}({})", name, arguments)
            }
            Column(left, right) => write!(f, "{}${}", left, right),
            Index(left, right) => {
                let indices = right
                    .iter()
                    .map(|maybe_expression| match maybe_expression {
                        Some(expression) => format!("{}", expression),
                        None => "".to_string(),
                    })
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "{}[{}]", left, indices)
            }
            ListIndex(left, right) => {
                let indices = right
                    .iter()
                    .map(|maybe_expression| match maybe_expression {
                        Some(expression) => format!("{}", expression),
                        None => "".to_string(),
                    })
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "{}[[{}]]", left, indices)
            }
            OneSidedFormula(formula) => write!(f, "~ {}", formula),
            TwoSidedFormula(left, right) => write!(f, "{} ~ {}", left, right),
            Function(params, body) => {
                let parameters = params
                    .iter()
                    .map(|(name, maybe_default)| {
                        let mut s = String::new();
                        write!(s, "{}", name)?;
                        if let Some(default) = maybe_default {
                            write!(s, " = {}", default)?;
                        }
                        Ok(s)
                    })
                    .collect::<Result<Vec<String>, std::fmt::Error>>()?
                    .join(", ");
                write!(
                    f,
                    "function ({}) {{\n{}\n}}",
                    parameters,
                    display_lines(body)
                )
            }
            Prefix(op, exp) => write!(f, "{}{}", op, exp),
            Infix(op, left, right) => write!(f, "{} {} {}", left, op, right),
        }
    }
}

pub type RIdentifier = String;

pub type Error = pest::error::Error<Rule>;

/// A collections of `Statement`s with associated `Meta`-data.
#[derive(Debug, Deserialize, Default, Serialize, Clone)]
pub struct Statements<Meta> {
    stmts: Vec<(Statement, Meta)>,
}

impl<M> FromIterator<(Statement, M)> for Statements<M> {
    fn from_iter<I: IntoIterator<Item = (Statement, M)>>(other: I) -> Self {
        Statements {
            stmts: other.into_iter().collect(),
        }
    }
}

impl FromIterator<Statement> for Statements<()> {
    fn from_iter<I: IntoIterator<Item = Statement>>(other: I) -> Self {
        Statements::from_iter(other.into_iter().map(|s| (s, ())))
    }
}

impl<M> IntoIterator for Statements<M> {
    type Item = (Statement, M);
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.stmts.into_iter()
    }
}

/// An id used for indexing `Statements`.
#[derive(
    Hash, PartialEq, PartialOrd, Default, Eq, Debug, Clone, Serialize, Deserialize, Copy, Ord,
)]
pub struct StatementId(usize);

impl<S, M> Index<S> for Statements<M>
where
    S: Borrow<StatementId>,
{
    type Output = (Statement, M);

    fn index(&self, id: S) -> &Self::Output {
        let idx = id.borrow().0;
        &self.stmts[idx]
    }
}

impl<M> Statements<M> {
    pub fn new() -> Self {
        Self { stmts: Vec::new() }
    }

    /// Append a statement with its meta-data to the collection.
    pub fn append(&mut self, stmt: Statement, meta: M) -> StatementId {
        self.concat(Statements::from_iter(vec![(stmt, meta)]))[0]
    }

    /// Concatenate the passed statements to this collection. Returns the ids of the statements added.
    pub fn concat(&mut self, mut stmts: Statements<M>) -> Vec<StatementId> {
        let first_index = self.stmts.len();
        self.stmts.append(&mut stmts.stmts);
        let last_index = self.stmts.len();

        let id_range = first_index..last_index;
        id_range.map(StatementId).collect()
    }

    /// Returns an iterator over the items with their ids.
    pub fn iter(&self) -> impl Iterator<Item = (StatementId, &Statement, &M)> {
        self.stmts
            .iter()
            .enumerate()
            .map(|(idx, (s, m))| (StatementId(idx), s, m))
    }

    /// Consumes this collection and returns a new collection with mapped entries.
    pub fn into_map<F, N>(self, mapping: &mut F) -> Statements<N>
    where
        F: FnMut(&Statement, M) -> N,
    {
        Statements {
            stmts: self
                .stmts
                .into_iter()
                .map(|(s, m)| {
                    let mapped = mapping(&s, m);
                    (s, mapped)
                })
                .collect(),
        }
    }

    pub fn as_map<F, N>(&self, mapping: &mut F) -> Vec<N>
    where
        F: FnMut(StatementId, &Statement, &M) -> N,
    {
        self.stmts
            .iter()
            .enumerate()
            .map(|(idx, (s, m))| mapping(StatementId(idx), s, m))
            .collect()
    }
}

/// A stateful collection to which new lines can be added continuously.
#[derive(Debug, Serialize, Default, Deserialize, Clone)]
pub struct Parsed<M = LineSpan> {
    statements: Statements<M>,
    unparsed: Vec<String>,
    line_count: usize,
}

impl Parsed<LineSpan> {
    /// Convenience function.
    pub fn append<S>(&mut self, lines: Vec<S>) -> Vec<StatementId>
    where
        S: AsRef<str>,
    {
        self.append_with_meta(lines, &mut |_, m| m)
    }
}

impl<M: std::fmt::Debug> Parsed<M> {
    pub fn new() -> Self {
        Parsed {
            statements: Statements::new(),
            unparsed: Vec::new(),
            line_count: 0,
        }
    }

    /// Parses `lines` and appends all resulting statments to this collection,
    /// while transforming the meta-data according to the `mapping` closure.
    pub fn append_with_meta<S, F>(&mut self, lines: Vec<S>, mapping: &mut F) -> Vec<StatementId>
    where
        S: AsRef<str>,
        F: FnMut(&Statement, LineSpan) -> M,
    {
        let mut added_ids = Vec::new();
        for line in lines.iter() {
            self.line_count += 1;
            self.unparsed.push(line.as_ref().to_string()); // Push to unparsed, such that all currently unparsed lines are treated together.

            let to_parse = &self.unparsed.join("\n");
            let parse_result = parse_statements(to_parse);

            match parse_result {
                Ok(stmts) => {
                    let stmts = stmts.into_map(&mut |stmt, span| {
                        let first_unparsed_line = self.line_count - self.unparsed.len() + 1; // +1 because unparsed always contains the current line.
                        let shifted_span = span.shifted(first_unparsed_line);
                        mapping(&stmt, shifted_span)
                    });
                    let mut new_ids = self.statements.concat(stmts);
                    self.unparsed.clear(); // We have parsed everything successfully.
                    added_ids.append(&mut new_ids);
                }
                Err(e) => {
                    // If the parsing error occurred at the very last symbol,
                    // we assume that it is simply incomplete and will try again when we have more input.
                    trace!("Encountered error while parsing {}:\n{}", to_parse, e);
                    if let pest::error::InputLocation::Pos(pos) = e.location {
                        trace!(
                            "Error position is {}, last position is {}.",
                            pos,
                            to_parse.len()
                        );
                        if pos == to_parse.len() {
                            debug!("Will retry with more input.");
                            continue; // Current line is already pushed to self.unparsed, so it will be retried on next iteration.
                        }
                    }
                    debug!("Skipping this input.");
                    self.unparsed.clear(); // We determined that there is an error in the currently unparsed code.
                }
            }
        }

        added_ids
    }

    /// Returns this collection's `Statements` by reference.
    pub fn statements(&self) -> &Statements<M> {
        &self.statements
    }

    /// Consumes this collection and returns its `Statements`.
    pub fn into_statements(self) -> Statements<M> {
        self.statements
    }
}

/// Parses `code` into a collection of `Statements` associated with their `LineSpan` information.
/// If the parser fails, the parsing error is returned instead.
pub fn parse_statements(code: &str) -> Result<Statements<LineSpan>, Error> {
    let parsed = RParser::parse(Rule::r_code, code)?;

    let new_statements: Vec<(Statement, LineSpan)> = parsed
        .filter_map(|token| match token.as_rule() {
            Rule::EOI => None,
            _ => {
                // parse_line handles all other cases.
                let line_span = LineSpan::from(token.as_span());
                Some((parse_line(token), line_span))
            }
        })
        .collect();

    Ok(Statements::from_iter(new_statements))
}

/// Information on which source code lines a statement spans.
#[derive(Debug, Serialize, PartialEq, Eq, Default, Deserialize, Clone)]
pub struct LineSpan {
    /// First line number the statement occupies.
    from: usize,
    /// Last line number the statement occupies.
    to: usize,
}

impl LineSpan {
    /// Shift this span's line numbers such that it starts at the `new_start`.
    pub fn shifted(self, new_start: usize) -> Self {
        let offset = new_start - 1; // Minus one, because line count starts at one.
        Self {
            from: self.from + offset,
            to: self.to + offset,
        }
    }
}

impl<'a, S> From<S> for LineSpan
where
    S: Borrow<pest::Span<'a>>,
{
    fn from(other: S) -> Self {
        let span = other.borrow();
        Self {
            from: span.start_pos().line_col().0,
            to: span.end_pos().line_col().0,
        }
    }
}

/// Helper macro for use instead of `unreachable!()` that outputs more information.
macro_rules! unexpected_rule {
    ( $rule:ident, $pair:ident) => {
        panic!(
            "Encountered unexpected rule {:?} for input {:#?}.",
            $rule,
            $pair.as_str()
        )
    };
}

/// Parses a token representing a single line of code.
///
/// # Panics
///
/// This function panics if the token does not represent a line.
fn parse_line(line_pair: pest::iterators::Pair<Rule>) -> Statement {
    match line_pair.as_rule() {
        Rule::empty => Statement::Empty,
        Rule::line => {
            let mut line = line_pair.into_inner();
            let first_pair = line.next().unwrap(); // A line always contains at least a statement or a comment.
            let first: Statement = match first_pair.as_rule() {
                Rule::statement => {
                    let statement = first_pair.into_inner().next().unwrap(); // Take statement out of line.
                    match statement.as_rule() {
                        Rule::expression => Statement::Expression(parse_expression(statement)),
                        Rule::assignment => {
                            // Can be multiple assignment, e. g. a=b=c=1. We want to extract the right-most expression,
                            // which is assigned to all others.
                            let mut elements: Vec<Expression> =
                                statement.into_inner().map(parse_expression).collect();
                            let error = "Assignment did not have enough elements.";
                            let right = elements.pop().expect(error);
                            if elements.is_empty() {
                                panic!(error);
                            }
                            let left = elements.remove(0);
                            let additional = elements;
                            Statement::Assignment(left, additional, right)
                        }
                        Rule::if_statement => {
                            let mut elements = statement.into_inner();
                            let condition = if let Some(condition_pair) = elements.next() {
                                parse_expression(condition_pair)
                            } else {
                                panic!("If statement did not have enough elements.");
                            };
                            let body = elements.next().unwrap().into_inner(); // If statement always has a body.
                            let body: Vec<Statement> = body.map(parse_line).collect();

                            let else_body = elements.next().map(|else_body| {
                                else_body
                                    .into_inner()
                                    .map(parse_line)
                                    .collect::<Vec<Statement>>()
                            });

                            Statement::If(condition, body, else_body)
                        }
                        Rule::while_statement => {
                            let mut elements = statement.into_inner();
                            let condition = if let Some(condition_pair) = elements.next() {
                                parse_expression(condition_pair)
                            } else {
                                panic!("While statement did not have enough elements.");
                            };
                            let body = elements.next().unwrap().into_inner(); // For statement always has a body.
                            let body: Vec<Statement> = body.map(parse_line).collect();
                            Statement::While(condition, body)
                        }
                        Rule::for_statement => {
                            let mut elements = statement.into_inner();
                            let (pattern, range) = if let (Some(pattern_pair), Some(range_pair)) =
                                (elements.next(), elements.next())
                            {
                                (parse_expression(pattern_pair), parse_expression(range_pair))
                            } else {
                                panic!("For statement did not have enough elements.");
                            };
                            let body = elements.next().unwrap().into_inner(); // For statement always has a body.
                            let body: Vec<Statement> = body.map(parse_line).collect();
                            Statement::For(pattern, range, body)
                        }
                        Rule::library => {
                            let name = statement.into_inner().next().unwrap(); // Library name always exists.
                            Statement::Library(name.as_str().into())
                        }
                        r => unexpected_rule!(r, statement),
                    }
                }
                Rule::comment => Statement::Comment(first_pair.as_str().to_string()),
                r => unexpected_rule!(r, first_pair),
            };

            let maybe_comment = line.next();
            if let Some(comment) = maybe_comment {
                // Only two-part structure is a tail comment.
                Statement::TailComment(Box::new(first), comment.as_str().to_string())
            } else {
                first
            }
        }
        r => unexpected_rule!(r, line_pair),
    }
}

/// Parses a token representing an expression.
///
/// # Panics
///
/// This function panics if the token does not represent an expression.
fn parse_expression(expression_pair: pest::iterators::Pair<Rule>) -> Expression {
    let mut whole_expression = expression_pair.into_inner();
    let expression = whole_expression.next().unwrap(); // Expression is always non-empty.
    let mut rexp: Expression = match expression.as_rule() {
        Rule::constant => Expression::Constant(expression.as_str().to_string()),
        Rule::identifier => Expression::Variable(expression.as_str().to_string()),
        Rule::prefix => {
            let mut prefix_expression = expression.into_inner();
            let operator = prefix_expression.next().unwrap(); // Prefix always has operator.
            let exp = prefix_expression.next().unwrap(); // Prefix always has expression.
            Expression::Prefix(
                operator.as_str().to_string(),
                Box::new(parse_expression(exp)),
            )
        }
        Rule::formula => Expression::OneSidedFormula(Box::new(parse_expression(expression))),
        Rule::function_definition => {
            let mut function = expression.into_inner();
            let args = function.next().unwrap(); // Function always has (possibly empty) arguments.
            let args: Vec<(RIdentifier, Option<Expression>)> = args
                .into_inner()
                .map(|arg| {
                    match arg.as_rule() {
                        Rule::required_parameter => (arg.as_str().into(), None),
                        Rule::parameter_with_default => {
                            let (arg, expression) = arg.into_inner().next_tuple().unwrap(); // Parameter with default always has name and default value.
                            (arg.as_str().into(), Some(parse_expression(expression)))
                        }
                        _ => unreachable!(),
                    }
                })
                .collect();
            let body = function.next().unwrap().into_inner(); // Function always has a body.
            let body: Vec<Statement> = body.map(parse_line).collect();
            Expression::Function(args, body)
        }
        Rule::expression => parse_expression(expression),
        r => unexpected_rule!(r, expression),
    };

    // Process all binary operators that follow.
    // This process is due to the workaround for preventing left-recursion for these binary operators in the parser.
    for infix in whole_expression {
        rexp = match infix.as_rule() {
            Rule::function_call => parse_function_expression(rexp, infix),
            Rule::column => Expression::Column(Box::new(rexp), Box::new(parse_expression(infix))),
            Rule::index => {
                let indices = infix
                    .into_inner()
                    .map(|maybe_expression| match maybe_expression.as_rule() {
                        Rule::expression => Some(parse_expression(maybe_expression)),
                        Rule::empty => None,
                        _ => unreachable!(),
                    })
                    .collect();
                Expression::Index(Box::new(rexp), indices)
            }
            Rule::list_index => {
                let indices = infix
                    .into_inner()
                    .map(|maybe_expression| match maybe_expression.as_rule() {
                        Rule::expression => Some(parse_expression(maybe_expression)),
                        Rule::empty => None,
                        _ => unreachable!(),
                    })
                    .collect();
                Expression::ListIndex(Box::new(rexp), indices)
            }
            Rule::infix => {
                let mut infix_operator = infix.into_inner();
                let operator = infix_operator.next().unwrap(); // Operator is always present.
                let right = infix_operator.next().unwrap(); // Infix operator always has right-hand side.
                Expression::Infix(
                    operator.as_str().into(),
                    Box::new(rexp),
                    Box::new(parse_expression(right)),
                )
            }
            Rule::formula => {
                Expression::TwoSidedFormula(Box::new(rexp), Box::new(parse_expression(infix)))
            }
            r => unexpected_rule!(r, infix),
        };
    }

    rexp
}

/// Parse a token representing a function expression.
///
/// # Panics
///
/// This function panics if the token does not represent function expression.
fn parse_function_expression(
    expression: Expression,
    function_pair: pest::iterators::Pair<Rule>,
) -> Expression {
    let mut function = function_pair.into_inner();
    let maybe_arguments = function.next();
    let args: Vec<(Option<RIdentifier>, Expression)> = match maybe_arguments {
        Some(args) => {
            args.into_inner()
                .map(|arg| {
                    match arg.as_rule() {
                        Rule::named_argument => {
                            let mut argument = arg.into_inner();
                            let key = argument.next().unwrap(); // Key always exists.
                            let value = argument.next().unwrap(); // Value always exists.
                            let value = parse_expression(value);
                            (Some(key.as_str().to_string()), value)
                        }
                        Rule::unnamed_argument => {
                            let value = arg.into_inner().next().unwrap(); // Argument's value always exists.
                            let value = parse_expression(value);
                            (None, value)
                        }
                        r => unexpected_rule!(r, arg),
                    }
                })
                .collect()
        }
        None => vec![],
    };
    Expression::Call(Box::new(expression), args)
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    // Helper macros for succinctly defining expected ASTs.

    #[macro_export]
    macro_rules! empty {
        () => {
            Statement::Empty
        };
    }

    #[macro_export]
    macro_rules! comment {
        ($text: literal) => {
            Statement::Comment($text.to_string())
        };
    }

    #[macro_export]
    macro_rules! tail_comment {
        ($exp: expr, $text: literal) => {
            Statement::TailComment(Box::new($exp), $text.to_string())
        };
    }

    #[macro_export]
    macro_rules! assignment {
        ($left: expr, $additional: expr, $right: expr) => {
            Statement::Assignment($left, $additional, $right)
        };
    }

    #[macro_export]
    macro_rules! if_stmt {
        ($cond: expr, $body: expr, $else_body: expr) => {
            Statement::If($cond, $body, $else_body)
        };
    }

    #[macro_export]
    macro_rules! while_stmt {
        ($cond:expr, $body: expr) => {
            Statement::While($cond, $body)
        };
    }

    #[macro_export]
    macro_rules! for_stmt {
        ($pattern:expr, $range:expr, $body:expr) => {
            Statement::For($pattern, $range, $body)
        };
    }

    #[macro_export]
    macro_rules! library {
        ($name:literal) => {
            Statement::Library($name.to_string())
        };
    }

    #[macro_export]
    macro_rules! expression {
        ($exp: expr) => {
            Statement::Expression($exp)
        };
    }

    #[macro_export]
    macro_rules! constant {
        ($value: literal) => {
            Expression::Constant($value.to_string())
        };
    }

    #[macro_export]
    macro_rules! variable {
        ($name: literal) => {
            Expression::Variable($name.to_string())
        };
    }

    #[macro_export]
    macro_rules! call {
        ($exp: expr, $args: expr) => {
            Expression::Call(Box::new($exp), $args)
        };
    }

    #[macro_export]
    macro_rules! column {
        ($left:expr, $right:expr) => {
            Expression::Column(Box::new($left), Box::new($right))
        };
    }

    #[macro_export]
    macro_rules! index {
        ($left:expr,$right:expr) => {
            Expression::Index(Box::new($left), $right)
        };
    }

    #[macro_export]
    macro_rules! list_index {
        ($left:expr, $right:expr) => {
            Expression::ListIndex(Box::new($left), $right)
        };
    }

    #[macro_export]
    macro_rules! one_sided_formula {
        ($exp:expr) => {
            Expression::OneSidedFormula(Box::new($exp))
        };
    }

    #[macro_export]
    macro_rules! two_sided_formula {
        ($left:expr,$right:expr) => {
            Expression::TwoSidedFormula(Box::new($left), Box::new($right))
        };
    }

    #[macro_export]
    macro_rules! function {
        ($params:expr, $body:expr) => {
            Expression::Function($params, $body)
        };
    }

    #[macro_export]
    macro_rules! prefix {
        ($op:literal,$exp:expr) => {
            Expression::Prefix($op.to_string(), Box::new($exp))
        };
    }

    #[macro_export]
    macro_rules! infix {
        ($op:literal, $left:expr,$right:expr) => {
            Expression::Infix($op.to_string(), Box::new($left), Box::new($right))
        };
    }

    /// Assert that the parsed result of `code` matches the `expected` AST.
    fn assert_matches(code: &'static str, expected: Vec<Statement>) {
        let actual: Vec<Statement> = parse_statements(code)
            .unwrap_or_else(|e| panic!("{}", e))
            .into_iter()
            .map(|(stmt, _)| stmt)
            .collect();
        assert_eq!(expected, actual, "Failed using parse_statements");

        let mut parsed = Parsed::new();
        parsed.append(code.lines().collect());
        let actual_parsed: Vec<Statement> = parsed
            .into_statements()
            .into_iter()
            .map(|(stmt, _)| stmt)
            .collect();
        let expected: Vec<Statement> = expected.into_iter().flat_map(clean_for_parsed).collect();
        assert_eq!(expected, actual_parsed, "Failed using Parsed.");
    }

    /// The line-by-line nature of `Parsed` leads to a slightly different AST without empty statements and without else statements.
    /// This functions takes a statement and returns a collection of statement that satisfies these constraints.
    fn clean_for_parsed(stmt: Statement) -> Vec<Statement> {
        use Statement::*;
        match stmt {
            Empty => vec![],
            If(condition, body, maybe_else_body) => {
                if let Some(else_body) = maybe_else_body {
                    let mut broken = vec![If(condition, body, None)];
                    let mut else_body = else_body.into_iter().flat_map(clean_for_parsed).collect();
                    broken.append(&mut else_body);
                    broken
                } else {
                    vec![If(condition, body, None)]
                }
            }
            _ => vec![stmt],
        }
    }

    #[test]
    fn parses_comments() {
        let code = "\
#123
hello() # world
# another thing   ";
        let expected = vec![
            comment!("#123"),
            tail_comment!(expression!(call!(variable!("hello"), vec![])), "# world"),
            comment!("# another thing   "),
        ];
        assert_matches(code, expected)
    }

    #[test]
    fn parses_empty_lines() {
        let code = "
# First block
a <- 1

# Second block
b <- 1
c <- 2


# Third block
d <- 1


";
        let expected = vec![
            empty!(),
            comment!("# First block"),
            assignment!(variable!("a"), vec![], constant!("1")),
            empty!(),
            comment!("# Second block"),
            assignment!(variable!("b"), vec![], constant!("1")),
            assignment!(variable!("c"), vec![], constant!("2")),
            empty!(),
            empty!(),
            comment!("# Third block"),
            assignment!(variable!("d"), vec![], constant!("1")),
            empty!(),
            empty!(),
        ];
        assert_matches(code, expected);
    }

    #[test]
    fn parses_assignments() {
        let code = "\
a <- 1
line <-
    \"break\"
b = 2
a=b=c=1
colnames(something) <- c(\"R\", \"is\", \"crazy\")";
        let expected = vec![
            assignment!(variable!("a"), vec![], constant!("1")),
            assignment!(variable!("line"), vec![], constant!("\"break\"")),
            assignment!(variable!("b"), vec![], constant!("2")),
            assignment!(
                variable!("a"),
                vec![variable!("b"), variable!("c")],
                constant!("1")
            ),
            assignment!(
                call!(variable!("colnames"), vec![(None, variable!("something"))]),
                vec![],
                call!(
                    variable!("c"),
                    vec![
                        (None, constant!("\"R\"")),
                        (None, constant!("\"is\"")),
                        (None, constant!("\"crazy\"")),
                    ]
                )
            ),
        ];
        assert_matches(code, expected);
    }

    #[test]
    fn parses_function_calls() {
        let code = "\
empty()
single(1)
with_args(1, x, name = value)
break_down(
    \"long\",
    \"argument\"
    , \"chains\"
    )
weird(\"name\" = 1)
name:::space()
higher_order()(10)";
        let expected = vec![
            expression!(call!(variable!("empty"), vec![])),
            expression!(call!(variable!("single"), vec![(None, constant!("1"))])),
            expression!(call!(
                variable!("with_args"),
                vec![
                    (None, constant!("1")),
                    (None, variable!("x")),
                    (Some("name".to_string()), variable!("value")),
                ]
            )),
            expression!(call!(
                variable!("break_down"),
                vec![
                    (None, constant!("\"long\"")),
                    (None, constant!("\"argument\"")),
                    (None, constant!("\"chains\"")),
                ]
            )),
            expression!(call!(
                variable!("weird"),
                vec![(Some("\"name\"".to_string()), constant!("1"))]
            )),
            expression!(call!(variable!("name:::space"), vec![])),
            expression!(call!(
                call!(variable!("higher_order"), vec![]),
                vec![(None, constant!("10"))]
            )),
        ];
        assert_matches(code, expected);
    }

    #[test]
    fn parses_strings() {
        let code = "\
'first'
\"second\"
`third`";
        let expected = vec![
            expression!(constant!("'first'")),
            expression!(constant!("\"second\"")),
            expression!(constant!("`third`")),
        ];
        assert_matches(code, expected);
    }

    #[test]
    fn parses_numbers() {
        let code = "\
1
.20
0.10
-2
2e-30
+3.4e+1";
        let expected = vec![
            expression!(constant!("1")),
            expression!(constant!(".20")),
            expression!(constant!("0.10")),
            expression!(prefix!("-", constant!("2"))),
            expression!(constant!("2e-30")),
            expression!(prefix!("+", constant!("3.4e+1"))),
        ];
        assert_matches(code, expected);
    }

    #[test]
    fn parses_library_calls() {
        let code = "\
library(plyr)
library(MASS)";
        let expected = vec![library!("plyr"), library!("MASS")];
        assert_matches(code, expected);
    }

    #[test]
    fn parses_indexing() {
        let code = "\
item$column
item[other$thing]
item[[1]]
other[multiple, index, arguments]
list[[1,2]]
get_matrix()$column [ 1 ]
item[empty,]";
        let expected = vec![
            expression!(column!(variable!("item"), variable!("column"))),
            expression!(index!(
                variable!("item"),
                vec![Some(column!(variable!("other"), variable!("thing")))]
            )),
            expression!(list_index!(variable!("item"), vec![Some(constant!("1"))])),
            expression!(index!(
                variable!("other"),
                vec![
                    Some(variable!("multiple")),
                    Some(variable!("index")),
                    Some(variable!("arguments")),
                ]
            )),
            expression!(list_index!(
                variable!("list"),
                vec![Some(constant!("1")), Some(constant!("2")),]
            )),
            expression!(index!(
                column!(call!(variable!("get_matrix"), vec![]), variable!("column")),
                vec![Some(constant!("1"))]
            )),
            expression!(index!(
                variable!("item"),
                vec![Some(variable!("empty")), None]
            )),
        ];
        assert_matches(code, expected);
    }

    #[test]
    fn parses_formulae() {
        let code = "\
~ one_sided
two ~ sided
~ one + sided + multiple
two ~ sided + 1
~ transform(x)
other ~ transform(x)
lm(y[subk]~factor(x[subk]))";
        let expected = vec![
            expression!(one_sided_formula!(variable!("one_sided"))),
            expression!(two_sided_formula!(variable!("two"), variable!("sided"))),
            expression!(one_sided_formula!(infix!(
                "+",
                variable!("one"),
                infix!("+", variable!("sided"), variable!("multiple"))
            ))),
            expression!(two_sided_formula!(
                variable!("two"),
                infix!("+", variable!("sided"), constant!("1"))
            )),
            expression!(one_sided_formula!(call!(
                variable!("transform"),
                vec![(None, variable!("x"))]
            ))),
            expression!(two_sided_formula!(
                variable!("other"),
                call!(variable!("transform"), vec![(None, variable!("x"))])
            )),
            expression!(call!(
                variable!("lm"),
                vec![(
                    None,
                    two_sided_formula!(
                        index!(variable!("y"), vec![Some(variable!("subk"))]),
                        call!(
                            variable!("factor"),
                            vec![(None, index!(variable!("x"), vec![Some(variable!("subk"))]),)]
                        )
                    ),
                )]
            )),
        ];
        assert_matches(code, expected);
    }

    #[test]
    fn parses_function_definition() {
        let code = "\
func1 <- function () {
    1
}
func2 <- function (with, arguments)
    { 2 }
func3 <- function (with,
 default = 'arguments') {
    a <- other()
    a
} ";
        let expected = vec![
            assignment!(
                variable!("func1"),
                vec![],
                function!(vec![], vec![expression!(constant!("1"))])
            ),
            assignment!(
                variable!("func2"),
                vec![],
                function!(
                    vec![("with".to_string(), None), ("arguments".to_string(), None)],
                    vec![expression!(constant!("2"))]
                )
            ),
            assignment!(
                variable!("func3"),
                vec![],
                function!(
                    vec![
                        ("with".to_string(), None),
                        ("default".to_string(), Some(constant!("'arguments'"))),
                    ],
                    vec![
                        assignment!(variable!("a"), vec![], call!(variable!("other"), vec![])),
                        expression!(variable!("a")),
                    ]
                )
            ),
        ];
        assert_matches(code, expected);
    }

    #[test]
    fn parses_prefix_operators() {
        let code = "\
x <- !TRUE
y <- negate(!x)
-(1 + 2)";
        let expected = vec![
            assignment!(variable!("x"), vec![], prefix!("!", constant!("TRUE"))),
            assignment!(
                variable!("y"),
                vec![],
                call!(
                    variable!("negate"),
                    vec![(None, prefix!("!", variable!("x")),)]
                )
            ),
            expression!(prefix!("-", infix!("+", constant!("1"), constant!("2")))),
        ];
        assert_matches(code, expected);
    }

    #[test]
    fn parses_infix_operators() {
        let code = "\
1 <= 3
TRUE && FALSE
'a' %custom% 'infix'
1 +
    3";
        let expected = vec![
            expression!(infix!("<=", constant!("1"), constant!("3"))),
            expression!(infix!("&&", constant!("TRUE"), constant!("FALSE"))),
            expression!(infix!("%custom%", constant!("'a'"), constant!("'infix'"))),
            expression!(infix!("+", constant!("1"), constant!("3"))),
        ];
        assert_matches(code, expected);
    }

    #[test]
    fn parses_expression_in_parens() {
        let code = "\
1
(2)
( 1 + (2 + 3))
((1 + 2) + 3)";
        let expected = vec![
            expression!(constant!("1")),
            expression!(constant!("2")),
            expression!(infix!(
                "+",
                constant!("1"),
                infix!("+", constant!("2"), constant!("3"))
            )),
            expression!(infix!(
                "+",
                infix!("+", constant!("1"), constant!("2")),
                constant!("3")
            )),
        ];
        assert_matches(code, expected);
    }

    #[test]
    fn parses_if() {
        let code = "\
if (0 == 1) {
    do_something()

    do_something_else()
}
if (is_ok())
    do_something_again()

if (TRUE)
{
    is_true()
}
else {
    is_false()
}
if (FALSE)
    is_false()
else if(TRUE){
    is_true()
}
if (TRUE)
    cry()
else (why <- 1)";
        let expected = vec![
            if_stmt!(
                infix!("==", constant!("0"), constant!("1")),
                vec![
                    expression!(call!(variable!("do_something"), vec![])),
                    empty!(),
                    expression!(call!(variable!("do_something_else"), vec![])),
                ],
                None
            ),
            if_stmt!(
                call!(variable!("is_ok"), vec![]),
                vec![expression!(call!(variable!("do_something_again"), vec![]))],
                None
            ),
            empty!(),
            if_stmt!(
                constant!("TRUE"),
                vec![expression!(call!(variable!("is_true"), vec![]))],
                Some(vec![expression!(call!(variable!("is_false"), vec![]))])
            ),
            if_stmt!(
                constant!("FALSE"),
                vec![expression!(call!(variable!("is_false"), vec![]))],
                Some(vec![if_stmt!(
                    constant!("TRUE"),
                    vec![expression!(call!(variable!("is_true"), vec![]))],
                    None
                )])
            ),
            if_stmt!(
                constant!("TRUE"),
                vec![expression!(call!(variable!("cry"), vec![]))],
                Some(vec![assignment!(variable!("why"), vec![], constant!("1"))])
            ),
        ];
        assert_matches(code, expected);
    }

    #[test]
    fn parses_for() {
        let code = "\
for (i in something) {
    do_something_with(i)

    do_something_else()
}
for (i in get())
    do_something_again(i)
for(row in 1:15) l[[row]] = row
";
        let expected = vec![
            for_stmt!(
                variable!("i"),
                variable!("something"),
                vec![
                    expression!(call!(
                        variable!("do_something_with"),
                        vec![(None, variable!("i"))]
                    )),
                    empty!(),
                    expression!(call!(variable!("do_something_else"), vec![])),
                ]
            ),
            for_stmt!(
                variable!("i"),
                call!(variable!("get"), vec![]),
                vec![expression!(call!(
                    variable!("do_something_again"),
                    vec![(None, variable!("i"))]
                ))]
            ),
            for_stmt!(
                variable!("row"),
                infix!(":", constant!("1"), constant!("15")),
                vec![assignment!(
                    list_index!(variable!("l"), vec![Some(variable!("row"))]),
                    vec![],
                    variable!("row")
                )]
            ),
        ];
        assert_matches(code, expected);
    }

    #[test]
    fn parses_while() {
        let code = "\
while (i < 10) {
    do_stuff()
    i <- i + 1
}
while (TRUE)
    annoy()
";
        let expected = vec![
            while_stmt!(
                infix!("<", variable!("i"), constant!("10")),
                vec![
                    expression!(call!(variable!("do_stuff"), vec![])),
                    assignment!(
                        variable!("i"),
                        vec![],
                        infix!("+", variable!("i"), constant!("1"))
                    ),
                ]
            ),
            while_stmt!(
                constant!("TRUE"),
                vec![expression!(call!(variable!("annoy"), vec![]))]
            ),
        ];
        assert_matches(code, expected);
    }

    mod ids {
        use super::*;
        use pretty_assertions::assert_eq;

        #[test]
        fn iter_ids_match_statement() {
            let code = "\
# First
second <- 2
third(second)";
            let stmts =
                parse_statements(code).unwrap_or_else(|e| panic!("Could not parse code:\n{}", e));
            for (id, stmt, meta) in stmts.iter() {
                let by_id = &stmts[id];
                assert_eq!((&by_id.0, &by_id.1), (stmt, meta));
            }
        }

        #[test]
        fn batch_insert_ids_correspond_to_lines() {
            let code = "\
# First
second <- 2
if (third)
    fourth()
";
            let mut parsed = Parsed::new();
            let inserted = parsed.append(code.lines().collect());
            assert_eq!(3, inserted.len());
            assert_eq!(parsed.statements()[inserted[0]].0, comment!("# First"));
            assert_eq!(
                parsed.statements()[inserted[1]].0,
                assignment!(variable!("second"), vec![], constant!("2"))
            );
            assert_eq!(
                parsed.statements()[inserted[2]].0,
                if_stmt!(
                    variable!("third"),
                    vec![expression!(call!(variable!("fourth"), vec![]))],
                    None
                )
            );

            let more_code = "\
# Fifth
sixth()";
            let inserted = parsed.append(more_code.lines().collect());
            assert_eq!(parsed.statements()[inserted[0]].0, comment!("# Fifth"));
            assert_eq!(
                parsed.statements()[inserted[1]].0,
                expression!(call!(variable!("sixth"), vec![]))
            );
        }
    }

    mod extracts_variable_name {
        use super::*;
        use pretty_assertions::assert_eq;

        fn assert_matches(expected: Option<&'static str>, actual: Expression) {
            let actual_name = actual.extract_variable_name();
            let expected = expected.map(|s| s.to_string());
            assert_eq!(expected, actual_name);
        }

        #[test]
        fn from_variable() {
            let name = variable!("x");
            assert_matches(Some("x"), name);
        }

        #[test]
        fn from_column() {
            let name = column!(variable!("x"), variable!("a"));
            assert_matches(Some("x"), name);
        }

        #[test]
        fn from_index() {
            let name = index!(variable!("x"), vec![Some(variable!("a"))]);
            assert_matches(Some("x"), name);
        }

        #[test]
        fn from_list_index() {
            let name = list_index!(variable!("x"), vec![Some(variable!("a"))]);
            assert_matches(Some("x"), name);
        }

        #[test]
        fn from_colnames() {
            let name = call!(variable!("colnames"), vec![(None, variable!("x"))]);
            assert_matches(Some("x"), name);
        }

        #[test]
        fn rejects_constants() {
            let name = constant!("x");
            assert_matches(None, name);
        }

        #[test]
        fn rejects_constant_in_column() {
            let name = column!(constant!("x"), variable!("a"));
            assert_matches(None, name);
        }
    }

    mod line_span {
        use super::*;
        use pretty_assertions::assert_eq;

        #[test]
        fn associates_correct_line_number() {
            let code = "\
# First
second <- 2
if (third)
    fourth()
";
            let stmts =
                parse_statements(code).unwrap_or_else(|e| panic!("Failed to parse code:\n{}", e));
            let spans: Vec<LineSpan> = stmts.into_iter().map(|(_, span)| span).collect();
            assert_eq!(
                vec![
                    LineSpan { from: 1, to: 1 },
                    LineSpan { from: 2, to: 2 },
                    LineSpan { from: 3, to: 4 }
                ],
                spans
            );
        }

        #[test]
        fn successive_appends_remember_line_number() {
            let code = "\
# First
second <- 2
if (third)
    fourth()
";
            let mut parsed = Parsed::new();
            let inserted = parsed.append(code.lines().collect());
            let spans: Vec<LineSpan> = inserted
                .iter()
                .map(|id| parsed.statements()[id].1.clone())
                .collect();
            assert_eq!(
                vec![
                    LineSpan { from: 1, to: 1 },
                    LineSpan { from: 2, to: 2 },
                    LineSpan { from: 3, to: 4 }
                ],
                spans
            );

            let more_code = "\
# Fifth
sixth()";
            let inserted = parsed.append(more_code.lines().collect());
            let spans: Vec<LineSpan> = inserted
                .iter()
                .map(|id| parsed.statements()[id].1.clone())
                .collect();
            assert_eq!(
                vec![LineSpan { from: 5, to: 5 }, LineSpan { from: 6, to: 6 },],
                spans
            );
        }
    }
}

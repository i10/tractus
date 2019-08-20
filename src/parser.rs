use std::borrow::Borrow;
use std::fmt::{Display, Write};
use std::hash::{Hash, Hasher};
use std::iter::FromIterator;
use std::rc::Rc;

use itertools::Itertools;
use log::debug;
use pest::Parser;
use serde::{Serialize, Serializer};

#[derive(Parser)]
#[grammar = "r.pest"]
struct RParser;

#[derive(PartialEq, Eq, Debug, Clone, Serialize)]
pub enum RStatement<Meta> {
    Empty(Meta),
    Comment(String, Meta),
    TailComment(Rc<RStatement<Meta>>, String, Meta),
    Assignment(
        Rc<RExpression<Meta>>,
        Vec<Rc<RExpression<Meta>>>,
        Rc<RExpression<Meta>>,
        Meta,
    ),
    If(
        Rc<RExpression<Meta>>,
        Vec<Rc<RStatement<Meta>>>,
        Option<Vec<Rc<RStatement<Meta>>>>,
        Meta,
    ),
    While(Rc<RExpression<Meta>>, Vec<Rc<RStatement<Meta>>>, Meta),
    For(
        Rc<RExpression<Meta>>,
        Rc<RExpression<Meta>>,
        Vec<Rc<RStatement<Meta>>>,
        Meta,
    ),
    Library(RIdentifier, Meta),
    Expression(Rc<RExpression<Meta>>, Meta),
}

/*
impl<T> std::fmt::Debug for RStatement<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "RStmt {{ {} }}", self)
    }
}
*/

impl<T> Hash for RStatement<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        use RStatement::*;
        match self {
            Empty(_) => ().hash(state),
            Comment(text, _) => text.hash(state),
            TailComment(statement, text, _) => {
                statement.hash(state);
                text.hash(state);
            }
            Assignment(left, additional, right, _) => {
                left.hash(state);
                additional.hash(state);
                right.hash(state);
            }
            If(cond, body, else_body, _) => {
                cond.hash(state);
                body.hash(state);
                else_body.hash(state);
            }
            While(cond, body, _) => {
                cond.hash(state);
                body.hash(state);
            }
            For(var, range, body, _) => {
                var.hash(state);
                range.hash(state);
                body.hash(state);
            }
            Library(name, _) => name.hash(state),
            Expression(exp, _) => exp.hash(state),
        }
    }
}

pub type Statement = RStatement<Span>;

impl<T> RStatement<T> {
    pub fn map<F, U>(&self, mapping: &mut F) -> Rc<RStatement<U>>
    where
        F: FnMut(&T) -> U,
    {
        use RStatement::*;
        Rc::new(match self {
            Empty(m) => Empty(mapping(m)),
            Comment(comment, m) => Comment(comment.clone(), mapping(m)),
            TailComment(stmt, comment, m) => {
                TailComment(stmt.map(mapping), comment.clone(), mapping(m))
            }
            Assignment(left, additional, right, m) => Assignment(
                left.map(mapping),
                additional.iter().map(|exp| exp.map(mapping)).collect(),
                right.map(mapping),
                mapping(m),
            ),
            If(exp, body, else_body, m) => If(
                exp.map(mapping),
                body.iter().map(|stmt| stmt.map(mapping)).collect(),
                else_body
                    .as_ref()
                    .map(|b| b.iter().map(|stmt| stmt.map(mapping)).collect()),
                mapping(m),
            ),
            While(condition, body, m) => While(
                condition.map(mapping),
                body.iter().map(|stmt| stmt.map(mapping)).collect(),
                mapping(m),
            ),
            For(variable, range, body, m) => For(
                variable.map(mapping),
                range.map(mapping),
                body.iter().map(|stmt| stmt.map(mapping)).collect(),
                mapping(m),
            ),
            Library(id, m) => Library(id.clone(), mapping(m)),
            Expression(exp, m) => Expression(exp.map(mapping), mapping(m)),
        })
    }

    pub fn get_meta(&self) -> &T {
        use RStatement::*;
        match self {
            Empty(m) => m,
            Comment(comment, m) => m,
            TailComment(stmt, comment, m) => m,
            Assignment(left, additional, right, m) => m,
            If(exp, body, else_body, m) => m,
            While(condition, body, m) => m,
            For(variable, range, body, m) => m,
            Library(id, m) => m,
            Expression(exp, m) => m,
        }
    }

    pub fn expression(&self) -> Option<Rc<RExpression<T>>> {
        use RStatement::*;
        match self {
            Assignment(_, _, expression, _) => Some(expression.clone()),
            Expression(expression, _) => Some(expression.clone()),
            TailComment(statement, _, _) => statement.expression(),
            // TODO: Check how to handle if, while, and for.
            If(_, _, _, _) => None,
            For(_, _, _, _) => None,
            While(_, _, _) => None,
            Empty(_) => None,
            Comment(_, _) => None,
            Library(_, _) => None,
        }
    }
}

impl<T> Display for RStatement<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use RStatement::*;
        match self {
            Empty(_) => writeln!(f),
            Comment(text, _) => write!(f, "{}", text),
            TailComment(expression, text, _) => write!(f, "{} {}", expression, text),
            Assignment(left, additional, right, _) => {
                let mut assigned = vec![left];
                assigned.append(&mut additional.iter().collect());
                for variable in assigned.iter() {
                    write!(f, "{} <- ", variable)?
                }
                write!(f, "{}", right)
            }
            If(condition, body, maybe_else_body, _) => {
                write!(f, "if ({}) {{\n{}\n}}", condition, Lines::from(body))?;
                if let Some(else_body) = maybe_else_body {
                    write!(f, "\nelse {{\n{}\n}}", Lines::from(else_body))?;
                }
                Ok(())
            }
            While(condition, body, _) => {
                write!(f, "while ({}) {{\n{}\n}}", condition, Lines::from(body))
            }
            For(variable, range, body, _) => write!(
                f,
                "for ({} in {}) {{\n{}\n}}",
                variable,
                range,
                Lines::from(body)
            ),
            Library(name, _) => write!(f, "{}", name),
            Expression(exp, _) => write!(f, "{}", exp),
        }
    }
}

#[derive(Eq, PartialEq, Debug, Clone, Hash)]
pub struct Lines<'a, T>(&'a Vec<Rc<RStatement<T>>>);

impl<'a, T> Display for Lines<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.iter().join("\n"))
    }
}

impl<'a, T> From<&'a Vec<Rc<RStatement<T>>>> for Lines<'a, T> {
    fn from(other: &'a Vec<Rc<RStatement<T>>>) -> Lines<T> {
        Lines(other)
    }
}

#[derive(PartialEq, Debug, Eq, Clone, Serialize)]
pub enum RExpression<Meta> {
    Constant(String, Meta),
    Variable(RIdentifier, Meta),
    Call(
        Rc<RExpression<Meta>>,
        Vec<(Option<RIdentifier>, Rc<RExpression<Meta>>)>,
        Meta,
    ),
    Column(Rc<RExpression<Meta>>, Rc<RExpression<Meta>>, Meta),
    Index(
        Rc<RExpression<Meta>>,
        Vec<Option<Rc<RExpression<Meta>>>>,
        Meta,
    ),
    ListIndex(
        Rc<RExpression<Meta>>,
        Vec<Option<Rc<RExpression<Meta>>>>,
        Meta,
    ),
    OneSidedFormula(Rc<RExpression<Meta>>, Meta),
    TwoSidedFormula(Rc<RExpression<Meta>>, Rc<RExpression<Meta>>, Meta),
    Function(
        Vec<(RIdentifier, Option<Rc<RExpression<Meta>>>)>,
        Vec<Rc<RStatement<Meta>>>,
        Meta,
    ),
    Prefix(String, Rc<RExpression<Meta>>, Meta),
    Infix(String, Rc<RExpression<Meta>>, Rc<RExpression<Meta>>, Meta),
}

/*
impl<T> fmt::Debug for RExpression<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "RExp {{ {} }}", self)
    }
}
*/
impl<T> Hash for RExpression<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        use RExpression::*;
        match self {
            Constant(value, _) => value.hash(state),
            Variable(id, _) => id.hash(state),
            Call(exp, args, _) => {
                exp.hash(state);
                args.hash(state);
            }
            Column(left, right, _) => {
                left.hash(state);
                right.hash(state);
            }
            Index(left, right, _) => {
                left.hash(state);
                right.hash(state);
            }
            ListIndex(left, right, _) => {
                left.hash(state);
                right.hash(state);
            }
            OneSidedFormula(exp, _) => exp.hash(state),
            TwoSidedFormula(left, right, _) => {
                left.hash(state);
                right.hash(state);
            }
            Function(params, body, _) => {
                params.hash(state);
                body.hash(state);
            }
            Prefix(op, exp, _) => {
                op.hash(state);
                exp.hash(state);
            }
            Infix(op, left, right, _) => {
                op.hash(state);
                left.hash(state);
                right.hash(state);
            }
        }
    }
}

pub type RExp = RExpression<()>;
pub type Expression = RExpression<Span>;

impl RExp {
    pub fn constant(content: &'static str) -> RExp {
        RExpression::Constant(content.to_string(), ())
    }

    pub fn variable(content: &'static str) -> RExp {
        RExpression::Variable(content.to_string(), ())
    }

    pub fn boxed_constant(content: impl Into<String>) -> Rc<RExp> {
        Rc::new(RExpression::Constant(content.into(), ()))
    }

    pub fn boxed_variable(content: impl Into<String>) -> Rc<RExp> {
        Rc::new(RExpression::Variable(content.into(), ()))
    }
}

impl<T> RExpression<T> {
    pub fn map<F, U>(&self, mapping: &mut F) -> Rc<RExpression<U>>
    where
        F: FnMut(&T) -> U,
    {
        use RExpression::*;
        Rc::new(match self {
            Constant(constant, m) => Constant(constant.clone(), mapping(m)),
            Variable(id, m) => Variable(id.clone(), mapping(m)),
            Call(exp, args, m) => Call(
                exp.map(mapping),
                args.iter()
                    .map(|(name, exp)| (name.clone(), exp.map(mapping)))
                    .collect(),
                mapping(m),
            ),
            Column(left, right, m) => Column(left.map(mapping), right.map(mapping), mapping(m)),
            Index(left, right, m) => Index(
                left.map(mapping),
                right
                    .iter()
                    .map(|maybe_exp| maybe_exp.as_ref().map(|exp| exp.map(mapping)))
                    .collect(),
                mapping(m),
            ),
            ListIndex(left, right, m) => ListIndex(
                left.map(mapping),
                right
                    .iter()
                    .map(|maybe_exp| maybe_exp.as_ref().map(|exp| exp.map(mapping)))
                    .collect(),
                mapping(m),
            ),
            OneSidedFormula(formula, m) => OneSidedFormula(formula.map(mapping), mapping(m)),
            TwoSidedFormula(left, right, m) => {
                TwoSidedFormula(left.map(mapping), right.map(mapping), mapping(m))
            }
            Function(args, body, m) => Function(
                args.iter()
                    .map(|(name, maybe_statement)| {
                        (
                            name.clone(),
                            maybe_statement
                                .as_ref()
                                .map(|statement| statement.map(mapping)),
                        )
                    })
                    .collect(),
                body.iter()
                    .map(|statement| statement.map(mapping))
                    .collect(),
                mapping(m),
            ),
            Prefix(operator, exp, m) => Prefix(operator.clone(), exp.map(mapping), mapping(m)),
            Infix(operator, left, right, m) => Infix(
                operator.clone(),
                left.map(mapping),
                right.map(mapping),
                mapping(m),
            ),
        })
    }

    pub fn get_meta(&self) -> &T {
        use RExpression::*;
        match self {
            Constant(constant, m) => m,
            Variable(id, m) => m,
            Call(exp, args, m) => m,
            Column(left, right, m) => m,
            Index(left, right, m) => m,
            ListIndex(left, right, m) => m,
            OneSidedFormula(formula, m) => m,
            TwoSidedFormula(left, right, m) => m,
            Function(args, body, m) => m,
            Prefix(operator, exp, m) => m,
            Infix(operator, left, right, m) => m,
        }
    }

    pub fn extract_variable_name(&self) -> Option<RIdentifier> {
        use RExpression::*;
        match self {
            Variable(name, _) => Some(name.to_string()),
            Column(left, _, _) => left.extract_variable_name(),
            Index(left, _, _) => left.extract_variable_name(),
            ListIndex(left, _, _) => left.extract_variable_name(),
            Call(_, args, _) => {
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

impl<T> Display for RExpression<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use RExpression::*;
        match self {
            Constant(constant, _) => write!(f, "{}", constant),
            Variable(name, _) => write!(f, "{}", name),
            Call(name, args, _) => {
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
            Column(left, right, _) => write!(f, "{}${}", left, right),
            Index(left, right, _) => {
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
            ListIndex(left, right, _) => {
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
            OneSidedFormula(formula, _) => write!(f, "~ {}", formula),
            TwoSidedFormula(left, right, _) => write!(f, "{} ~ {}", left, right),
            Function(params, body, _) => {
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
                write!(f, "function ({}) {{\n{}\n}}", parameters, Lines::from(body))
            }
            Prefix(op, exp, _) => write!(f, "{}{}", op, exp),
            Infix(op, left, right, _) => write!(f, "{} {} {}", left, op, right),
        }
    }
}

pub struct LineDisplay<'a, P>(&'a P)
where
    &'a P: Extract<Span> + Display;

impl<'a, P> From<&'a P> for LineDisplay<'a, P>
where
    &'a P: Extract<Span> + Display,
{
    fn from(other: &'a P) -> Self {
        LineDisplay(other)
    }
}

impl<'a, P> Display for LineDisplay<'a, P>
where
    &'a P: Extract<Span> + Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let expression = self.0.borrow();
        let span = expression.extract();
        write!(f, "{}", span.from.line)?;
        if span.to.line > span.from.line {
            write!(f, "-{}", span.to.line)?;
        }
        write!(f, ": {}", self.0)
    }
}

impl<'a, P> Serialize for LineDisplay<'a, P>
where
    &'a P: Extract<Span> + Display,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

pub trait Extract<T> {
    fn extract(&self) -> &T;
}

impl<T> Extract<T> for &Rc<RStatement<T>> {
    fn extract(&self) -> &T {
        use RStatement::*;
        match &***self {
            Empty(m) => m,
            Comment(_, m) => m,
            TailComment(_, _, m) => m,
            Assignment(_, _, _, m) => m,
            If(_, _, _, m) => m,
            While(_, _, m) => m,
            For(_, _, _, m) => m,
            Library(_, m) => m,
            Expression(_, m) => m,
        }
    }
}

impl<T> Extract<T> for &Rc<RExpression<T>> {
    fn extract(&self) -> &T {
        use RExpression::*;
        match &***self {
            Constant(_, m) => m,
            Variable(_, m) => m,
            Call(_, _, m) => m,
            Column(_, _, m) => m,
            Index(_, _, m) => m,
            ListIndex(_, _, m) => m,
            OneSidedFormula(_, m) => m,
            TwoSidedFormula(_, _, m) => m,
            Function(_, _, m) => m,
            Prefix(_, _, m) => m,
            Infix(_, _, _, m) => m,
        }
    }
}

pub type RIdentifier = String;

#[derive(PartialEq, Eq, Debug, Clone, Hash, Serialize)]
pub struct Span {
    from: Position,
    to: Position,
}

impl<'i, P: Borrow<pest::iterators::Pair<'i, Rule>>> From<P> for Span {
    fn from(other: P) -> Self {
        let span = other.borrow().as_span();
        Span {
            from: Position::from(&span.start_pos()),
            to: Position::from(&span.end_pos()),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Hash, Serialize)]
pub struct Position {
    line: usize,
    column: usize,
}

impl<'i> From<&pest::Position<'i>> for Position {
    fn from(other: &pest::Position) -> Self {
        let (line, column) = other.line_col();
        Position { line, column }
    }
}

pub type Error = pest::error::Error<Rule>;

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

#[derive(Debug, Default)]
pub struct Parsed(Vec<Rc<Statement>>);

impl Parsed {
    pub fn new() -> Self {
        Parsed(vec![])
    }

    pub fn parse(code: &str) -> Result<Self, Error> {
        let mut parsed = Self::new();
        parsed.append(code)?;
        Ok(parsed)
    }

    pub fn append(&mut self, code: &str) -> Result<&[Rc<Statement>], Error> {
        debug!("Pest parsing...");
        let parsed = RParser::parse(Rule::r_code, code)?;

        debug!("Assembling AST...");
        let mut new_statements: Vec<Rc<Statement>> = parsed
            .filter_map(|token| match token.as_rule() {
                Rule::EOI => None,
                _ => Some(parse_line(token)),
            })
            .collect();

        let added = new_statements.len();
        self.0.append(&mut new_statements);
        let len = self.0.len();
        Ok(&self.0[len - added..])
    }

    pub fn iter(&self) -> impl Iterator<Item = &Rc<Statement>> {
        self.0.iter()
    }
}

impl FromIterator<Statement> for Parsed {
    fn from_iter<I: IntoIterator<Item = Statement>>(iter: I) -> Self {
        Parsed(iter.into_iter().map(Rc::new).collect())
    }
}

impl IntoIterator for Parsed {
    type Item = Rc<Statement>;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

fn parse_line(line_pair: pest::iterators::Pair<Rule>) -> Rc<Statement> {
    Rc::new(match line_pair.as_rule() {
        Rule::empty => RStatement::Empty(line_pair.into()),
        Rule::line => {
            let mut line = line_pair.into_inner();
            let first_pair = line.next().unwrap(); // A line always contains at least a statement or a comment.
            let first_pair_span = Span::from(&first_pair);
            let first: Statement = match first_pair.as_rule() {
                Rule::statement => {
                    let statement = first_pair.into_inner().next().unwrap(); // Take statement out of line.
                    let statement_span = Span::from(&statement);
                    match statement.as_rule() {
                        Rule::expression => {
                            RStatement::Expression(parse_expression(statement), statement_span)
                        }
                        Rule::assignment => {
                            // Can be multiple assignment, e. g. a=b=c=1. We want to extract the right-most expression,
                            // wich is assigned to all others, and the left-most one, which prevents an empty left side.
                            let mut elements: Vec<Rc<Expression>> =
                                statement.into_inner().map(parse_expression).collect();
                            let error = "Assignment did not have enough elements.";
                            let right = elements.pop().expect(error);
                            if elements.is_empty() {
                                panic!(error);
                            }
                            let left = elements.remove(0);
                            let additional = elements;
                            RStatement::Assignment(left, additional, right, statement_span)
                        }
                        Rule::if_statement => {
                            let mut elements = statement.into_inner();
                            let condition = if let Some(condition_pair) = elements.next() {
                                parse_expression(condition_pair)
                            } else {
                                panic!("If statement did not have enough elements.");
                            };
                            let body = elements.next().unwrap().into_inner(); // If statement always has a body.
                            let body: Vec<Rc<Statement>> = body.map(parse_line).collect();

                            let else_body = elements.next().map(|else_body| {
                                else_body
                                    .into_inner()
                                    .map(parse_line)
                                    .collect::<Vec<Rc<Statement>>>()
                            });

                            RStatement::If(condition, body, else_body, statement_span)
                        }
                        Rule::while_statement => {
                            let mut elements = statement.into_inner();
                            let condition = if let Some(condition_pair) = elements.next() {
                                parse_expression(condition_pair)
                            } else {
                                panic!("While statement did not have enough elements.");
                            };
                            let body = elements.next().unwrap().into_inner(); // For statement always has a body.
                            let body: Vec<Rc<Statement>> = body.map(parse_line).collect();
                            RStatement::While(condition, body, statement_span)
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
                            let body: Vec<Rc<Statement>> = body.map(parse_line).collect();
                            RStatement::For(pattern, range, body, statement_span)
                        }
                        Rule::library => {
                            let name = statement.into_inner().next().unwrap(); // Library name always exists.
                            RStatement::Library(name.as_str().into(), statement_span)
                        }
                        r => unexpected_rule!(r, statement),
                    }
                }
                Rule::comment => {
                    RStatement::Comment(first_pair.as_str().to_string(), first_pair_span)
                }
                r => unexpected_rule!(r, first_pair),
            };

            let maybe_comment = line.next();
            if let Some(comment) = maybe_comment {
                // Second line component has to be a comment.
                let comment_span = Span::from(&comment);
                RStatement::TailComment(Rc::new(first), comment.as_str().to_string(), comment_span)
            } else {
                first
            }
        }
        r => unexpected_rule!(r, line_pair),
    })
}

fn parse_expression(expression_pair: pest::iterators::Pair<Rule>) -> Rc<Expression> {
    let mut whole_expression = expression_pair.into_inner();
    let expression = whole_expression.next().unwrap(); // Expression is always non-empty.
    let expression_span = Span::from(&expression);
    let mut rexp: Rc<Expression> = Rc::new(match expression.as_rule() {
        Rule::constant => {
            RExpression::Constant(expression.as_str().to_string(), (&expression).into())
        }
        Rule::identifier => {
            RExpression::Variable(expression.as_str().to_string(), (&expression).into())
        }
        Rule::prefix => {
            let mut prefix_expression = expression.into_inner();
            let operator = prefix_expression.next().unwrap(); // Prefix always has operator.
            let exp = prefix_expression.next().unwrap(); // Prefix always has expression.
            RExpression::Prefix(
                operator.as_str().to_string(),
                parse_expression(exp),
                expression_span,
            )
        }
        Rule::formula => {
            RExpression::OneSidedFormula(parse_expression(expression), expression_span)
        }
        Rule::function_definition => {
            let mut function = expression.into_inner();
            let args = function.next().unwrap(); // Function always has (possibly empty) arguments.
            let args: Vec<(RIdentifier, Option<Rc<Expression>>)> = args
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
            let body: Vec<Rc<Statement>> = body.map(parse_line).collect();
            RExpression::Function(args, body, expression_span)
        }
        Rule::expression => Rc::try_unwrap(parse_expression(expression)).unwrap(), // We are the sole owners.
        r => unexpected_rule!(r, expression),
    });

    // Process all indexing expressions that follow.
    for infix in whole_expression {
        let infix_span = Span::from(&infix);
        rexp = Rc::new(match infix.as_rule() {
            Rule::function_call => parse_function_expression(rexp, infix),
            Rule::column => RExpression::Column(rexp, parse_expression(infix), infix_span),
            Rule::index => {
                let indices = infix
                    .into_inner()
                    .map(|maybe_expression| match maybe_expression.as_rule() {
                        Rule::expression => Some(parse_expression(maybe_expression)),
                        Rule::empty => None,
                        _ => unreachable!(),
                    })
                    .collect();
                RExpression::Index(rexp, indices, infix_span)
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
                RExpression::ListIndex(rexp, indices, infix_span)
            }
            Rule::infix => {
                let mut infix_operator = infix.into_inner();
                let operator = infix_operator.next().unwrap(); // Operator is always present.
                let right = infix_operator.next().unwrap(); // Infix operator always has right-hand side.
                RExpression::Infix(
                    operator.as_str().into(),
                    rexp,
                    parse_expression(right),
                    infix_span,
                )
            }
            Rule::formula => {
                RExpression::TwoSidedFormula(rexp, parse_expression(infix), infix_span)
            }
            r => unexpected_rule!(r, infix),
        });
    }

    rexp
}

fn parse_function_expression(
    expression: Rc<Expression>,
    function_pair: pest::iterators::Pair<Rule>,
) -> Expression {
    let function_span = Span::from(&function_pair);
    let mut function = function_pair.into_inner();
    let maybe_arguments = function.next();
    let args: Vec<(Option<RIdentifier>, Rc<Expression>)> = match maybe_arguments {
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
                        _ => unreachable!(),
                    }
                })
                .collect()
        }
        None => vec![],
    };
    RExpression::Call(expression, args, function_span)
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[macro_export]
    macro_rules! empty {
        () => {
            Rc::new(RStatement::Empty(()).into())
        };
    }

    #[macro_export]
    macro_rules! comment {
        ($text: literal) => {
            Rc::new(RStatement::Comment($text.to_string(), ()).into())
        };
    }

    #[macro_export]
    macro_rules! tail_comment {
        ($exp: expr, $text: literal) => {
            Rc::new(RStatement::TailComment($exp, $text.to_string(), ()).into())
        };
    }

    #[macro_export]
    macro_rules! assignment {
        ($left: expr, $additional: expr, $right: expr) => {
            Rc::new(RStatement::Assignment($left, $additional, $right, ()).into())
        };
    }

    #[macro_export]
    macro_rules! if_stmt {
        ($cond: expr, $body: expr, $else_body: expr) => {
            Rc::new(RStatement::If($cond, $body, $else_body, ()).into())
        };
    }

    #[macro_export]
    macro_rules! while_stmt {
        ($cond:expr, $body: expr) => {
            Rc::new(RStatement::While($cond, $body, ()).into())
        };
    }

    #[macro_export]
    macro_rules! for_stmt {
        ($pattern:expr, $range:expr, $body:expr) => {
            Rc::new(RStatement::For($pattern, $range, $body, ()).into())
        };
    }

    #[macro_export]
    macro_rules! library {
        ($name:literal) => {
            Rc::new(RStatement::Library($name.to_string(), ()).into())
        };
    }

    #[macro_export]
    macro_rules! expression {
        ($exp: expr) => {
            Rc::new(RStatement::Expression($exp, ()).into())
        };
    }

    #[macro_export]
    macro_rules! constant {
        ($value: literal) => {
            Rc::new(RExpression::Constant($value.to_string(), ()).into())
        };
    }

    #[macro_export]
    macro_rules! variable {
        ($name: literal) => {
            Rc::new(RExpression::Variable($name.to_string(), ()).into())
        };
    }

    #[macro_export]
    macro_rules! call {
        ($exp: expr, $args: expr) => {
            Rc::new(RExpression::Call($exp, $args, ()).into())
        };
    }

    #[macro_export]
    macro_rules! column {
        ($left:expr, $right:expr) => {
            Rc::new(RExpression::Column($left, $right, ()).into())
        };
    }

    #[macro_export]
    macro_rules! index {
        ($left:expr,$right:expr) => {
            Rc::new(RExpression::Index($left, $right, ()).into())
        };
    }

    #[macro_export]
    macro_rules! list_index {
        ($left:expr, $right:expr) => {
            Rc::new(RExpression::ListIndex($left, $right, ()).into())
        };
    }

    #[macro_export]
    macro_rules! one_sided_formula {
        ($exp:expr) => {
            Rc::new(RExpression::OneSidedFormula($exp, ()).into())
        };
    }

    #[macro_export]
    macro_rules! two_sided_formula {
        ($left:expr,$right:expr) => {
            Rc::new(RExpression::TwoSidedFormula($left, $right, ()).into())
        };
    }

    #[macro_export]
    macro_rules! function {
        ($params:expr, $body:expr) => {
            Rc::new(RExpression::Function($params, $body, ()).into())
        };
    }

    #[macro_export]
    macro_rules! prefix {
        ($op:literal,$exp:expr) => {
            Rc::new(RExpression::Prefix($op.to_string(), $exp, ()).into())
        };
    }

    #[macro_export]
    macro_rules! infix {
        ($op:literal, $left:expr,$right:expr) => {
            Rc::new(RExpression::Infix($op.to_string(), $left, $right, ()).into())
        };
    }

    fn assert_matches(code: &'static str, expected: Vec<Rc<RStatement<()>>>) {
        let parsed = Parsed::parse(code).unwrap_or_else(|e| panic!("{}", e));
        let actual: Vec<Rc<RStatement<()>>> =
            parsed.iter().map(|stmt| stmt.map(&mut |_| ())).collect();
        assert_eq!(expected, actual);
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

    mod extracts_variable_name {
        use super::*;
        use pretty_assertions::assert_eq;

        fn assert_matches(expected: Option<RIdentifier>, actual: Rc<RExpression<()>>) {
            let actual_name = actual.extract_variable_name();
            assert_eq!(expected, actual_name);
        }

        #[test]
        fn from_variable() {
            let name = variable!("x");
            assert_matches(Some("x".to_string()), name);
        }

        #[test]
        fn from_column() {
            let name = column!(variable!("x"), variable!("a"));
            assert_matches(Some("x".to_string()), name);
        }

        #[test]
        fn from_index() {
            let name = index!(variable!("x"), vec![Some(variable!("a"))]);
            assert_matches(Some("x".to_string()), name);
        }

        #[test]
        fn from_list_index() {
            let name = list_index!(variable!("x"), vec![Some(variable!("a"))]);
            assert_matches(Some("x".to_string()), name);
        }

        #[test]
        fn from_colnames() {
            let name = call!(variable!("colnames"), vec![(None, variable!("x"))]);
            assert_matches(Some("x".to_string()), name);
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
}

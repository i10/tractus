use std::borrow::Borrow;
use std::fmt::{Display, Write};
use std::hash::{Hash, Hasher};
use std::iter::FromIterator;

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
    TailComment(Box<RStatement<Meta>>, String, Meta),
    Assignment(
        RExpression<Meta>,
        Vec<RExpression<Meta>>,
        RExpression<Meta>,
        Meta,
    ),
    If(
        RExpression<Meta>,
        Vec<RStatement<Meta>>,
        Option<Vec<RStatement<Meta>>>,
        Meta,
    ),
    While(RExpression<Meta>, Vec<RStatement<Meta>>, Meta),
    For(
        RExpression<Meta>,
        RExpression<Meta>,
        Vec<RStatement<Meta>>,
        Meta,
    ),
    Library(RIdentifier, Meta),
    Expression(RExpression<Meta>, Meta),
}

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
    pub fn map<F, U>(self, mapping: &mut F) -> RStatement<U>
    where
        F: FnMut(T) -> U,
    {
        use RStatement::*;
        match self {
            Empty(m) => Empty(mapping(m)),
            Comment(comment, m) => Comment(comment, mapping(m)),
            TailComment(stmt, comment, m) => {
                TailComment(Box::new(stmt.map(mapping)), comment, mapping(m))
            }
            Assignment(left, additional, right, m) => Assignment(
                left.map(mapping),
                additional.into_iter().map(|exp| exp.map(mapping)).collect(),
                right.map(mapping),
                mapping(m),
            ),
            If(exp, body, else_body, m) => If(
                exp.map(mapping),
                body.into_iter().map(|stmt| stmt.map(mapping)).collect(),
                else_body.map(|b| b.into_iter().map(|stmt| stmt.map(mapping)).collect()),
                mapping(m),
            ),
            While(condition, body, m) => While(
                condition.map(mapping),
                body.into_iter().map(|stmt| stmt.map(mapping)).collect(),
                mapping(m),
            ),
            For(variable, range, body, m) => For(
                variable.map(mapping),
                range.map(mapping),
                body.into_iter().map(|stmt| stmt.map(mapping)).collect(),
                mapping(m),
            ),
            Library(id, m) => Library(id, mapping(m)),
            Expression(exp, m) => Expression(exp.map(mapping), mapping(m)),
        }
    }

    pub fn expression(&self) -> Option<&RExpression<T>> {
        use RStatement::*;
        match self {
            Assignment(_, _, expression, _) => Some(&expression),
            Expression(expression, _) => Some(&expression),
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
pub struct Lines<'a, T>(&'a Vec<RStatement<T>>);

impl<'a, T> Display for Lines<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.iter().join("\n"))
    }
}

impl<'a, T> From<&'a Vec<RStatement<T>>> for Lines<'a, T> {
    fn from(other: &'a Vec<RStatement<T>>) -> Lines<T> {
        Lines(other)
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Serialize)]
pub enum RExpression<Meta> {
    Constant(String, Meta),
    Variable(RIdentifier, Meta),
    Call(
        Box<RExpression<Meta>>,
        Vec<(Option<RIdentifier>, RExpression<Meta>)>,
        Meta,
    ),
    Column(Box<RExpression<Meta>>, Box<RExpression<Meta>>, Meta),
    Index(Box<RExpression<Meta>>, Vec<Option<RExpression<Meta>>>, Meta),
    ListIndex(Box<RExpression<Meta>>, Vec<Option<RExpression<Meta>>>, Meta),
    OneSidedFormula(Box<RExpression<Meta>>, Meta),
    TwoSidedFormula(Box<RExpression<Meta>>, Box<RExpression<Meta>>, Meta),
    Function(
        Vec<(RIdentifier, Option<RExpression<Meta>>)>,
        Vec<RStatement<Meta>>,
        Meta,
    ),
    Prefix(String, Box<RExpression<Meta>>, Meta),
    Infix(String, Box<RExpression<Meta>>, Box<RExpression<Meta>>, Meta),
}

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

    pub fn boxed_constant(content: impl Into<String>) -> Box<RExp> {
        Box::new(RExpression::Constant(content.into(), ()))
    }

    pub fn boxed_variable(content: impl Into<String>) -> Box<RExp> {
        Box::new(RExpression::Variable(content.into(), ()))
    }
}

impl<T> RExpression<T> {
    pub fn map<F, U>(self, mapping: &mut F) -> RExpression<U>
    where
        F: FnMut(T) -> U,
    {
        use RExpression::*;
        match self {
            Constant(constant, m) => Constant(constant, mapping(m)),
            Variable(id, m) => Variable(id, mapping(m)),
            Call(exp, args, m) => Call(
                Box::new(exp.map(mapping)),
                args.into_iter()
                    .map(|(name, exp)| (name, exp.map(mapping)))
                    .collect(),
                mapping(m),
            ),
            Column(left, right, m) => Column(
                Box::new(left.map(mapping)),
                Box::new(right.map(mapping)),
                mapping(m),
            ),
            Index(left, right, m) => Index(
                Box::new(left.map(mapping)),
                right
                    .into_iter()
                    .map(|maybe_exp| maybe_exp.map(|exp| exp.map(mapping)))
                    .collect(),
                mapping(m),
            ),
            ListIndex(left, right, m) => ListIndex(
                Box::new(left.map(mapping)),
                right
                    .into_iter()
                    .map(|maybe_exp| maybe_exp.map(|exp| exp.map(mapping)))
                    .collect(),
                mapping(m),
            ),
            OneSidedFormula(formula, m) => {
                OneSidedFormula(Box::new(formula.map(mapping)), mapping(m))
            }
            TwoSidedFormula(left, right, m) => TwoSidedFormula(
                Box::new(left.map(mapping)),
                Box::new(right.map(mapping)),
                mapping(m),
            ),
            Function(args, body, m) => Function(
                args.into_iter()
                    .map(|(name, maybe_statement)| {
                        (
                            name,
                            maybe_statement.map(|statement| statement.map(mapping)),
                        )
                    })
                    .collect(),
                body.into_iter()
                    .map(|statement| statement.map(mapping))
                    .collect(),
                mapping(m),
            ),
            Prefix(operator, exp, m) => Prefix(operator, Box::new(exp.map(mapping)), mapping(m)),
            Infix(operator, left, right, m) => Infix(
                operator,
                Box::new(left.map(mapping)),
                Box::new(right.map(mapping)),
                mapping(m),
            ),
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
impl<'a, P> Serialize for LineDisplay<'a, P> where &'a P: Extract<Span> + Display{
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

impl<T> Extract<T> for &RStatement<T> {
    fn extract(&self) -> &T {
        use RStatement::*;
        match self {
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

impl<T> Extract<T> for &RExpression<T> {
    fn extract(&self) -> &T {
        use RExpression::*;
        match self {
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

#[derive(Debug)]
pub struct Parsed(Vec<Statement>);

impl Parsed {
    pub fn from(code: &str) -> Result<Self, Error> {
        debug!("Pest parsing...");
        let parse_result = RParser::parse(Rule::file, code)?;

        debug!("Assembling AST...");
        Ok(parse_result
            .filter_map(|token| match token.as_rule() {
                Rule::EOI => None,
                _ => Some(parse_line(token)),
            })
            .collect())
    }

    pub fn iter(&self) -> impl Iterator<Item = &Statement> {
        self.0.iter()
    }
}

impl FromIterator<Statement> for Parsed {
    fn from_iter<I: IntoIterator<Item = Statement>>(iter: I) -> Self {
        Parsed(iter.into_iter().collect())
    }
}

impl IntoIterator for Parsed {
    type Item = Statement;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

fn parse_line(line_pair: pest::iterators::Pair<Rule>) -> Statement {
    match line_pair.as_rule() {
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
                            let mut elements: Vec<Expression> =
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
                            let body: Vec<Statement> = body.map(parse_line).collect();

                            let else_body = elements.next().map(|else_body| {
                                else_body
                                    .into_inner()
                                    .map(parse_line)
                                    .collect::<Vec<Statement>>()
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
                            let body: Vec<Statement> = body.map(parse_line).collect();
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
                            let body: Vec<Statement> = body.map(parse_line).collect();
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
                RStatement::TailComment(Box::new(first), comment.as_str().to_string(), comment_span)
            } else {
                first
            }
        }
        r => unexpected_rule!(r, line_pair),
    }
}

fn parse_expression(expression_pair: pest::iterators::Pair<Rule>) -> Expression {
    let mut whole_expression = expression_pair.into_inner();
    let expression = whole_expression.next().unwrap(); // Expression is always non-empty.
    let expression_span = Span::from(&expression);
    let mut rexp: Expression = match expression.as_rule() {
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
                Box::new(parse_expression(exp)),
                expression_span,
            )
        }
        Rule::formula => {
            RExpression::OneSidedFormula(Box::new(parse_expression(expression)), expression_span)
        }
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
            RExpression::Function(args, body, expression_span)
        }
        Rule::expression => parse_expression(expression),
        r => unexpected_rule!(r, expression),
    };

    // Process all indexing expressions that follow.
    for infix in whole_expression {
        let infix_span = Span::from(&infix);
        match infix.as_rule() {
            Rule::function_call => rexp = parse_function_expression(rexp, infix),
            Rule::column => {
                rexp = RExpression::Column(
                    Box::new(rexp),
                    Box::new(parse_expression(infix)),
                    infix_span,
                )
            }
            Rule::index => {
                let indices = infix
                    .into_inner()
                    .map(|maybe_expression| match maybe_expression.as_rule() {
                        Rule::expression => Some(parse_expression(maybe_expression)),
                        Rule::empty => None,
                        _ => unreachable!(),
                    })
                    .collect();
                rexp = RExpression::Index(Box::new(rexp), indices, infix_span);
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
                rexp = RExpression::ListIndex(Box::new(rexp), indices, infix_span);
            }
            Rule::infix => {
                let mut infix_operator = infix.into_inner();
                let operator = infix_operator.next().unwrap(); // Operator is always present.
                let right = infix_operator.next().unwrap(); // Infix operator always has right-hand side.
                rexp = RExpression::Infix(
                    operator.as_str().into(),
                    Box::new(rexp),
                    Box::new(parse_expression(right)),
                    infix_span,
                );
            }
            Rule::formula => {
                rexp = RExpression::TwoSidedFormula(
                    Box::new(rexp),
                    Box::new(parse_expression(infix)),
                    infix_span,
                );
            }
            r => unexpected_rule!(r, infix),
        }
    }

    rexp
}

fn parse_function_expression(
    expression: Expression,
    function_pair: pest::iterators::Pair<Rule>,
) -> Expression {
    let function_span = Span::from(&function_pair);
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
                        _ => unreachable!(),
                    }
                })
                .collect()
        }
        None => vec![],
    };
    RExpression::Call(Box::new(expression), args, function_span)
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    fn test_parse(code: &'static str) -> Vec<RStatement<()>> {
        Parsed::from(code)
            .unwrap_or_else(|e| panic!("{}", e))
            .into_iter()
            .map(|stmt| stmt.map(&mut |_| ()))
            .collect()
    }

    #[test]
    fn parses_comments() {
        let code = "\
#123
hello() # world
# another thing   ";
        let result = test_parse(code);
        let expected = vec![
            RStatement::Comment("#123".into(), ()),
            RStatement::TailComment(
                Box::new(RStatement::Expression(
                    RExpression::Call(RExpression::boxed_variable("hello"), vec![], ()),
                    (),
                )),
                "# world".into(),
                (),
            ),
            RStatement::Comment("# another thing   ".into(), ()),
        ];
        assert_eq!(expected, result);
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
        let result = test_parse(code);
        let expected = vec![
            RStatement::Empty(()),
            RStatement::Comment("# First block".into(), ()),
            RStatement::Assignment(
                RExpression::variable("a"),
                vec![],
                RExpression::constant("1"),
                (),
            ),
            RStatement::Empty(()),
            RStatement::Comment("# Second block".into(), ()),
            RStatement::Assignment(
                RExpression::variable("b"),
                vec![],
                RExpression::constant("1"),
                (),
            ),
            RStatement::Assignment(
                RExpression::variable("c"),
                vec![],
                RExpression::constant("2"),
                (),
            ),
            RStatement::Empty(()),
            RStatement::Empty(()),
            RStatement::Comment("# Third block".into(), ()),
            RStatement::Assignment(
                RExpression::variable("d"),
                vec![],
                RExpression::constant("1"),
                (),
            ),
            RStatement::Empty(()),
            RStatement::Empty(()),
        ];
        assert_eq!(expected, result);
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
        let result = test_parse(code);
        let expected = vec![
            RStatement::Assignment(
                RExpression::variable("a"),
                vec![],
                RExpression::constant("1"),
                (),
            ),
            RStatement::Assignment(
                RExpression::variable("line"),
                vec![],
                RExpression::constant("\"break\""),
                (),
            ),
            RStatement::Assignment(
                RExpression::variable("b"),
                vec![],
                RExpression::constant("2"),
                (),
            ),
            RStatement::Assignment(
                RExpression::variable("a"),
                vec![RExpression::variable("b"), RExpression::variable("c")],
                RExpression::constant("1"),
                (),
            ),
            RStatement::Assignment(
                RExpression::Call(
                    RExpression::boxed_variable("colnames"),
                    vec![(None, RExpression::variable("something"))],
                    (),
                ),
                vec![],
                RExpression::Call(
                    RExpression::boxed_variable("c"),
                    vec![
                        (None, RExpression::constant("\"R\"")),
                        (None, RExpression::constant("\"is\"")),
                        (None, RExpression::constant("\"crazy\"")),
                    ],
                    (),
                ),
                (),
            ),
        ];
        assert_eq!(expected, result);
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
        let result = test_parse(code);
        let expected = vec![
            RStatement::Expression(
                RExpression::Call(RExpression::boxed_variable("empty"), vec![], ()),
                (),
            ),
            RStatement::Expression(
                RExpression::Call(
                    RExpression::boxed_variable("single"),
                    vec![(None, RExpression::constant("1"))],
                    (),
                ),
                (),
            ),
            RStatement::Expression(
                RExpression::Call(
                    RExpression::boxed_variable("with_args"),
                    vec![
                        (None, RExpression::constant("1")),
                        (None, RExpression::variable("x")),
                        (Some("name".to_string()), RExpression::variable("value")),
                    ],
                    (),
                ),
                (),
            ),
            RStatement::Expression(
                RExpression::Call(
                    RExpression::boxed_variable("break_down"),
                    vec![
                        (None, RExpression::constant("\"long\"")),
                        (None, RExpression::constant("\"argument\"")),
                        (None, RExpression::constant("\"chains\"")),
                    ],
                    (),
                ),
                (),
            ),
            RStatement::Expression(
                RExpression::Call(
                    RExpression::boxed_variable("weird"),
                    vec![(Some("\"name\"".into()), RExpression::constant("1"))],
                    (),
                ),
                (),
            ),
            RStatement::Expression(
                RExpression::Call(RExpression::boxed_variable("name:::space"), vec![], ()),
                (),
            ),
            RStatement::Expression(
                RExpression::Call(
                    Box::new(RExpression::Call(
                        RExpression::boxed_variable("higher_order"),
                        vec![],
                        (),
                    )),
                    vec![(None, RExpression::constant("10"))],
                    (),
                ),
                (),
            ),
        ];
        assert_eq!(expected, result);
    }

    #[test]
    fn parses_strings() {
        let code = "\
'first'
\"second\"
`third`";
        let result = test_parse(code);
        let expected = vec![
            RStatement::Expression(RExpression::constant("'first'"), ()),
            RStatement::Expression(RExpression::constant("\"second\""), ()),
            RStatement::Expression(RExpression::constant("`third`"), ()),
        ];
        assert_eq!(expected, result);
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
        let result = test_parse(code);
        let expected = vec![
            RStatement::Expression(RExpression::constant("1"), ()),
            RStatement::Expression(RExpression::constant(".20"), ()),
            RStatement::Expression(RExpression::constant("0.10"), ()),
            RStatement::Expression(
                RExpression::Prefix("-".into(), Box::new(RExpression::constant("2")), ()),
                (),
            ),
            RStatement::Expression(RExpression::constant("2e-30"), ()),
            RStatement::Expression(
                RExpression::Prefix("+".into(), Box::new(RExpression::constant("3.4e+1")), ()),
                (),
            ),
        ];
        assert_eq!(expected, result);
    }

    #[test]
    fn parses_library_calls() {
        let code = "\
library(plyr)
library(MASS)";
        let result = test_parse(code);
        let expected = vec![
            RStatement::Library("plyr".into(), ()),
            RStatement::Library("MASS".into(), ()),
        ];
        assert_eq!(expected, result);
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
        let result = test_parse(code);
        let expected = vec![
            RStatement::Expression(
                RExpression::Column(
                    Box::new(RExpression::variable("item")),
                    Box::new(RExpression::variable("column")),
                    (),
                ),
                (),
            ),
            RStatement::Expression(
                RExpression::Index(
                    Box::new(RExpression::variable("item")),
                    vec![Some(RExpression::Column(
                        Box::new(RExpression::variable("other")),
                        Box::new(RExpression::variable("thing")),
                        (),
                    ))],
                    (),
                ),
                (),
            ),
            RStatement::Expression(
                RExpression::ListIndex(
                    Box::new(RExpression::variable("item")),
                    vec![Some(RExpression::constant("1"))],
                    (),
                ),
                (),
            ),
            RStatement::Expression(
                RExpression::Index(
                    Box::new(RExpression::variable("other")),
                    vec![
                        Some(RExpression::variable("multiple")),
                        Some(RExpression::variable("index")),
                        Some(RExpression::variable("arguments")),
                    ],
                    (),
                ),
                (),
            ),
            RStatement::Expression(
                RExpression::ListIndex(
                    Box::new(RExpression::variable("list")),
                    vec![
                        Some(RExpression::constant("1")),
                        Some(RExpression::constant("2")),
                    ],
                    (),
                ),
                (),
            ),
            RStatement::Expression(
                RExpression::Index(
                    Box::new(RExpression::Column(
                        Box::new(RExpression::Call(
                            RExpression::boxed_variable("get_matrix"),
                            vec![],
                            (),
                        )),
                        Box::new(RExpression::variable("column")),
                        (),
                    )),
                    vec![Some(RExpression::constant("1"))],
                    (),
                ),
                (),
            ),
            RStatement::Expression(
                RExpression::Index(
                    Box::new(RExpression::variable("item")),
                    vec![Some(RExpression::variable("empty")), None],
                    (),
                ),
                (),
            ),
        ];
        assert_eq!(expected, result);
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
        let result = test_parse(code);
        let expected = vec![
            RStatement::Expression(
                RExpression::OneSidedFormula(Box::new(RExpression::variable("one_sided")), ()),
                (),
            ),
            RStatement::Expression(
                RExpression::TwoSidedFormula(
                    Box::new(RExpression::variable("two")),
                    Box::new(RExpression::variable("sided")),
                    (),
                ),
                (),
            ),
            RStatement::Expression(
                RExpression::OneSidedFormula(
                    Box::new(RExpression::Infix(
                        "+".into(),
                        Box::new(RExpression::variable("one")),
                        Box::new(RExpression::Infix(
                            "+".into(),
                            Box::new(RExpression::variable("sided")),
                            Box::new(RExpression::variable("multiple")),
                            (),
                        )),
                        (),
                    )),
                    (),
                ),
                (),
            ),
            RStatement::Expression(
                RExpression::TwoSidedFormula(
                    Box::new(RExpression::variable("two")),
                    Box::new(RExpression::Infix(
                        "+".into(),
                        Box::new(RExpression::variable("sided")),
                        Box::new(RExpression::constant("1")),
                        (),
                    )),
                    (),
                ),
                (),
            ),
            RStatement::Expression(
                RExpression::OneSidedFormula(
                    Box::new(RExpression::Call(
                        RExpression::boxed_variable("transform"),
                        vec![(None, RExpression::variable("x"))],
                        (),
                    )),
                    (),
                ),
                (),
            ),
            RStatement::Expression(
                RExpression::TwoSidedFormula(
                    Box::new(RExpression::variable("other")),
                    Box::new(RExpression::Call(
                        RExpression::boxed_variable("transform"),
                        vec![(None, RExpression::variable("x"))],
                        (),
                    )),
                    (),
                ),
                (),
            ),
            RStatement::Expression(
                RExpression::Call(
                    RExpression::boxed_variable("lm"),
                    vec![(
                        None,
                        RExpression::TwoSidedFormula(
                            Box::new(RExpression::Index(
                                Box::new(RExpression::variable("y")),
                                vec![Some(RExpression::variable("subk"))],
                                (),
                            )),
                            Box::new(RExpression::Call(
                                RExpression::boxed_variable("factor"),
                                vec![(
                                    None,
                                    RExpression::Index(
                                        Box::new(RExpression::variable("x")),
                                        vec![Some(RExpression::variable("subk"))],
                                        (),
                                    ),
                                )],
                                (),
                            )),
                            (),
                        ),
                    )],
                    (),
                ),
                (),
            ),
        ];
        assert_eq!(expected, result);
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
        let result = test_parse(code);
        let expected = vec![
            RStatement::Assignment(
                RExpression::variable("func1"),
                vec![],
                RExpression::Function(
                    vec![],
                    vec![RStatement::Expression(RExpression::constant("1"), ())],
                    (),
                ),
                (),
            ),
            RStatement::Assignment(
                RExpression::variable("func2"),
                vec![],
                RExpression::Function(
                    vec![("with".into(), None), ("arguments".into(), None)],
                    vec![RStatement::Expression(RExpression::constant("2"), ())],
                    (),
                ),
                (),
            ),
            RStatement::Assignment(
                RExpression::variable("func3"),
                vec![],
                RExpression::Function(
                    vec![
                        ("with".into(), None),
                        ("default".into(), Some(RExpression::constant("'arguments'"))),
                    ],
                    vec![
                        RStatement::Assignment(
                            RExpression::variable("a"),
                            vec![],
                            RExpression::Call(RExpression::boxed_variable("other"), vec![], ()),
                            (),
                        ),
                        RStatement::Expression(RExpression::variable("a"), ()),
                    ],
                    (),
                ),
                (),
            ),
        ];
        assert_eq!(expected, result);
    }

    #[test]
    fn parses_prefix_operators() {
        let code = "\
x <- !TRUE
y <- negate(!x)
-(1 + 2)";
        let result = test_parse(code);
        let expected = vec![
            RStatement::Assignment(
                RExpression::variable("x"),
                vec![],
                RExpression::Prefix("!".into(), Box::new(RExpression::constant("TRUE")), ()),
                (),
            ),
            RStatement::Assignment(
                RExpression::variable("y"),
                vec![],
                RExpression::Call(
                    RExpression::boxed_variable("negate"),
                    vec![(
                        None,
                        RExpression::Prefix("!".into(), Box::new(RExpression::variable("x")), ()),
                    )],
                    (),
                ),
                (),
            ),
            RStatement::Expression(
                RExpression::Prefix(
                    "-".into(),
                    Box::new(RExpression::Infix(
                        "+".into(),
                        Box::new(RExpression::constant("1")),
                        Box::new(RExpression::constant("2")),
                        (),
                    )),
                    (),
                ),
                (),
            ),
        ];
        assert_eq!(expected, result);
    }

    #[test]
    fn parses_infix_operators() {
        let code = "\
1 <= 3
TRUE && FALSE
'a' %custom% 'infix'
1 +
    3";
        let result = test_parse(code);
        let expected = vec![
            RStatement::Expression(
                RExpression::Infix(
                    "<=".into(),
                    Box::new(RExpression::constant("1")),
                    Box::new(RExpression::constant("3")),
                    (),
                ),
                (),
            ),
            RStatement::Expression(
                RExpression::Infix(
                    "&&".into(),
                    Box::new(RExpression::constant("TRUE")),
                    Box::new(RExpression::constant("FALSE")),
                    (),
                ),
                (),
            ),
            RStatement::Expression(
                RExpression::Infix(
                    "%custom%".into(),
                    Box::new(RExpression::constant("'a'")),
                    Box::new(RExpression::constant("'infix'")),
                    (),
                ),
                (),
            ),
            RStatement::Expression(
                RExpression::Infix(
                    "+".into(),
                    Box::new(RExpression::constant("1")),
                    Box::new(RExpression::constant("3")),
                    (),
                ),
                (),
            ),
        ];
        assert_eq!(expected, result);
    }

    #[test]
    fn parses_expression_in_parens() {
        let code = "\
1
(2)
( 1 + (2 + 3))
((1 + 2) + 3)";
        let result = test_parse(code);
        let expected = vec![
            RStatement::Expression(RExpression::constant("1"), ()),
            RStatement::Expression(RExpression::constant("2"), ()),
            RStatement::Expression(
                RExpression::Infix(
                    "+".into(),
                    Box::new(RExpression::constant("1")),
                    Box::new(RExpression::Infix(
                        "+".into(),
                        Box::new(RExpression::constant("2")),
                        Box::new(RExpression::constant("3")),
                        (),
                    )),
                    (),
                ),
                (),
            ),
            RStatement::Expression(
                RExpression::Infix(
                    "+".into(),
                    Box::new(RExpression::Infix(
                        "+".into(),
                        Box::new(RExpression::constant("1")),
                        Box::new(RExpression::constant("2")),
                        (),
                    )),
                    Box::new(RExpression::constant("3")),
                    (),
                ),
                (),
            ),
        ];
        assert_eq!(expected, result);
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
        let result = test_parse(code);
        let expected = vec![
            RStatement::If(
                RExpression::Infix(
                    "==".into(),
                    Box::new(RExpression::constant("0")),
                    Box::new(RExpression::constant("1")),
                    (),
                ),
                vec![
                    RStatement::Expression(
                        RExpression::Call(RExpression::boxed_variable("do_something"), vec![], ()),
                        (),
                    ),
                    RStatement::Empty(()),
                    RStatement::Expression(
                        RExpression::Call(
                            RExpression::boxed_variable("do_something_else"),
                            vec![],
                            (),
                        ),
                        (),
                    ),
                ],
                None,
                (),
            ),
            RStatement::If(
                RExpression::Call(RExpression::boxed_variable("is_ok"), vec![], ()),
                vec![RStatement::Expression(
                    RExpression::Call(
                        RExpression::boxed_variable("do_something_again"),
                        vec![],
                        (),
                    ),
                    (),
                )],
                None,
                (),
            ),
            RStatement::Empty(()),
            RStatement::If(
                RExpression::constant("TRUE"),
                vec![RStatement::Expression(
                    RExpression::Call(RExpression::boxed_variable("is_true"), vec![], ()),
                    (),
                )],
                Some(vec![RStatement::Expression(
                    RExpression::Call(RExpression::boxed_variable("is_false"), vec![], ()),
                    (),
                )]),
                (),
            ),
            RStatement::If(
                RExpression::constant("FALSE"),
                vec![RStatement::Expression(
                    RExpression::Call(RExpression::boxed_variable("is_false"), vec![], ()),
                    (),
                )],
                Some(vec![RStatement::If(
                    RExpression::constant("TRUE"),
                    vec![RStatement::Expression(
                        RExpression::Call(RExpression::boxed_variable("is_true"), vec![], ()),
                        (),
                    )],
                    None,
                    (),
                )]),
                (),
            ),
            RStatement::If(
                RExpression::constant("TRUE"),
                vec![RStatement::Expression(
                    RExpression::Call(RExpression::boxed_variable("cry"), vec![], ()),
                    (),
                )],
                Some(vec![RStatement::Assignment(
                    RExpression::variable("why"),
                    vec![],
                    RExpression::constant("1"),
                    (),
                )]),
                (),
            ),
        ];
        assert_eq!(expected, result);
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
        let result = test_parse(code);
        let expected = vec![
            RStatement::For(
                RExpression::variable("i"),
                RExpression::variable("something"),
                vec![
                    RStatement::Expression(
                        RExpression::Call(
                            RExpression::boxed_variable("do_something_with"),
                            vec![(None, RExpression::variable("i"))],
                            (),
                        ),
                        (),
                    ),
                    RStatement::Empty(()),
                    RStatement::Expression(
                        RExpression::Call(
                            RExpression::boxed_variable("do_something_else"),
                            vec![],
                            (),
                        ),
                        (),
                    ),
                ],
                (),
            ),
            RStatement::For(
                RExpression::variable("i"),
                RExpression::Call(RExpression::boxed_variable("get"), vec![], ()),
                vec![RStatement::Expression(
                    RExpression::Call(
                        RExpression::boxed_variable("do_something_again"),
                        vec![(None, RExpression::variable("i"))],
                        (),
                    ),
                    (),
                )],
                (),
            ),
            RStatement::For(
                RExpression::variable("row"),
                RExpression::Infix(
                    ":".into(),
                    Box::new(RExpression::constant("1")),
                    Box::new(RExpression::constant("15")),
                    (),
                ),
                vec![RStatement::Assignment(
                    RExpression::ListIndex(
                        Box::new(RExpression::variable("l")),
                        vec![Some(RExpression::variable("row"))],
                        (),
                    ),
                    vec![],
                    RExpression::variable("row"),
                    (),
                )],
                (),
            ),
        ];
        assert_eq!(expected, result);
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
        let result = test_parse(code);
        let expected = vec![
            RStatement::While(
                RExpression::Infix(
                    "<".into(),
                    Box::new(RExpression::variable("i")),
                    Box::new(RExpression::constant("10")),
                    (),
                ),
                vec![
                    RStatement::Expression(
                        RExpression::Call(RExpression::boxed_variable("do_stuff"), vec![], ()),
                        (),
                    ),
                    RStatement::Assignment(
                        RExpression::variable("i"),
                        vec![],
                        RExpression::Infix(
                            "+".into(),
                            Box::new(RExpression::variable("i")),
                            Box::new(RExpression::constant("1")),
                            (),
                        ),
                        (),
                    ),
                ],
                (),
            ),
            RStatement::While(
                RExpression::constant("TRUE"),
                vec![RStatement::Expression(
                    RExpression::Call(RExpression::boxed_variable("annoy"), vec![], ()),
                    (),
                )],
                (),
            ),
        ];
        assert_eq!(expected, result);
    }

    mod extracts_variable_name {
        use super::*;
        use pretty_assertions::assert_eq;

        #[test]
        fn from_variable() {
            let name = RExpression::variable("x").extract_variable_name();
            assert_eq!(Some("x".to_string()), name);
        }

        #[test]
        fn from_column() {
            let name = RExpression::Column(
                Box::new(RExpression::variable("x")),
                Box::new(RExpression::variable("a")),
                (),
            )
            .extract_variable_name();
            assert_eq!(Some("x".to_string()), name);
        }

        #[test]
        fn from_index() {
            let name = RExpression::Index(
                Box::new(RExpression::variable("x")),
                vec![Some(RExpression::variable("a"))],
                (),
            )
            .extract_variable_name();
            assert_eq!(Some("x".to_string()), name);
        }

        #[test]
        fn from_list_index() {
            let name = RExpression::ListIndex(
                Box::new(RExpression::variable("x")),
                vec![Some(RExpression::variable("a"))],
                (),
            )
            .extract_variable_name();
            assert_eq!(Some("x".to_string()), name);
        }

        #[test]
        fn from_colnames() {
            let name = RExpression::Call(
                RExpression::boxed_variable("colnames"),
                vec![(None, RExpression::variable("x"))],
                (),
            )
            .extract_variable_name();
            assert_eq!(Some("x".to_string()), name);
        }

        #[test]
        fn rejects_constants() {
            let name = RExpression::constant("x").extract_variable_name();
            assert_eq!(None, name);
        }

        #[test]
        fn rejects_constant_in_column() {
            let name = RExpression::Column(
                Box::new(RExpression::constant("x")),
                Box::new(RExpression::variable("a")),
                (),
            )
            .extract_variable_name();
            assert_eq!(None, name);
        }
    }

}

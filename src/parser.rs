use std::fmt::Write;

use itertools::Itertools;
use pest::Parser;

#[derive(Parser)]
#[grammar = "r.pest"]
struct RParser;

#[derive(Eq, PartialEq, Debug, Clone, Hash)]
pub struct Lines(Vec<RStmt>);

impl Lines {
    fn vec(&self) -> &Vec<RStmt> {
        &self.0
    }
}

impl std::fmt::Display for Lines {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.vec().iter().join("\n"))
    }
}

impl From<Vec<RStmt>> for Lines {
    fn from(other: Vec<RStmt>) -> Lines {
        Lines(other)
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub enum RStmt {
    Empty,
    Comment(String),
    Assignment(RExp, Vec<RExp>, RExp),
    If(RExp, Lines),
    Library(RIdentifier),
    Expression(RExp),
}

impl RStmt {
    pub fn expression(&self) -> Option<&RExp> {
        use RStmt::*;
        match self {
            Assignment(_, _, expression) => Some(expression),
            Expression(expression) => Some(expression),
            _ => None,
        }
    }
}

impl std::fmt::Display for RStmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use RStmt::*;
        match self {
            Empty => writeln!(f),
            Comment(text) => write!(f, "{}", text),
            Assignment(left, additional, right) => {
                let mut assigned = vec![left];
                assigned.append(&mut additional.iter().collect());
                for variable in assigned.iter() {
                    write!(f, "{} <- ", variable)?
                }
                write!(f, "{}", right)
            }
            If(condition, body) => write!(f, "if ({}) {{\n\t{}\n}}", condition, body),
            Library(name) => write!(f, "{}", name),
            Expression(exp) => write!(f, "{}", exp),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub enum RExp {
    Constant(String),
    Variable(RIdentifier),
    Call(RIdentifier, Vec<(Option<RIdentifier>, RExp)>),
    Column(Box<RExp>, Box<RExp>),
    Index(Box<RExp>, Vec<Option<RExp>>),
    Formula(RFormula),
    Function(Vec<(RIdentifier, Option<RExp>)>, String),
    Prefix(String, Box<RExp>),
    Infix(String, Box<RExp>, Box<RExp>),
}

impl RExp {
    pub fn constant(content: &'static str) -> RExp {
        RExp::Constant(content.to_string())
    }

    pub fn variable(content: &'static str) -> RExp {
        RExp::Variable(content.to_string())
    }
}

impl std::fmt::Display for RExp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use RExp::*;
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
            Formula(formula) => write!(f, "{}", formula),
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
                write!(f, "function ({}) {{\n{}\n}}", parameters, body)
            }
            Prefix(op, exp) => write!(f, "{}{}", op, exp),
            Infix(op, left, right) => write!(f, "{} {} {}", left, op, right),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub enum RFormula {
    OneSided(RFormulaExpression),
    TwoSided(RIdentifier, RFormulaExpression),
}

impl std::fmt::Display for RFormula {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use RFormula::*;
        match self {
            OneSided(right) => write!(f, "~ {}", right),
            TwoSided(left, right) => write!(f, "{} ~ {}", left, right),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub enum RFormulaExpression {
    Variable(RIdentifier),
    Call(RIdentifier, Vec<(Option<RIdentifier>, RExp)>),
    Plus(Box<RFormulaExpression>, RIdentifier),
    /*Minus(Box<RFormulaExpression>, RIdentifier),
    Colon(Box<RFormulaExpression>, RIdentifier),
    Star(Box<RFormulaExpression>, RIdentifier),
    In(Box<RFormulaExpression>, RIdentifier),
    Hat(Box<RFormulaExpression>, RIdentifier),*/
}

impl std::fmt::Display for RFormulaExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use RFormulaExpression::*;
        match self {
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
            Plus(left, right) => write!(f, "{} + {}", left, right),
        }
    }
}

pub type RIdentifier = String;

pub type Error = pest::error::Error<Rule>;

pub fn parse(code: &str) -> Result<Vec<RStmt>, Error> {
    let parse_result = RParser::parse(Rule::file, code)?;

    Ok(parse_result
        .filter_map(|token| match token.as_rule() {
            Rule::EOI => None,
            _ => Some(parse_line(token)),
        })
        .collect())
}

fn parse_line(line_pair: pest::iterators::Pair<Rule>) -> RStmt {
    match line_pair.as_rule() {
        Rule::empty => RStmt::Empty,
        Rule::line => {
            let line = line_pair.into_inner().next().unwrap(); // A line always contains at least a statement or a comment.
            match line.as_rule() {
                Rule::statement => {
                    let statement = line.into_inner().next().unwrap(); // Take statement out of line.
                    match statement.as_rule() {
                        Rule::raw_expression => {
                            RStmt::Expression(parse_expression(statement)) // Expression is always non-empty.
                        }
                        Rule::assignment => {
                            // Can be multiple assignment, e. g. a=b=c=1. We want to extract the right-most expression,
                            // wich is assigned to all others, and the left-most one, which prevents an empty left side.
                            let mut elements: Vec<RExp> =
                                statement.into_inner().map(parse_expression).collect();
                            let error = "Assignment did not have enough elements.";
                            let right = elements.pop().expect(error);
                            if elements.is_empty() {
                                panic!(error);
                            }
                            let left = elements.remove(0);
                            let additional = elements;
                            RStmt::Assignment(left, additional, right)
                        }
                        Rule::if_statement => {
                            let mut elements = statement.into_inner();
                            let condition = if let Some(condition_pair) = elements.next() {
                                parse_expression(condition_pair)
                            } else {
                                panic!("If statement did not have enough elements.");
                            };
                            let body: Vec<RStmt> = elements.map(parse_line).collect();
                            RStmt::If(condition, Lines::from(body))
                        }
                        Rule::library => {
                            let name = statement.into_inner().next().unwrap(); // Library name always exists.
                            RStmt::Library(name.as_str().into())
                        }
                        _ => unreachable!(),
                    }
                }
                Rule::comment => RStmt::Comment(line.as_str().to_string()),
                e => panic!("Rule: {:?}", e),
            }
        }
        _ => unreachable!(),
    }
}

fn parse_expression(expression_pair: pest::iterators::Pair<Rule>) -> RExp {
    let mut whole_expression = expression_pair.into_inner();
    let expression = whole_expression.next().unwrap(); // Expression is always non-empty.
    let mut rexp = match expression.as_rule() {
        Rule::constant => RExp::Constant(expression.as_str().to_string()),
        Rule::identifier => RExp::Variable(expression.as_str().to_string()),
        Rule::function_call => parse_function_expression(expression),
        Rule::prefix => {
            let mut prefix_expression = expression.into_inner();
            let operator = prefix_expression.next().unwrap(); // Prefix always has operator.
            let exp = prefix_expression.next().unwrap(); // Prefix always has expression.
            RExp::Prefix(
                operator.as_str().to_string(),
                Box::new(parse_expression(exp)),
            )
        }
        Rule::formula => {
            let formula_kind = expression.into_inner().next().unwrap(); // Formula always has a kind.
            match formula_kind.as_rule() {
                Rule::one_sided => {
                    let right = formula_kind.into_inner();
                    RExp::Formula(RFormula::OneSided(parse_formula_expression(right)))
                }
                Rule::two_sided => {
                    let mut formula = formula_kind.into_inner();
                    let left = formula.next().unwrap(); // Two-sided formula always has left side.
                    RExp::Formula(RFormula::TwoSided(
                        left.as_str().into(),
                        parse_formula_expression(formula),
                    ))
                }
                _ => unreachable!(),
            }
        }
        Rule::function_definition => {
            let mut function = expression.into_inner();
            let args = function.next().unwrap(); // Function always has (possibly empty) arguments.
            let args: Vec<(RIdentifier, Option<RExp>)> = args
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
            let body = function.next().unwrap(); // Function always has a body.
            RExp::Function(args, body.as_str().into())
        }
        _ => unreachable!(),
    };

    // Process all indexing expressions that follow.
    for infix in whole_expression {
        match infix.as_rule() {
            Rule::column => rexp = RExp::Column(Box::new(rexp), Box::new(parse_expression(infix))),
            Rule::index => {
                let indices = infix
                    .into_inner()
                    .map(|maybe_expression| match maybe_expression.as_rule() {
                        Rule::raw_expression => Some(parse_expression(maybe_expression)),
                        Rule::empty => None,
                        _ => unreachable!(),
                    })
                    .collect();
                rexp = RExp::Index(Box::new(rexp), indices)
            }
            Rule::infix => {
                let mut infix_operator = infix.into_inner();
                let operator = infix_operator.next().unwrap(); // Operator is always present.
                let right = infix_operator.next().unwrap(); // Infix operator always has right-hand side.
                rexp = RExp::Infix(
                    operator.as_str().into(),
                    Box::new(rexp),
                    Box::new(parse_expression(right)),
                );
            }
            _ => unreachable!(),
        }
    }

    rexp
}

fn parse_function_expression(function_pair: pest::iterators::Pair<Rule>) -> RExp {
    let mut function = function_pair.into_inner();
    let name = function.next().unwrap(); // Function name always exists.
    let maybe_arguments = function.next();
    let args: Vec<(Option<RIdentifier>, RExp)> = match maybe_arguments {
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
    RExp::Call(name.as_str().into(), args)
}

fn parse_formula_expression(mut expression: pest::iterators::Pairs<Rule>) -> RFormulaExpression {
    let first_expression = expression.next().unwrap(); // Expression always has at least one element.
    let mut result = match first_expression.as_rule() {
        Rule::identifier => RFormulaExpression::Variable(first_expression.as_str().to_string()),
        Rule::function_call => {
            let function_call = parse_function_expression(first_expression);
            if let RExp::Call(name, args) = function_call {
                RFormulaExpression::Call(name, args)
            } else {
                panic!("Expected functional call, found {}.", function_call);
            }
        }
        _ => unreachable!(),
    };
    for (operator, right) in expression.tuples() {
        match operator.as_str() {
            "+" => result = RFormulaExpression::Plus(Box::new(result), right.as_str().into()),
            // TODO: Implement missing operators.
            _ => unreachable!(),
        }
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    fn test_parse(code: &'static str) -> Vec<RStmt> {
        parse(code).unwrap_or_else(|e| panic!("{}", e))
    }

    #[test]
    fn parses_comments() {
        let code = "\
#123
#hello
# another thing   ";
        let result = test_parse(code);
        let expected = vec![
            RStmt::Comment("#123".into()),
            RStmt::Comment("#hello".into()),
            RStmt::Comment("# another thing   ".into()),
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
            RStmt::Empty,
            RStmt::Comment("# First block".into()),
            RStmt::Assignment(RExp::variable("a"), vec![], RExp::constant("1")),
            RStmt::Empty,
            RStmt::Comment("# Second block".into()),
            RStmt::Assignment(RExp::variable("b"), vec![], RExp::constant("1")),
            RStmt::Assignment(RExp::variable("c"), vec![], RExp::constant("2")),
            RStmt::Empty,
            RStmt::Empty,
            RStmt::Comment("# Third block".into()),
            RStmt::Assignment(RExp::variable("d"), vec![], RExp::constant("1")),
            RStmt::Empty,
            RStmt::Empty,
        ];
        assert_eq!(expected, result);
    }

    #[test]
    fn parses_assignments() {
        let code = "\
a <- 1
b = 2
a=b=c=1";
        let result = test_parse(code);
        let expected = vec![
            RStmt::Assignment(RExp::variable("a"), vec![], RExp::constant("1")),
            RStmt::Assignment(RExp::variable("b"), vec![], RExp::constant("2")),
            RStmt::Assignment(
                RExp::variable("a"),
                vec![RExp::variable("b"), RExp::variable("c")],
                RExp::constant("1"),
            ),
        ];
        assert_eq!(expected, result);
    }

    #[test]
    fn parses_function_calls() {
        let code = "\
empty()
single(1)
with_args(1, x, name = value)";
        let result = test_parse(code);
        let expected = vec![
            RStmt::Expression(RExp::Call("empty".into(), vec![])),
            RStmt::Expression(RExp::Call(
                "single".into(),
                vec![(None, RExp::constant("1"))],
            )),
            RStmt::Expression(RExp::Call(
                "with_args".into(),
                vec![
                    (None, RExp::constant("1")),
                    (None, RExp::variable("x")),
                    (Some("name".to_string()), RExp::variable("value")),
                ],
            )),
        ];
        assert_eq!(expected, result);
    }

    #[test]
    fn parses_strings() {
        let code = "\
'first'
\"second\"";
        let result = test_parse(code);
        let expected = vec![
            RStmt::Expression(RExp::constant("'first'")),
            RStmt::Expression(RExp::constant("\"second\"")),
        ];
        assert_eq!(expected, result);
    }

    #[test]
    fn parses_library_calls() {
        let code = "\
library(plyr)
library(MASS)";
        let result = test_parse(code);
        let expected = vec![RStmt::Library("plyr".into()), RStmt::Library("MASS".into())];
        assert_eq!(expected, result);
    }

    #[test]
    fn parses_indexing() {
        let code = "\
item$column
item[other$thing]
other[multiple, index, arguments]
get_matrix()$column[1]
item[empty,]";
        let result = test_parse(code);
        let expected = vec![
            RStmt::Expression(RExp::Column(
                Box::new(RExp::variable("item")),
                Box::new(RExp::variable("column")),
            )),
            RStmt::Expression(RExp::Index(
                Box::new(RExp::variable("item")),
                vec![Some(RExp::Column(
                    Box::new(RExp::variable("other")),
                    Box::new(RExp::variable("thing")),
                ))],
            )),
            RStmt::Expression(RExp::Index(
                Box::new(RExp::variable("other")),
                vec![
                    Some(RExp::variable("multiple")),
                    Some(RExp::variable("index")),
                    Some(RExp::variable("arguments")),
                ],
            )),
            RStmt::Expression(RExp::Index(
                Box::new(RExp::Column(
                    Box::new(RExp::Call("get_matrix".into(), vec![])),
                    Box::new(RExp::variable("column")),
                )),
                vec![Some(RExp::constant("1"))],
            )),
            RStmt::Expression(RExp::Index(
                Box::new(RExp::variable("item")),
                vec![Some(RExp::variable("empty")), None],
            )),
        ];
        assert_eq!(expected, result);
    }

    #[test]
    fn parses_formulae() {
        let code = "\
~ one_sided
two ~ sided
~ one + sided + multiple
two ~ sided + multiple
~ transform(x)
other ~ transform(x)";
        let result = test_parse(code);
        let expected = vec![
            RStmt::Expression(RExp::Formula(RFormula::OneSided(
                RFormulaExpression::Variable("one_sided".into()),
            ))),
            RStmt::Expression(RExp::Formula(RFormula::TwoSided(
                "two".into(),
                RFormulaExpression::Variable("sided".into()),
            ))),
            RStmt::Expression(RExp::Formula(RFormula::OneSided(RFormulaExpression::Plus(
                Box::new(RFormulaExpression::Plus(
                    Box::new(RFormulaExpression::Variable("one".into())),
                    "sided".into(),
                )),
                "multiple".into(),
            )))),
            RStmt::Expression(RExp::Formula(RFormula::TwoSided(
                "two".into(),
                RFormulaExpression::Plus(
                    Box::new(RFormulaExpression::Variable("sided".into())),
                    "multiple".into(),
                ),
            ))),
            RStmt::Expression(RExp::Formula(RFormula::OneSided(RFormulaExpression::Call(
                "transform".into(),
                vec![(None, RExp::Variable("x".into()))],
            )))),
            RStmt::Expression(RExp::Formula(RFormula::TwoSided(
                "other".into(),
                RFormulaExpression::Call(
                    "transform".into(),
                    vec![(None, RExp::Variable("x".into()))],
                ),
            ))),
        ];
        assert_eq!(expected, result);
    }

    #[test]
    fn parses_function_definition() {
        let code = "\
func1 <- function () 1
func2 <- function (with, arguments) 2
func3 <- function (with, default = 'arguments') 3 ";
        let result = test_parse(code);
        let expected = vec![
            RStmt::Assignment(
                RExp::variable("func1"),
                vec![],
                RExp::Function(vec![], "1".into()),
            ),
            RStmt::Assignment(
                RExp::variable("func2"),
                vec![],
                RExp::Function(
                    vec![("with".into(), None), ("arguments".into(), None)],
                    "2".into(),
                ),
            ),
            RStmt::Assignment(
                RExp::variable("func3"),
                vec![],
                RExp::Function(
                    vec![
                        ("with".into(), None),
                        ("default".into(), Some(RExp::constant("'arguments'"))),
                    ],
                    "3".into(),
                ),
            ),
        ];
        assert_eq!(expected, result);
    }

    #[test]
    fn parses_prefix_operators() {
        let code = "\
x <- !TRUE
y <- negate(!x)";
        let result = test_parse(code);
        let expected = vec![
            RStmt::Assignment(
                RExp::variable("x"),
                vec![],
                RExp::Prefix("!".into(), Box::new(RExp::constant("TRUE"))),
            ),
            RStmt::Assignment(
                RExp::variable("y"),
                vec![],
                RExp::Call(
                    "negate".into(),
                    vec![(
                        None,
                        RExp::Prefix("!".into(), Box::new(RExp::variable("x"))),
                    )],
                ),
            ),
        ];
        assert_eq!(expected, result);
    }

    #[test]
    fn parses_infix_operators() {
        let code = "\
1 < 3
TRUE && FALSE
'a' %custom% 'infix'";
        let result = test_parse(code);
        let expected = vec![
            RStmt::Expression(RExp::Infix(
                "<".into(),
                Box::new(RExp::constant("1")),
                Box::new(RExp::constant("3")),
            )),
            RStmt::Expression(RExp::Infix(
                "&&".into(),
                Box::new(RExp::constant("TRUE")),
                Box::new(RExp::constant("FALSE")),
            )),
            RStmt::Expression(RExp::Infix(
                "%custom%".into(),
                Box::new(RExp::constant("'a'")),
                Box::new(RExp::constant("'infix'")),
            )),
        ];
        assert_eq!(expected, result);
    }

    #[test]
    fn parses_expression_in_parens() {
        let code = "\
1
(2)";
        let result = test_parse(code);
        let expected = vec![
            RStmt::Expression(RExp::constant("1")),
            RStmt::Expression(RExp::constant("2")),
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
";
        let result = test_parse(code);
        let expected = vec![
            RStmt::If(
                RExp::Infix(
                    "==".into(),
                    Box::new(RExp::constant("0")),
                    Box::new(RExp::constant("1")),
                ),
                Lines::from(vec![
                    RStmt::Expression(RExp::Call("do_something".into(), vec![])),
                    RStmt::Empty,
                    RStmt::Expression(RExp::Call("do_something_else".into(), vec![])),
                ]),
            ),
            RStmt::If(
                RExp::Call("is_ok".into(), vec![]),
                Lines::from(vec![RStmt::Expression(RExp::Call(
                    "do_something_again".into(),
                    vec![],
                ))]),
            ),
        ];
        assert_eq!(expected, result);
    }
}

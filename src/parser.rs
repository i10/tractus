use itertools::Itertools;
use pest::Parser;

#[derive(Parser)]
#[grammar = "r.pest"]
struct RParser;

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub enum RStmt {
    Empty,
    Comment(String),
    Assignment(RExp, RExp),
    Library(RIdentifier),
    Expression(RExp),
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

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub enum RFormula {
    OneSided(RFormulaExpression),
    TwoSided(RIdentifier, RFormulaExpression),
}

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub enum RFormulaExpression {
    Variable(RIdentifier),
    Plus(Box<RFormulaExpression>, RIdentifier),
    Minus(Box<RFormulaExpression>, RIdentifier),
    Colon(Box<RFormulaExpression>, RIdentifier),
    Star(Box<RFormulaExpression>, RIdentifier),
    In(Box<RFormulaExpression>, RIdentifier),
    Hat(Box<RFormulaExpression>, RIdentifier),
}

pub type RIdentifier = String;

type Error = pest::error::Error<Rule>;

pub fn parse(code: &str) -> Result<Vec<RStmt>, Error> {
    let mut parse_result = RParser::parse(Rule::file, code)?;

    let file = parse_result.next().unwrap();
    Ok(file
        .into_inner()
        .filter_map(|token| match token.as_rule() {
            Rule::statement => {
                let maybe_line = token.into_inner().next();
                match maybe_line {
                    None => Some(RStmt::Empty), // empty line
                    Some(line) => {
                        match line.as_rule() {
                            Rule::comment => Some(RStmt::Comment(line.as_str().to_string())),
                            Rule::expression => {
                                Some(RStmt::Expression(parse_expression(line))) // Expression is always non-empty.
                            }
                            Rule::assignment => {
                                let mut assignment = line.into_inner();
                                let left = assignment.next().unwrap(); // Left-hand side always exists.
                                let right = assignment.next().unwrap(); // Righ-hand side always exists.
                                let left = parse_expression(left);
                                let right = parse_expression(right);
                                Some(RStmt::Assignment(left, right))
                            }
                            Rule::library => {
                                let name = line.into_inner().next().unwrap(); // Library name always exists.
                                Some(RStmt::Library(name.as_str().into()))
                            }
                            _ => unreachable!(),
                        }
                    }
                }
            }
            Rule::EOI => None,
            _ => unreachable!(),
        })
        .collect())
}

fn parse_expression(expression_pair: pest::iterators::Pair<'_, Rule>) -> RExp {
    let mut whole_expression = expression_pair.into_inner();
    let expression = whole_expression.next().unwrap(); // Expression is always non-empty.
    let mut rexp = match expression.as_rule() {
        Rule::constant => RExp::Constant(expression.as_str().to_string()),
        Rule::identifier => RExp::Variable(expression.as_str().to_string()),
        Rule::function_call => {
            let mut function = expression.into_inner();
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
        Rule::formula => {
            let formula_kind = expression.into_inner().next().unwrap(); // Formula always has a kind.
            println!("{:?}", formula_kind);
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
                        Rule::expression => Some(parse_expression(maybe_expression)),
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

fn parse_formula_expression(
    mut expression: pest::iterators::Pairs<'_, Rule>,
) -> RFormulaExpression {
    let first = expression.next().unwrap();
    let mut result = RFormulaExpression::Variable(first.as_str().into()); // Right-hand side of formula always has at least one element.
    for (operator, right) in expression.tuples() {
        match operator.as_str() {
            "+" => result = RFormulaExpression::Plus(Box::new(result), right.as_str().into()),
            _ => unreachable!(),
        }
    }
    result
}

#[cfg(test)]
mod tests {
    use super::{parse, RExp, RFormula, RFormulaExpression, RStmt};

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
            RStmt::Empty,
            RStmt::Comment("# another thing   ".into()),
        ];
        assert_eq!(expected, result);
    }

    #[test]
    fn parses_assignments() {
        let code = "\
a <- 1
b = 2";
        let result = test_parse(code);
        let expected = vec![
            RStmt::Assignment(RExp::variable("a"), RExp::constant("1")),
            RStmt::Assignment(RExp::variable("b"), RExp::constant("2")),
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
two ~ sided + multiple";
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
            RStmt::Assignment(RExp::variable("func1"), RExp::Function(vec![], "1".into())),
            RStmt::Assignment(
                RExp::variable("func2"),
                RExp::Function(
                    vec![("with".into(), None), ("arguments".into(), None)],
                    "2".into(),
                ),
            ),
            RStmt::Assignment(
                RExp::variable("func3"),
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
}

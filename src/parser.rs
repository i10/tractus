use pest::Parser;

#[derive(Parser)]
#[grammar = "r.pest"]
struct RParser;

#[derive(PartialEq, Debug)]
pub enum RStmt {
    Empty,
    Comment(String),
    Assignment(RExp, RExp),
    Library(RIdentifier),
    Expression(RExp),
}

#[derive(PartialEq, Debug)]
pub enum RExp {
    Constant(String),
    Variable(RIdentifier),
    Call(RIdentifier, Vec<(Option<RIdentifier>, RExp)>),
    Column(Box<RExp>, Box<RExp>),
    Index(Box<RExp>, Vec<RExp>),
}

impl RExp {
    pub fn constant(content: &'static str) -> RExp {
        RExp::Constant(content.to_string())
    }

    pub fn variable(content: &'static str) -> RExp {
        RExp::Variable(content.to_string())
    }
}

pub type RIdentifier = String;

type Error = pest::error::Error<Rule>;

pub fn parse(code: &'static str) -> Result<Vec<RStmt>, Error> {
    let mut parse_result = RParser::parse(Rule::file, code)?;

    let file = parse_result.next().unwrap();
    Ok(file
        .into_inner()
        .filter_map(|token| match token.as_rule() {
            Rule::statement => {
                let maybe_line = token.into_inner().next();
                match maybe_line {
                    None => None, // empty line
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
    println!("{:?}", expression_pair);
    let mut whole_expression = expression_pair.into_inner();
    let expression = whole_expression.next().unwrap(); // Expression is always non-empty.
    let mut rexp = match expression.as_rule() {
        Rule::constant => RExp::Constant(expression.as_str().to_string()),
        Rule::identifier => RExp::Variable(expression.as_str().to_string()),
        Rule::function => {
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
        _ => unreachable!(),
    };

    // Process all indexing expressions that follow.
    for index in whole_expression {
        match index.as_rule() {
            Rule::column => rexp = RExp::Column(Box::new(rexp), Box::new(parse_expression(index))),
            Rule::index => {
                let indices = index.into_inner().map(parse_expression).collect();
                rexp = RExp::Index(Box::new(rexp), indices)
            }
            _ => unreachable!(),
        }
    }

    rexp
}
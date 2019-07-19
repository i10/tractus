use pest::Parser;

#[derive(Parser)]
#[grammar = "r.pest"]
struct RParser;

#[derive(PartialEq, Debug)]
pub enum RExp {
    Comment(String),
    Assignment(String, String),
    Call(String, Vec<(Option<String>, String)>),
}

type Error = pest::error::Error<Rule>;

pub fn parse(code: &'static str) -> Result<Vec<RExp>, Error> {
    let mut parse_result = RParser::parse(Rule::file, code)?;

    let file = parse_result.next().unwrap();
    Ok(file
        .into_inner()
        .filter_map(|token| match token.as_rule() {
            Rule::line => {
                let maybe_line = token.into_inner().next();
                match maybe_line {
                    None => None, // empty line
                    Some(line) => {
                        match line.as_rule() {
                            Rule::comment => Some(RExp::Comment(line.as_str().to_string())),
                            Rule::assignment => {
                                let mut assignment = line.into_inner();
                                let left = assignment.next().unwrap(); // Left-hand side always exists.
                                let right = assignment.next().unwrap(); // Righ-hand side always exists.
                                Some(RExp::Assignment(
                                    left.as_str().to_string(),
                                    right.as_str().to_string(),
                                ))
                            }
                            Rule::function => {
                                let mut function = line.into_inner();
                                let name = function.next().unwrap(); // Function name always exists.
                                let maybe_arguments = function.next();
                                let args = match maybe_arguments {
                                    Some(args) => {
                                        args.into_inner()
                                            .map(|arg| {
                                                match arg.as_rule() {
                                                    Rule::named_argument => {
                                                        let mut argument = arg.into_inner();
                                                        let key = argument.next().unwrap(); // Key always exists.
                                                        let value = argument.next().unwrap(); // Value always exists.
                                                        (
                                                            Some(key.as_str().to_string()),
                                                            value.as_str().to_string(),
                                                        )
                                                    }
                                                    Rule::unnamed_argument => {
                                                        let value =
                                                            arg.into_inner().next().unwrap(); // Argument's value always exists.
                                                        (None, value.as_str().to_string())
                                                    }
                                                    _ => unreachable!(),
                                                }
                                            })
                                            .collect()
                                    }
                                    None => vec![],
                                };
                                Some(RExp::Call(name.as_str().to_string(), args))
                            }
                            _ => None,
                        }
                    }
                }
            }
            Rule::EOI => None,
            _ => unreachable!(),
        })
        .collect())
}
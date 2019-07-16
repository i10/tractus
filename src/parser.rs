use pest::Parser;

#[derive(Parser)]
#[grammar = "r.pest"]
struct RParser;

#[derive(PartialEq, Debug)]
pub enum RExp {
    Comment(String),
    Assignment(String, String),
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
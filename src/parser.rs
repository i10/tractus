use pest::Parser;

#[derive(Parser)]
#[grammar = "r.pest"]
struct RParser;

#[derive(PartialEq, Debug)]
pub enum RExp {
    Comment(String),
}

type Error = pest::error::Error<Rule>;

pub fn parse(code: &'static str) -> Result<Vec<RExp>, Error> {
    let mut parse_result = RParser::parse(Rule::file, code)?;

    let file = parse_result.next().unwrap();
    Ok(file
        .into_inner()
        .filter_map(|token| match token.as_rule() {
            Rule::line => {
                let line = token.into_inner().next().unwrap();
                match line.as_rule() {
                    Rule::comment => Some(RExp::Comment(line.as_str().to_string())),
                    _ => None,
                }
            }
            Rule::EOI => None,
            _ => unreachable!(),
        })
        .collect())
}
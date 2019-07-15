#[macro_use]
extern crate pest_derive;

mod parser;

#[cfg(test)]
mod tests {
    use crate::parser::{parse, RExp};

    #[test]
    fn parses_comments() {
        let code = "
#123
#hello

# another thing   ";
        let result = parse(code);
        let expected = vec!["#123", "#hello", "# another thing   "]
            .iter()
            .map(|text| RExp::Comment(text.to_string()))
            .collect();
        assert_eq!(Ok(expected), result);
    }
}

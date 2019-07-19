#[macro_use]
extern crate pest_derive;

mod parser;

pub use parser::{parse, RExp};

#[cfg(test)]
mod tests {
    use crate::parser::{parse, RExp};

    #[test]
    fn parses_comments() {
        let code = "\
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

    #[test]
    fn parses_assignments() {
        let code = "\
a <- 1
b = 2";
        let result = parse(code);
        let expected = vec![
            RExp::Assignment("a".into(), "1".into()),
            RExp::Assignment("b".into(), "2".into()),
        ];
        assert_eq!(Ok(expected), result);
    }

    #[test]
    fn parses_function_calls() {
        let code = "\
empty()
single(1)
with_args(1, x, name = value)";
        let result = parse(code);
        let expected = vec![
            RExp::Call("empty".into(), vec![]),
            RExp::Call("single".into(), vec![(None, "1".into())]),
            RExp::Call(
                "with_args".into(),
                vec![
                    (None, "1".into()),
                    (None, "x".into()),
                    (Some("name".into()), "value".into()),
                ],
            ),
        ];
        assert_eq!(Ok(expected), result);
    }
}

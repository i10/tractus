use std::collections::BTreeSet;
use std::iter::FromIterator;

use crate::parser::RExpression;

pub type Hypothesis = String;

pub fn detect_hypotheses<T>(expression: &RExpression<T>) -> BTreeSet<Hypothesis> {
    use RExpression::*;
    match expression {
        TwoSidedFormula(left, right, _) => {
            BTreeSet::from_iter(vec![format!("{} ~ {}", left, right)])
        }

        // Reference magic to work around Box. The box pattern is currently only available in nightly.
        Column(left, dependent, _) => match &**left {
            Index(variable, inner, _) => {
                // variable[variable$independent == "level",]$dependent
                // |----------------left--------------------|
                //          |-----------inner--------------|
                //          |----inner_left----|
                // If the column matches the above format, then return the hypotheses, else return nothing.
                if let [Some(Infix(operator, inner_left, _, _)), None] = inner.as_slice() {
                    if operator == "==" {
                        if let Column(inner_variable, independent, _) = &**inner_left {
                            if let Variable(_, _) = &**independent {
                                if let (Variable(first, _), Variable(second, _)) =
                                    (&**variable, &**inner_variable)
                                {
                                    if first == second {
                                        return BTreeSet::from_iter(vec![format!(
                                            "{} ~ {}",
                                            dependent, independent
                                        )]);
                                    }
                                }
                            }
                        }
                    }
                }
                BTreeSet::new()
            }

            left => detect_hypotheses(left),
        },

        Call(_, args, _) => args
            .iter()
            .map(|(_, exp)| detect_hypotheses(exp))
            .flatten()
            .collect(),
        _ => BTreeSet::new(),
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use crate::parser::Parsed;

    use super::*;

    #[test]
    fn parses_formula() {
        let code = r#"dependent ~ independent"#;
        let expected = BTreeSet::from_iter(vec!["dependent ~ independent".to_string()]);
        test_hypothesis(expected, code);
    }

    #[test]
    fn parses_formula_notation_in_call() {
        let code = r#"test(speed ~ layout + age)"#;
        let expected = BTreeSet::from_iter(vec!["speed ~ layout + age".to_string()]);
        test_hypothesis(expected, code);
    }

    #[test]
    fn parses_column_hypothesis() {
        let code = r#"variable[variable$independent == "level",]$dependent"#;
        let expected = BTreeSet::from_iter(vec!["dependent ~ independent".to_string()]);
        test_hypothesis(expected, code);
    }

    #[test]
    fn parses_column_hypothesis_in_call() {
        let code = r#"fitdistr(kbd[kbd$Layout == "QWERTY",]$Speed, "lognormal")$estimate"#;
        let expected = BTreeSet::from_iter(vec!["Speed ~ Layout".to_string()]);
        test_hypothesis(expected, code);
    }

    fn test_hypothesis(expected: BTreeSet<Hypothesis>, code: &'static str) {
        let parsed = Parsed::parse(code).unwrap();
        let stmt = parsed.into_iter().next().unwrap();
        let exp = stmt.expression().unwrap();
        let result = detect_hypotheses(exp);

        assert_eq!(expected, result);
    }

}

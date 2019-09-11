use std::collections::BTreeSet;
use std::iter::FromIterator;
use std::ops::Deref;

use crate::parser::{Expression, RIdentifier};

pub type Hypothesis = String;

pub type Hypotheses = BTreeSet<Hypothesis>;

/// Analyzes the `expression` for Hypotheses.
/// 
/// Requires the expression to have all dependencies inlined, or hypothesis behind variables may not be detected.
pub fn detect_hypotheses(expression: &Expression) -> Hypotheses {
    use Expression::*;
    match expression {
        TwoSidedFormula(left, right) => BTreeSet::from_iter(vec![format!("{} ~ {}", left, right)]),

        // Because of the Box, we cannot (in current Rust) use one general pattern, but have to go step by step with some referencing magic.
        Column(left, dependent) => match &**left {
            Index(_variable, inner) => {
                // variable[variable$independent == "level",]$dependent
                // |----------------left--------------------|
                //          |-----------inner--------------|
                //          |----inner_left----|
                // If the column matches the above format, then return the hypotheses, else return nothing.

                //if let [Some(Infix(operator, inner_left, _, _)), None] = inner.as_slice() {
                if inner.len() == 2 && inner[1].is_none() {
                    if let Some(infix) = &inner[0] {
                        if let Infix(_, inner_left, _) = &*infix {
                            if let Column(_, independent) = &**inner_left {
                                if let Variable(_) = &**independent {
                                    if let Variable(_) = &**dependent {
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
            // subset(data, independent == "level")$dependent
            Call(fun, args) => {
                if let Variable(fun_name) = fun.deref() {
                    if fun_name == "subset" {
                        if let Some(right) = args.get(1) {
                            if let Infix(_, independent, _) = &right.1 {
                                if let Variable(_) = &**independent {
                                    if let Variable(_) = &**dependent {
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
                detect_hypotheses_in_args(args)
            }

            left => detect_hypotheses(left),
        },

        Call(_, args) => detect_hypotheses_in_args(args),
        _ => BTreeSet::new(),
    }
}

/// Helper function for extracting and merging the hypotheses out of function arguments.
fn detect_hypotheses_in_args(args: &[(Option<RIdentifier>, Expression)]) -> Hypotheses {
    args.iter()
        .map(|(_, exp)| detect_hypotheses(exp))
        .flatten()
        .collect()
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use crate::parser;

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
    fn parses_inlined_hypothesis() {
        let code = r#"read("file")[read("file")$independent == "level",]$dependent"#;
        let expected = BTreeSet::from_iter(vec!["dependent ~ independent".to_string()]);
        test_hypothesis(expected, code);
    }

    #[test]
    fn parses_column_hypothesis_in_call() {
        let code = r#"fitdistr(kbd[kbd$Layout == "QWERTY",]$Speed, "lognormal")$estimate"#;
        let expected = BTreeSet::from_iter(vec!["Speed ~ Layout".to_string()]);
        test_hypothesis(expected, code);
    }

    #[test]
    fn parses_subset_hypothesis() {
        let code = r#"subset(data, independent < 3)$dependent"#;
        let expected = BTreeSet::from_iter(vec!["dependent ~ independent".to_string()]);
        test_hypothesis(expected, code);
    }

    fn test_hypothesis(expected: BTreeSet<Hypothesis>, code: &'static str) {
        let parsed = parser::parse_statements(code).unwrap();
        let stmt = parsed.into_iter().next().unwrap().0;
        let exp = stmt.expression().unwrap();
        let result = detect_hypotheses(&exp);

        assert_eq!(expected, result);
    }

}

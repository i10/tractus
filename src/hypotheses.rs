use std::collections::BTreeSet;
use std::iter::FromIterator;
use std::ops::Deref;
use std::rc::Rc;

use crate::parser::{RExpression, RIdentifier};

pub type Hypothesis = String;

/// Requires the expression to have all dependencies inlined!
pub fn detect_hypotheses<T>(expression: &RExpression<T>) -> BTreeSet<Hypothesis> {
    use RExpression::*;
    match expression {
        TwoSidedFormula(left, right, _) => {
            BTreeSet::from_iter(vec![format!("{} ~ {}", left, right)])
        }

        // Because of the Rc, we cannot use one general pattern, but have to go step by step.
        Column(left, dependent, _) => match left.deref() {
            Index(variable, inner, _) => {
                // variable[variable$independent == "level",]$dependent
                // |----------------left--------------------|
                //          |-----------inner--------------|
                //          |----inner_left----|
                // If the column matches the above format, then return the hypotheses, else return nothing.

                //if let [Some(Infix(operator, inner_left, _, _)), None] = inner.as_slice() {
                if inner.len() == 2 && inner[1].is_none() {
                    if let Some(infix) = &inner[0] {
                        if let Infix(_, inner_left, _, _) = infix.deref() {
                            if let Column(_, independent, _) = inner_left.deref() {
                                if let Variable(_, _) = independent.deref() {
                                    if let Variable(_, _) = dependent.deref() {
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
            Call(fun, args, _) => {
                if let Variable(fun_name, _) = fun.deref() {
                    if fun_name == "subset" {
                        if let Some(right) = args.get(1) {
                            if let Infix(_, independent, _, _) = right.1.deref() {
                                if let Variable(_, _) = independent.deref() {
                                    if let Variable(_, _) = dependent.deref() {
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

        Call(_, args, _) => detect_hypotheses_in_args(args),
        _ => BTreeSet::new(),
    }
}

fn detect_hypotheses_in_args<T>(
    args: &Vec<(Option<RIdentifier>, Rc<RExpression<T>>)>,
) -> BTreeSet<Hypothesis> {
    args.iter()
        .map(|(_, exp)| detect_hypotheses(exp))
        .flatten()
        .collect()
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
        let parsed = Parsed::parse(code).unwrap();
        let stmt = parsed.into_iter().next().unwrap();
        let exp = stmt.expression().unwrap();
        let result = detect_hypotheses(&exp);

        assert_eq!(expected, result);
    }

}

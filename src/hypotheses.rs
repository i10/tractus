use std::cmp::Ordering;
use std::hash::Hash;
use std::collections::HashSet;
use std::iter::FromIterator;

use crate::parser::{RExpression};

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct Hypothesis<'a, T: Eq> {
    pub left: &'a RExpression<T>,
    pub right: &'a RExpression<T>
}

impl<'a, T:Eq> std::fmt::Display for Hypothesis<'a,T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} ~ {}", self.left, self.right)
    }
}

impl<'a,T:Eq> Ord for Hypothesis<'a,T> {
    fn cmp(&self, other: &Self) -> Ordering {
        format!("{}", self).cmp(&format!("{}", other))
    }
}

impl<'a,T:Eq> PartialOrd for Hypothesis<'a,T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

pub fn detect_hypotheses<T:Eq+ Hash>(expression: &RExpression<T>) -> HashSet<Hypothesis<T>> {
    use RExpression::*;
    match expression {
        TwoSidedFormula(left, right, _) => HashSet::from_iter(vec![Hypothesis {
            left: left,
            right: right,
        }]),

        // variable[variable$independent == "level",]$dependent
        // |----------------left--------------------|
        //          |-----------inner--------------|
        //          |----inner_left----|
        Column(left, dependent, _) => match &**left {
            // Reference magic to work around Box. The box pattern is currently only available in nightly.
            Index(variable, inner, _) => match inner.as_slice() {
                [Some(Infix(operator, inner_left, _, _)), None] => {
                    if operator == "==" {
                        match &**inner_left {
                            Column(inner_variable, independent, _) => {
                                if let Variable(_, _) = &**independent {
                                    if inner_variable == variable {
                                        HashSet::from_iter(vec![Hypothesis {
                                            left: dependent,
                                            right: independent,
                                        }])
                                    } else {
                                        HashSet::new()
                                    }
                                } else {
                                    HashSet::new()
                                }
                            }
                            _ => HashSet::new(),
                        }
                    } else {
                        HashSet::new()
                    }
                }
                _ => HashSet::new(),
            },

            left => detect_hypotheses(left),
        },

        Call(_, args, _) => args
            .iter()
            .map(|(_, exp)| detect_hypotheses(exp))
            .flatten()
            .collect(),
        _ => HashSet::new(),
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use super::*;

    #[test]
    fn parses_formula() {
        let expression = RExpression::TwoSidedFormula(
            Box::new(RExpression::variable("dependent")),
            Box::new(RExpression::variable("independent")),()
        );
        let result = detect_hypotheses(&expression);
        let left = RExpression::variable("dependent");
        let right = RExpression::variable("independent");
        let expected = HashSet::from_iter(vec![Hypothesis {
            left: &left,
            right: &right,
        }]);
        assert_eq!(expected, result);
    }

    #[test]
    fn parses_formula_notation_in_call() {
        let expression = RExpression::Call(
            RExpression::boxed_variable("test"),
            vec![(
                None,
                RExpression::TwoSidedFormula(
                    Box::new(RExpression::variable("speed")),
                    Box::new(RExpression::Infix(
                        "+".into(),
                        Box::new(RExpression::variable("layout")),
                        Box::new(RExpression::variable("age")),()
                    )),()
                ),
            )],()
        );
        let result = detect_hypotheses(&expression);
        let left = RExpression::variable("speed");
        let right = RExpression::Infix(
            "+".into(),
            Box::new(RExpression::variable("layout")),
            Box::new(RExpression::variable("age")),()
        );
        let expected = HashSet::from_iter(vec![Hypothesis {
            left: &left,
            right: &right,
        }]);
        assert_eq!(expected, result);
    }

    #[test]
    fn parses_column_hypothesis() {
        // variable[variable$independent == "level",]$dependent
        let expression = RExpression::Column(
            Box::new(RExpression::Index(
                Box::new(RExpression::variable("variable")),
                vec![
                    Some(RExpression::Infix(
                        "==".into(),
                        Box::new(RExpression::Column(
                            Box::new(RExpression::variable("variable")),
                            Box::new(RExpression::variable("independent")),()
                        )),
                        Box::new(RExpression::constant("\"level\"")),()
                    )),
                    None,
                ],()
            )),
            Box::new(RExpression::variable("dependent")),()
        );
        let result = detect_hypotheses(&expression);
        let left = RExpression::variable("dependent");
        let right = RExpression::variable("independent");
        let expected = HashSet::from_iter(vec![Hypothesis {
            left: &left,
            right: &right,
        }]);
        assert_eq!(expected, result);
    }

    #[test]
    fn parses_column_hypothesis_in_call() {
        // fitdistr(kbd[kbd$Layout == "QWERTY",]$Speed, "lognormal")$estimate
        let expression = RExpression::Column(
            Box::new(RExpression::Call(
                RExpression::boxed_variable("fitdistr"),
                vec![
                    (
                        None,
                        RExpression::Column(
                            Box::new(RExpression::Index(
                                Box::new(RExpression::variable("kbd")),
                                vec![
                                    Some(RExpression::Infix(
                                        "==".into(),
                                        Box::new(RExpression::Column(
                                            Box::new(RExpression::variable("kbd")),
                                            Box::new(RExpression::variable("Layout")),()
                                        )),
                                        Box::new(RExpression::constant("\"QWERTY\"")),()
                                    )),
                                    None,
                                ],()
                            )),
                            Box::new(RExpression::variable("Speed")),()
                        ),
                    ),
                    (None, RExpression::constant("\"lognormal\""))
                ],()
            )),
            Box::new(RExpression::variable("estimate")),()
        );
        let result = detect_hypotheses(&expression);
        let left = RExpression::variable("Speed");
        let right = RExpression::variable("Layout");
        let expected = HashSet::from_iter(vec![Hypothesis {
            left: &left,
            right: &right,
        }]);
        assert_eq!(expected, result);
    }
}

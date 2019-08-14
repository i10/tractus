use std::cmp::Ordering;
use std::collections::HashSet;
use std::iter::FromIterator;

use crate::parser::{RExp, RFormula};

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct Hypothesis<'a> {
    pub left: &'a RExp,
    pub right: &'a RExp,
}

impl<'a> std::fmt::Display for Hypothesis<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} ~ {}", self.left, self.right)
    }
}

impl<'a> Ord for Hypothesis<'a> {
    fn cmp(&self, other: &Self) -> Ordering {
        format!("{}", self).cmp(&format!("{}", other))
    }
}

impl<'a> PartialOrd for Hypothesis<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

pub fn detect_hypotheses(expression: &RExp) -> HashSet<Hypothesis> {
    use RExp::*;
    match expression {
        Formula(RFormula::TwoSided(left, right)) => HashSet::from_iter(vec![Hypothesis {
            left: left,
            right: right,
        }]),

        // variable[variable$independent == "level",]$dependent
        // |----------------left--------------------|
        //          |-----------inner--------------|
        //          |----inner_left----|
        Column(left, dependent) => match &**left {
            // Reference magic to work around Box. The box pattern is currently only available in nightly.
            Index(variable, inner) => match inner.as_slice() {
                [Some(Infix(operator, inner_left, _)), None] => {
                    if operator == "==" {
                        match &**inner_left {
                            Column(inner_variable, independent) => {
                                if let Variable(_) = &**independent {
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

        Call(_, args) => args
            .iter()
            .map(|(_, exp)| detect_hypotheses(exp))
            .flatten()
            .collect(),
        _ => HashSet::new(),
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::RFormula;
    use pretty_assertions::assert_eq;

    use super::*;

    #[test]
    fn parses_formula() {
        let expression = RExp::Formula(RFormula::TwoSided(
            Box::new(RExp::variable("dependent")),
            Box::new(RExp::variable("independent")),
        ));
        let result = detect_hypotheses(&expression);
        let left = RExp::variable("dependent");
        let right = RExp::variable("independent");
        let expected = HashSet::from_iter(vec![Hypothesis {
            left: &left,
            right: &right,
        }]);
        assert_eq!(expected, result);
    }

    #[test]
    fn parses_formula_notation_in_call() {
        let expression = RExp::Call(
            RExp::boxed_variable("test"),
            vec![(
                None,
                RExp::Formula(RFormula::TwoSided(
                    Box::new(RExp::variable("speed")),
                    Box::new(RExp::Infix(
                        "+".into(),
                        Box::new(RExp::variable("layout")),
                        Box::new(RExp::variable("age")),
                    )),
                )),
            )],
        );
        let result = detect_hypotheses(&expression);
        let left = RExp::variable("speed");
        let right = RExp::Infix(
            "+".into(),
            Box::new(RExp::variable("layout")),
            Box::new(RExp::variable("age")),
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
        let expression = RExp::Column(
            Box::new(RExp::Index(
                Box::new(RExp::variable("variable")),
                vec![
                    Some(RExp::Infix(
                        "==".into(),
                        Box::new(RExp::Column(
                            Box::new(RExp::variable("variable")),
                            Box::new(RExp::variable("independent")),
                        )),
                        Box::new(RExp::constant("\"level\"")),
                    )),
                    None,
                ],
            )),
            Box::new(RExp::variable("dependent")),
        );
        let result = detect_hypotheses(&expression);
        let left = RExp::variable("dependent");
        let right = RExp::variable("independent");
        let expected = HashSet::from_iter(vec![Hypothesis {
            left: &left,
            right: &right,
        }]);
        assert_eq!(expected, result);
    }

    #[test]
    fn parses_column_hypothesis_in_call() {
        // fitdistr(kbd[kbd$Layout == "QWERTY",]$Speed, "lognormal")$estimate
        let expression = RExp::Column(
            Box::new(RExp::Call(
                RExp::boxed_variable("fitdistr"),
                vec![
                    (
                        None,
                        RExp::Column(
                            Box::new(RExp::Index(
                                Box::new(RExp::variable("kbd")),
                                vec![
                                    Some(RExp::Infix(
                                        "==".into(),
                                        Box::new(RExp::Column(
                                            Box::new(RExp::variable("kbd")),
                                            Box::new(RExp::variable("Layout")),
                                        )),
                                        Box::new(RExp::constant("\"QWERTY\"")),
                                    )),
                                    None,
                                ],
                            )),
                            Box::new(RExp::variable("Speed")),
                        ),
                    ),
                    (None, RExp::constant("\"lognormal\"")),
                ],
            )),
            Box::new(RExp::variable("estimate")),
        );
        let result = detect_hypotheses(&expression);
        let left = RExp::variable("Speed");
        let right = RExp::variable("Layout");
        let expected = HashSet::from_iter(vec![Hypothesis {
            left: &left,
            right: &right,
        }]);
        assert_eq!(expected, result);
    }
}

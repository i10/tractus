use std::cmp::Ordering;
use std::collections::HashSet;
use std::iter::FromIterator;

use crate::parser::{RExp, RFormula, RFormulaExpression, RIdentifier};

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct Hypothesis {
    pub left: RIdentifier,
    pub right: RFormulaExpression,
}

impl std::fmt::Display for Hypothesis {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} ~ {}", self.left, self.right)
    }
}

impl Ord for Hypothesis {
    fn cmp(&self, other: &Self) -> Ordering {
        format!("{}", self).cmp(&format!("{}", other))
    }
}

impl PartialOrd for Hypothesis {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

pub fn detect_hypotheses(expression: &RExp) -> HashSet<Hypothesis> {
    use RExp::*;
    match expression {
        Formula(RFormula::TwoSided(left, right)) => HashSet::from_iter(vec![Hypothesis {
            left: left
                .extract_variable_name()
                .expect("Left side of formula was not a valid identifier."),
            right: right.clone(),
        }]),

        // variable[variable$independent == "level",]$dependent
        // |----------------left--------------------| |-right-|
        //          |-----------inner--------------|
        //          |----inner_left----|
        Column(left, right) => match (&**left, &**right) {
            // Reference magic to work around Box.
            (Index(variable, inner), Variable(dependent_name)) => match inner.as_slice() {
                [Some(Infix(operator, inner_left, _)), None] => {
                    if operator == "==" {
                        match &**inner_left {
                            Column(inner_variable, independent) => {
                                if let Variable(independent_name) = &**independent {
                                    if inner_variable == variable {
                                        HashSet::from_iter(vec![Hypothesis {
                                            left: dependent_name.clone(),
                                            right: RFormulaExpression::Variable(
                                                independent_name.to_string(),
                                            ),
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

            (left, _) => detect_hypotheses(left),
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

    use super::*;

    #[test]
    fn parses_formula() {
        let expression = RExp::Formula(RFormula::TwoSided(
            Box::new(RExp::variable("dependent")),
            RFormulaExpression::Variable("independent".into()),
        ));
        let result = detect_hypotheses(&expression);
        let expected = HashSet::from_iter(vec![Hypothesis {
            left: "dependent".into(),
            right: RFormulaExpression::Variable("independent".into()),
        }]);
        assert_eq!(expected, result);
    }

    #[test]
    fn parses_formula_notation_in_call() {
        let expression = RExp::Call(
            "test".into(),
            vec![(
                None,
                RExp::Formula(RFormula::TwoSided(
                    Box::new(RExp::variable("speed")),
                    RFormulaExpression::Plus(
                        Box::new(RFormulaExpression::Variable("layout".into())),
                        "age".into(),
                    ),
                )),
            )],
        );
        let result = detect_hypotheses(&expression);
        let expected = HashSet::from_iter(vec![Hypothesis {
            left: "speed".into(),
            right: RFormulaExpression::Plus(
                Box::new(RFormulaExpression::Variable("layout".into())),
                "age".into(),
            ),
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
        let expected = HashSet::from_iter(vec![Hypothesis {
            left: "dependent".into(),
            right: RFormulaExpression::Variable("independent".into()),
        }]);
        assert_eq!(expected, result);
    }

    #[test]
    fn parses_column_hypothesis_in_call() {
        // fitdistr(kbd[kbd$Layout == "QWERTY",]$Speed, "lognormal")$estimate
        let expression = RExp::Column(
            Box::new(RExp::Call(
                "fitdistr".into(),
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
        let expected = HashSet::from_iter(vec![Hypothesis {
            left: "Speed".into(),
            right: RFormulaExpression::Variable("Layout".into()),
        }]);
        assert_eq!(expected, result);
    }
}

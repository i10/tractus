use std::collections::HashSet;
use std::iter::FromIterator;

use crate::parser::{RExp, RFormula, RFormulaExpression, RIdentifier};

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct Hypothesis {
    pub left: RIdentifier,
    pub right: RFormulaExpression,
}

pub fn detect_hypotheses(expression: &RExp) -> HashSet<Hypothesis> {
    use RExp::*;
    HashSet::from_iter(match expression {
        Formula(RFormula::TwoSided(left, right)) => vec![Hypothesis {
            left: left.clone(),
            right: right.clone(),
        }],

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
                                        vec![Hypothesis {
                                            left: dependent_name.clone(),
                                            right: RFormulaExpression::Variable(
                                                independent_name.to_string(),
                                            ),
                                        }]
                                    } else {
                                        vec![]
                                    }
                                } else {
                                    vec![]
                                }
                            }
                            _ => vec![],
                        }
                    } else {
                        vec![]
                    }
                }
                _ => vec![],
            },

            _ => vec![],
        },

        Call(_, args) => args
            .iter()
            .map(|(_, exp)| detect_hypotheses(exp))
            .flatten()
            .collect(),
        _ => vec![],
    })
}

#[cfg(test)]
mod tests {
    use crate::parser::RFormula;

    use super::*;

    #[test]
    fn parses_formula() {
        let expression = RExp::Formula(RFormula::TwoSided(
            "dependent".into(),
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
                    "speed".into(),
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
}

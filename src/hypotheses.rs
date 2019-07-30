use crate::parser::{RExp, RFormula, RFormulaExpression, RIdentifier};

#[derive(PartialEq, Debug)]
pub struct Hypothesis {
    left: RIdentifier,
    right: RFormulaExpression,
}

pub fn detect_hypothesis(expression: &RExp) -> Option<Vec<Hypothesis>> {
    use RExp::*;
    match expression {
        Formula(RFormula::TwoSided(left, right)) => Some(vec![Hypothesis {
            left: left.clone(),
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
                                        Some(vec![Hypothesis {
                                            left: dependent_name.clone(),
                                            right: RFormulaExpression::Variable(
                                                independent_name.to_string(),
                                            ),
                                        }])
                                    } else {
                                        None
                                    }
                                } else {
                                    None
                                }
                            }
                            _ => None,
                        }
                    } else {
                        None
                    }
                }
                _ => None,
            },

            _ => None,
        },

        Call(_, args) => {
            let hypotheses: Vec<Hypothesis> = args
                .iter()
                .filter_map(|(_, exp)| detect_hypothesis(exp))
                .flatten()
                .collect();
            if hypotheses.is_empty() {
                None
            } else {
                Some(hypotheses)
            }
        }

        _ => None,
    }
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
        let result = detect_hypothesis(&expression);
        let expected = vec![Hypothesis {
            left: "dependent".into(),
            right: RFormulaExpression::Variable("independent".into()),
        }];
        assert_eq!(Some(expected), result);
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
        let result = detect_hypothesis(&expression);
        let expected = vec![Hypothesis {
            left: "speed".into(),
            right: RFormulaExpression::Plus(
                Box::new(RFormulaExpression::Variable("layout".into())),
                "age".into(),
            ),
        }];
        assert_eq!(Some(expected), result);
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
        let result = detect_hypothesis(&expression);
        let expected = vec![Hypothesis {
            left: "dependent".into(),
            right: RFormulaExpression::Variable("independent".into()),
        }];
        assert_eq!(Some(expected), result);
    }
}

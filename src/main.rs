#[macro_use]
extern crate horrorshow;
extern crate structopt;

use std::io;
use std::io::{Read, Write};
use std::path::PathBuf;

use horrorshow::helper::doctype;
use horrorshow::prelude::*;
use structopt::StructOpt;

use tractus::{HypothesisTree, RExp, Tractus};

#[derive(Debug, StructOpt)]
#[structopt(name = "tractus")]
struct Opt {
    /// Input file, stdin if not present
    #[structopt(short = "i", parse(from_os_str))]
    input: Option<PathBuf>,
    /// Output file, stdout if not present
    #[structopt(short = "o", parse(from_os_str))]
    output: Option<PathBuf>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let opt = Opt::from_args();
    let code = read(opt.input)?;

    let tractus = Tractus::parse(&code).unwrap_or_else(|e| panic!("{}", e));
    let hypotheses = tractus.generate_hypothesis_tree();

    let html = render(&hypotheses).into_string()?;
    match opt.output {
        Some(path) => {
            std::fs::write(path, html)?;
        }
        None => {
            let stdout = io::stdout();
            let mut handle = stdout.lock();
            handle.write_all(html.as_bytes())?;
        }
    }

    Ok(())
}

fn read(file: Option<PathBuf>) -> Result<String, Box<dyn std::error::Error>> {
    let code = match file {
        Some(path) => std::fs::read_to_string(path)?,
        None => {
            let stdin = io::stdin();
            let mut handle = stdin.lock();
            let mut code = String::new();
            handle.read_to_string(&mut code)?;
            code
        }
    };
    Ok(code)
}

fn render<'a>(tree: &'a HypothesisTree) -> Box<Render + 'a> {
    box_html! {
        : doctype::HTML ;
        html {
            head {
                style {
                    : Raw("
                    .hypotheses {
                        display: flex;
                        align-items: flex-start;
                    }

                    ol {
                        padding: 0;
                        list-style: none;
                    }

                    ol.nodes {
                        padding-left: 12px;
                    }

                    .hypotheses > li {
                        margin-top: 0.5em;
                        padding-right: 12px;
                    }

                    .hypothesis {
                        margin-left: 0.5em;
                        font-style: italic;
                    }

                    .expression {
                        font-family: monospace;
                        font-weight: bold;
                    }

                    /*
                     * Directory lines inspired by https://two-wrongs.com/draw-a-tree-structure-with-only-css.
                     */
                    ol.nodes > li {
                        position: relative;
                    }

                    ol.nodes > li::before, ol.nodes > li::after {
                        content: \"\";
                        position: absolute;
                        left: -12px;
                    }

                    ol.nodes > li::before {
                        border-top: 1px solid #000;
                        width: 8px;
                        height: 0;
                        transform: translateY(10px);
                    }

                    ol.nodes > li::after {
                        border-left: 1px solid #000;
                        height: 100%;
                        width: 0;
                        top: 2px;
                    }

                    ol.nodes > li:last-child::after {
                        height: 8px;
                    }

                    .hypotheses > li {
                        position: relative;
                    }

                    ol.hypotheses > li::before, ol.hypotheses > li::after {
                        content: \"\";
                        position: absolute;
                        top: 0;
                    }

                    ol.hypotheses > li::before {
                        border-left: 1px solid #000;
                        width: 0;
                        height: 100%;
                    }

                    ol.hypotheses > li::after {
                        border-top: 1px solid #000;
                        width: 100%;
                        height: 0;
                        left: 0;
                    }

                    ol.hypotheses > li:first-child::before {
                        transform: translateY(-8px);
                    }

                    ol.hypotheses > li:last-child::after {
                        width: 0;
                    }
                    ") ;
                }
            }
            body {
                : render_hypothesis_tree(tree) ;
            }
        }
    }
}

fn render_hypothesis_tree<'a>(tree: &'a HypothesisTree) -> Box<Render + 'a> {
    box_html! {
        ol(class="hypotheses") {
            @ for (maybe_hypothesis, nodes) in tree.iter() { // TODO: Consider sorting.
                li {
                    span(class="hypothesis") { : match maybe_hypothesis {
                        Some(hypothesis) => format!("{}", hypothesis),
                        None => "No hypothesis".to_string()
                    } ; }
                    ol(class="nodes") {
                        @ for node in nodes.iter() {
                            li {
                                span(class="expression") { : render_preferrably_as_function(node.expression) ; }
                                : render_hypothesis_tree(&node.children) ;
                            }
                        }
                    }
                }
            }
        }
    }
}

fn render_preferrably_as_function(expression: &RExp) -> String {
    extract_function_name(expression).unwrap_or_else(|| format!("{}", expression))
}

fn extract_function_name(expression: &RExp) -> Option<String> {
    match expression {
        RExp::Call(name, _) => name.extract_variable_name(),
        RExp::Column(left, _) => extract_function_name(left),
        _ => None,
    }
}

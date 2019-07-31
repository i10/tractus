#[macro_use]
extern crate horrorshow;
extern crate structopt;

use std::io;
use std::io::{Read, Write};
use std::path::PathBuf;

use horrorshow::helper::doctype;
use horrorshow::prelude::*;
use structopt::StructOpt;

use tractus::{HypothesisTree, Tractus};

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

    let tractus = Tractus::parse(&code)?;
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
                        margin-top: 1em;
                    }

                    ol {
                        padding: 0;
                        list-style: none;
                    }

                    .hypothesis {
                        font-style: italic;
                    }

                    .expression {
                        font-weight: bold;
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
            @ for (hypothesis, nodes) in tree.iter() { // TODO: Consider sorting.
                li {
                    span(class="hypothesis") { : format!("{:?}", hypothesis) ; }
                    ol(class="nodes") {
                        @ for node in nodes.iter() {
                            li {
                                span(class="expression") { : format!("{:?}", node.expression) ; }
                                : render_hypothesis_tree(&node.children) ;
                            }
                        }
                    }
                }
            }
        }
    }
}

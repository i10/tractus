extern crate structopt;

use std::io;
use std::io::{Read, Write};
use std::path::PathBuf;

use env_logger;
use log::{debug, info};
use serde_json;
use structopt::StructOpt;

use tractus::Parsed;

#[derive(Debug, StructOpt)]
#[structopt(name = "tractus")]
struct Opt {
    /// Input file, stdin if not present
    #[structopt(short = "i", parse(from_os_str))]
    input: Option<PathBuf>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    env_logger::init();

    debug!("Started processing.");
    let opt = Opt::from_args();
    debug!("Reading from input...");
    let code = read(opt.input)?;

    info!("Parsing...");
    let parsed = Parsed::from(&code).unwrap_or_else(|e| panic!("{}", e));
    debug!("Parsing dependency graph...");
    let dependency_graph = tractus::DependencyGraph::parse(parsed.iter());
    debug!("Parsing hypothesis tree...");
    let hypotheses = tractus::parse_hypothesis_tree(parsed.iter(), &dependency_graph);

    debug!("Serializing...");
    let output = serde_json::to_string_pretty(&hypotheses)?;
    info!("Outputting...");
    let stdout = io::stdout();
    let mut handle = stdout.lock();
    handle.write_all(output.as_bytes())?;

    info!("Done.");
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

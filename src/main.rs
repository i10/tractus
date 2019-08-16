extern crate structopt;

use std::io;
use std::io::{Read, Write};
use std::path::PathBuf;

use env_logger;
use log::{debug,trace, info};
use serde_json;
use structopt::StructOpt;

use tractus::{Parsed, LineTree};

#[derive(Debug, StructOpt)]
#[structopt(name = "tractus")]
struct Opt {
    /// Input file, stdin if not present
    #[structopt(short = "i", parse(from_os_str))]
    input: Option<PathBuf>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    env_logger::init();

    let opt = Opt::from_args();
    if let Some(path) = opt.input {
        debug!("Reading from file {}", path.display());
        let mut file = std::fs::File::open(path)?;
        parse(&mut file)?;
    } else {
        debug!("Reading from stdin.");
        let stdin = io::stdin();
        let mut handle = stdin.lock();
        parse(&mut handle)?;
    };

    Ok(())
}

fn parse(handle: &mut impl Read) -> Result<(), Box<dyn std::error::Error>> {
    trace!("Reading...");
    let mut code = String::new();
    handle.read_to_string(&mut code)?;
    process(&code)
}

fn process(code: &str) -> Result<(), Box<dyn std::error::Error>> {
    info!("Parsing...");
    let parsed = Parsed::parse(&code).unwrap_or_else(|e| panic!("{}", e));
    debug!("Parsing dependency graph...");
    let dependency_graph = tractus::DependencyGraph::from_input(parsed.iter());
    debug!("Parsing hypothesis tree...");
    let hypotheses = tractus::parse_hypothesis_tree(parsed.iter(), &dependency_graph);

    debug!("Serializing...");
    let output = serde_json::to_string_pretty(&LineTree::from(&hypotheses))?;
    debug!("Outputting...");
    let stdout = io::stdout();
    let mut handle = stdout.lock();
    handle.write_all(output.as_bytes())?;

    info!("Done.");
    Ok(())
}

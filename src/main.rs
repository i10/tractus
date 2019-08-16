extern crate structopt;

use std::fs;
use std::io;
use std::io::{Read, Write};
use std::path;
use std::path::PathBuf;

use env_logger;
use log::{debug, info};
use serde_json;
use structopt::StructOpt;

use tractus::{LineTree, Parsed};

#[derive(Debug, StructOpt)]
#[structopt(name = "tractus")]
struct Opt {
    /// Input file, stdin if not present
    #[structopt(short = "i", parse(from_os_str))]
    input: Option<PathBuf>,
    /// Output file, stdout if not present
    #[structopt(short = "o", parse(from_os_str))]
    output: Option<PathBuf>,
    /// Force overwriting the output, without prompting
    #[structopt(short = "f")]
    overwrite: bool,
}

fn main() -> Res {
    env_logger::init();

    let opt = Opt::from_args();

    let code = read_from(&opt.input)?;
    let result = process(&code)?;
    output(&opt.output, opt.overwrite, result)?;

    Ok(())
}

fn read_from(input: &Option<PathBuf>) -> Result<String, Box<dyn std::error::Error>> {
    if let Some(path) = input {
        debug!("Reading from file {}", path.display());
        let mut file = std::fs::File::open(path)?;
        read(&mut file)
    } else {
        debug!("Reading from stdin.");
        let stdin = io::stdin();
        let mut handle = stdin.lock();
        read(&mut handle)
    }
}

fn read(handle: &mut impl Read) -> Result<String, Box<dyn std::error::Error>> {
    let mut code = String::new();
    handle.read_to_string(&mut code)?;
    Ok(code)
}

fn process(code: &str) -> Result<String, Box<dyn std::error::Error>> {
    info!("Parsing...");
    let parsed = Parsed::parse(&code).unwrap_or_else(|e| panic!("{}", e));
    debug!("Parsing dependency graph...");
    let dependency_graph = tractus::DependencyGraph::from_input(parsed.iter());
    debug!("Parsing hypothesis tree...");
    let hypotheses = tractus::parse_hypothesis_tree(parsed.iter(), &dependency_graph);

    debug!("Serializing...");
    let result = serde_json::to_string_pretty(&LineTree::from(&hypotheses))?;

    Ok(result)
}

fn output(to: &Option<PathBuf>, force: bool, output: String) -> Res {
    if let Some(watch_path) = to {
        if path::Path::exists(&watch_path) && !force {
            let mut confirmation = dialoguer::Confirmation::new();
            confirmation.with_text(&format!(
                "The file {} already exists. Do you want to overwrite it?",
                watch_path.display()
            ));
            if !confirmation.interact()? {
                println!("Canceled.");
                return Ok(());
            }
        }
        println!("Outputting to file {}.", watch_path.display());
        let mut output_file = fs::File::create(watch_path)?;
        output_to(&mut output_file, output)?;
    } else {
        debug!("Outputting to stdout.");
        let stdout = io::stdout();
        let mut handle = stdout.lock();
        output_to(&mut handle, output)?;
    }

    Ok(())
}

fn output_to(device: &mut impl Write, output: String) -> Res {
    device.write_all(output.as_bytes())?;

    Ok(())
}

pub type Res = std::result::Result<(), Box<dyn std::error::Error>>;

extern crate structopt;

use std::borrow::Borrow;
use std::convert::TryFrom;
use std::fs;
use std::io;
use std::io::{Read, Write};
use std::path;
use std::path::PathBuf;
use std::sync::mpsc;

use clap_verbosity_flag;
use ctrlc;
use env_logger;
use failure::Error;
use log::{debug, info, trace, warn};
use notify;
use regex::Regex;
use serde_json;
use structopt::StructOpt;

use tractus::Parsed;

#[derive(StructOpt)]
#[structopt(name = "tractus", about = "Generate a hypotheses tree from R code.")]
struct Opt {
    #[structopt(short = "i", parse(from_os_str))]
    /// Input file, stdin if not present
    input: Option<PathBuf>,
    #[structopt(short = "o", parse(from_os_str))]
    /// Output file, stdout if not present
    output: Option<PathBuf>,
    #[structopt(flatten)]
    verbose: clap_verbosity_flag::Verbosity,
    #[structopt(short = "f")]
    /// Force overwriting the output, without prompting
    overwrite: bool,
    #[structopt(subcommand)]
    subcmd: Option<Subcommand>,
}

#[derive(StructOpt)]
enum Subcommand {
    #[structopt(name = "watch")]
    /// Watch an input file and update the output file on changes
    Watch {
        #[structopt(short = "i", parse(from_os_str))]
        /// Input file, stdin if not present
        input: PathBuf,
        #[structopt(short = "o", parse(from_os_str))]
        /// Output file that will be overwritten whenever the input changes
        output: PathBuf,
        #[structopt(flatten)]
        r_history: RHistory,
    },
}

#[derive(StructOpt)]
struct RHistory {
    #[structopt(long, short)]
    append: bool,
    #[structopt(long, short)]
    clean: Option<Regex>,
    #[structopt(name = "history", long, short)]
    r_history: bool,
}

struct WatchOptions {
    offset: Option<io::SeekFrom>,
    clean: Option<Regex>,
}

impl TryFrom<RHistory> for WatchOptions {
    type Error = regex::Error;
    fn try_from(other: RHistory) -> Result<Self, Self::Error> {
        Ok(if other.r_history {
            WatchOptions {
                offset: Some(io::SeekFrom::Start(0)),
                clean: Some(Regex::new(r#"(?m)\d+:"#)?),
            }
        } else {
            let seek = if other.append {
                Some(io::SeekFrom::Start(0))
            } else {
                None
            };
            WatchOptions {
                offset: seek,
                clean: other.clean,
            }
        })
    }
}

fn main() {
    ctrlc::set_handler(move || {
        println!("Stopping tractus.");
    })
    .unwrap_or_else(|e| panic!("Error when trying to register ctrl+c handler: {}", e));

    let opt = Opt::from_args();
    let verbosity = opt.verbose.log_level().to_level_filter();
    init_logger(verbosity);

    if let Err(e) = run(opt) {
        eprintln!("An error occurred:\n");
        if verbosity >= log::LevelFilter::Debug {
            eprintln!("{:?}", e);
        } else {
            eprintln!("{}", e);
        }
        eprintln!("\nStopping tractus.");
    }
}

fn init_logger(level: log::LevelFilter) {
    env_logger::Builder::new().filter_level(level).init();
}

fn run(opt: Opt) -> Res {
    if let Some(subcmd) = opt.subcmd {
        debug!("Watch is enabled.");
        let Subcommand::Watch {
            input,
            output,
            r_history,
        } = subcmd;
        let options = WatchOptions::try_from(r_history)?;
        if let Some(offset) = options.offset {
            let execute = || Ok(()); // To be implemented.
            watch(&input, execute)?;
        } else {
            let execute = || parse_entirely(&input, &output, &options.clean);
            watch(&input, execute)?;
        }
    } else {
        debug!("Watch is disabled.");
        execute(&opt.input, &opt.output, opt.overwrite)?;
    }

    Ok(())
}

fn watch<F: FnMut() -> Res>(input_path: &PathBuf, mut execute: F) -> Res {
    use notify::{DebouncedEvent, RecommendedWatcher, RecursiveMode, Watcher};
    use std::time::Duration;
    let (tx, rx) = mpsc::channel();
    let mut watcher: RecommendedWatcher = Watcher::new(tx, Duration::from_millis(500))?;
    watcher.watch(&input_path, RecursiveMode::NonRecursive)?;

    // Run once initially.
    execute()?;

    println!("Watching file {}.", &input_path.display());
    for event in rx {
        trace!("Received new file event.");
        if let DebouncedEvent::Write(input) = event {
            debug!("File changed: {}", input.display());
            execute()?;
        }
    }

    Ok(())
}

fn parse_entirely(input_path: &PathBuf, output_path: &PathBuf, clean: &Option<Regex>) -> Res {
    let code = read_from(&Some(input_path))?;
    let code = clean_input(code, clean);

    let result = process(&code)?;
    println!("Outputting to file {}.", output_path.display());
    let mut output_file = fs::File::create(output_path)?;
    output_to(&mut output_file, result)?;

    Ok(())
}

fn clean_input(code: String, clean: &Option<Regex>) -> String {
    clean
        .as_ref()
        .map(|regex| {
            debug!("Cleaning input's line with regex {}.", regex);
            regex.replace(&code, "").into_owned()
        })
        .unwrap_or(code)
}

fn execute(
    input_path: &Option<impl Borrow<PathBuf>>,
    output_path: &Option<impl Borrow<PathBuf>>,
    overwrite: bool,
) -> Res {
    let code = read_from(input_path)?;
    let result = process(&code)?;
    write_output(output_path, overwrite, result)?;

    Ok(())
}

fn read_from(input: &Option<impl Borrow<PathBuf>>) -> Result<String, Error> {
    if let Some(path) = input {
        let path = path.borrow();
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

fn read(handle: &mut impl Read) -> Result<String, Error> {
    let mut code = String::new();
    handle.read_to_string(&mut code)?;
    Ok(code)
}

fn process(code: &str) -> Result<String, Error> {
    info!("Parsing...");
    let parsed = Parsed::parse(&code)?;
    debug!("Parsing dependency graph...");
    let dependency_graph = tractus::DependencyGraph::from_input(parsed.iter().cloned());
    debug!("Parsing hypothesis tree...");
    let hypotheses = tractus::parse_hypothesis_tree(parsed.iter().cloned(), &dependency_graph);

    debug!("Serializing...");
    let result = serde_json::to_string_pretty(&hypotheses)?;

    Ok(result)
}

fn write_output(to: &Option<impl Borrow<PathBuf>>, force: bool, output: String) -> Res {
    if let Some(watch_path) = to {
        let watch_path = watch_path.borrow();
        if path::Path::exists(watch_path) {
            warn!("Path {} already exists.", watch_path.display());
            if !force {
                debug!("Overwrite not enforced, asking user how to proceed.");
                let mut confirmation = dialoguer::Confirmation::new();
                confirmation.with_text(&format!(
                    "The file {} already exists. Do you want to overwrite it?",
                    watch_path.display()
                ));
                if !confirmation.interact()? {
                    println!("Canceled.");
                    return Ok(());
                }
            } else {
                debug!("Overwrite enforced, overwriting output file.");
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

pub type Res = std::result::Result<(), Error>;

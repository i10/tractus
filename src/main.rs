extern crate tractus;

use std::convert::{TryFrom, TryInto};
use std::io;
use std::io::prelude::*;
use std::path::{Path, PathBuf};

use env_logger;
use failure::Error;
use log::{debug, error, info, trace, warn};
use regex::Regex;
use structopt::StructOpt;

use tractus::Tractus;

#[derive(StructOpt)]
struct Opts {
    #[structopt(flatten)]
    verbosity: Verbosity,
    #[structopt(subcommand)]
    subcmd: Subcommand,
}

#[derive(StructOpt)]
enum Subcommand {
    #[structopt(name = "run")]
    /// Runs on files
    Run {
        #[structopt(flatten)]
        opts: CommonOpts,
    },
    #[structopt(name = "serve")]
    /// Starts a websocket server
    Serve {
        #[structopt(flatten)]
        opts: CommonOpts,
    },
}

#[derive(StructOpt)]
struct CommonOpts {
    #[structopt(short, long, parse(from_os_str))]
    /// Input file, stdin if not present
    input: Option<PathBuf>,
    #[structopt(short, long)]
    /// Declares that the input file is only appended to
    ///
    /// Can be more efficient. If set, tractus will parse the entire file once and then only the newly appended lines.
    ///
    /// Note that if the file is changed other by appending, tractus might not detect the changes or even stop.
    append: bool,
    #[structopt(short, long)]
    /// A regular expression to clean each input line with
    ///
    /// The regex syntax used is documented at https://docs.rs/regex/1.2.1/regex/#syntax.
    /// Whatever the regex matches will be removed from the line.
    clean: Option<Regex>,
    #[structopt(short, long)]
    /// Declares the input file as an RStudio `history_desktop` file
    ///
    /// Convenience flag for enabling --append and --clean "(?m)^\d+:".
    /// Cannot be used at the same time as --append or --clean.
    history_desktop: bool,
    #[structopt(short, long, parse(from_os_str))]
    /// Output file, stdout if not present
    output: Option<PathBuf>,
    #[structopt(short, long)]
    /// Forces overwriting the output without prompting
    force: bool,
}

#[derive(StructOpt)]
struct Verbosity {
    #[structopt(short, parse(from_occurrences))]
    /// Sets the logging level
    ///
    /// The logging levels can be set by repeating the flag as follows:
    /// -v error, -vv warn, -vvv info, -vvvv debug, -vvvvv trace
    ///
    /// If not set, logging can be configured via the `RUST_LOG` environment variables as described at https://docs.rs/env_logger/0.6.2/env_logger/.
    v: u8,
}

impl Verbosity {
    pub fn level(&self) -> Option<log::Level> {
        use log::Level::*;
        match self.v {
            0 => None,
            1 => Some(Error),
            2 => Some(Warn),
            3 => Some(Info),
            4 => Some(Debug),
            _ => Some(Trace),
        }
    }
}

fn main() {
    let opts = Opts::from_args();
    init_logger(opts.verbosity.level());

    if let Err(e) = execute(opts.subcmd) {
        eprintln!("An error occurred:\n{}", e);
        println!("Stopping tractus.");
    }
}

fn init_logger(level: Option<log::Level>) {
    match level {
        Some(l) => {
            env_logger::Builder::new()
                .filter_level(l.to_level_filter())
                .init();
        }
        None => {
            env_logger::Builder::from_default_env().init();
        }
    }
}

fn execute(cmd: Subcommand) -> Res {
    use Subcommand::*;
    match cmd {
        Run { opts } => {
            let config = CommonConfig::try_from(opts)?;
            run(config)
        }
        Serve { opts } => Ok(()),
    }
}

fn run(mut conf: CommonConfig) -> Res {
    let mut tractus = Tractus::new();

    run_once(&mut tractus, &mut conf)?;

    if let Some(input) = &conf.input {
        use notify::{DebouncedEvent, RecommendedWatcher, RecursiveMode, Watcher};
        use std::time::Duration;

        let (sender, receiver) = std::sync::mpsc::channel();
        let mut watcher: RecommendedWatcher = Watcher::new(sender, Duration::from_millis(500))?;
        watcher.watch(&input.path, RecursiveMode::NonRecursive)?;

        println!("Watching file {}.", &input.path.display());
        for event in receiver {
            trace!("Received new file event.");
            if let DebouncedEvent::Write(input) = event {
                debug!("File changed: {}", input.display());
                run_once(&mut tractus, &mut conf)?;
            }
        }
    }
    Ok(())
}

fn run_once(tractus: &mut Tractus, conf: &mut CommonConfig) -> Result<String, Error> {
    match &mut conf.input {
        None => {
            tractus.clear();

            let stdin = io::stdin();
            let mut reader = io::BufReader::new(stdin);

            let lines = read_lines(&mut reader, &conf.clean)?;
            tractus.parse_lines(lines)?;
        }
        Some(input) => {
            let file = std::fs::File::open(&input.path)?;
            let mut reader = io::BufReader::new(file);

            match input.append {
                None => {
                    // Since we do not append, we start fresh.
                    tractus.clear();
                }
                Some(offset) => {
                    trace!("Reading from offset {}.", offset);
                    reader.seek(io::SeekFrom::Start(offset))?;
                }
            }

            let lines = read_lines(&mut reader, &conf.clean)?;
            tractus.parse_lines(lines)?;

            input.append = input
                .append
                .map(|_| reader.seek(io::SeekFrom::Current(0)))
                .transpose()?;
        }
    }
    let result = serde_json::to_string(&tractus.hypotheses_tree())?;
    let output = if let Some(out) = &conf.output {
        out.path.display().to_string()
    } else {
        "stdout".to_string()
    };
    println!("Writing to {}.", output);
    write_result(&mut conf.output, &result)?;

    Ok(result)
}

fn read_lines(reader: &mut impl BufRead, clean: &Option<Regex>) -> Result<Vec<String>, io::Error> {
    reader
        .lines()
        .map(|line_result| {
            line_result.map(|mut line| {
                if let Some(regex) = &clean {
                    line = regex.replace_all(&line, "").to_string();
                }
                line
            })
        })
        .collect()
}

type Offset = u64;

// struct RunConfig {
//     common: CommonConfig,
//     single_run: bool,
// }

// impl TryFrom<RunOpts> for RunConfig {
//     type Error = <CommonConfig as TryFrom<CommonOpts>>::Error;

//     fn try_from(other: RunOpts) -> Result<Self, Self::Error> {
//         let common = other.common.try_into()?;
//         Ok(RunConfig {
//             common,
//             single_run: other.single_run,
//         })
//     }
// }

struct CommonConfig {
    input: Option<InputPath>,
    clean: Option<Regex>,
    output: Option<OutputPath>,
}

impl TryFrom<CommonOpts> for CommonConfig {
    type Error = ArgumentError;

    fn try_from(other: CommonOpts) -> Result<Self, Self::Error> {
        let (append_bool, clean) = if other.history_desktop {
            if other.append || other.clean.is_some() {
                return Err(ArgumentError::HistoryConflict);
            } else {
                // The regex removes the timestamp form each line.
                (true, Some(Regex::new(r#"(?m)^\d+:"#).unwrap()))
            }
        } else {
            (other.append, other.clean)
        };
        let append = if append_bool { Some(0) } else { None };
        let input = match (other.input, append) {
            (Some(path), app) => Some(InputPath { path, append: app }),
            (None, None) => None,
            (None, Some(_)) => return Err(ArgumentError::AppendWithoutPath),
        };
        let force = other.force;
        Ok(CommonConfig {
            input,
            clean,
            output: other.output.map(|path| OutputPath { path, force }),
        })
    }
}

#[derive(Debug)]
enum ArgumentError {
    HistoryConflict,
    AppendWithoutPath,
}

impl std::fmt::Display for ArgumentError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Invalid arguments:")?;
        use ArgumentError::*;
        match self {
            HistoryConflict => write!(f, "You cannot use --history-desktop along with --append or --clean, since it would overwrite your settings."),
            AppendWithoutPath=> write!(f, "You cannot use --append when reading from stdin. Please specify a file to read from with --input.")
        }
    }
}

impl std::error::Error for ArgumentError {}

struct InputPath {
    path: PathBuf,
    append: Option<Offset>,
}

fn write_result(output: &mut Option<OutputPath>, result: &str) -> Res {
    match output {
        Some(output_path) => {
            output_path.write_confirmed(result)?;
        }
        None => {
            let stdout = std::io::stdout();
            let mut handle = stdout.lock();
            handle.write_all(result.as_bytes())?;
        }
    }
    Ok(())
}

struct OutputPath {
    path: PathBuf,
    force: bool,
}

impl OutputPath {
    pub fn confirm(&mut self) -> Result<bool, Error> {
        Ok(if !std::path::Path::exists(&self.path) {
            true
        } else {
            trace!("Path {} already exists.", self.path.display());
            if self.force {
                trace!("Overwrite enforced.");
                true
            } else {
                trace!("Overwrite not enforced. Asking for confirmation.");
                let mut confirmation = dialoguer::Confirmation::new();
                confirmation.with_text(&format!(
                    "The file {} already exists. Overwrite it?",
                    self.path.display()
                ));
                if confirmation.interact()? {
                    trace!("User confirmed overwrite.");
                    self.force = true;
                    true
                } else {
                    println!("Canceled.");
                    false
                }
            }
        })
    }

    pub fn write_confirmed(&mut self, output: &str) -> Res {
        if self.confirm()? {
            std::fs::write(&self.path, output)?;
        }
        Ok(())
    }
}

type Res = Result<(), Error>;

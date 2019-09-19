extern crate tractus;

use std::collections::HashMap;
use std::convert::TryFrom;
use std::io;
use std::io::prelude::*;
use std::path::PathBuf;
use std::sync::{Arc, Mutex, RwLock};
use std::thread;
use std::time::Duration;

use env_logger;
use failure::Error;
use log::{debug, info, trace, warn};
use notify::{DebouncedEvent, RecommendedWatcher, RecursiveMode, Watcher};
use regex::Regex;
use serde::{Deserialize, Serialize};
use structopt::StructOpt;
use websocket::{sync::Server, Message, OwnedMessage};

use tractus::Tractus;

#[derive(StructOpt)]
#[structopt(about)]
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
        opts: RunOpts,
    },
    #[structopt(name = "serve")]
    /// Starts a websocket server
    Serve {
        #[structopt(flatten)]
        opts: ServeOpts,
    },
}

#[derive(StructOpt)]
struct RunOpts {
    #[structopt(short, long, parse(from_os_str))]
    /// Input file, stdin if not present
    input: Option<PathBuf>,
    #[structopt(flatten)]
    processing: ProcessingOpts,
    #[structopt(short, long, parse(from_os_str))]
    /// Output file, stdout if not present
    output: Option<PathBuf>,
    #[structopt(short, long)]
    /// Forces overwriting the output without prompting
    force: bool,
}

#[derive(StructOpt)]
struct ServeOpts {
    #[structopt(short, long, parse(from_os_str))]
    /// Input file, websocket if missing
    input: Option<PathBuf>,
    #[structopt(flatten)]
    processing: ProcessingOpts,
    #[structopt(short, long, parse(from_os_str))]
    /// File for persistency, no persistency if missing
    ///
    /// Cannot be used when `input` is set, because the input file is already persistent.
    store: Option<PathBuf>,
}

#[derive(StructOpt)]
struct ProcessingOpts {
    #[structopt(name = "append-only", short, long)]
    /// Only parses lines that are appended while watching
    ///
    /// The existing content of the file is ignored. Only newly added lines will be parsed.
    /// Note that if the file is changed other than by appending, tractus might not detect the changes or even stop.
    append_only: bool,
    #[structopt(short, long)]
    /// A regular expression to clean each input line with
    ///
    /// The regex syntax used is documented at https://docs.rs/regex/1.2.1/regex/#syntax.
    /// Whatever the regex matches will be removed from the line.
    clean: Option<Regex>,
    #[structopt(name = "history-database", short, long)]
    /// Declares the input file as an RStudio `history_database` file
    ///
    /// Convenience flag for enabling --append-only and --clean "(?m)^\d+:".
    /// Cannot be used at the same time as --append-only or --clean, since this flag would overwrite those options.
    history_database: bool,
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

/// Delegates to the correct subcommand.
fn execute(cmd: Subcommand) -> Res {
    use Subcommand::*;
    match cmd {
        Run { opts } => {
            let config = RunConfig::try_from(opts)?;
            run(config)?;
        }
        Serve { opts } => {
            let config = ServeConfig::try_from(opts)?;
            serve(config)?;
        }
    }

    Ok(())
}

/// Executes the `run` subcommand.
fn run(conf: RunConfig) -> Res {
    let input = conf.input;
    let clean = conf.clean;
    let mut output = conf.output;
    match input {
        RunInput::SingleRun(input) => {
            let mut process = get_process(input.clone(), clean);
            let mut run_once = || -> Res {
                let result = process()?;
                write_result(&mut output, &result)
            };

            run_once()?;

            if let Some(path) = &input {
                watch(path, run_once)?;
            }
        }
        RunInput::AppendOnly(path) => {
            let file = std::fs::File::open(&path)?;
            let mut reader = io::BufReader::new(file);
            let mut offset = reader.seek(io::SeekFrom::End(0))?; // Skip the inital contents of the file.
            trace!("Skipping file contents until offset {}.", offset);
            let mut tractus = Tractus::new();

            let mut clean_lines = get_cleaner(clean);
            let mut run_once = || -> Res {
                reader.seek(io::SeekFrom::Start(offset))?;
                let lines = (&mut reader)
                    .lines()
                    .collect::<Result<Vec<String>, io::Error>>()?;
                let lines = clean_lines(lines);
                offset = reader.seek(io::SeekFrom::Current(0))?; // Update offset for next run.

                tractus.parse_lines(lines)?;
                let result = serde_json::to_string(&tractus.serialize())?;
                write_result(&mut output, &result)
            };

            run_once()?;

            watch(&path, run_once)?;
        }
    }
    Ok(())
}

/// Configuration for the `run` subcommand.
struct RunConfig {
    input: RunInput,
    clean: Option<Regex>,
    output: Option<OutputPath>,
}

enum RunInput {
    SingleRun(Option<PathBuf>), // Run either on stdin when None, or on Some(path).
    AppendOnly(PathBuf),        // Run in append mode on a path.
}

impl TryFrom<RunOpts> for RunConfig {
    type Error = ArgumentError;

    /// Attempt to convert cli options into configuration.
    fn try_from(other: RunOpts) -> Result<Self, Self::Error> {
        let processing = ProcessingConfig::try_from(other.processing)?;
        let input = if processing.append_only {
            if let Some(path) = other.input {
                RunInput::AppendOnly(path)
            } else {
                return Err(ArgumentError::AppendWithoutPath);
            }
        } else {
            RunInput::SingleRun(other.input)
        };
        let force = other.force;
        let output = other.output.map(|path| OutputPath { path, force });
        Ok(RunConfig {
            input,
            clean: processing.clean,
            output,
        })
    }
}

/// Watch the file path and execute a closure on changes.
fn watch<F>(path: &PathBuf, mut execute: F) -> Res
where
    F: FnMut() -> Res,
{
    let (sender, receiver) = std::sync::mpsc::channel();
    let mut watcher: RecommendedWatcher = Watcher::new(sender, Duration::from_millis(500))?;
    watcher.watch(&path, RecursiveMode::NonRecursive)?;

    println!("Watching file {}...", &path.display());
    for event in receiver {
        trace!("Received new file event.");
        if let DebouncedEvent::Write(_) = event {
            println!("File {} changed. Updating.", path.display());
            execute()?;
        }
    }

    Ok(())
}

/// Execute the `serve` subcommand.
fn serve(conf: ServeConfig) -> Res {
    match conf.input {
        ServeInput::File { path, append_only } => {
            debug!("Serving from file.");
            let mut run_once: Box<dyn FnMut() -> Res> = if !append_only {
                debug!("Append-only inactive, reparsing whole file on changes.");

                let mut update_and_broadcast = init_server(|_, _| {})?;
                let mut process = get_process(Some(path.clone()), conf.clean);

                Box::new(move || -> Res {
                    let result = process()?;
                    update_and_broadcast(result);

                    Ok(())
                })
            } else {
                debug!("Only considering appends.");
                let file = std::fs::File::open(&path)?;
                let mut reader = io::BufReader::new(file);
                let mut offset = reader.seek(io::SeekFrom::End(0))?; // Skip the inital contents of the file.
                trace!("Skipping file contents until offset {}.", offset);
                let mut tractus = Tractus::new();

                let mut clean_lines = get_cleaner(conf.clean);
                let mut process = move || -> Result<String, Error> {
                    reader.seek(io::SeekFrom::Start(offset + 1))?; // The + 1 skips the newline, which would otherwise cause wrong line numbers.
                    let lines = (&mut reader)
                        .lines()
                        .collect::<Result<Vec<String>, io::Error>>()?;
                    let lines = clean_lines(lines);
                    offset = reader.seek(io::SeekFrom::End(0))?; // Update offset for next run.

                    tractus.parse_lines(lines)?;
                    let result = serde_json::to_string(&tractus.serialize())?;
                    Ok(result)
                };
                let mut update_and_broadcast = init_server(|_, _| {})?;
                Box::new(move || {
                    let result = process()?;
                    update_and_broadcast(result);

                    Ok(())
                })
            };

            run_once()?;

            watch(&path, run_once)?;
        }
        ServeInput::Websocket { store } => {
            let mut tractus = if let Some(path) = &store {
                if let Ok(file) = std::fs::File::open(path) {
                    println!("Restoring from store at {}.", path.display());
                    serde_json::from_reader(file)?
                } else {
                    println!("No store file at {}. Starting fresh.", path.display());
                    let tractus = Tractus::new();
                    std::fs::write(path, serde_json::to_string(&tractus)?)?; // Store file does not yet exist, so create it.
                    tractus
                }
            } else {
                Tractus::new()
            };
            let (stmt_sender, stmt_receiver) = std::sync::mpsc::channel(); // Channel for passing new statements from websockets to the main loop.

            let mut update_and_broadcast =
                init_server(move |ip, mut receiver: websocket::sync::Reader<_>| {
                    let stmt_sender_clone = stmt_sender.clone();
                    thread::spawn(move || {
                        for msg in receiver.incoming_messages() {
                            debug!("Received new input.");
                            match msg {
                                Ok(message) => {
                                    if let OwnedMessage::Text(stmt) = message {
                                        stmt_sender_clone.send((ip, stmt)).unwrap();
                                    }
                                }
                                Err(e) => {
                                    debug!("Message contained error: {}", e);
                                }
                            }
                        }
                    });
                })?;
            let mut clean_lines = get_cleaner(conf.clean);
            update_and_broadcast(serde_json::to_string(&tractus.serialize())?);

            println!("Waiting for input via websockets.");
            for (ip, stmt) in stmt_receiver {
                match serde_json::from_str::<StatementInput>(&stmt) {
                    Ok(stmt_input) => {
                        debug!("Parsing statement received from {}.", ip);
                        let lines = stmt_input
                            .statement
                            .lines()
                            .map(|line| line.to_string())
                            .collect();
                        let lines = clean_lines(lines);
                        tractus.parse_lines_with_meta(lines, stmt_input.meta)?;
                        if let Some(path) = &store {
                            debug!("Updating store.");
                            std::fs::write(path, serde_json::to_string(&tractus)?)?;
                        }
                        let result = serde_json::to_string(&tractus.serialize())?;

                        update_and_broadcast(result);
                    }
                    Err(e) => {
                        eprintln!(
                            "Error parsing input received via websocket:\n{}\nInput was:\n{}",
                            e, stmt
                        );
                    }
                }
            }
        }
    };

    Ok(())
}

/// The interface for accepting new statements via websocket.
#[derive(Serialize, Deserialize)]
struct StatementInput {
    statement: String,
    meta: serde_json::Value,
}

/// Start a websocket server and execute the `new_client` closure whenever a new websocket client connects.
fn init_server<'a, F>(mut new_client: F) -> Result<Box<dyn FnMut(String) + 'a>, Error>
where
    F: std::marker::Send
        + FnMut(std::net::SocketAddr, websocket::receiver::Reader<std::net::TcpStream>)
        + 'static,
{
    let result: Arc<RwLock<String>> = Arc::new(RwLock::new(String::new())); // Cache the current result in order to send it to newly connected clients.
    let clients: Arc<Mutex<HashMap<std::net::SocketAddr, websocket::sender::Writer<_>>>> =
        Arc::new(Mutex::new(HashMap::new()));

    let result_clone = Arc::clone(&result);
    let clients_clone = Arc::clone(&clients);
    let update_and_broadcast = move |res| {
        println!("Broadcasting new hypotheses tree to all websockets.");
        let mut clients = clients_clone.lock().unwrap();
        for (ip, client) in clients.iter_mut() {
            let message = Message::text(&res);
            if let Err(e) = client.send_message(&message) {
                debug!("Error while attempting to send message to {}:\n{}", ip, e);
                // TODO: Remove client.
            }
        }

        let mut result = result_clone.write().unwrap();
        *result = res;
    };

    let result_clone = Arc::clone(&result);
    let clients_clone = Arc::clone(&clients);
    thread::spawn(move || {
        let address = "127.0.0.1:2794";
        let protocol = "tractus-websocket";
        let server = Server::bind(address).unwrap();
        println!(
            "Listening for websocket clients on `ws://{}` under protocol `{}`.",
            address, protocol
        );
        for request in server.filter_map(Result::ok) {
            if !request.protocols().contains(&protocol.into()) {
                warn!(
                    "New websocket request rejected, because it didn't offer the protocol `{}`.",
                    protocol
                );
                request.reject().unwrap();
            } else {
                let mut client = request.use_protocol("tractus-websocket").accept().unwrap();
                info!("New websocket request accepted.");

                let res = result_clone.read().unwrap().to_string();
                let ip = client.peer_addr().unwrap();
                // Send it current result.
                let message = Message::text(&res);
                if let Err(e) = client.send_message(&message) {
                    warn!("Error while attempting to send message to {}:\n{}", ip, e);
                }

                let (client_receiver, client_sender) = client.split().unwrap();
                let mut clients = clients_clone.lock().unwrap();
                clients.insert(ip, client_sender);

                new_client(ip, client_receiver);
            }
        }
        debug!("Stopping websocket server.")
    });

    Ok(Box::new(update_and_broadcast))
}

// Configuration for `serve` subcommand.
struct ServeConfig {
    input: ServeInput,
    clean: Option<Regex>,
}

enum ServeInput {
    File { path: PathBuf, append_only: bool }, // Serve from file, possibly in append-only mode.
    Websocket { store: Option<PathBuf> }, // Listen to websockets, possibly persist state in store.
}

impl TryFrom<ServeOpts> for ServeConfig {
    type Error = ArgumentError;

    /// Attempt to convert cli options into configuration.
    fn try_from(other: ServeOpts) -> Result<Self, Self::Error> {
        let processing = ProcessingConfig::try_from(other.processing)?;
        let input = match other.input {
            None => ServeInput::Websocket { store: other.store },
            Some(path) => {
                if other.store.is_some() {
                    return Err(ArgumentError::StoreWithPath);
                }
                ServeInput::File {
                    path,
                    append_only: processing.append_only,
                }
            }
        };
        let clean = processing.clean;
        Ok(ServeConfig { input, clean })
    }
}

/// Shared config for processing input file.
struct ProcessingConfig {
    append_only: bool,
    clean: Option<Regex>,
}

impl TryFrom<ProcessingOpts> for ProcessingConfig {
    type Error = ArgumentError;

    /// Attempt to convert cli options into configuration.
    fn try_from(other: ProcessingOpts) -> Result<Self, ArgumentError> {
        let (append_only, clean) = if other.history_database {
            if other.append_only || other.clean.is_some() {
                return Err(ArgumentError::HistoryConflict);
            } else {
                // This regex removes the timestamp from each line in the `history_database`.
                (true, Some(Regex::new(r#"(?m)^\d+:"#).unwrap()))
            }
        } else {
            (other.append_only, other.clean)
        };

        Ok(ProcessingConfig { append_only, clean })
    }
}

#[derive(Debug)]
enum ArgumentError {
    HistoryConflict,
    AppendWithoutPath,
    StoreWithPath,
}

impl std::fmt::Display for ArgumentError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Invalid arguments:")?;
        use ArgumentError::*;
        match self {
            HistoryConflict => write!(f, "You cannot use --history-desktop along with --append or --clean, since it would overwrite your settings."),
            AppendWithoutPath=> write!(f, "You cannot use --append when reading from stdin. Please specify a file to read from with --input."),
            StoreWithPath => write!(f, "You cannot use --store with --input. The input file is already persistent.")
        }
    }
}

impl std::error::Error for ArgumentError {}

/// Construct a closure that processes the input file with the provided config and returns the serialized result.
fn get_process(
    input: Option<PathBuf>,
    clean: Option<Regex>,
) -> Box<dyn FnMut() -> Result<String, Error>> {
    let mut get_reader: Box<dyn FnMut() -> Result<Box<dyn BufRead>, Error>> = match input {
        None => Box::new(|| {
            let stdin = io::stdin();
            Ok(Box::new(io::BufReader::new(stdin)))
        }),
        Some(path) => Box::new(move || {
            let file = std::fs::File::open(&path)?;
            Ok(Box::new(io::BufReader::new(file)))
        }),
    };
    let mut clean_lines = get_cleaner(clean);

    Box::new(move || {
        let mut reader = get_reader()?;
        let lines = (&mut reader)
            .lines()
            .collect::<Result<Vec<String>, io::Error>>()?;
        let lines = clean_lines(lines);
        let mut tractus = Tractus::new();
        tractus.parse_lines(lines)?;
        let result = serde_json::to_string(&tractus.serialize())?;
        Ok(result)
    })
}

/// Construct a closure that cleans the input according to the passed config.
fn get_cleaner(clean: Option<Regex>) -> Box<dyn FnMut(Vec<String>) -> Vec<String>> {
    match clean {
        None => Box::new(|lines| lines),
        Some(regex) => Box::new(move |lines| {
            lines
                .iter()
                .map(|line| regex.replace_all(&line, "").to_string())
                .collect()
        }),
    }
}

/// Write the result to the output specified.
fn write_result(output: &mut Option<OutputPath>, result: &str) -> Res {
    let out = if let Some(out) = &output {
        out.path.display().to_string()
    } else {
        "stdout".to_string()
    };
    println!("Writing to {}.", out);
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

/// A path that keeps track of whether it is allowed to overwrite.
struct OutputPath {
    path: PathBuf,
    force: bool,
}

impl OutputPath {
    /// Ask the user for confirmation, if overwrite is not already enforced. If overwrite is confirmed, remember to force next time.
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

    /// Write to the file. If overwrite is not yet enforced, ask the user for confirmation.
    pub fn write_confirmed(&mut self, output: &str) -> Res {
        if self.confirm()? {
            std::fs::write(&self.path, output)?;
        }
        Ok(())
    }
}

type Res = Result<(), Error>;

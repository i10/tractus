extern crate structopt;

use std::borrow::Borrow;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::fs;
use std::io;
use std::io::{Read, Write};
use std::ops::Deref;
use std::path;
use std::path::PathBuf;
use std::rc::Rc;
use std::sync::{mpsc, Arc, Mutex, RwLock};
use std::thread;
use std::time::Duration;

use clap_verbosity_flag;
use env_logger;
use failure::Error;
use log::{debug, info, trace, warn};
use notify;
use notify::{DebouncedEvent, RecommendedWatcher, RecursiveMode, Watcher};
use regex::Regex;
use serde::Deserialize;
use serde_json;
use serde_json::json;
use structopt::StructOpt;
use websocket::sync::{Client, Server};
use websocket::OwnedMessage;

use tractus::parser;
use tractus::{LineTree, Parsed};

#[derive(StructOpt)]
#[structopt(name = "tractus", about = "Generate a hypotheses tree from R code.")]
struct Opt {
    #[structopt(short, long, parse(from_os_str))]
    /// Input file, stdin if not present
    input: Option<PathBuf>,
    #[structopt(short, long, parse(from_os_str))]
    /// Output file, stdout if not present
    output: Option<PathBuf>,
    #[structopt(flatten)]
    verbose: clap_verbosity_flag::Verbosity,
    #[structopt(short = "f", long)]
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
        #[structopt(short, long, parse(from_os_str))]
        /// Input file, stdin if not present
        input: PathBuf,
        #[structopt(short, long, parse(from_os_str))]
        /// Output file that will be overwritten whenever the input changes
        output: PathBuf,
        #[structopt(flatten)]
        r_history: RHistory,
    },
    #[structopt(name = "serve")]
    /// Output updates on a websocket.
    Serve {
        #[structopt(short, long, parse(from_os_str))]
        /// Input file, listens to websockets if not present
        input: Option<PathBuf>,
        #[structopt(short, long, parse(from_os_str))]
        output: Option<PathBuf>,
        #[structopt(flatten)]
        r_history: RHistory,
    },
}

#[derive(StructOpt)]
struct RHistory {
    #[structopt(long, short)]
    /// Indicate that this file will only be appended to
    ///
    /// Tractus can optimize the calculations, but it might stop with an error if the file is modified other than appending.
    append: bool,
    #[structopt(long, short)]
    /// Filter the lines with a regex
    ///
    /// The regex syntax used is documented at https://docs.rs/regex/1.2.1/regex/#syntax.
    /// Whatever is matched by the regex will be deleted. You can debug the regex by enabling logging at the trace level.
    clean: Option<Regex>,
    #[structopt(name = "history", long, short)]
    /// Indicate that the input file is an RStudio `history_desktop` file.
    r_history: bool,
}

struct WatchOptions {
    offset: Option<Offset>,
    clean: Option<Regex>,
}

impl TryFrom<RHistory> for WatchOptions {
    type Error = regex::Error;
    fn try_from(other: RHistory) -> Result<Self, Self::Error> {
        Ok(if other.r_history {
            WatchOptions {
                offset: Some(0),
                clean: Some(Regex::new(r#"(?m)^\d+:"#)?),
            }
        } else {
            let seek = if other.append { Some(0) } else { None };
            WatchOptions {
                offset: seek,
                clean: other.clean,
            }
        })
    }
}

fn main() {
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
    env_logger::Builder::from_default_env().init(); //filter_level(level).init();
}

fn run(opt: Opt) -> Res {
    if let Some(subcmd) = opt.subcmd {
        match subcmd {
            Subcommand::Watch {
                input,
                output,
                r_history,
            } => {
                debug!("Watch is enabled.");
                let options = WatchOptions::try_from(r_history)?;
                if let Some(mut offset) = options.offset {
                    let mut dependency_graph = tractus::DependencyGraph::new();
                    let mut index = 1;
                    let execute = || {
                        let result = parse_from_offset(
                            &input,
                            &options.clean,
                            offset,
                            &mut dependency_graph,
                            &mut index,
                        )?;
                        offset = result.0;
                        let output_result = result.1;

                        println!("Outputting to file {}.", output.display());
                        let mut output_file = fs::File::create(&output)?;
                        output_to(&mut output_file, output_result)?;

                        Ok(())
                    };
                    watch(&input, execute)?;
                } else {
                    let execute = || {
                        let result = parse_entirely(&input, &options.clean)?;

                        println!("Outputting to file {}.", &output.display());
                        let mut output_file = fs::File::create(&output)?;
                        output_to(&mut output_file, result)?;

                        Ok(())
                    };
                    watch(&input, execute)?;
                }
            }
            Subcommand::Serve {
                input,
                output,
                r_history,
            } => match input {
                Some(input_path) => {
                    debug!("Serving from file.");
                    let options = WatchOptions::try_from(r_history)?;
                    let offset = 0;
                    let mut index = 1;

                    let mut dependency_graph = tractus::DependencyGraph::new();
                    let res = parse_from_offset(
                        &input_path,
                        &options.clean,
                        offset,
                        &mut dependency_graph,
                        &mut index,
                    )?
                    .1;
                    let result: Arc<RwLock<String>> = Arc::new(RwLock::new(res));

                    let clients: Arc<Mutex<Vec<Client<std::net::TcpStream>>>> =
                        Arc::new(Mutex::new(vec![]));

                    let result_clone = result.clone();
                    let clients_clone = clients.clone();
                    let input_clone = input_path.clone();
                    let mut execute = || -> Res {
                        let mut dependency_graph = tractus::DependencyGraph::new();
                        let res = parse_from_offset(
                            &input_clone,
                            &options.clean,
                            offset,
                            &mut dependency_graph,
                            &mut index,
                        )?
                        .1;

                        let mut clients = clients_clone.lock().unwrap();
                        for client in clients.iter_mut() {
                            let ip = client.peer_addr().unwrap();
                            trace!("Sending to IP: {}", ip);
                            let message = OwnedMessage::Text(res.clone());
                            if let Err(e) = client.send_message(&message) {
                                debug!(
                                    "Client {} responded with error after sending message: {}",
                                    ip, e
                                );
                            };
                        }

                        let mut result_store = result_clone.write().unwrap();
                        *result_store = res;

                        Ok(())
                    };

                    let clients_clone2 = clients.clone();
                    let result_clone2 = result.clone();
                    thread::spawn(move || {
                        debug!("Listening for requests.");
                        let server = Server::bind("127.0.0.1:2794").unwrap();
                        for request in server.filter_map(Result::ok) {
                            trace!("New request.");
                            if !request
                                .protocols()
                                .contains(&"tractus-websocket".to_string())
                            {
                                debug!("New request rejected.");
                                request.reject().unwrap();
                            } else {
                                let mut client =
                                    request.use_protocol("tractus-websocket").accept().unwrap();
                                debug!("New request accepted.");

                                let res = result_clone2.read().unwrap().to_string();
                                let message = OwnedMessage::Text(res);
                                client.send_message(&message).unwrap();

                                let mut clients = clients_clone2.lock().unwrap();
                                clients.push(client);
                            }
                        }
                    });

                    let (tx, rx) = mpsc::channel();
                    let mut watcher: RecommendedWatcher =
                        Watcher::new(tx, Duration::from_millis(500))?;
                    watcher.watch(&input_path, RecursiveMode::NonRecursive)?;

                    println!("Watching file {}.", &input_path.display());
                    for event in rx {
                        trace!("Received new file event.");
                        if let DebouncedEvent::Write(input) = event {
                            debug!("File changed: {}", input_path.display());
                            execute()?;
                        }
                    }
                }
                None => {
                    debug!("Listening to websocket for input.");
                    let options = WatchOptions::try_from(r_history)?;
                    let init_deps = match &output {
                        Some(out_path) => {
                            let mut file_res = std::fs::File::open(out_path);
                            if let Ok(mut file) = file_res {
                                serde_json::from_str(&read(&mut file)?)?
                            } else {
                                tractus::DependencyGraph::new()
                            }
                        }
                        None => tractus::DependencyGraph::new(),
                    };

                    let dependency_graph: Arc<RwLock<tractus::DependencyGraph<_>>> =
                        Arc::new(RwLock::new(init_deps));

                    fn ser(
                        dep: &tractus::DependencyGraph<(
                            parser::Span,
                            serde_json::Value,
                            String,
                            Vec<String>,
                            Option<String>,
                            usize,
                        )>,
                        out_path: &Option<PathBuf>,
                    ) -> Result<String, Error> {
                        trace!("Hypothesis tree.");
                        let hypotheses = tractus::parse_hypothesis_tree(&dep);

                        debug!("Serializing...");
                        let res =
                            serde_json::to_string_pretty(&LineTree::with(&hypotheses, &mut |e| {
                                let meta = e.get_meta();
                                json!({
                                    "expression": format!("{}", e),
                                    "span": meta.0,
                                    "meta": meta.1,
                                    "statement": meta.2,
                                    "assigned_variables": meta.3,
                                    "function_name": meta.4,
                                    "index": meta.5
                                })
                            }))?;
                        if let Some(p) = out_path {
                            write_output(&Some(p), true, serde_json::to_string(dep)?)?;
                        }
                        Ok(res)
                    }
                    let res = ser(&dependency_graph.read().unwrap(), &output)?;
                    let result: Arc<RwLock<String>> = Arc::new(RwLock::new(res));

                    let (msg_sender, msg_receiver) = mpsc::channel::<OwnedMessage>();
                    let (stmt_sender, stmt_receiver) =
                        mpsc::channel::<(std::net::SocketAddr, String)>();
                    let mut clients: Arc<
                        Mutex<HashMap<std::net::SocketAddr, websocket::sender::Writer<_>>>,
                    > = Arc::new(Mutex::new(HashMap::new()));

                    debug!("Listening for requests.");
                    let server = Server::bind("127.0.0.1:2794").unwrap();
                    let clients_clone = clients.clone();
                    let result_clone = result.clone();
                    thread::spawn(move || {
                        for request in server.filter_map(Result::ok) {
                            trace!("New request.");
                            if !request
                                .protocols()
                                .contains(&"tractus-websocket".to_string())
                            {
                                debug!("New request rejected.");
                                request.reject().unwrap();
                            } else {
                                let mut client =
                                    request.use_protocol("tractus-websocket").accept().unwrap();
                                debug!("New request accepted.");

                                let res = result_clone.read().unwrap().to_string();
                                let message = OwnedMessage::Text(res);
                                client.send_message(&message).unwrap();
                                let ip = client.peer_addr().unwrap();

                                let (mut client_receiver, client_sender) = client.split().unwrap();
                                let mut clients = clients_clone.lock().unwrap();
                                clients.insert(ip, client_sender);

                                let stmt_sender_clone = stmt_sender.clone();
                                thread::spawn(move || {
                                    for msg in client_receiver.incoming_messages() {
                                        info!("Received new statement.");
                                        match msg {
                                            Ok(message) => {
                                                use websocket::OwnedMessage::*;
                                                match message {
                                                    OwnedMessage::Text(stmt) => {
                                                        stmt_sender_clone.send((ip, stmt));
                                                    }
                                                    _ => (),
                                                }
                                            }
                                            Err(e) => {
                                                debug!("Message contained error: {}", e);
                                            }
                                        }
                                    }
                                });
                            }
                        }
                    });

                    thread::spawn(move || {
                        for msg in msg_receiver {
                            trace!("Received message to send: {:?}", msg);
                            let mut clients = clients.lock().unwrap();
                            for (ip, client_sender) in clients.iter_mut() {
                                trace!("Sending to IP: {}", ip);
                                if let Err(e) = client_sender.send_message(&msg) {
                                    debug!(
                                        "Client {} responded with error after sending message: {}",
                                        ip, e
                                    );
                                };
                            }
                        }
                    });

                    let mut index = 0;
                    for (ip, stmt) in stmt_receiver {
                        trace!("Parsing new statement.");
                        if let Ok(meta) = serde_json::from_str::<MetaStmt>(&stmt) {
                            let mut parser = Parsed::new();
                            let stmts_result = parser.append(&meta.statement);
                            if let Ok(stmts) = stmts_result {
                                let stmts_meta = stmts.iter().map(|stmt| {
                                    let vars =
                                        if let parser::RStatement::Assignment(left, add, _, _) =
                                            stmt.deref()
                                        {
                                            let mut vs = vec![format!("{}", left)];
                                            let mut addition: Vec<String> =
                                                add.iter().map(|v| format!("{}", v)).collect();
                                            vs.append(&mut addition);
                                            vs
                                        } else {
                                            vec![]
                                        };
                                    let fun_name =
                                        stmt.expression().and_then(|e| extract_function_name(&e));
                                    index += 1;
                                    stmt.map(&mut |s| {
                                        (
                                            s.clone(),
                                            meta.meta.clone(),
                                            format!("{}", stmt),
                                            vars.clone(),
                                            fun_name.clone(),
                                            index,
                                        )
                                    })
                                });
                                let mut dep = dependency_graph.write().unwrap();
                                if !dep.batch_insert(stmts_meta).is_empty() {
                                    trace!("Publishing new graph.");
                                    let res = ser(&dep, &output)?;
                                    let message = OwnedMessage::Text(res.clone());
                                    trace!("Sending message.");
                                    msg_sender.send(message)?;

                                    let mut result_store = result.write().unwrap();
                                    *result_store = res;
                                }
                            }
                        }
                    }
                }
            },
        }
    } else {
        debug!("Watch is disabled.");
        execute(&opt.input, &opt.output, opt.overwrite)?;
    }

    Ok(())
}

fn extract_function_name<T>(expression: &Rc<parser::RExpression<T>>) -> Option<String> {
    use parser::RExpression::*;
    match expression.deref() {
        Call(name, _, _) => name.extract_variable_name(),
        Column(left, _, _) => extract_function_name(&left),
        _ => None,
    }
}

#[derive(Deserialize)]
struct MetaStmt {
    statement: String,
    meta: serde_json::Value,
}

fn watch<F: FnMut() -> Res>(input_path: &PathBuf, mut execute: F) -> Res {
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

fn parse_entirely(input_path: &PathBuf, clean: &Option<Regex>) -> Result<String, Error> {
    let code = read_from(&Some(input_path))?;
    let code = clean_input(code, clean);

    process(&code)
}

type Offset = u64;

fn parse_from_offset(
    input_path: &PathBuf,
    clean: &Option<Regex>,
    offset: Offset,
    dependency_graph: &mut tractus::DependencyGraph<(
        parser::Span,
        (),
        String,
        Vec<String>,
        Option<String>,
        usize,
    )>,
    index: &mut usize,
) -> Result<(Offset, String), Error> {
    debug!(
        "Reading from file {} with offset {}.",
        input_path.display(),
        offset
    );
    let file = std::fs::File::open(input_path)?;
    let mut reader = io::BufReader::new(file);

    use io::Seek;
    reader.seek(io::SeekFrom::Start(offset))?;

    let mut unparsed: Vec<String> = vec![];
    let mut unparsed_offset = offset;
    let mut total_offset = offset;
    let mut unparsed_index = *index;

    use io::BufRead;
    info!("Parsing...");
    let new_parsed_statements: Vec<Rc<_>> = reader
        .lines()
        .map(|line| {
            let line = line?;
            total_offset += line.len() as u64;
            *index += 1;
            trace!(
                "Parsing from offset {}, current offset is {}.",
                unparsed_offset,
                total_offset
            );

            trace!("Parsing line: {}", line);
            let cleaned = clean_input(line, clean);

            unparsed.push(cleaned);

            let to_parse = unparsed.join("\n");
            let mut parsed = Parsed::new();
            let parsed_result = parsed.append(&to_parse);
            trace!("Input was: {}", to_parse);
            let current_line = unparsed_index;
            match parsed_result {
                Err(e) => {
                    debug!("Parsing error in input: {}", e);

                    // If the parsing error occurred at the very last symbol,
                    // we assume that it is simply incomplete and will try again when we have more input.
                    if let pest::error::InputLocation::Pos(pos) = e.location {
                        trace!(
                            "Error position is {}, last position is {}.",
                            pos,
                            to_parse.len()
                        );
                        if pos == to_parse.len() {
                            debug!("Will retry with more input.");
                            return Ok(vec![]); // Retry with more input by not updating the offset.
                        }
                    }
                    debug!("Skipping this input.");
                    unparsed_offset = total_offset;
                    Ok(vec![])
                }
                Ok(parsed) => {
                    debug!("Parsed input successfully.");
                    unparsed_offset = total_offset;
                    unparsed.clear();
                    let parsed_meta = parsed.iter().map(|stmt| {
                        let vars =
                            if let parser::RStatement::Assignment(left, add, _, _) = stmt.deref() {
                                let mut vs = vec![format!("{}", left)];
                                let mut addition: Vec<String> =
                                    add.iter().map(|v| format!("{}", v)).collect();
                                vs.append(&mut addition);
                                vs
                            } else {
                                vec![]
                            };
                        let fun_name = stmt.expression().and_then(|e| extract_function_name(&e));
                        stmt.map(&mut |s| {
                            (
                                s.clone(),
                                (),
                                format!("{}", stmt),
                                vars.clone(),
                                fun_name.clone(),
                                current_line,
                            )
                        })
                    });
                    unparsed_index = *index;
                    Ok(parsed_meta.collect())
                }
            }
        })
        .collect::<Result<Vec<Vec<Rc<_>>>, Error>>()?
        .into_iter()
        .flatten()
        .collect();

    debug!("Parsing dependency graph...");
    dependency_graph.batch_insert(new_parsed_statements.into_iter());
    debug!("Parsing hypothesis tree...");
    let hypotheses = tractus::parse_hypothesis_tree(&dependency_graph);

    debug!("Serializing...");
    let result = serde_json::to_string_pretty(&LineTree::with(&hypotheses, &mut |e| {
        let meta = e.get_meta();
        json!({
            "expression": format!("{}", e),
            "span": meta.0,
            "meta": meta.1,
            "statement": meta.2,
            "assigned_variables": meta.3,
            "function_name": meta.4,
            "index": meta.5
        })
    }))?;

    Ok((unparsed_offset, result))
}

fn clean_input(code: String, clean: &Option<Regex>) -> String {
    clean
        .as_ref()
        .map(|regex| {
            trace!("Cleaning input's line with regex `{}`.", regex);
            let cleaned = regex.replace_all(&code, "").into_owned();
            trace!("Cleaned input:\n{}", cleaned);
            cleaned
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
    let hypotheses = tractus::parse_hypothesis_tree(&dependency_graph);

    debug!("Serializing...");
    let result = serde_json::to_string_pretty(&LineTree::with_span(&hypotheses))?;

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

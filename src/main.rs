use std::{
    fs::File,
    io::{stdin, stdout, BufRead, BufReader, Read, Write},
};

use clap::{self, Parser, Subcommand};
use combine::{EasyParser, ParseError};

use niao::scanner::Scanner;

#[derive(Parser)]
#[clap(author, version, about, long_about = None)]
struct Cli {
    #[clap(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// run some files or interact
    Run { file: Option<String> },

    /// debug mode
    Debug { file: Option<String> },
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();

    match &cli.command {
        Command::Debug { file } => {
            let mut buf = BufReader::new(stdin());
            let mut line = String::new();
            let mut skip_readline = false;

            if let Some(file) = file {
                let mut f = File::open(file)?;
                f.read_to_string(&mut line)?;
                skip_readline = true
            }

            loop {
                print!("> ");
                stdout().flush()?;

                if !skip_readline {
                    let len = buf.read_line(&mut line)?;
                    if len == 0 {
                        break;
                    }
                    if line.ends_with("\\\n") {
                        let all_len = line.len();
                        line.replace_range((all_len - 2)..all_len, "\n");
                        continue;
                    }
                } else {
                    println!("!!load from file: {:}", file.as_ref().unwrap());
                    skip_readline = false;
                }

                let mut scanner = Scanner::new(&line);
                scanner.scan()?;

                for (i, tok) in scanner.tokens.iter().enumerate() {
                    println!("{}: {:?}", i, tok);
                }

                let mut parser = niao::parser::chunk();
                let stream = scanner.token_stream();

                match parser.easy_parse(stream) {
                    Ok((result, input)) => {
                        dbg!(result);
                        if input.stream.len() != 0 {
                            eprintln!("[!!] Unprocessed tokens left:");
                            dbg!(input);
                        }
                    }
                    Err(e) => {
                        use combine::easy::{Error, Info};
                        eprintln!("[!!] Parse error:");
                        eprintln!("     On Token = {:?}", e.position());
                        for err in e.errors.iter() {
                            match err {
                                Error::Expected(v) => {
                                    if let Info::Token(t) = v {
                                        eprintln!("     Expected token = {:?}", t.token_type);
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                }

                line.clear();
            }

            eprintln!("Finished REPL.");
            Ok(())
        }
        Command::Run { .. } => todo!("going to"),
    }
}

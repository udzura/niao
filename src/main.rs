use std::io::{stdin, stdout, BufRead, BufReader, Write};

use combine::{EasyParser, ParseError};
use niao::scanner::Scanner;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut buf = BufReader::new(stdin());
    let mut line = String::new();
    loop {
        print!("> ");
        stdout().flush()?;

        let len = buf.read_line(&mut line)?;
        if len == 0 {
            break;
        }
        if line.ends_with("\\\n") {
            let all_len = line.len();
            line.replace_range((all_len - 2)..all_len, "\n");
            continue;
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

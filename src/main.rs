use std::io::{stdin, stdout, BufRead, BufReader, Write};

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
            line.replace_range((all_len - 2)..all_len, " ");
            continue;
        }

        let mut scanner = Scanner::new(&line);
        scanner.scan()?;

        for (i, tok) in scanner.tokens.iter().enumerate() {
            println!("{}: {:?}", i, tok);
        }
        line.clear();
    }

    eprintln!("Finished REPL.");
    Ok(())
}

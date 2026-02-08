use std::io::{self, BufReader};

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() > 1 {
        // File arg → execute the file
        let mut output = io::stdout();
        if let Err(e) = strawman::repl::run_file(&args[1], &mut output) {
            eprintln!("Error: {}", e);
            std::process::exit(1);
        }
    } else {
        // No args → REPL
        let stdin = io::stdin();
        let mut reader = BufReader::new(stdin.lock());
        let mut output = io::stdout();
        strawman::repl::run_repl(&mut reader, &mut output);
    }
}

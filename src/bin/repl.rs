use rust_dice::stringifiers::MarkdownStringifier;
use std::io::{self, BufRead, Write};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let stdin = io::stdin();
    let mut lines = stdin.lock().lines();
    let mut buf = String::new();
    print!("> ");
    io::stdout().flush()?;
    while let Some(Ok(line)) = lines.next() {
        if line.is_empty() {
            match rust_dice::roll(&buf).and_then(|r| r.result::<MarkdownStringifier>()) {
                Ok(r) => println!("{}", r),
                Err(why) => eprintln!("Error: {}", why),
            }
            print!("> ");
            io::stdout().flush()?;
            buf.clear();
        } else {
            buf.push_str(&line);
            buf.push('\n');
            print!("... ");
            io::stdout().flush()?;
        }
    }
    Ok(())
}

use std::fs::File;
use std::io::{BufReader, Read};
use std::path::PathBuf;

use clap::*;
use solvr::*;

#[derive(Copy, Clone, Eq, PartialEq, Debug, Subcommand)]
enum Command {
    /// Dump input file and exit
    Dump,
}

#[derive(clap::Parser, Debug)]
#[command(name = "solvr", version, about = "Calculate numbers")]
struct Args {
    // #[clap(subcommand)]
    // command: Command,
    #[arg(help = "Input files")]
    files: Vec<PathBuf>,
    #[arg(short, long, help = "Increase verbosity")]
    verbose: bool,
}

fn main() {
    let args = Args::parse();

    let mut solvr = Solvr::new();
    for fname in args.files {
        let fh = File::open(&fname);
        match fh {
            Ok(fh) => {
                let mut input = String::new();
                match BufReader::new(fh).read_to_string(&mut input) {
                    Ok(_) => solvr.parse(&input),
                    Err(e) => panic!("Can't read {fname:?}: {e}"),
                }
            }
            Err(err) => panic!("Trying to open {fname:?}: {err}"),
        }
    }
}

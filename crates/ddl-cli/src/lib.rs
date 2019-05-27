#![warn(rust_2018_idioms)]

use std::error::Error;
use std::ffi::OsStr;
use std::fs::{self, File};
use std::io::prelude::*;
use std::path::{Path, PathBuf};

#[derive(structopt::StructOpt)]
pub struct Options {
    /// Write output to <FILENAME>
    #[structopt(short = "o", long = "out", name = "FILENAME")]
    pub output: Option<PathBuf>,
    /// Write output to compiler-chosen filename in <DIR>
    #[structopt(long = "out-dir", name = "DIR")]
    pub out_dir: Option<PathBuf>,
    /// The data definition to compile
    #[structopt(name = "INPUT")]
    pub input: PathBuf,
}

impl Options {
    fn input_stem(&self) -> &OsStr {
        self.input.file_stem().unwrap()
    }

    fn out_dir(&self) -> &Path {
        match &self.out_dir {
            None => self.input.parent().unwrap(),
            Some(out_dir) => out_dir.as_path(),
        }
    }

    fn output(&self) -> PathBuf {
        match &self.output {
            Some(output) => self.out_dir().join(output),
            None => self.out_dir().join(self.input_stem()).with_extension("rs"),
        }
    }
}

pub fn run(options: Options) -> Result<(), Box<dyn Error>> {
    let src = fs::read_to_string(&options.input)?;
    let _module = ddl_parse::parse_module(&src)?;

    let mut output = File::create(options.output())?;
    write!(output, "hahahaha")?;

    Ok(())
}

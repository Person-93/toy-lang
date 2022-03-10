#![forbid(unsafe_code)]

use clap::{ArgGroup, Parser};
use std::{
  io::{self, Read},
  path::PathBuf,
  process,
};
use toyc_errors::emitter::JsonWriterEmitter;
use toyc_errors::Handler;
use toyc_hir::HirContext;
use toyc_session::Session;

#[derive(Debug, Parser)]
#[clap(author, version, about)]
#[clap(group(ArgGroup::new("src").required(true).args(&["input", "stdin"])))]
struct Cli {
  /// The path of the file to compile
  input: Option<PathBuf>,

  /// The path of the output file (not including the file extension)
  #[clap(default_value = "a.out")]
  output: PathBuf,

  /// Read from stdin instead of a file
  #[clap(long, conflicts_with("input"))]
  stdin: bool,
}

pub fn main() -> ! {
  let Cli {
    input,
    output,
    stdin: _,
  } = Cli::parse();

  let name = output.file_name().unwrap().to_str().unwrap().to_owned();
  let handler =
    Handler::new(Box::new(JsonWriterEmitter::new(io::stderr(), true)));

  let session = match input {
    Some(input) => Session::new(name, input, handler),
    None => {
      let mut text = String::new();
      match io::stdin().read_to_string(&mut text) {
        Ok(_) => Session::from_str(name, text, handler),
        Err(err) => {
          handler
            .fatal("failed to read package root file from stdin")
            .attach_note(format!("caused by {err}"))
            .emit();
          process::exit(1);
        }
      }
    }
  };

  let ast = toyc_ast::parse_single_file(&session.root_source, &session.handler);
  let ast = match ast {
    Some(ast) => ast,
    None => session.handler.err_exit(1.try_into().unwrap()),
  };
  toyc_ast_passes::check_package(&session.handler, &ast.attrs, &ast.items);

  let hir: HirContext = toyc_ast_lowering::LoweringContext::new(&ast).into();
  toyc_hir_passes::check_package(&session, &hir);

  session.handler.finish();

  process::exit(0);
}

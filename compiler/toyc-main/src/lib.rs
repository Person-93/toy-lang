use clap::{ArgGroup, Parser};
use std::{
  fs,
  io::{self, Read},
  path::PathBuf,
  process,
};
use toyc_errors::emitter::JsonWriterEmitter;
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
  let session = Session::new(
    output.file_name().unwrap().to_str().unwrap().to_owned(),
    input,
    toyc_errors::Handler::new(Box::new(JsonWriterEmitter::new(
      io::stderr(),
      true,
    ))),
  );

  let text = match match &session.root_source_file {
    Some(input) => fs::read_to_string(input),
    None => {
      let mut text = String::new();
      io::stdin().read_to_string(&mut text).map(|_| text)
    }
  } {
    Ok(text) => text,
    Err(err) => {
      session
        .handler
        .fatal("failed to read package root file")
        .attach_note(format!("caused by {err}"))
        .emit();
      process::exit(1);
    }
  };

  let ast = toyc_ast::parse_single_file(&text, &session.handler);
  let ast = match ast {
    Some(ast) => ast,
    None => session.handler.err_exit(1.try_into().unwrap()),
  };
  toyc_ast_passes::check_package(&session.handler, &ast.attrs, &ast.items);

  session
    .handler
    .note(
      "all checks passed but generating the output file is not yet implemented",
    )
    .emit();

  process::exit(0);
}

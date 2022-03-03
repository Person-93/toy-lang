use clap::Parser;
use std::{fs, io, path::PathBuf, process};
use toyc_errors::emitter::JsonWriterEmitter;
use toyc_session::Session;

#[derive(Debug, Parser)]
#[clap(author, version, about)]
struct Cli {
  /// The path of the file to compile
  input: PathBuf,

  /// The path of the output file (not including the file extension)
  #[clap(default_value = "a.out")]
  output: PathBuf,
}

fn main() -> ! {
  let Cli { input, output } = Cli::parse();
  let session = Session::new(
    output.file_name().unwrap().to_str().unwrap().to_owned(),
    Some(input),
    toyc_errors::Handler::new(Box::new(JsonWriterEmitter::new(
      io::stderr(),
      true,
    ))),
  );

  let text =
    match fs::read_to_string(session.root_source_file.as_ref().unwrap()) {
      Ok(text) => text,
      Err(err) => {
        session
          .handler
          .fatal(format!(
            "failed to read input file `{}`",
            session.root_source_file.as_ref().unwrap().display()
          ))
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

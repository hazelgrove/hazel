open Cmdliner;

/* Read from stdin or file depending on argument */
let read_input = path =>
  switch (path) {
  | "-" =>
    let buf = Buffer.create(1024);
    try(
      {
        while (true) {
          let line = input_line(stdin);
          Buffer.add_string(buf, line);
          Buffer.add_char(buf, '\n');
        };
        assert(false);
      }
    ) {
    /* unreachable */

    | End_of_file => Buffer.contents(buf)
    | _ => failwith("Unexpected error while reading input")
    };
  | file =>
    let ic = open_in(file);
    let len = in_channel_length(ic);
    let content = really_input_string(ic, len);
    close_in(ic);
    content;
  };

/* Placeholder implementations for each command */
let run_hazel = path => {
  let program = read_input(path);
  let parsed = Parse.parse_program(program);
  let evaluated = Run.evaluate(parsed);

  print_endline(Print.print(evaluated));
};

let format_hazel = path => {
  let program = read_input(path);
  let parsed = Parse.parse_program(program);
  print_endline(Print.print(parsed));
};

let analyze_hazel = path => {
  let _program = read_input(path);
  /* Printf.printf "Analyzing Hazel program:\n%s\n%!" program; */
  /* TODO Use statics to output marks */
  ();
};

/* Common arg: path or "-" */
let input_arg = {
  let doc = "Path to Hazel source file, or '-' to read from stdin.";
  Arg.(
    required & pos(0, some(string), None) & info([], ~docv="INPUT", ~doc)
  );
};

/* Subcommand terms using Cmd.info */
let run_cmd = {
  let doc = "Run a Hazel program.";
  let info = Cmd.info("run", ~doc);
  Cmd.v(info, Term.(const(run_hazel) $ input_arg));
};

let format_cmd = {
  let doc = "Reconstructs Hazel code from its abstract syntax tree (AST), producing\n      concrete syntax without preserving original whitespace or comments.\n      This process uses a recovering parser and automatically inserts holes\n      where necessary to ensure syntactic correctness.";

  let info = Cmd.info("format", ~doc);
  Cmd.v(info, Term.(const(format_hazel) $ input_arg));
};

let _analyze_cmd = {
  let doc = "Perform static analysis on Hazel code.";
  let info = Cmd.info("analyze", ~doc);
  Cmd.v(info, Term.(const(analyze_hazel) $ input_arg));
};

/* Default to help if no subcommand is given */
let default_cmd = {
  let doc = "CLI tool for running and analyzing Hazel programs.";
  let info = Cmd.info("hazel", ~version="0.1.0", ~doc);
  Cmd.group(info, [run_cmd, format_cmd]);
};

let () = exit(Cmd.eval(default_cmd));

open Cmdliner;

/* Read from stdin or file depending on argument */
let read_input = path => {
  Core.(
    switch (path) {
    | "-" => In_channel.input_all(In_channel.stdin)
    | file => In_channel.read_all(file)
    }
  );
};

let run_hazel = path => {
  let program = read_input(path);
  let parsed = Parse.parse_program(program);
  let evaluated = Run.evaluate(parsed(~root=Exp));

  print_endline(Print.print(evaluated));
};

let format_hazel = path => {
  let program = read_input(path);
  let parsed = Parse.parse_program(program);
  print_endline(Print.print(parsed(~root=Exp)));
};

let analyze_hazel = path => {
  let program = read_input(path);
  let parsed = Parse.parse_program(program);
  open Language;
  let static_map =
    Statics.mk(
      CoreSettings.on,
      Builtins.ctx_init(Some(Int)),
      parsed(~root=Exp),
    );
  let errors = List.map(snd, Statics.Map.errors(static_map));
  switch (errors) {
  | [] =>
    print_endline("No static errors found.");
    `Ok();
  | _ =>
    prerr_endline("Static errors:");
    List.iter(error => print_endline(Info.show_error(error)), errors);
    prerr_endline("");
    `Error((false, "Static errors found"));
  };
};

/* Common arg: path or "-" for stdin */
let input_arg = {
  let doc = "Path to Hazel source file, or '-' to read from stdin.";
  Arg.(
    required & pos(0, some(string), None) & info([], ~docv="INPUT", ~doc)
  );
};

let run_cmd = {
  let doc = "Run a Hazel program.";
  let info = Cmd.info("run", ~doc);
  Cmd.v(info, Term.(const(run_hazel) $ input_arg));
};

let format_cmd = {
  let doc = {|
    Reconstructs Hazel code from its abstract syntax tree (AST), producing
    concrete syntax without preserving original whitespace or comments.
    This process uses a recovering parser and automatically inserts holes
    where necessary to ensure syntactic correctness.
  |};

  let info = Cmd.info("format", ~doc);
  Cmd.v(info, Term.(const(format_hazel) $ input_arg));
};

let analyze_cmd = {
  let doc = "Perform static analysis on Hazel code.";
  let info = Cmd.info("analyze", ~doc);
  Cmd.v(info, Term.ret(Term.(const(analyze_hazel) $ input_arg)));
};

/* Default to help if no subcommand is given */
let default_cmd = {
  let doc = "CLI tool for running and analyzing Hazel programs.";
  let info = Cmd.info("hazel", ~doc);
  Cmd.group(info, [run_cmd, format_cmd, analyze_cmd]);
};

let () = exit(Cmd.eval(default_cmd));

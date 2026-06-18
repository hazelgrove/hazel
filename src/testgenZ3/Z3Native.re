/* Native Z3 backend for test input generation.
 *
 * This is the native half of the "two backends behind SMT-LIB2" design: it
 * consumes the SMT-LIB2 script produced by Haz3lcore.TestGen and invokes the
 * system `z3` binary, then parses the solver's textual output back through
 * the shared Haz3lcore.TestGen.parse_model.
 *
 * We shell out to the `z3` executable rather than linking the native OCaml
 * `z3` opam bindings: the bindings build Z3 from source (pulling in llvm and
 * python), whereas a `z3` binary is small, ubiquitous, and keeps this
 * backend free of heavy native dependencies. The browser/node frontends use
 * the `z3-solver` WASM package instead (see src/web/projectors/Z3Wasm.re);
 * both share TestGen's SMT-LIB2 generation and model parsing. */

module TestGen = Haz3lcore.TestGen;

let default_z3 = "z3";

/* Is a usable `z3` binary on PATH (or at the given path)? */
let is_available = (~z3_path=default_z3, ()): bool => {
  let cmd = Printf.sprintf("%s --version > /dev/null 2>&1", z3_path);
  Sys.command(cmd) == 0;
};

let read_all = (ic: in_channel): string => {
  let rec loop = acc =>
    switch (input_line(ic)) {
    | line => loop([line, ...acc])
    | exception End_of_file => List.rev(acc) |> String.concat("\n")
    };
  loop([]);
};

/* Run the SMT-LIB2 script through `z3` and parse the result. */
let solve = (~z3_path=default_z3, script: string): TestGen.outcome => {
  let (tmp, oc) = Filename.open_temp_file("hazel_testgen", ".smt2");
  output_string(oc, script);
  close_out(oc);
  let cmd =
    Printf.sprintf("%s %s 2>/dev/null", z3_path, Filename.quote(tmp));
  let ic = Unix.open_process_in(cmd);
  let output = read_all(ic);
  let _: Unix.process_status = Unix.close_process_in(ic);
  Sys.remove(tmp);
  TestGen.parse_model(output);
};

/* Convenience: build the SMT-LIB2 script for a boolean expression's statics
 * and solve it, threading build errors through the outcome. */
let solve_info =
    (~z3_path=default_z3, info: Language.Statics.Info.exp): TestGen.outcome =>
  switch (TestGen.build(info)) {
  | Error(msg) => TestGen.Error(msg)
  | Ok(script) => solve(~z3_path, script)
  };

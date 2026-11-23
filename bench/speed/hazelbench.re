/* Times evaluation only (parsing and statics run once), like `hazel
   bench-eval`, so js_of_ocaml and wasm_of_ocaml builds can be compared. */
open Language;

let now = () => Unix.gettimeofday() *. 1000.;

let () = {
  let path = Sys.argv[1];
  let iterations =
    Array.length(Sys.argv) > 2 ? int_of_string(Sys.argv[2]) : 10;
  let program = In_channel.with_open_bin(path, In_channel.input_all);
  let z =
    switch (
      Haz3lcore.PersistentZipper.parse_text(
        ~source="hazelbench",
        ~root=Exp,
        program,
      )
    ) {
    | Some(z) => z
    | None => failwith("parse failed")
    };
  let exp = Haz3lcore.MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  let (_, elab) =
    Statics.mk(
      CoreSettings.on,
      Builtins.ctx_init(Some(Operators.default_mode)),
      exp,
    );
  let eval = () => fst(Evaluator.evaluate(~env=Builtins.env_init, elab));
  let shown = Exp.show(eval());
  let t0 = now();
  for (_ in 1 to iterations) {
    ignore(eval());
  };
  let t1 = now();
  Printf.printf(
    "%s: %d iterations, %.1f ms/eval, result %s\n",
    path,
    iterations,
    (t1 -. t0) /. float_of_int(iterations),
    String.length(shown) > 60 ? String.sub(shown, 0, 60) ++ "..." : shown,
  );
};

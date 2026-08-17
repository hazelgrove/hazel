/* Native smoke check: parse and evaluate `1 + 2` without any
 * js_of_ocaml dependency, mirroring src/CLI/Run.re. Prints `3`. */
open Haz3lcore;

let elaborate = (exp: Language.Exp.t): Language.Exp.t =>
  snd(
    Language.Statics.mk(
      Language.CoreSettings.on,
      Language.Builtins.ctx_init(Some(Language.Operators.default_mode)),
      exp,
    ),
  );

let evaluate = (exp: Language.Exp.t): Language.Exp.t => {
  let (result, _) =
    Language.Evaluator.evaluate(
      ~env=Language.Builtins.env_init,
      elaborate(exp),
    );
  result;
};

let () = {
  let program = "1 + 2";
  switch (Parser.to_term(program, ~root=Exp)) {
  | None => failwith("Failed to parse expression: " ++ program)
  | Some(exp) =>
    let settings: ExpToSegment.Settings.t = {
      secondary: AutoFormat,
      parenthesization: Defensive,
      label_format: QuoteWhenNecessary,
      inline: true,
      fold_case_clauses: false,
      fold_fn_bodies: `NoFold,
      hide_fixpoints: false,
      show_ascriptions: true,
      show_filters: true,
      show_unknown_as_hole: true,
      hole_tiles: false,
      project_tables: false,
    };
    let result = evaluate(exp);
    print_endline(
      Printer.of_segment(
        ~holes="?",
        ExpToSegment.exp_to_segment(~settings, result),
      ),
    );
  };
};

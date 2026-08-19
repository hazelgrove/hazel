open Alcotest;
open Web;

module Calc = Util.Calc;

/* `MissingStep.Update.calculate` is the most `Calc`-dense component in the app
 * (5 `let.calc`, ~40 Calc uses) and had no coverage. It rebuilds the proof
 * sidebar's assumption set from the semantic context, and the guard around that
 * is the difference between deriving it once and deriving it on every frame of
 * the stepper.
 *
 * Unlike AxiomsBox -- whose filtered rewrite list comes back empty for an
 * ordinary expression, making `x === y` vacuous -- the assumption set here is
 * seeded from `Axioms.v` and is non-empty, so physical equality really does
 * distinguish a reuse from a rebuild. That is why this component is testable
 * without a proof fixture and its neighbour is not. */

let core = Settings.Model.init.core;

let statics_of_text = (text: string): Haz3lcore.CachedStatics.t =>
  switch (Haz3lcore.Parser.to_zipper(~root=Haz3lcore.Sort.Exp, text)) {
  | None => failwith("could not parse: " ++ text)
  | Some(z) =>
    let model =
      Haz3lcore.Editor.Model.mk(z, ~root=Haz3lcore.Sort.Exp)
      |> CodeWithStatics.Model.mk;
    CodeWithStatics.Update.calculate(
      ~settings=core,
      ~is_edited=true,
      ~stitch=x => x,
      ~dynamics=model.dynamics,
      ~is_dynamic_term=false,
      model,
    ).
      statics;
  };

let model_of_text = (text: string) =>
  switch (Haz3lcore.Parser.to_zipper(~root=Haz3lcore.Sort.Exp, text)) {
  | None => failwith("could not parse: " ++ text)
  | Some(z) =>
    let m =
      Haz3lcore.Editor.Model.mk(z, ~root=Haz3lcore.Sort.Exp)
      |> CodeWithStatics.Model.mk;
    CodeWithStatics.Update.calculate(
      ~settings=core,
      ~is_edited=true,
      ~stitch=x => x,
      ~dynamics=m.dynamics,
      ~is_dynamic_term=false,
      m,
    );
  };

let statics = statics_of_text("1 + 1");
let rewrite_editor = model_of_text("?");
let exp = statics.elaborated;

let sem_ctx =
  Language.SemanticCtx.of_ctx_and_env(
    Language.Builtins.ctx_init(None),
    Language.Builtins.closure_env,
  );

let status =
  Language.EvaluatorStep.get_status(
    ~settings=core,
    exp,
    Language.Environment.empty,
  );

let calculate =
    (
      ~exp as e=Calc.OldValue(exp),
      ~ctx=Calc.OldValue(sem_ctx),
      ~info_map as im=Calc.OldValue(statics.info_map),
      model,
    ) =>
  MissingStep.Update.calculate(
    ~settings=core,
    e,
    im,
    ctx,
    Calc.OldValue(status),
    model,
    Calc.OldValue(rewrite_editor),
  );

let assumptions = (m: MissingStep.Model.t) =>
  switch (Calc.get_saved_opt(m.assumptions)) {
  | Some(Some(l)) => Some(l)
  | _ => None
  };

let same = (a, b) =>
  switch (a, b) {
  | (Some(x), Some(y)) => x === y
  | _ => false
  };

/* A settled model: everything derived, every input old. */
let settled = () =>
  MissingStep.Model.init
  |> calculate(~exp=Calc.NewValue(exp), ~ctx=Calc.NewValue(sem_ctx))
  |> calculate;

let tests = (
  "MissingStep",
  [
    /* Fixture guard: the assumption set has to be non-empty, or every reuse
       test below would compare `[]` with `[]` and pass vacuously. */
    test_case(
      "the assumption set is non-empty",
      `Quick,
      () => {
        let m =
          calculate(
            ~exp=Calc.NewValue(exp),
            ~ctx=Calc.NewValue(sem_ctx),
            MissingStep.Model.init,
          );
        check(
          bool,
          "derived at least one assumption",
          true,
          switch (assumptions(m)) {
          | Some(l) => List.length(l) > 0
          | None => false
          },
        );
      },
    ),
    test_case(
      "unchanged inputs reuse the assumption set",
      `Quick,
      () => {
        let m1 = settled();
        let m2 = calculate(m1);
        check(
          bool,
          "the same list is kept",
          true,
          same(assumptions(m1), assumptions(m2)),
        );
      },
    ),
    /* The assumptions are derived from the semantic context, so a new ctx has to
       reach them -- otherwise the sidebar keeps showing assumptions from a scope
       the stepper has left. */
    test_case(
      "a new ctx rebuilds the assumption set",
      `Quick,
      () => {
        let m1 = settled();
        let m2 = calculate(~ctx=Calc.NewValue(sem_ctx), m1);
        check(
          bool,
          "the list was rebuilt",
          false,
          same(assumptions(m1), assumptions(m2)),
        );
      },
    ),
  ],
);

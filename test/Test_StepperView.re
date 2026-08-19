open Alcotest;
open Web;

module Calc = Util.Calc;

/* `StepperView.Update.calculate` caches one genuinely expensive step in the
 * model: substituting the builtin environment into the elaborated program and
 * re-issuing every id.
 *
 *   let elab_subst =
 *     cached_elab_subst |> { let.calc elab = elab; ... Substitution.in_exp ... };
 *
 * That is the `Calc` contract applied in a real component, and the failure is
 * silent either way: recompute every frame and stepping through a large program
 * crawls, cache too eagerly and the stepper steps through a stale program.
 * `Exp.replace_all_ids` makes it observable -- a recomputation necessarily
 * produces a different allocation, so physical equality distinguishes the two
 * paths here (unlike in CodeWithStatics, where the layer below memoises). */

let settings = Calc.OldValue(Settings.Model.init.core);

let ctx =
  Calc.OldValue(
    Language.SemanticCtx.of_ctx_and_env(
      Language.Builtins.ctx_init(None),
      Language.Builtins.closure_env,
    ),
  );

let elab_of_text = (text: string): Language.Exp.t =>
  switch (Haz3lcore.Parser.to_zipper(~root=Haz3lcore.Sort.Exp, text)) {
  | None => failwith("could not parse: " ++ text)
  | Some(z) =>
    let model =
      Haz3lcore.Editor.Model.mk(z, ~root=Haz3lcore.Sort.Exp)
      |> CodeWithStatics.Model.mk;
    CodeWithStatics.Update.calculate(
      ~settings=Settings.Model.init.core,
      ~is_edited=true,
      ~stitch=x => x,
      ~dynamics=model.dynamics,
      ~is_dynamic_term=false,
      model,
    ).
      statics.
      elaborated;
  };

let calculate = (elab, model) =>
  StepperView.Update.calculate(~settings, ~ctx, elab, model);

let cached = (model: StepperView.Model.t) =>
  Calc.get_saved_opt(model.cached_elab_subst);

let same = (a, b) =>
  switch (a, b) {
  | (Some(x), Some(y)) => x === y
  | _ => false
  };

let elab = elab_of_text("let f = fun x -> x + 1 in f(2)");

let tests = (
  "StepperView",
  [
    test_case(
      "a new elaboration is substituted and cached",
      `Quick,
      () => {
        let model = calculate(Calc.NewValue(elab), StepperView.Model.init);
        check(bool, "something was cached", true, cached(model) != None);
      },
    ),
    /* The point of the cache: an unchanged elaboration must not pay for the
       substitution again. */
    test_case(
      "an old elaboration reuses the cached substitution",
      `Quick,
      () => {
        let m1 = calculate(Calc.NewValue(elab), StepperView.Model.init);
        let m2 = calculate(Calc.OldValue(elab), m1);
        check(
          bool,
          "the same allocation is kept",
          true,
          same(cached(m1), cached(m2)),
        );
      },
    ),
    /* And the other direction: a new elaboration must not serve the cache, or
       the stepper walks a program the user has already edited away. */
    test_case(
      "a new elaboration invalidates the cache",
      `Quick,
      () => {
        let m1 = calculate(Calc.NewValue(elab), StepperView.Model.init);
        let m2 = calculate(Calc.NewValue(elab), m1);
        check(
          bool,
          "the substitution was redone",
          false,
          same(cached(m1), cached(m2)),
        );
      },
    ),
  ],
);

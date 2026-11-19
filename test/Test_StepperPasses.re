open Alcotest;
open Haz3lcore;

/* A pass over a stepper (every click makes one) re-analyzes only the steps
   that changed: the first step's analysis survives a second step and a
   selection */

let elab = (s: string): Language.Exp.t =>
  switch (Parser.to_term(s, ~root=Language.Sort.Exp)) {
  | Some(u) =>
    let (_, elab) =
      Language.Statics.mk(
        Language.CoreSettings.on,
        Language.Builtins.ctx_init(Some(Int)),
        u,
      );
    elab;
  | None => Alcotest.fail("could not parse: " ++ s)
  };

let ctx =
  Language.SemanticCtx.of_ctx_and_env(
    Language.Builtins.ctx_init(None),
    Language.Builtins.closure_env,
  );

/* the analysis itself: a pass rebuilds the record around it (targets) */
let first_info_map = (m: Web.StepperView.Model.t) =>
  Util.Calc.get_saved_exc(~print="first step", m.root.editor).statics.
    info_map;

let passes = () => {
  let e = elab("(1 + 2) + (3 + 4)");
  let calc = (exp, m) =>
    Web.StepperView.Update.calculate(
      ~settings=Util.Calc.OldValue(Language.CoreSettings.on),
      ~ctx=Util.Calc.OldValue(ctx),
      exp,
      m,
    );
  let step = (a, m) =>
    calc(
      Util.Calc.OldValue(e),
      Web.StepperView.Update.update(~settings=Web.Settings.Model.init, a, m).
        model,
    );
  /* the first steps are taken automatically (hidden), so a click lands at
     the end of the trace */
  let take = (m: Web.StepperView.Model.t) => {
    let rec at_end =
            (s: Web.StepperBase.step_model): Web.StepperBase.step_action =>
      switch (s.next_step) {
      | Some(n) => NextStep(at_end(n))
      | None => StepForward(0)
      };
    step(at_end(m.root), m);
  };
  let one = take(calc(Util.Calc.NewValue(e), Web.StepperView.Model.init));
  let before = first_info_map(one);
  let again = step(EditorAction(Select(All)), take(one));
  check(
    bool,
    "the first step's analysis kept",
    true,
    before === first_info_map(again),
  );
};

let tests = (
  "StepperPasses",
  [test_case("passes are incremental", `Quick, passes)],
);

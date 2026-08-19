open Alcotest;
open Web;

module Calc = Util.Calc;

/* `Theorems.Update.calculate` rebuilds one stepper per theorem in the program,
 * and caches almost everything: the theorem id list, and per theorem its ctx,
 * env, semantic ctx and goal expression. Each is guarded by `Calc`, and each
 * guard is the difference between re-elaborating every proof on every frame and
 * showing the user a proof of the wrong goal.
 *
 * Real theorems are needed to see any of it -- with no theorems every path
 * returns the empty list and the cached and recomputed answers are
 * indistinguishable -- so these tests evaluate an actual `theorem` program and
 * feed the resulting `Dynamics.t` in. */

let base = Settings.Model.init.core;
let core = {
  ...base,
  evaluation: {
    ...base.evaluation,
    enable_proof: true,
    stepper_history: true,
  },
};

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

let dynamics_of = (statics): option(Language.Dynamics.t) => {
  let m =
    EvalResult.Update.calculate(
      ~settings=core,
      ~queue_worker=None,
      ~is_edited=true,
      statics,
      EvalResult.Model.init,
    );
  Calc.get_saved(None, m.dynamics);
};

let program = {|theorem t = true in 0|};

let statics = statics_of_text(program);
let dyn = dynamics_of(statics);

let calculate = (~dynamics, model) =>
  Theorems.Update.calculate(
    ~settings=Calc.OldValue(core),
    ~statics=Calc.OldValue(statics),
    ~dynamics,
    model,
  );

let thms = (m: Theorems.Model.t) => Calc.get_saved_opt(m.thms);

let thm_count = (m: Theorems.Model.t) =>
  switch (thms(m)) {
  | Some(ids) => List.length(ids)
  | None => (-1)
  };

let same_thms = (a, b) =>
  switch (thms(a), thms(b)) {
  | (Some(x), Some(y)) => x === y
  | _ => false
  };

/* The goal expression of the single theorem, as cached in its model. */
let goal = (m: Theorems.Model.t) =>
  switch (Haz3lcore.Id.Map.bindings(m.thm_map)) {
  | [(_, thm)] => Calc.get_saved_opt(thm.goal_exp)
  | _ => None
  };

let tests = (
  "Theorems",
  [
    /* The fixture guard: if a `theorem` program ever stops reaching
       `Dynamics.theorems`, every test below would pass vacuously. */
    test_case(
      "the program contributes one theorem",
      `Quick,
      () => {
        let n =
          switch (dyn) {
          | None => (-1)
          | Some(d) => List.length(d.theorems)
          };
        check(int, "theorems in dynamics", 1, n);
      },
    ),
    test_case(
      "a new dynamics builds the theorem list",
      `Quick,
      () => {
        let m = calculate(~dynamics=Calc.NewValue(dyn), Theorems.Model.init);
        check(int, "theorem ids", 1, thm_count(m));
        check(bool, "the goal was cached", true, goal(m) != None);
      },
    ),
    /* The cache: an unchanged dynamics must not rebuild the list. */
    test_case(
      "an old dynamics reuses the theorem list",
      `Quick,
      () => {
        let m1 =
          calculate(~dynamics=Calc.NewValue(dyn), Theorems.Model.init);
        let m2 = calculate(~dynamics=Calc.OldValue(dyn), m1);
        check(bool, "the same list is kept", true, same_thms(m1, m2));
      },
    ),
    /* `Calc.old_if_same'` means a rebuild that produces an equal list still
       reports itself old, so downstream steppers do not rerun. The list is
       rebuilt (a fresh allocation) but must stay equal. */
    test_case(
      "a rebuild that changes nothing keeps the same ids",
      `Quick,
      () => {
        let m1 =
          calculate(~dynamics=Calc.NewValue(dyn), Theorems.Model.init);
        let m2 = calculate(~dynamics=Calc.NewValue(dyn), m1);
        check(int, "still one theorem", 1, thm_count(m2));
        check(bool, "ids are equal", true, thms(m1) == thms(m2));
      },
    ),
    /* No theorems means no steppers, and the model must not keep stale ones. */
    test_case(
      "dynamics without theorems yields no theorem ids",
      `Quick,
      () => {
        let m =
          calculate(~dynamics=Calc.NewValue(None), Theorems.Model.init);
        check(int, "theorem ids", 0, thm_count(m));
      },
    ),
  ],
);

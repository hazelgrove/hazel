/* Regression: a reused cache entry must carry its own environment.
 *
 * `Transition`'s `Closure` rule evaluates subterms with `~in_closure`, which
 * suppresses the `Closure` wrapper for a function value nested under a
 * `Closure` -- correct in place, because the enclosing `Closure` supplies the
 * environment. A cache entry is keyed by id alone and is replayed at top level,
 * where that enclosing `Closure` is gone, so applying the replayed function
 * lands in `Transition`'s `| FunNoEnv(_) => Indet`: the application becomes
 * final-but-stuck and never reduces again.
 *
 * Reaching it needs the EDITOR, not a bare evaluation. The cached ids have to
 * survive into the next program, which only happens when the same document is
 * edited in place -- two separate parses of the same text mint disjoint ids and
 * nothing is reused. Plain typing is enough; no projector is involved.
 *
 * Three ingredients, all of which have to have happened by the time the second
 * program is evaluated:
 *   1. `inner` appears as a DEFERRED application in the result, which is what
 *      gets its value recorded without a `Closure`;
 *   2. `outer` has been APPLIED at some point, so the `inner` occurrence inside
 *      its body was reached and cached;
 *   3. the second program applies `outer` again, reusing that entry.
 *
 * Dropping any one of the three from THIS test makes it evaluate fine. In a live
 * session they are not independent, and that is why the bug looks
 * nondeterministic there: `prev` is a function of the whole editing history, not
 * of the current text. Ingredient 2 can be supplied by any earlier state -- a
 * `test` block that has since been deleted, or just the intermediate text an
 * edit passes through -- and deleting that state does NOT clear the entry it
 * recorded. Only a reload does, since `prev` starts empty. So the same final
 * program reproduces or not depending on how it was arrived at. The `test` block
 * below pins ingredient 2 down so this test does not depend on history.
 */
open Web;
open Haz3lcore;
open Language;

/* The app's own defaults, not a hand-built CoreSettings. */
let settings = Settings.Model.init;

/* One frame of the editor's real update loop, exactly as ScratchMode runs it.
   With `~queue_worker=None`, `EvalResult.Update.calculate` builds the request
   itself and evaluates through `WorkerServer.evaluate_sync`, then stores
   `incr_eval` in the model -- so the request construction, the choice of
   `EvalInfo.of_info_map`, and the threading of `prev` across edits are all
   production code here, not restated by the test. That matters: the reason
   this bug went unnoticed is that a harness which builds its own request with
   `EvalInfo.of_targets` leaves `reuse_check`'s `EvalInfo.find_opt` empty and
   never reuses anything, so it passes no matter what `reuse_check` does. */
let calculate = (~is_edited, model) =>
  CellEditor.Update.calculate(
    ~settings=settings.core,
    ~is_edited,
    ~queue_worker=None,
    ~stitch=x => x,
    model,
  );

let of_text = (text: string): CellEditor.Model.t =>
  switch (Parser.to_zipper(~root=Sort.Exp, text)) {
  | None => failwith("could not parse")
  | Some(z) =>
    Editor.Model.mk(z, ~root=Sort.Exp)
    |> CellEditor.Model.mk
    |> calculate(~is_edited=true)
  };

/* An editor action, dispatched and recalculated the way the app does. */
let perform = (a: Action.t, model: CellEditor.Model.t) =>
  CellEditor.Update.update(~settings, MainEditor(Perform(a)), model).model
  |> calculate(~is_edited=true);

let type_at_end = (text, model) =>
  String.fold_left(
    (m, c) => perform(Insert(String.make(1, c)), m),
    perform(Move(End), model),
    text,
  );

let value = (model: CellEditor.Model.t): option(Exp.t) =>
  switch (Util.Calc.get_value(model.result.result)) {
  | ProgramResult.ResultOk({result, _}) => Some(result)
  | _ => None
  };

let program = {|let inner = fun (node, a) -> node in
let outer = fun (node, b) -> inner(node, b) in
let mk = fun n -> (n, inner(_, 0)) in
test let (x, _) = mk(0) |> outer(_, 8) in x == 0 end;
mk(0)|};

let typed = {| |> outer(_, 1)|};

let test = () => {
  let model = of_text(program) |> type_at_end(typed);
  /* Without the guard in `reuse_check` this is
     `Ap(Forward, Fun((node, a), ...), ...)` -- `inner` applied to a final
     pair, stuck forever -- instead of the pair `mk(0)` returns. */
  let stuck =
    switch (value(model)) {
    | None => false
    | Some(v) =>
      switch (Exp.term_of(v)) {
      | Ap(_, fn, _) =>
        switch (Exp.term_of(fn)) {
        | Fun(_) => true
        | _ => false
        }
      | _ => false
      }
    };
  switch (value(model)) {
  | Some(v) when stuck => print_endline("stuck result: " ++ Exp.show(v))
  | _ => ()
  };
  Alcotest.check(
    Alcotest.bool,
    "the typed application reduces",
    false,
    stuck,
  );
};

let tests = (
  "CellEditor",
  [
    Alcotest.test_case(
      "reuse keeps a function value's environment across an edit",
      `Quick,
      test,
    ),
  ],
);

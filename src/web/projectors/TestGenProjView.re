open Virtual_dom.Vdom;
open Node;
open Haz3lcore;
open ProjectorBase;
open ProjectorViewBase;

/* Test input generation refractor (web view half).
 *
 * Like the Probe and Statics refractors, this is additive: the underlying
 * boolean expression stays visible and editable, and the decoration lives in
 * an offside at the end of the line. It builds the SMT-LIB2 (TestGen.build),
 * runs the z3-solver WASM backend asynchronously (Z3Wasm.solve), and stores
 * the result. Solving is automatic: a debounce (SolveDebounce) re-solves a
 * short while after the underlying expression last changed, so there is no
 * manual "generate" button. */

let assignment_str = (a: TestGen.assignment): string =>
  a.name ++ " = " ++ a.value;

let result_view = (outcome: TestGen.outcome): Node.t =>
  switch (outcome) {
  | Sat([]) =>
    span(
      ~attrs=[Attr.classes(["testgen-result", "sat"])],
      [text("∃ ✓")],
    )
  | Sat(assignments) =>
    span(
      ~attrs=[Attr.classes(["testgen-result", "sat"])],
      [
        text(
          "∃ "
          ++ String.concat(", ", List.map(assignment_str, assignments)),
        ),
      ],
    )
  | Unsat =>
    span(
      ~attrs=[Attr.classes(["testgen-result", "unsat"])],
      [text("no inputs (unsat)")],
    )
  | Unknown =>
    span(
      ~attrs=[Attr.classes(["testgen-result", "unknown"])],
      [text("unknown")],
    )
  | Error(msg) =>
    span(~attrs=[Attr.classes(["testgen-result", "error"])], [text(msg)])
  };

module V: ProjectorView = {
  module L = TestGenProj.M;

  let focusable = Focusable.non;

  /* One debouncer for all TestGen points (keyed by id inside). */
  let debounce = SolveDebounce.make();

  let view =
      ({model, info, local_transient, _}: View.args(L.model, L.action)) => {
    /* Build the SMT-LIB2 for the attached boolean expression, then solve it
       automatically (debounced so edits settle first) and store the outcome.
       Results are stored transiently so they stay off the undo history. When
       statics aren't available yet we simply don't solve — a later frame, once
       statics arrive, re-triggers via the changed signature. */
    switch (info.statics) {
    | Some(InfoExp(e)) =>
      let built = TestGen.build(e);
      let run = () =>
        switch (built) {
        | Error(msg) =>
          Bonsai.Effect.Expert.handle(
            local_transient(TestGenProj.SetResult(TestGen.Error(msg))),
          )
        | Ok(script) =>
          Z3Wasm.solve(
            ~k=
              outcome =>
                Bonsai.Effect.Expert.handle(
                  local_transient(TestGenProj.SetResult(outcome)),
                ),
            script,
          )
        };
      SolveDebounce.tick(
        debounce,
        ~id=info.id,
        ~sig_=
          switch (built) {
          | Ok(script) => script
          | Error(msg) => "err:" ++ msg
          },
        ~run,
      );
    | _ => ()
    };
    View.{
      inline: div([]),
      offside:
        Some(
          div(
            ~attrs=[
              Attr.id(Id.cls(info.id)),
              Attr.tabindex(0),
              Attr.classes(["offside", "testgen-offside"]),
            ],
            switch (model.result) {
            | None => []
            | Some(outcome) => [result_view(outcome)]
            },
          ),
        ),
      overlay: None,
      error: false,
    };
  };
};

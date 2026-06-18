open Virtual_dom.Vdom;
open Node;
open Haz3lcore;
open ProjectorBase;
open ProjectorViewBase;

/* Test input generation refractor (web view half).
 *
 * Like the Probe and Statics refractors, this is additive: the underlying
 * boolean expression stays visible and editable, and the decoration lives in
 * an offside at the end of the line. It shows a "generate" button; on click
 * it builds the SMT-LIB2 (TestGen.build), runs the z3-solver WASM backend
 * asynchronously (Z3Wasm.solve), and dispatches SetResult when the solver
 * resolves. */

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

  let view = ({model, info, local, _}: View.args(L.model, L.action)) => {
    let on_generate = _ =>
      switch (info.statics) {
      | Some(InfoExp(e)) =>
        switch (TestGen.build(e)) {
        | Error(msg) => local(TestGenProj.SetResult(TestGen.Error(msg)))
        | Ok(script) =>
          Z3Wasm.solve(
            ~k=
              outcome =>
                Bonsai.Effect.Expert.handle(
                  local(TestGenProj.SetResult(outcome)),
                ),
            script,
          );
          Effect.Ignore;
        }
      | _ =>
        local(
          TestGenProj.SetResult(
            TestGen.Error("Statics unavailable; enable statics to generate."),
          ),
        )
      };
    let generate_btn =
      span(
        ~attrs=[
          Attr.classes(["testgen-generate"]),
          Attr.title("Generate test inputs"),
          Attr.on_click(on_generate),
        ],
        [text("⚗")],
      );
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
            [generate_btn]
            @ (
              switch (model.result) {
              | None => []
              | Some(outcome) => [result_view(outcome)]
              }
            ),
          ),
        ),
      overlay: None,
      error: false,
    };
  };
};

open Virtual_dom.Vdom;
open Node;
open Haz3lcore;
open ProjectorBase;
open ProjectorViewBase;

/* Reach refractor (web view half).
 *
 * Additive (like Probe/Statics/TestGen): the underlying expression stays
 * visible. The offside shows the symbolic path condition and a button that
 * solves it — reporting an input that reaches this point, or "unreachable —
 * dead code". The path condition (info.reach) is precomputed in mk_info; the
 * solve runs the z3-solver WASM backend asynchronously on click. */

let assignment_str = (a: TestGen.assignment): string =>
  a.name ++ " = " ++ a.value;

let result_view = (~grouped: bool, outcome: TestGen.outcome): Node.t =>
  switch (outcome) {
  | Sat([]) =>
    span(
      ~attrs=[Attr.classes(["reach-result", "reachable"])],
      [
        text(grouped ? "all reachable (any input)" : "reachable (any input)"),
      ],
    )
  | Sat(assignments) =>
    span(
      ~attrs=[Attr.classes(["reach-result", "reachable"])],
      [
        text(
          (grouped ? "all reached when " : "reached when ")
          ++ String.concat(", ", List.map(assignment_str, assignments)),
        ),
      ],
    )
  | Unsat =>
    span(
      ~attrs=[Attr.classes(["reach-result", "dead"])],
      [text(grouped ? "incompatible" : "unreachable — dead code")],
    )
  | Unknown =>
    span(
      ~attrs=[Attr.classes(["reach-result", "unknown"])],
      [text("unknown")],
    )
  | Error(msg) =>
    span(~attrs=[Attr.classes(["reach-result", "error"])], [text(msg)])
  };

let group_chip = (local, group: int): Node.t =>
  span(
    ~attrs=[
      Attr.classes(["reach-group", "g" ++ string_of_int(group)]),
      Attr.title("Merge group — click to cycle (• = solo)"),
      Attr.on_click(_ => local(ReachProj.CycleGroup)),
    ],
    [text(group == 0 ? {js|•|js} : string_of_int(group))],
  );

/* Render the symbolic path condition (the conjoined guards) as Hazel text. */
let path_view = (utility: ProjectorBase.utility, r: Reach.t): list(Node.t) => {
  let render = (g: Language.Exp.t): string =>
    Exp(g) |> utility.term_to_seg(~inline=true) |> utility.seg_to_string;
  switch (r.guards) {
  | [] => []
  | guards => [
      span(
        ~attrs=[Attr.classes(["reach-path"])],
        [text(String.concat(" ∧ ", List.map(render, guards)))],
      ),
    ]
  };
};

module V: ProjectorView = {
  module L = ReachProj.M;

  let focusable = Focusable.non;

  let view = ({model, info, local, _}: View.args(L.model, L.action)) => {
    let on_generate = _ =>
      switch (info.reach) {
      | None =>
        local(
          ReachProj.SetResult(
            TestGen.Error("Enable statics to analyze reachability."),
          ),
        )
      | Some(r) =>
        let (script, complete) = Reach.smtlib2(r);
        Z3Wasm.solve(
          ~k=
            outcome =>
              Bonsai.Effect.Expert.handle(
                local(
                  ReachProj.SetResult(
                    Reach.interpret(~complete, ~inputs=r.inputs, outcome),
                  ),
                ),
              ),
          script,
        );
        Effect.Ignore;
      };
    let generate_btn =
      span(
        ~attrs=[
          Attr.classes(["reach-generate"]),
          Attr.title("Find reaching inputs"),
          Attr.on_click(on_generate),
        ],
        [text({js|🎯|js})],
      );
    View.{
      inline: div([]),
      offside:
        Some(
          div(
            ~attrs=[
              Attr.id(Id.cls(info.id)),
              Attr.tabindex(0),
              Attr.classes(["offside", "reach-offside"]),
            ],
            [group_chip(local, model.group), generate_btn]
            @ (
              switch (info.reach) {
              | Some(r) => path_view(info.utility, r)
              | None => []
              }
            )
            @ (
              switch (model.result) {
              | None => []
              | Some(o) => [result_view(~grouped=model.group != 0, o)]
              }
            ),
          ),
        ),
      overlay: None,
      error: false,
    };
  };
};

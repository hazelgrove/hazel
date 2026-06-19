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

/* Dynamic, palette-free color per group: a distinct hue via the golden angle.
 * Solo (0) is a neutral gray. */
let group_color = (group: int): string =>
  group == 0
    ? "hsl(0, 0%, 72%)"
    : Printf.sprintf("hsl(%d, 62%%, 50%%)", group * 137 mod 360);

/* The chip cycles through solo, the groups currently in use, and one fresh
 * group — so it doesn't walk a fixed palette when only a few groups exist.
 * group_count = distinct groups in use; the next id wraps at group_count + 2
 * (solo + the in-use groups + one new), which stays bounded as groups are
 * relabeled. */
let next_group = (~group_count: int, group: int): int =>
  (group + 1) mod (group_count + 2);

/* Tie a node to its merge group by setting its text color; the path/result
 * pills derive their tinted background and border from this via `currentColor`
 * in CSS. Solo (0) → no inline color, i.e. neutral theme defaults. */
let group_text_attrs = (g: int): list(Attr.t) =>
  g == 0 ? [] : [Attr.create("style", "color: " ++ group_color(g))];

/* `multiline` renders the assignments as an aligned `var = value` table — used
 * by the Reach sidebar, which has vertical room and scrolls; the offside keeps
 * the compact single-line pill (multiline=false). Color follows the merge
 * group (`group`); solo (0) stays neutral. Errors are always red. */
let result_view =
    (~group: int, ~multiline=false, outcome: TestGen.outcome): Node.t => {
  let grouped = group != 0;
  let accent = group_text_attrs(group);
  switch (outcome) {
  | Sat([]) =>
    span(
      ~attrs=[Attr.classes(["reach-result", "reachable"]), ...accent],
      [
        text(grouped ? "all reachable (any input)" : "reachable (any input)"),
      ],
    )
  | Sat(assignments) when multiline =>
    div(
      ~attrs=[
        Attr.classes(["reach-result", "reachable", "reach-result-list"]),
        ...accent,
      ],
      [
        span(
          ~attrs=[Attr.classes(["reach-result-label"])],
          [text(grouped ? "all reached when" : "reached when")],
        ),
        Node.table(
          ~attrs=[Attr.classes(["reach-assignments"])],
          List.map(
            (a: TestGen.assignment) =>
              Node.tr([
                Node.td(
                  ~attrs=[Attr.classes(["reach-var"])],
                  [text(a.name)],
                ),
                Node.td(~attrs=[Attr.classes(["reach-eq"])], [text("=")]),
                Node.td(
                  ~attrs=[Attr.classes(["reach-val"])],
                  [text(a.value)],
                ),
              ]),
            assignments,
          ),
        ),
      ],
    )
  | Sat(assignments) =>
    span(
      ~attrs=[Attr.classes(["reach-result", "reachable"]), ...accent],
      [
        text(
          (grouped ? "all reached when " : "reached when ")
          ++ String.concat(", ", List.map(assignment_str, assignments)),
        ),
      ],
    )
  | Unsat =>
    span(
      ~attrs=[Attr.classes(["reach-result", "dead"]), ...accent],
      [text(grouped ? "incompatible" : "unreachable — dead code")],
    )
  | Unknown =>
    span(
      ~attrs=[Attr.classes(["reach-result", "unknown"]), ...accent],
      [text("unknown")],
    )
  | Error(msg) =>
    span(~attrs=[Attr.classes(["reach-result", "error"])], [text(msg)])
  };
};

let group_chip = (local, ~group_count: int, group: int): Node.t =>
  span(
    ~attrs=[
      Attr.classes(["reach-group"]),
      Attr.create("style", "background-color: " ++ group_color(group)),
      Attr.title("Merge group — click to cycle (• = solo)"),
      Attr.on_click(_ =>
        local(ReachProj.SetGroup(next_group(~group_count, group)))
      ),
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
      /* Disabled points are excluded from solving (and from group merges); the
         enable toggle lives in the Reach sidebar. */
      !model.enabled
        ? Effect.Ignore
        : (
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
          }
        );
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
              Attr.classes(
                ["offside", "reach-offside"]
                @ (model.enabled ? [] : ["disabled"]),
              ),
            ],
            [
              group_chip(
                local,
                ~group_count=info.reach_group_count,
                model.group,
              ),
              generate_btn,
            ]
            @ (
              switch (info.reach) {
              /* Color the path condition by the merge group (neutral if solo). */
              | Some(r) => [
                  span(
                    ~attrs=group_text_attrs(model.group),
                    path_view(info.utility, r),
                  ),
                ]
              | None => []
              }
            )
            @ (
              switch (model.result) {
              | None => []
              | Some(o) => [result_view(~group=model.group, o)]
              }
            ),
          ),
        ),
      overlay: None,
      error: false,
    };
  };
};

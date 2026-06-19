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

/* Smallest positive group id not already in `groups` — the group the offside
 * "+" chip adds (so repeated "+" builds up 1, 2, 3, …, and different points
 * land in the same low-numbered groups, ready to be merged). */
let next_free_group = (groups: list(int)): int => {
  let rec find = n => List.mem(n, groups) ? find(n + 1) : n;
  find(1);
};

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
    /* The offside solves the point on its own (solo, group 0); group merges
       live in the Reach sidebar, which can see every point. */
    let on_generate = _ =>
      !model.enabled
        ? Effect.Ignore
        : (
          switch (info.reach) {
          | None =>
            local(
              ReachProj.SetResult(
                0,
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
                        0,
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
    /* Edit group membership right on the point: one chip per group (click to
       leave it), then a "+" chip to add it to the next group. Naming/solving
       groups happens in the Reach sidebar. */
    let chip = (~title, ~on_click, ~color, label) =>
      span(
        ~attrs=[
          Attr.classes(["reach-group-chip"]),
          Attr.create("style", "background-color: " ++ color),
          Attr.title(title),
          Attr.on_click(on_click),
        ],
        [text(label)],
      );
    let group_chips =
      List.map(
        g =>
          chip(
            ~title="In group " ++ string_of_int(g) ++ " — click to remove",
            ~on_click=_ => local(ReachProj.ToggleGroup(g)),
            ~color=group_color(g),
            string_of_int(g),
          ),
        model.groups,
      )
      @ [
        chip(
          ~title="Add to a group",
          ~on_click=
            _ =>
              local(ReachProj.ToggleGroup(next_free_group(model.groups))),
          ~color="var(--BR1)",
          {js|+|js},
        ),
      ];
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
            group_chips
            @ [generate_btn]
            @ (
              switch (info.reach) {
              | Some(r) => path_view(info.utility, r)
              | None => []
              }
            )
            @ (
              switch (List.assoc_opt(0, model.results)) {
              | None => []
              | Some(o) => [result_view(~group=0, o)]
              }
            ),
          ),
        ),
      overlay: None,
      error: false,
    };
  };
};

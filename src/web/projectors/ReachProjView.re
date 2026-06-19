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
      [text(String.concat(", ", List.map(assignment_str, assignments)))],
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

/* Debounced auto-solve: each reach point re-renders fresh every frame (Reach
 * bypasses the projector view cache), so the offside can watch its own
 * condition signature and re-solve a short while after it last changed. State
 * is keyed by reach-point id. Because identical SMT scripts are cached
 * (Z3Wasm), every member of a merge group reuses one group solve, so a group's
 * solution lands on all of its members. */
module ReachAuto = {
  let debounce_ms = 400.0;
  let timers: Hashtbl.t(Id.t, Js_of_ocaml.Dom_html.timeout_id) =
    Hashtbl.create(64);
  let last_sigs: Hashtbl.t(Id.t, string) = Hashtbl.create(64);

  /* Re-run `run` ~debounce_ms after this point's condition signature settles. */
  let tick = (~id: Id.t, ~sig_: string, ~run: unit => unit): unit =>
    if (Hashtbl.find_opt(last_sigs, id) != Some(sig_)) {
      Hashtbl.replace(last_sigs, id, sig_);
      switch (Hashtbl.find_opt(timers, id)) {
      | Some(t) => Js_of_ocaml.Dom_html.window##clearTimeout(t)
      | None => ()
      };
      let t =
        Js_of_ocaml.Dom_html.window##setTimeout(
          Js_of_ocaml.Js.wrap_callback(() => {
            Hashtbl.remove(timers, id);
            run();
          }),
          debounce_ms,
        );
      Hashtbl.replace(timers, id, t);
    };
};

module V: ProjectorView = {
  module L = ReachProj.M;

  let focusable = Focusable.non;

  let view =
      (
        {model, info, local, local_transient, _}:
          View.args(L.model, L.action),
      ) => {
    /* Solving a reach point solves it on its own (group 0) AND every group it
       belongs to (info.reach_group_conds carries each group's merged
       condition). The outcomes are gathered and stored in one SetResults so
       concurrent solves can't clobber each other; they show in each group's
       color (offside and sidebar). */
    let jobs: list((int, Reach.t)) =
      model.enabled
        ? (
            switch (info.reach) {
            | Some(r) => [(0, r)]
            | None => []
            }
          )
          @ info.reach_group_conds
        : [];
    /* Fire-and-forget: solve every job, then store all outcomes at once. The
       automatic path drops Error outcomes so a transient failure (e.g. the
       solver not yet loaded) never auto-fills error pills; the manual button
       reports everything. */
    let run_solve = (~auto: bool) => {
      let total = List.length(jobs);
      let outs = ref([]);
      let done_ = ref(0);
      List.iter(
        ((g, cond: Reach.t)) =>
          Z3Wasm.solve(
            ~k=
              outcome => {
                let o =
                  Reach.interpret(
                    ~complete=cond.complete,
                    ~inputs=cond.inputs,
                    outcome,
                  );
                outs := [(g, o), ...outs^];
                incr(done_);
                if (done_^ >= total) {
                  let results =
                    auto
                      ? List.filter(
                          ((_, o)) =>
                            switch (o) {
                            | TestGen.Error(_) => false
                            | _ => true
                            },
                          outs^,
                        )
                      : outs^;
                  switch (results) {
                  | [] => ()
                  | _ =>
                    Bonsai.Effect.Expert.handle(
                      local_transient(ReachProj.SetResults(results)),
                    )
                  };
                };
              },
            Reach.smtlib2(cond) |> fst,
          ),
        jobs,
      );
    };
    /* Re-solve automatically (debounced) whenever the conditions change. The
       signature is built from the conditions only — never the results — so
       storing results doesn't retrigger it. */
    ReachAuto.tick(
      ~id=info.id,
      ~sig_=
        String.concat(
          ";",
          List.map(
            ((g, cond)) =>
              string_of_int(g) ++ ":" ++ (Reach.smtlib2(cond) |> fst),
            jobs,
          ),
        ),
      ~run=() =>
      run_solve(~auto=true)
    );
    let on_generate = _ =>
      switch (jobs) {
      | _ when !model.enabled => Effect.Ignore
      | [] =>
        local_transient(
          ReachProj.SetResult(
            0,
            TestGen.Error("Enable statics to analyze reachability."),
          ),
        )
      | _ =>
        run_solve(~auto=false);
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
    /* Edit group membership right on the point. The point's groups show as
       solid chips; hovering the area expands it (briefly animated) to reveal
       every other group as a dimmed chip you can toggle on. The "+" chip always
       shows and creates a brand-new group. Naming/solving is in the sidebar. */
    let chip = (~classes, ~title, ~on_click, ~color, label) =>
      span(
        ~attrs=[
          Attr.classes(["reach-group-chip", ...classes]),
          Attr.create("style", "background-color: " ++ color),
          Attr.title(title),
          Attr.on_click(on_click),
        ],
        [text(label)],
      );
    let all_groups =
      List.sort_uniq(compare, model.groups @ info.reach_groups);
    let toggle_chip = g => {
      let selected = List.mem(g, model.groups);
      chip(
        ~classes=[selected ? "selected" : "unselected"],
        ~title=
          (selected ? "In group " : "Add to group ")
          ++ string_of_int(g)
          ++ (selected ? " — click to remove" : ""),
        ~on_click=_ => local(ReachProj.ToggleGroup(g)),
        ~color=group_color(g),
        string_of_int(g),
      );
    };
    let new_chip =
      chip(
        ~classes=["reach-group-new"],
        ~title="Create a new group",
        ~on_click=
          _ => local(ReachProj.ToggleGroup(next_free_group(all_groups))),
        ~color="var(--BR1)",
        {js|+|js},
      );
    let groups_edit =
      span(
        ~attrs=[Attr.classes(["reach-groups-edit"])],
        List.map(toggle_chip, all_groups) @ [new_chip],
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
            [groups_edit, generate_btn]
            @ (
              switch (info.reach) {
              | Some(r) => path_view(info.utility, r)
              | None => []
              }
            )
            @ (
              /* Each group's solution colored by its group, then the neutral
                 solo (no-group, key 0) solution last. */
              model.results
              |> List.sort(((a, _), (b, _)) => {
                   let key = g => g == 0 ? max_int : g;
                   compare(key(a), key(b));
                 })
              |> List.map(((g, o)) => result_view(~group=g, o))
            ),
          ),
        ),
      overlay: None,
      error: false,
    };
  };
};

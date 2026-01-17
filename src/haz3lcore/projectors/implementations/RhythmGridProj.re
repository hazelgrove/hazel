open Util;
open Virtual_dom.Vdom;
open ProjectorBase;

/* A rhythm grid (step sequencer) projector for Strudel drum patterns.
 * Shows a grid where rows are drum sounds and columns are steps. */

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = {steps: int};
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    | SetSteps(int);

  /* Drum sound abbreviations and display names */
  let drums = [
    ("bd", "BD"),   /* Bass drum */
    ("sd", "SD"),   /* Snare */
    ("hh", "HH"),   /* Hi-hat */
    ("oh", "OH"),   /* Open hi-hat */
  ];

  let default_steps = 8;

  let string_of = (any: Language.Any.t): option(string) =>
    switch (any) {
    | Exp({term: Atom(String(s)), _}) => Some(s)
    | _ => None
    };

  let init = (any: Language.Any.t) =>
    switch (string_of(any)) {
    | Some(_) => Some({steps: default_steps})
    | None => None
    };

  let get = (info: info): string =>
    switch (
      info.syntax |> info.utility.seg_to_term |> OptUtil.and_then(string_of)
    ) {
    | Some(s) => s
    | None => ""
    };

  let put = (info: info, v: string): Base.segment =>
    switch (
      info.utility.lift_syntax(
        fun
        | Exp(t) =>
          Exp({
            ...t,
            term: Atom(String(v)),
          })
        | _ => failwith("RhythmGrid: Put: not string literal"),
        info.syntax,
      )
    ) {
    | Some(s) => s
    | None => failwith("RhythmGrid: Put: lift failed")
    };

  let focusable = Focusable.non;
  let dynamics = false;
  let placeholder = ({steps, _}, _) =>
    ProjectorCore.Shape.inline(steps * 2 + 4);
  let update = (_model, _, action) =>
    switch (action) {
    | SetSteps(n) => {steps: max(4, min(16, n))}
    };

  /* Parse pattern into a map of (drum -> list of step indices) */
  let parse_pattern = (pattern: string, steps: int): list((string, list(int))) => {
    /* Parse mini-notation: "bd ~ sd ~" means bd on step 0, sd on step 2 */
    let tokens =
      pattern
      |> String.split_on_char(' ')
      |> List.filter(s => String.length(s) > 0);
    let per_step = max(1, List.length(tokens)) / steps;
    List.map(
      ((drum, _)) => {
        let active_steps =
          List.mapi(
            (i, token) =>
              if (token == drum) {
                Some(i / max(1, per_step));
              } else {
                None;
              },
            tokens,
          )
          |> List.filter_map(x => x);
        (drum, active_steps);
      },
      drums,
    );
  };

  /* Generate pattern string from grid state */
  let generate_pattern = (grid: list((string, list(int))), steps: int): string => {
    let pattern = Array.make(steps, "~");
    List.iter(
      ((drum, active_steps)) =>
        List.iter(
          step =>
            if (step >= 0 && step < steps) {
              pattern[step] = drum;
            },
          active_steps,
        ),
      grid,
    );
    Array.to_list(pattern) |> String.concat(" ");
  };

  /* Toggle a step for a drum */
  let toggle_step =
      (pattern: string, drum: string, step: int, steps: int): string => {
    let grid = parse_pattern(pattern, steps);
    let new_grid =
      List.map(
        ((d, active)) =>
          if (d == drum) {
            if (List.mem(step, active)) {
              (d, List.filter(s => s != step, active));
            } else {
              (d, [step, ...active]);
            };
          } else {
            (d, active);
          },
        grid,
      );
    generate_pattern(new_grid, steps);
  };

  /* Check if a step is active for a drum */
  let is_active = (grid: list((string, list(int))), drum: string, step: int): bool =>
    switch (List.find_opt(((d, _)) => d == drum, grid)) {
    | Some((_, active)) => List.mem(step, active)
    | None => false
    };

  /* Create a grid cell */
  let grid_cell = (~pattern, ~parent, ~info, ~steps, drum, step, grid) => {
    let active = is_active(grid, drum, step);
    Node.div(
      ~attrs=[
        Attr.classes([
          "grid-cell",
          active ? "active" : "inactive",
          step mod 4 == 0 ? "beat-start" : "",
        ]),
        Attr.on_click(_ => {
          let new_pattern = toggle_step(pattern, drum, step, steps);
          parent(SetSyntax(put(info, new_pattern)));
        }),
      ],
      [],
    );
  };

  /* Create a row for a drum */
  let drum_row = (~pattern, ~parent, ~info, ~steps, drum, label, grid) =>
    Node.div(
      ~attrs=[Attr.classes(["grid-row"])],
      [
        Node.span(~attrs=[Attr.classes(["drum-label"])], [Node.text(label)]),
        Node.div(
          ~attrs=[Attr.classes(["grid-cells"])],
          List.init(steps, step =>
            grid_cell(~pattern, ~parent, ~info, ~steps, drum, step, grid)
          ),
        ),
      ],
    );

  let view = ({model: {steps}, info, parent, local, _}: View.args(model, action)) => {
    let pattern = get(info);
    let grid = parse_pattern(pattern, steps);
    let rows =
      List.map(
        ((drum, label)) =>
          drum_row(~pattern, ~parent, ~info, ~steps, drum, label, grid),
        drums,
      );
    View.mk(
      Node.div(
        ~attrs=[Attr.classes(["rhythm-grid"])],
        [
          Node.div(
            ~attrs=[Attr.classes(["grid-controls"])],
            [
              Node.button(
                ~attrs=[
                  Attr.classes(["step-btn"]),
                  Attr.on_click(_ => local(SetSteps(steps - 1))),
                ],
                [Node.text("-")],
              ),
              Node.span(
                ~attrs=[Attr.classes(["step-count"])],
                [Node.text(string_of_int(steps))],
              ),
              Node.button(
                ~attrs=[
                  Attr.classes(["step-btn"]),
                  Attr.on_click(_ => local(SetSteps(steps + 1))),
                ],
                [Node.text("+")],
              ),
            ],
          ),
          Node.div(~attrs=[Attr.classes(["grid-rows"])], rows),
        ],
      ),
    );
  };
};

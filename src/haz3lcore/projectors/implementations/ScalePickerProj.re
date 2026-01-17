open Util;
open Virtual_dom.Vdom;
open ProjectorBase;

/* A scale picker projector for Strudel note patterns.
 * Shows preset scale patterns organized by type.
 *
 * Applicable to: Note(String) with simple note patterns
 * (space-separated notes without complex mini-notation)
 *
 * Provides preset scales in C4:
 * - Major, Minor, Dorian, Phrygian, Lydian, Mixolydian, Locrian
 * - Pentatonic Major/Minor, Blues */

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = unit;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;

  /* Scale presets - each is (name, pattern) */
  let scales = [
    ("Major", "c4 d4 e4 f4 g4 a4 b4"),
    ("Minor", "c4 d4 eb4 f4 g4 ab4 bb4"),
    ("Dorian", "c4 d4 eb4 f4 g4 a4 bb4"),
    ("Phrygian", "c4 db4 eb4 f4 g4 ab4 bb4"),
    ("Lydian", "c4 d4 e4 f#4 g4 a4 b4"),
    ("Mixolydian", "c4 d4 e4 f4 g4 a4 bb4"),
    ("Locrian", "c4 db4 eb4 f4 gb4 ab4 bb4"),
    ("Pent Maj", "c4 d4 e4 g4 a4"),
    ("Pent Min", "c4 eb4 f4 g4 bb4"),
    ("Blues", "c4 eb4 f4 gb4 g4 bb4"),
  ];

  /* Validate that string is a simple note pattern (no complex mini-notation) */
  let is_simple_pattern = (s: string): bool => {
    let trimmed = String.trim(s);
    /* Allow empty or space-separated tokens without complex syntax */
    String.length(trimmed) == 0
    || !String.contains(trimmed, '*')
    && !String.contains(trimmed, '/')
    && !String.contains(trimmed, '[')
    && !String.contains(trimmed, ']')
    && !String.contains(trimmed, '<')
    && !String.contains(trimmed, '>');
  };

  /* Extract string from Note constructor application */
  let string_of = (any: Language.Any.t): option(string) =>
    switch (any) {
    | Exp({term: Ap(_, {term: Constructor("Note", _), _}, arg), _}) =>
      switch (arg.term) {
      | Atom(String(s)) when is_simple_pattern(s) => Some(s)
      | _ => None
      }
    | _ => None
    };

  let init = (any: Language.Any.t) =>
    switch (string_of(any)) {
    | Some(_) => Some()
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
        | Exp({term: Ap(dir, ctor, arg), _} as t) =>
          Exp({
            ...t,
            term:
              Ap(
                dir,
                ctor,
                {
                  ...arg,
                  term: Atom(String(v)),
                },
              ),
          })
        | _ => failwith("ScalePicker: Put: not Note constructor"),
        info.syntax,
      )
    ) {
    | Some(s) => s
    | None => failwith("ScalePicker: Put: lift failed")
    };

  let focusable = Focusable.non;
  let dynamics = false;
  /* 3 rows for scale grid */
  let placeholder = (_, _) => {
    ProjectorShape.horizontal: 21,
    vertical: Tab(4),
  };
  let update = (model, _, _) => model;

  /* Create a scale option button */
  let scale_option = (~current, ~parent, ~info, (name, pattern)) => {
    let is_selected = current == pattern;
    Node.button(
      ~attrs=[
        Attr.classes(["scale-option"] @ (is_selected ? ["selected"] : [])),
        Attr.on_click(_ => {parent(SetSyntax(put(info, pattern)))}),
        Attr.title(pattern),
      ],
      [Node.text(name)],
    );
  };

  let view = ({info, parent, _}: View.args(model, action)) => {
    let current = get(info);

    View.mk(
      Node.div(
        ~attrs=[Attr.classes(["scale-picker"])],
        [
          Node.div(
            ~attrs=[Attr.classes(["current-pattern"])],
            [Node.text(current == "" ? "(empty)" : current)],
          ),
          Node.div(
            ~attrs=[Attr.classes(["scale-options"])],
            List.map(scale_option(~current, ~parent, ~info), scales),
          ),
        ],
      ),
    );
  };
};

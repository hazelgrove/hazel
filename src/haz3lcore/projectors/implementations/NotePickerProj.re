open Util;
open Virtual_dom.Vdom;
open ProjectorBase;

/* A simple note picker projector for Strudel patterns.
 * Shows a single-octave piano keyboard for selecting notes. */

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = unit;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;

  /* Notes in a single octave */
  let white_notes = ["c", "d", "e", "f", "g", "a", "b"];
  let black_notes = [
    ("c#", 0),
    ("d#", 1),
    ("f#", 3),
    ("g#", 4),
    ("a#", 5),
  ];

  let string_of = (any: Language.Any.t): option(string) =>
    switch (any) {
    | Exp({term: Atom(String(s)), _}) => Some(s)
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
        | Exp(t) =>
          Exp({
            ...t,
            term: Atom(String(v)),
          })
        | _ => failwith("NotePicker: Put: not string literal"),
        info.syntax,
      )
    ) {
    | Some(s) => s
    | None => failwith("NotePicker: Put: lift failed")
    };

  let focusable = Focusable.non;
  let dynamics = false;
  let placeholder = (_, _) => ProjectorCore.Shape.inline(16);
  let update = (model, _, _) => model;

  /* Toggle a note in the pattern string */
  let toggle_note = (pattern: string, note: string): string => {
    let notes =
      pattern
      |> String.split_on_char(' ')
      |> List.filter(s => String.length(s) > 0);
    if (List.mem(note, notes)) {
      /* Remove the note */
      notes |> List.filter(n => n != note) |> String.concat(" ");
    } else {
      /* Add the note at the end */
      (notes @ [note]) |> String.concat(" ");
    };
  };

  /* Check if a note is in the pattern */
  let has_note = (pattern: string, note: string): bool => {
    let notes =
      pattern
      |> String.split_on_char(' ')
      |> List.filter(s => String.length(s) > 0);
    List.mem(note, notes);
  };

  /* Create a white key */
  let white_key = (~pattern, ~parent, ~info, note) => {
    let note4 = note ++ "4";
    let active = has_note(pattern, note4);
    Node.div(
      ~attrs=[
        Attr.classes(["note-key", "white", active ? "active" : "inactive"]),
        Attr.on_click(_ => {
          let new_pattern = toggle_note(pattern, note4);
          parent(SetSyntax(put(info, new_pattern)));
        }),
      ],
      [Node.text(String.uppercase_ascii(note))],
    );
  };

  /* Create a black key */
  let black_key = (~pattern, ~parent, ~info, note, position) => {
    let note4 = note ++ "4";
    let active = has_note(pattern, note4);
    let left_offset = (Float.of_int(position) *. 2.0 +. 1.5) *. 14.29;
    Node.div(
      ~attrs=[
        Attr.classes(["note-key", "black", active ? "active" : "inactive"]),
        Attr.style(
          Css_gen.create(~field="left", ~value=Printf.sprintf("%.1f%%", left_offset)),
        ),
        Attr.on_click(_ => {
          let new_pattern = toggle_note(pattern, note4);
          parent(SetSyntax(put(info, new_pattern)));
        }),
      ],
      [],
    );
  };

  let view = ({info, parent, _}: View.args(model, action)) => {
    let pattern = get(info);
    let white_keys =
      List.map(note => white_key(~pattern, ~parent, ~info, note), white_notes);
    let black_keys =
      List.map(
        ((note, pos)) => black_key(~pattern, ~parent, ~info, note, pos),
        black_notes,
      );
    View.mk(
      Node.div(
        ~attrs=[Attr.classes(["note-picker"])],
        [
          Node.div(~attrs=[Attr.classes(["piano-keys"])], white_keys @ black_keys),
          Node.span(
            ~attrs=[Attr.classes(["pattern-text"])],
            [Node.text(pattern)],
          ),
        ],
      ),
    );
  };
};

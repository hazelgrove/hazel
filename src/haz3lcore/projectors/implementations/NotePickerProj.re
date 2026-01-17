open Util;
open Virtual_dom.Vdom;
open ProjectorBase;

/* A simple note picker projector for Strudel patterns.
 * Shows a single-octave piano keyboard for selecting notes.
 *
 * Supported subset:
 * - Space-separated note tokens
 * - Each token: note letter (a-g), optional sharp (#) or flat (b), octave digit
 * - All notes must be at the same octave
 * - Examples: "c4 e4 g4", "c#3 d3 f#3", "" (empty)
 *
 * Not supported (projector won't be applicable):
 * - Mixed octaves: "c3 e4 g5"
 * - Repetition: "c4*2"
 * - Grouping: "[c4 e4]"
 * - Rests: "c4 ~ e4"
 * - Any other mini-notation syntax */

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = {octave: int};
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

  /* All valid note names (for validation) */
  let all_notes = [
    "c",
    "c#",
    "d",
    "d#",
    "e",
    "f",
    "f#",
    "g",
    "g#",
    "a",
    "a#",
    "b",
    "db",
    "eb",
    "gb",
    "ab",
    "bb",
  ];

  /* Parse a single note token, returning (note_name, octave) if valid */
  let parse_note = (token: string): option((string, int)) => {
    let len = String.length(token);
    if (len < 2 || len > 3) {
      None;
    } else {
      /* Last character should be octave digit */
      let octave_char = token.[len - 1];
      if (octave_char < '0' || octave_char > '9') {
        None;
      } else {
        let octave = Char.code(octave_char) - Char.code('0');
        let note_name =
          String.lowercase_ascii(String.sub(token, 0, len - 1));
        if (List.mem(note_name, all_notes)) {
          Some((note_name, octave));
        } else {
          None;
        };
      };
    };
  };

  /* Validate pattern and extract octave if all notes are at same octave.
   * Returns Some(octave) if valid, None if invalid or mixed octaves. */
  let validate_pattern = (pattern: string): option(int) => {
    let tokens =
      pattern
      |> String.split_on_char(' ')
      |> List.filter(s => String.length(s) > 0);

    if (List.length(tokens) == 0) {
      /* Empty pattern is valid, default to octave 4 */
      Some(4);
    } else {
      /* Parse all tokens */
      let parsed = List.map(parse_note, tokens);
      if (List.exists(opt => opt == None, parsed)) {
        None;
            /* Some token didn't parse as a valid note */
      } else {
        /* All parsed - check they're all same octave */
        let notes = List.filter_map(x => x, parsed);
        let octaves = List.map(snd, notes);
        switch (octaves) {
        | [] => Some(4)
        | [first, ...rest] =>
          if (List.for_all(o => o == first, rest)) {
            Some(first);
          } else {
            None; /* Mixed octaves */
          }
        };
      };
    };
  };

  /* Extract string from Note or Sample constructor application */
  let string_of = (any: Language.Any.t): option(string) =>
    switch (any) {
    | Exp({
        term: Ap(_, {term: Constructor("Note" | "Sample", _), _}, arg),
        _,
      }) =>
      switch (arg.term) {
      | Atom(String(s)) => Some(s)
      | _ => None
      }
    | _ => None
    };

  let init = (any: Language.Any.t) =>
    switch (string_of(any)) {
    | Some(s) =>
      switch (validate_pattern(s)) {
      | Some(octave) => Some({octave: octave})
      | None => None
      }
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
        | _ => failwith("NotePicker: Put: not Note/Sample constructor"),
        info.syntax,
      )
    ) {
    | Some(s) => s
    | None => failwith("NotePicker: Put: lift failed")
    };

  let focusable = Focusable.non;
  let dynamics = false;
  /* Piano needs ~4 rows of height for keys + pattern display */
  let placeholder = (_, _) => {
    ProjectorShape.horizontal: 20,
    vertical: Tab(4),
  };
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
      notes @ [note] |> String.concat(" ");
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
  let white_key = (~pattern, ~parent, ~info, ~octave, note) => {
    let full_note = note ++ string_of_int(octave);
    let active = has_note(pattern, full_note);
    Node.div(
      ~attrs=[
        Attr.classes(["note-key", "white", active ? "active" : "inactive"]),
        Attr.on_click(_ => {
          let new_pattern = toggle_note(pattern, full_note);
          parent(SetSyntax(put(info, new_pattern)));
        }),
      ],
      [Node.text(String.uppercase_ascii(note))],
    );
  };

  /* Create a black key */
  let black_key = (~pattern, ~parent, ~info, ~octave, note, position) => {
    let full_note = note ++ string_of_int(octave);
    let active = has_note(pattern, full_note);
    let left_offset = (Float.of_int(position) *. 2.0 +. 1.5) *. 14.29;
    Node.div(
      ~attrs=[
        Attr.classes(["note-key", "black", active ? "active" : "inactive"]),
        Attr.style(
          Css_gen.create(
            ~field="left",
            ~value=Printf.sprintf("%.1f%%", left_offset),
          ),
        ),
        Attr.on_click(_ => {
          let new_pattern = toggle_note(pattern, full_note);
          parent(SetSyntax(put(info, new_pattern)));
        }),
      ],
      [],
    );
  };

  let view = ({model: {octave}, info, parent, _}: View.args(model, action)) => {
    let pattern = get(info);
    let white_keys =
      List.map(
        note => white_key(~pattern, ~parent, ~info, ~octave, note),
        white_notes,
      );
    let black_keys =
      List.map(
        ((note, pos)) =>
          black_key(~pattern, ~parent, ~info, ~octave, note, pos),
        black_notes,
      );
    View.mk(
      Node.div(
        ~attrs=[Attr.classes(["note-picker"])],
        [
          Node.div(
            ~attrs=[Attr.classes(["piano-keys"])],
            white_keys @ black_keys,
          ),
          Node.div(
            ~attrs=[Attr.classes(["octave-label"])],
            [Node.text("Oct " ++ string_of_int(octave))],
          ),
          Node.span(
            ~attrs=[Attr.classes(["pattern-text"])],
            [Node.text(pattern)],
          ),
        ],
      ),
    );
  };
};

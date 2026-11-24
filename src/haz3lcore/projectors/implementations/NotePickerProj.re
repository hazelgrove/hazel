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

  /* All valid note names (for validation) - sharps only */
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
  ];

  /* Normalize flats to sharps for consistent representation */
  let normalize_note = (note: string): string =>
    switch (note) {
    | "db" => "c#"
    | "eb" => "d#"
    | "gb" => "f#"
    | "ab" => "g#"
    | "bb" => "a#"
    | n => n
    };

  /* Parse a single note token, returning (note_name, octave) if valid.
   * Flats are normalized to sharps for consistent internal representation. */
  let parse_note = (token: string): option((string, int)) => {
    let len = String.length(token);
    if (len < 2 || len > 3) {
      None;
    } else {
      /* Last character should be octave digit */
      let octave_char = token.[len - 1];
      if (Char.(octave_char < '0' || octave_char > '9')) {
        None;
      } else {
        let octave = Char.to_int(octave_char) - Char.to_int('0');
        let note_name =
          String.lowercase(String.sub(token, ~pos=0, ~len=len - 1))
          |> normalize_note;
        if (List.mem(all_notes, note_name, ~equal=String.equal)) {
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
      |> String.split(~on=' ')
      |> List.filter(~f=s => String.length(s) > 0);

    if (List.length(tokens) == 0) {
      /* Empty pattern is valid, default to octave 4 */
      Some(4);
    } else {
      /* Parse all tokens */
      let parsed = List.map(tokens, ~f=parse_note);
      if (List.exists(parsed, ~f=Option.is_none)) {
        None;
            /* Some token didn't parse as a valid note */
      } else {
        /* All parsed - check they're all same octave */
        let notes = List.filter_map(parsed, ~f=x => x);
        let octaves = List.map(notes, ~f=snd);
        switch (octaves) {
        | [] => Some(4)
        | [first, ...rest] =>
          if (List.for_all(rest, ~f=o => o == first)) {
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
        ~inline=true,
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
  let elaborate_syntax = false;
  let error = (_, _): option(ProjectorBase.error) => None;
  /* Piano keys + pattern display */
  let placeholder = (_, _) => {
    ProjectorShape.horizontal: 20,
    vertical: Tab(4),
  };
  let update = (model, _, _) => model;

  /* Normalize a full note token (e.g., "db4" -> "c#4") */
  let normalize_full_note = (token: string): string => {
    let len = String.length(token);
    if (len < 2 || len > 3) {
      token;
    } else {
      let octave_char = token.[len - 1];
      if (Char.(octave_char < '0' || octave_char > '9')) {
        token;
      } else {
        let note_name =
          String.lowercase(String.sub(token, ~pos=0, ~len=len - 1));
        normalize_note(note_name) ++ String.make(1, octave_char);
      };
    };
  };

  /* Toggle a note in the pattern string.
   * Normalizes flats to sharps when checking/removing. */
  let toggle_note = (pattern: string, note: string): string => {
    let notes =
      pattern
      |> String.split(~on=' ')
      |> List.filter(~f=s => String.length(s) > 0);
    let normalized_notes = List.map(notes, ~f=normalize_full_note);
    if (List.mem(normalized_notes, note, ~equal=String.equal)) {
      /* Remove the note - filter by normalized comparison */
      notes
      |> List.filter(~f=n => !String.equal(normalize_full_note(n), note))
      |> String.concat(~sep=" ");
    } else {
      /* Add the note at the end */
      notes @ [note] |> String.concat(~sep=" ");
    };
  };

  /* Check if a note is in the pattern (normalizes flats to sharps) */
  let has_note = (pattern: string, note: string): bool => {
    let notes =
      pattern
      |> String.split(~on=' ')
      |> List.filter(~f=s => String.length(s) > 0)
      |> List.map(~f=normalize_full_note);
    List.mem(notes, note, ~equal=String.equal);
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
      [Node.text(String.uppercase(note))],
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
      List.map(white_notes, ~f=note =>
        white_key(~pattern, ~parent, ~info, ~octave, note)
      );
    let black_keys =
      List.map(black_notes, ~f=((note, pos)) =>
        black_key(~pattern, ~parent, ~info, ~octave, note, pos)
      );
    View.mk(
      Node.div(
        ~attrs=[Attr.classes(["note-picker"])],
        [
          Node.div(
            ~attrs=[Attr.classes(["piano-keys"])],
            white_keys @ black_keys,
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

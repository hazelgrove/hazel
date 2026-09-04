open Util_web.OptUtil.Syntax;

/* Segment cache for paste optimization. When a copy/cut captures a
   complete segment, it's cached here. On paste, if the clipboard text
   matches the cached text, we splice the segment directly instead of
   reparsing. Cache is set from Page.re on copy/cut. */
let segment_cache: ref(option((string, Segment.t))) = ref(None);

let set_segment_cache = (seg: option(Segment.t), str: string): unit =>
  switch (seg) {
  | Some(seg) when Segment.deep_tile_complete(seg) =>
    segment_cache := Some((str, seg))
  | _ => ()
  };

/* Would splicing [text] at the caret merge with a neighboring token
   (e.g. pasting `+2` right after `x1`)? Shared by the segment-cache
   paste and the FastParse paste gate. */
let boundary_merges = (text: string, z: Zipper.t): bool => {
  let chars = Token.to_list(text);
  switch (chars) {
  | [] => false
  | _ =>
    let first_char = List.hd(chars);
    let last_char = Util_web.ListUtil.last(chars);
    let left =
      switch (Zipper.neighbor_token(Left, z)) {
      | None => false
      | Some(t) => Token.is_potential_token(Token.append(t, first_char))
      };
    let right =
      switch (Zipper.neighbor_token(Right, z)) {
      | None => false
      | Some(t) => Token.is_potential_token(Token.append(last_char, t))
      };
    left || right;
  };
};

/* Try pasting from segment cache. Returns Some if cache hits and
   guards pass (caret Outer, no token merging at boundaries).
   The segment gets fresh IDs to support multiple pastes. */
let try_segment_paste =
    (clipboard: string, z: Zipper.t, ~root): option(Zipper.t) => {
  let trim = Util_web.StringUtil.trim_leading;
  switch (segment_cache^) {
  | Some((cached, seg)) when trim(cached) == trim(clipboard) =>
    if (z.caret != Outer) {
      None;
    } else if (trim(clipboard) != "" && !boundary_merges(trim(clipboard), z)) {
      let seg = Segment.IDs.replace(seg);
      Some(Zipper.insert_segment(z, seg, ~root));
    } else {
      None;
    }
  | _ => None
  };
};

/* Insert characters one-by-one into a zipper. Used for paste and
   other operations that start from an existing zipper state. */
let to_zipper =
    (~root, ~zipper_init=Zipper.init(), str: string): option(Zipper.t) => {
  let insert = (z: option(Zipper.t), c: string): option(Zipper.t) => {
    let* z = z;
    try(c == "\r" ? Some(z) : Insert.go(c, z, ~root)) {
    | exn =>
      print_endline("WARN: Parser.to_zipper: " ++ Printexc.to_string(exn));
      None;
    };
  };
  let+ z = str |> Token.to_list |> List.fold_left(insert, Some(zipper_init));
  Zipper.rescan_reassemble(~with_parent=true, Left, z, ~root);
};

/* Check if the zipper is at a "safe split point": top level with
   no incomplete tiles (empty backpack), caret between tokens,
   and we just inserted a whitespace char (ensuring we're at a real
   token boundary, not mid-identifier like 't' before 'type'). */
let is_split_point = (c: string, z: Zipper.t): bool =>
  Token.is_secondary(c)
  && z.caret == Outer
  && z.relatives.ancestors == []
  && Zipper.local_backpack(z) == [];

/* Strip trailing convex grout from a segment. This grout is the
   artifact of Zipper.init()'s initial placeholder that was never
   consumed because we split before content filled it. */
let strip_trailing_grout = (seg: Segment.t): Segment.t => {
  let rec strip_right = (rev_seg: Segment.t): Segment.t =>
    switch (rev_seg) {
    | [Grout({shape: Convex, _}), ...rest] => rest
    | [Secondary(_) as s, ...rest] =>
      switch (strip_right(rest)) {
      | stripped when stripped != rest => [s, ...stripped]
      | _ => rev_seg
      }
    | _ => rev_seg
    };
  seg |> List.rev |> strip_right |> List.rev;
};

/* Segmented parser: splits into independent segments at top-level
   delimiter-complete boundaries to avoid O(n^2) scaling. Each segment
   is parsed independently; trailing grout (from Zipper.init) is
   stripped, segments are concatenated, and a final top-level regrout
   ensures shape consistency across boundaries. */
let to_segment = (str: string, ~root): option(Segment.t) => {
  let chars = str |> Token.to_list;
  let segments = ref([]);
  let current_z = ref(Some(Zipper.init()));
  let chars_since_split = ref(0);
  let min_segment_size = 100;

  let insert_char = (z: option(Zipper.t), c: string): option(Zipper.t) => {
    let* z = z;
    try(c == "\r" ? Some(z) : Insert.go(c, z, ~root)) {
    | exn =>
      print_endline("WARN: Parser.to_segment: " ++ Printexc.to_string(exn));
      None;
    };
  };

  List.iter(
    c => {
      current_z := insert_char(current_z^, c);
      incr(chars_since_split);
      switch (current_z^) {
      | None => ()
      | Some(z) =>
        if (chars_since_split^ >= min_segment_size && is_split_point(c, z)) {
          let z = Zipper.remold_regrout(Left, z, ~root);
          let seg = Zipper.unselect_and_zip(~erase_buffer=true, z);
          segments := [strip_trailing_grout(seg), ...segments^];
          current_z := Some(Zipper.init());
          chars_since_split := 0;
        }
      };
    },
    chars,
  );

  let+ z = current_z^;
  let z = Zipper.remold_regrout(Left, z, ~root);
  let final_seg = Zipper.unselect_and_zip(~erase_buffer=true, z);
  let all_segments = List.rev([final_seg, ...segments^]);
  let combined = List.concat(all_segments);
  Segment.regrout(Nib.Shape.(concave(), concave()), combined);
};

/* Quick O(n) check that clipboard has balanced parens/brackets/braces.
   Under the Menhir path unbalanced text just fails to parse and falls
   back, so this is a cheap pre-filter (skip the parse attempt), not a
   correctness requirement. Conservative: delimiters inside string
   literals cause false negatives, falling back to the slow path. */
let has_balanced_delimiters = (s: string): bool => {
  let chars = Token.to_list(s);
  let stack = ref([]);
  let ok = ref(true);
  List.iter(
    c =>
      switch (c) {
      | "(" => stack := [")", ...stack^]
      | "[" => stack := ["]", ...stack^]
      | "{" => stack := ["}", ...stack^]
      | ")"
      | "]"
      | "}" =>
        switch (stack^) {
        | [top, ...rest] when top == c => stack := rest
        | _ => ok := false
        }
      | _ => ()
      },
    chars,
  );
  ok^ && stack^ == [];
};

/* Gate for the FastParse paste attempt (segment splice at the caret).
   Requires: caret between tokens, no incomplete tiles, Exp sort, no
   token merging at boundaries, and balanced delimiters in the clipboard.
   Unlike dev's can_fast_paste this does NOT require a top-level caret:
   the splice + remold doesn't depend on ancestors beyond the sort check,
   so nested pastes (inside parens, case arms) take the fast path too.
   Returns the first failing condition (console telemetry), or None when
   the splice is safe. */
let fast_paste_blocker =
    (clipboard: string, z: Zipper.t, ~root): option(string) =>
  if (String.length(clipboard) == 0) {
    Some("empty clipboard");
  } else if (z.caret != Outer) {
    Some("caret is inside a token");
  } else if (Zipper.local_backpack(z) != []) {
    Some("backpack is nonempty");
  } else if (Relatives.sort(~root, z.relatives) != Sort.Exp) {
    Some("caret sort is not Exp");
  } else if (!has_balanced_delimiters(clipboard)) {
    Some("clipboard delimiters unbalanced");
  } else if (boundary_merges(clipboard, z)) {
    Some("clipboard would merge with a token at the caret boundary");
  } else {
    None;
  };

/* Fast paste: linear Menhir zip of the clipboard spliced at the caret.
   A failed attempt costs ~1ms, and a hit turns the worst paste case (a
   whole external program) into milliseconds with formatting kept
   verbatim. Error carries why the fast path lost — a gate refusal or the
   parser's bail note — so the call site can report it; the failure POLICY
   (falling back to the quadratic typing parser) lives there too. */
let fast_paste =
    (clipboard: string, z: Zipper.t, ~root): result(Zipper.t, string) =>
  switch (fast_paste_blocker(clipboard, z, ~root)) {
  | Some(why) => Error("gate refused — " ++ why)
  | None =>
    switch (
      FastParse.parsed_of_text(
        ~materialize=Triggers.invoked_projector,
        ~collect_refractors=true,
        ~root,
        String.trim(clipboard),
      )
    ) {
    | Error(why) => Error("parse bailed — " ++ why)
    | Ok({segment, refractors}) =>
      /* Like Zipper.insert_segment, but regrout with Left so the caret
         lands BEFORE any grout a body-less fragment opens (matching the
         typing path), not after it. */
      Ok(
        Zipper.rescan_reassemble(
          ~with_parent=true,
          Left,
          z
          |> Zipper.replace_selection(Right, segment)
          |> Zipper.unselect
          |> Zipper.remold_regrout(Left, ~root),
          ~root,
        )
        |> Triggers.apply_refractors(refractors),
      )
    }
  };

let to_term = (s: string, ~root): option(Language.Exp.t) => {
  let+ seg = to_segment(s, ~root);
  let z = Zipper.unzip(seg);
  MakeTerm.from_zip_for_sem(z, ~root).term;
};

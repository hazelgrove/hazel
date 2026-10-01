open Util.OptUtil.Syntax;

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
    let last_char = Util.ListUtil.last(chars);
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
  let trim = Util.StringUtil.trim_leading;
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

/* Line endings as the editor keeps them. Token.to_list segments by
   grapheme, and "\r\n" is ONE grapheme, so text with Windows line endings
   arrives as "\r\n" characters, never as a lone "\r": each was inserted as
   an unknown token, leaving a `¿` hole at the start of every line. A lone
   "\r" (old Mac endings, or a stray one) is a line break too: skipping it
   would glue `in\rx` into `inx`. */
let line_ending = (c: string): string =>
  switch (c) {
  | "\r\n"
  | "\r" => "\n"
  | c => c
  };

/* The longest prefix of [chars] that stays one operand, or one operator,
   at every step: exactly the characters typing would keep appending to
   the token they start. Brackets, quotes, comment delimiters and
   whitespace are in neither class, so they always come one at a time. */
let take_run = (chars: list(string)): option((string, list(string))) => {
  let same_class =
    switch (chars) {
    | [c, ..._] when Token.is_potential_operand(c) =>
      Some(Token.is_potential_operand)
    | [c, ..._] when Token.is_potential_operator(c) =>
      Some(Token.is_potential_operator)
    | _ => None
    };
  let+ same_class = same_class;
  let fits = t => Token.is_potential_token(t) && same_class(t);
  let rec go = (acc, rest) =>
    switch (rest) {
    | [c, ...rest'] when fits(acc ++ c) => go(acc ++ c, rest')
    | _ => (acc, rest)
    };
  go(List.hd(chars), List.tl(chars));
};

/* Insert characters one-by-one into a zipper. Used for paste and
   other operations that start from an existing zipper state.
   With ~by_run, a run from take_run goes in as one insertion whenever
   the caret is between tokens, and insertions skip their regrout, which
   is done once at the end: regrouting walks the whole sibling run, so
   doing it per insertion made loading quadratic in the run's length. */
let to_zipper =
    (~by_run=false, ~root, ~zipper_init=Zipper.init(), str: string)
    : option(Zipper.t) => {
  /* auto_indent off, so the parser reproduces its input without adding
     spaces. */
  let insert = (z: Zipper.t, c: string): option(Zipper.t) =>
    try(
      Insert.go(
        ~auto_indent=false,
        ~regrout=!by_run,
        line_ending(c),
        z,
        ~root,
      )
    ) {
    | exn =>
      print_endline("WARN: Parser.to_zipper: " ++ Printexc.to_string(exn));
      None;
    };
  let rec go = (z: Zipper.t, chars: list(string)): option(Zipper.t) =>
    switch (chars) {
    | [] => Some(z)
    | [c, ...rest] =>
      let (s, rest) =
        switch (
          by_run && z.caret == Outer && z.selection.content == []
            ? take_run(chars) : None
        ) {
        | Some(run) => run
        | None => (c, rest)
        };
      /* A direct self call, which js_of_ocaml compiles to a loop. Through
         `let*` it was a call inside Option.bind's closure: a stack frame per
         run of characters, each holding the zipper it started from, so a
         20 KB slide overflowed the stack or ran out of memory. */
      switch (insert(z, s)) {
      | None => None
      | Some(z) => go(z, rest)
      };
    };
  let+ z = go(zipper_init, Token.to_list(str));
  /* ~by_run skipped every per-insertion regrout; do it once here. */
  let z = by_run ? Zipper.remold_regrout(Left, z, ~root) : z;
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
  && Zipper.local_missing_shards(z) == [];

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
let to_segment_with_manuals =
    (~by_run=true, str: string, ~root)
    : option((Segment.t, Refractors.RefractorList.t)) => {
  let segments = ref([]);
  /* Projectors typed along the way (`^^probe(` and the like) are pinned
     in each piece's refractors, by piece id; ids survive the split, so
     every piece's pins are kept and handed back with the segment. */
  let manuals = ref([]);
  let current_z = ref(Some(Zipper.init()));
  let chars_since_split = ref(0);
  let min_segment_size = 100;
  /* With ~by_run (the default), as in to_zipper: a run that stays one
     token goes in as one insertion, and the regrout waits for the end of
     the segment, where it happens anyway. Each segment starts from a fresh
     zipper, so this always parses text on its own, which is when that is
     safe. Over all 117 hazel-programs, with and without it, the result is
     identical. */
  let insert = (z: Zipper.t, s: string): option(Zipper.t) =>
    try(
      Insert.go(
        ~auto_indent=false,
        ~regrout=!by_run,
        line_ending(s),
        z,
        ~root,
      )
    ) {
    | exn =>
      print_endline("WARN: Parser.to_segment: " ++ Printexc.to_string(exn));
      None;
    };
  /* A direct self call, so js_of_ocaml compiles it to a loop. */
  let rec go = (chars: list(string)) =>
    switch (chars, current_z^) {
    | ([], _)
    | (_, None) => ()
    | ([c, ...rest], Some(z)) =>
      let (s, rest) =
        switch (
          by_run && z.caret == Outer && z.selection.content == []
            ? take_run(chars) : None
        ) {
        | Some(run) => run
        | None => (c, rest)
        };
      current_z := insert(z, s);
      chars_since_split :=
        chars_since_split^ + List.length(chars) - List.length(rest);
      switch (current_z^) {
      | Some(z)
          when
            chars_since_split^ >= min_segment_size
            && is_split_point(line_ending(s), z) =>
        let z = Zipper.remold_regrout(Left, z, ~root);
        manuals := z.refractors.manuals @ manuals^;
        let seg = Zipper.unselect_and_zip(~erase_buffer=true, z);
        segments := [strip_trailing_grout(seg), ...segments^];
        current_z := Some(Zipper.init());
        chars_since_split := 0;
      | _ => ()
      };
      go(rest);
    };
  go(Token.to_list(str));

  let+ z = current_z^;
  let z = Zipper.remold_regrout(Left, z, ~root);
  let manuals = z.refractors.manuals @ manuals^;
  let final_seg = Zipper.unselect_and_zip(~erase_buffer=true, z);
  let all_segments = List.rev([final_seg, ...segments^]);
  let combined = List.concat(all_segments);
  (Segment.regrout(Nib.Shape.(concave(), concave()), combined), manuals);
};

let to_segment = (~by_run=true, str: string, ~root): option(Segment.t) =>
  to_segment_with_manuals(~by_run, str, ~root) |> Option.map(fst);

/* to_zipper's result, from the segmented parser (hazelgrove/hazel#2610):
   linear where to_zipper is quadratic in a long top-level sequence, and
   the same zipper, projectors and all, on hazel-programs. For text parsed
   on its own, not inserted into a program. */
let to_zipper_segmented =
    (~by_run=true, ~root, str: string): option(Zipper.t) => {
  let+ (seg, manuals) = to_segment_with_manuals(~by_run, str, ~root);
  Zipper.unzip(seg)
  |> Zipper.rescan_reassemble(~with_parent=true, Left, _, ~root)
  |> ZipperBase.update_manuals(existing =>
       manuals
       @ List.filter(((id, _)) => !List.mem_assoc(id, manuals), existing)
     );
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
  } else if (Zipper.local_missing_shards(z) != []) {
    Some("incomplete tiles (missing shards) at the caret");
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

/* Typing-parser splice paste: parse the clipboard in isolation with the
   segmented typing parser, then splice the segment and regrout. Slower
   than fast_paste's Menhir path but handles INCOMPLETE forms (flush
   let chains, dangling defs) that Menhir rejects, while producing the
   splice-shaped grout layout the partition-aware auto-indent reads as
   evidence (Test_Indentation flush pins). Sits between fast_paste and
   the char-by-char to_zipper fallback. */
let can_splice_paste = (clipboard: string, z: Zipper.t, ~root): bool => {
  let len = String.length(clipboard);
  len > 0
  && z.caret == Outer
  && z.relatives.ancestors == []
  && Zipper.local_missing_shards(z) == []
  && Relatives.sort(~root, z.relatives) == Sort.Exp
  && has_balanced_delimiters(clipboard)
  && {
    let chars = Token.to_list(clipboard);
    let first_char = List.hd(chars);
    let last_char = Util.ListUtil.last(chars);
    let no_left_merge =
      switch (Zipper.neighbor_token(Left, z)) {
      | None => true
      | Some(t) => !Token.is_potential_token(Token.append(t, first_char))
      };
    let no_right_merge =
      switch (Zipper.neighbor_token(Right, z)) {
      | None => true
      | Some(t) => !Token.is_potential_token(Token.append(last_char, t))
      };
    no_left_merge && no_right_merge;
  };
};

let splice_paste = (clipboard: string, z: Zipper.t, ~root): option(Zipper.t) => {
  let+ seg = to_segment(clipboard, ~root);
  let z = Zipper.insert_segment(z, seg, ~root);
  Zipper.rescan_reassemble(Left, z, ~root);
};

let to_term = (s: string, ~root): option(Language.Exp.t) => {
  let+ seg = to_segment(s, ~root);
  let z = Zipper.unzip(seg);
  MakeTerm.from_zip_for_sem(z, ~root).term;
};

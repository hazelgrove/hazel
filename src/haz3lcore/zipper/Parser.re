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

/* Try pasting from segment cache. Returns Some if cache hits and
   guards pass (caret Outer, no token merging at boundaries).
   The segment gets fresh IDs to support multiple pastes. */
let try_segment_paste = (clipboard: string, z: Zipper.t): option(Zipper.t) => {
  let trim = Util.StringUtil.trim_leading;
  switch (segment_cache^) {
  | Some((cached, seg)) when trim(cached) == trim(clipboard) =>
    if (z.caret != Outer) {
      None;
    } else {
      /* Check token merging at boundaries */
      let chars = Token.to_list(trim(clipboard));
      switch (chars) {
      | [] => None
      | _ =>
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
        if (no_left_merge && no_right_merge) {
          let seg = Segment.IDs.replace(seg);
          Some(Zipper.insert_segment(z, seg));
        } else {
          None;
        };
      };
    }
  | _ => None
  };
};

let to_zipper = (~zipper_init=Zipper.init(), str: string): option(Zipper.t) => {
  let insert = (z: option(Zipper.t), c: string): option(Zipper.t) => {
    let* z = z;
    try(c == "\r" ? Some(z) : Insert.go(c, z)) {
    | exn =>
      print_endline("WARN: Parser.to_zipper: " ++ Printexc.to_string(exn));
      None;
    };
  };
  let+ z = str |> Token.to_list |> List.fold_left(insert, Some(zipper_init));
  Zipper.remold_regrout(Left, z);
};

let to_segment = (s: string): option(Segment.t) => {
  let+ z = to_zipper(s);
  Zipper.unselect_and_zip(~erase_buffer=true, z);
};

let to_term = (s: string): option(Language.Exp.t) => {
  let+ z = to_zipper(s);
  MakeTerm.from_zip_for_sem(z).term;
};

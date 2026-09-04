/* BOTH halves of the `¿` convention: Grout (the editor's implicit
   holes) has no textual form, so [to_text] renders it with a marker
   token, and [of_text] turns marked text back into real Grout.

   The default marker is `¿` (U+00BF): a single non-identifier,
   non-operator character wired as an `ImplicitHoleMarker` atomic form
   (see `Form.re` / `Token.re`) so it tokenises in isolation — it
   doesn't glue with adjacent keywords (`in¿` reads as `in`, `¿`) or
   operators (`¿,` reads as `¿`, `,`) — and stays distinct from the
   parser's `?` empty-hole token, so explicit user-typed `?` tiles
   round-trip distinct from implicit Grout.

   Reparsing marked text yields literal `¿` TILES, which the original
   program never had. [of_text] parses, then Destructs each marker
   tile; the remold/regrout pass that runs on every edit re-inserts
   Grout wherever shape requires it, reconstructing the original
   zipper. This is the RECOVERING-PARSER half: the fast path reads the
   same markers structurally during its weave (see FastParse), but the
   recovering parser has no notion of `¿`, and incomplete programs —
   the grout-heavy ones — are exactly what falls back to it. Sits
   below PersistentZipper so persistence loading can fall back to it. */
open Util_web;
open Base;

let default_implicit_hole = Token.implicit_hole_marker;

/* The PRINT half: parseable Hazel source with Grout rendered as the
   marker. Projectors are unfolded to trigger syntax (`^^fold(body)`)
   by `Triggers.projector_to_invoke`, which `Printer.of_segment` uses
   by default; the parsers reconstruct the wrapper from that syntax.
   `~indent=""` keeps the output minimal: Printer would otherwise
   prepend each row's indent level, and those chars come back as
   Secondary whitespace pieces, breaking structural round-trip. */
let seg_to_text =
    (~implicit_hole=default_implicit_hole, ~refractors=[], segment): string =>
  Printer.of_segment(
    ~holes=implicit_hole,
    ~concave_holes=implicit_hole,
    ~indent="",
    ~refractors,
    segment,
  );

let to_text = (~implicit_hole=default_implicit_hole, z: Zipper.t): string =>
  seg_to_text(
    ~implicit_hole,
    ~refractors=z.refractors.manuals,
    Zipper.unselect_and_zip(~erase_buffer=true, z),
  );

let is_marker = (~implicit_hole: string, p: piece): bool =>
  switch (p) {
  | Tile(t) => t.label == [implicit_hole]
  | _ => false
  };

let rec find_marker = (~implicit_hole: string, seg: Segment.t): option(Id.t) => {
  let rec scan = (rest: list(piece)): option(Id.t) =>
    switch (rest) {
    | [] => None
    | [p, ...tail] =>
      if (is_marker(~implicit_hole, p)) {
        Some(Piece.id(p));
      } else {
        switch (descend(p)) {
        | Some(id) => Some(id)
        | None => scan(tail)
        };
      }
    }
  and descend = (p: piece): option(Id.t) =>
    switch (p) {
    | Tile(t) =>
      List.fold_left(
        (acc, child) =>
          switch (acc) {
          | Some(_) => acc
          | None => find_marker(~implicit_hole, child)
          },
        None,
        t.children,
      )
    | Projector(_) =>
      /* KNOWN GAP: markers inside projector syntax (`^^fold(¿)`) are not
         found — and the strip below couldn't destruct inside a projector
         anyway. The fast path handles these (its weave maps ¿ to Grout
         before materializing the projector), so only a slow-path load of
         a projector-wrapped hole leaves a literal ¿ tile inside. */
      None
    | _ => None
    };
  scan(seg);
};

let rec strip_implicit_holes =
        (~implicit_hole: string, ~root, z: Zipper.t): Zipper.t => {
  let segment = Zipper.zip(z);
  switch (find_marker(~implicit_hole, segment)) {
  | None => z
  | Some(id) =>
    /* Select the whole marker tile first, otherwise Destruct nibbles one
     * char at a time via Token.rm_edge and never removes the full tile. */
    switch (Select.tile(id, z)) {
    | None => z
    | Some(z) =>
      switch (Destruct.go(Left, z, ~root)) {
      | None => z
      | Some(z) => strip_implicit_holes(~implicit_hole, ~root, z)
      }
    }
  };
};

let of_text =
    (~implicit_hole=default_implicit_hole, ~root, text: string)
    : option(Zipper.t) =>
  switch (Parser.to_zipper(~root, text)) {
  | None => None
  | Some(z) =>
    let refractors = z.refractors;
    let z = strip_implicit_holes(~implicit_hole, ~root, z);
    Some(ZipperBase.update_refractors(z, _ => refractors));
  };

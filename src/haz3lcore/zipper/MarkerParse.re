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
   program never had. [of_text] parses, then swaps each marker tile for
   a Grout piece in place. (It used to Destruct the tile and let the
   edit-time regrout put Grout back, but that only recovers a hole
   where regrout independently wants one: `[¿]` came back as `[]`,
   #2518.) This is the RECOVERING-PARSER half: the fast path reads the
   same markers structurally during its weave (see FastParse), but the
   recovering parser has no notion of `¿`, and incomplete programs —
   the grout-heavy ones — are exactly what falls back to it. Sits
   below PersistentZipper so persistence loading can fall back to it. */
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

/* Swap every marker tile for a convex Grout with the same id. Projector
   contents are not entered: markers inside projector syntax (`^^fold(¿)`)
   are a KNOWN GAP (#2455) — the fast path handles those (its weave maps
   ¿ to Grout before materializing the projector), so only a slow-path
   load of a projector-wrapped hole leaves a literal ¿ tile inside. */
let replace_markers = (~implicit_hole: string, seg: Segment.t): Segment.t =>
  List.map(
    Base.map_piece(~f_piece=(rec_call, p: piece) =>
      switch (p) {
      | Tile({id, label: [marker], _}) when marker == implicit_hole =>
        Grout({
          id,
          shape: Convex,
        })
      | _ => rec_call(p)
      }
    ),
    seg,
  );

let strip_implicit_holes = (~implicit_hole: string, z: Zipper.t): Zipper.t =>
  Zipper.zip(z)
  |> replace_markers(~implicit_hole)
  |> Zipper.unzip
  |> ZipperBase.update_refractors(_, _ => z.refractors);

let of_text =
    (~implicit_hole=default_implicit_hole, ~root, text: string)
    : option(Zipper.t) =>
  switch (Parser.to_zipper(~root, text)) {
  | None => None
  | Some(z) => Some(strip_implicit_holes(~implicit_hole, z))
  };

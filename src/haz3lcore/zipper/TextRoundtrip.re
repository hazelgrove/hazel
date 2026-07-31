/* Round-trip a PersistentSegment through plaintext.
 *
 * `to_text` prints a slide's zipper as parseable Hazel source, rendering
 * implicit holes (Grout) with a settable marker token. `of_text` parses
 * the text back and walks the resulting zipper destructing every marker
 * tile via `Destruct.go`, letting `remold_regrout` re-insert Grout where
 * shape requires it.
 *
 * Default marker is `¿` (U+00BF). It's a single non-identifier,
 * non-operator character wired as an `ImplicitHoleMarker` atomic form
 * (see `Form.re` / `Token.re`) so it tokenises in isolation — it doesn't
 * glue with adjacent keywords (`in¿` parses as `in`, `¿`) or with
 * adjacent operators (`¿,` parses as `¿`, `,`). It's also distinct from
 * the parser's `?` empty-hole token, so explicit user-typed `?` tiles
 * round-trip distinct from implicit Grout. */

open Util;
open Base;

let default_implicit_hole = "\xc2\xbf";

let to_text =
    (~implicit_hole=default_implicit_hole, persisted: PersistentSegment.t)
    : string => {
  let z = PersistentSegment.restore(persisted);
  /* Projectors are unfolded to trigger syntax (`^^fold(body)` etc.) by
   * `Triggers.projector_to_invoke`, which `Printer.of_segment` already
   * uses by default. The parser reconstructs the projector wrapper from
   * the same trigger syntax via `Triggers.expand_projector`. */
  let segment = Zipper.unselect_and_zip(~erase_buffer=true, z);
  /* `~indent=""` keeps the output minimal: Printer would otherwise prepend
   * a space at each row's indent level, and those chars come back as
   * Secondary whitespace pieces, breaking structural round-trip. Pretty
   * formatting can be applied separately via `hazel format`. */
  Printer.of_segment(
    ~holes=implicit_hole,
    ~concave_holes=implicit_hole,
    ~indent="",
    ~refractors=z.refractors.manuals,
    segment,
  );
};

/* A marker piece is whatever the parser produced from inserting
 * `implicit_hole`. In practice that is a Tile with label `[implicit_hole]`
 * (the token round-trips through `Insert.go` unchanged). */
let is_marker = (~implicit_hole: string, p: piece): bool =>
  switch (p) {
  | Tile(t) => t.label == [implicit_hole]
  | _ => false
  };

/* Depth-first scan for the id of the next marker piece, anywhere in the
 * segment tree. We delete every marker — `remold_regrout` after each
 * destruct will insert Grout where shape requires it. */
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

let persist_from_text =
    (~implicit_hole=default_implicit_hole, ~root, text: string)
    : option(PersistentSegment.t) =>
  Option.map(
    PersistentSegment.persist,
    of_text(~implicit_hole, ~root, text),
  );

/* Turns printed hole markers back into real Grout. Grout (the editor's
   implicit holes) has no textual form, so the lossless printers
   (PersistentZipper/PersistentSegment/TextRoundtrip) render it as `¿`.
   Reparsing that text yields literal `¿` TILES, which the original
   program never had. [of_text] parses, then Destructs each marker tile;
   the remold/regrout pass that runs on every edit re-inserts Grout
   wherever shape requires it, reconstructing the original zipper. Sits
   below PersistentZipper so persistence loading can fall back to it;
   TextRoundtrip delegates here. */
open Util;
open Base;

let default_implicit_hole = Token.implicit_hole_marker;

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

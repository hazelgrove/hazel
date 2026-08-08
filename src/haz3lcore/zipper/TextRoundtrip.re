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

/* Marker detection/stripping lives in MarkerParse (below
   PersistentZipper, so slide loading can use it); delegated here. */
let of_text = (~implicit_hole=default_implicit_hole, ~root, text: string) =>
  MarkerParse.of_text(~implicit_hole, ~root, text);

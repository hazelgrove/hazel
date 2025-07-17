open Util;

let remove_projector: Piece.t => Segment.t =
  fun
  | Projector(pr) => Piece.unparenthesize(pr.syntax)
  | x => [x];

let measured_no_projectors = (segment: Segment.t) =>
  segment
  |> ZipperBase.MapPiece.of_segment(remove_projector)
  |> Measured.of_segment(_, ProjectorCore.Shape.Map.empty);

let add_caret =
    (caret: option((string, Point.t)), rows: list(string)): list(string) =>
  switch (caret) {
  | Some((caret_str, {row, col})) =>
    switch (ListUtil.split_nth_opt(row, rows)) {
    | Some((pre, caret_row, suf)) when col < String.length(caret_row) =>
      pre @ [StringUtil.insert_nth(col, caret_str, caret_row)] @ suf
    | Some((pre, caret_row, suf)) => pre @ [caret_row ++ caret_str] @ suf
    | None => rows
    }
  | None => rows
  };

let add_indent = (measured: Measured.t, indent: string, i: int, r: string) =>
  StringUtil.repeat(Measured.Rows.find(i, measured.rows).indent, indent) ++ r;

let add_indents = (segment, measured, indent: string, rows: list(string)) =>
  if (indent == "") {
    /* If no indentation is needed, we don't need to bother calculating measured */
    rows;
  } else {
    let measured =
      switch (measured) {
      | Some(m) => m
      | None => measured_no_projectors(segment)
      };
    List.mapi(add_indent(measured, indent), rows);
  };

/* Use this to pretty-print segments. Note that printing holes with
 * a space may result in extraneous whitespace, but printing without
 * a space may result in tokens getting glued together. You can't win */
let of_segment =
    (
      ~holes=" ",
      ~concave_holes=" ",
      ~indent=" ",
      ~caret: option((string, Point.t))=None,
      ~measured=?,
      segment: Segment.t,
    )
    : string =>
  segment
  |> Segment.to_string(~holes, ~concave_holes)
  |> String.split_on_char('\n')
  |> add_indents(segment, measured, indent)
  |> add_caret(caret)
  |> String.concat("\n");

/* Use this to pretty-print zippers. See above comments on holes */
let of_zipper =
    (~holes=?, ~concave_holes=?, ~indent=?, ~caret=?, z: Zipper.t): string => {
  let segment = Zipper.seg_without_buffer(z);
  /* Note that we can't just pass in the measured from editor as
   * we must recalculate the measured after removing projectors */
  let measured = measured_no_projectors(segment);
  let caret =
    Option.map(char => (char, Zipper.caret_point(measured, z)), caret);
  of_segment(~holes?, ~concave_holes?, ~indent?, ~caret, ~measured, segment);
};

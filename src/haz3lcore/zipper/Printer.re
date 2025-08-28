open Util;

let remove_projector: Piece.t => Segment.t =
  fun
  | Projector(pr) => Triggers.projector_to_invoke(pr)
  | x => [x];

let measured_no_projectors = (segment: Segment.t) =>
  segment
  |> ZipperBase.MapPiece.of_segment(remove_projector)
  |> Measured.of_segment(_, ProjectorCore.Shape.Map.empty);

let insert_string = (s: string, point: Point.t, rows: list(string)) => {
  switch (ListUtil.split_nth_opt(point.row, rows)) {
  | Some((pre, caret_row, suf)) when point.col < String.length(caret_row) =>
    pre @ [StringUtil.insert_nth(point.col, s, caret_row)] @ suf
  | Some((pre, caret_row, suf)) => pre @ [caret_row ++ s] @ suf
  | None => rows
  };
};

let add_caret =
    (
      ~caret: option((string, Point.t)),
      ~selection_anchor: option((string, Point.t)),
      rows: list(string),
    )
    : list(string) => {
  switch (caret, selection_anchor) {
  | (Some((caret_str, caret_point)), Some((anchor_str, anchor_point))) =>
    // Insert in reverse order to prevent offsetting the insertion position in the string
    if (Point.compare(caret_point, anchor_point) < 0) {
      insert_string(anchor_str, anchor_point, rows)
      |> insert_string(caret_str, caret_point);
    } else {
      insert_string(caret_str, caret_point, rows)
      |> insert_string(anchor_str, anchor_point);
    }
  | (Some((caret_str, caret_point)), None) =>
    insert_string(caret_str, caret_point, rows)
  | (None, Some((anchor_str, anchor_point))) =>
    insert_string(anchor_str, anchor_point, rows)
  | (None, None) => rows
  };
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
      ~selection_anchor: option((string, Point.t))=None,
      ~measured=?,
      segment: Segment.t,
    )
    : string =>
  segment
  |> Segment.to_string(
       ~holes,
       ~concave_holes,
       ~projector_to_segment=Triggers.projector_to_invoke,
     )
  |> String.split_on_char('\n')
  |> add_indents(segment, measured, indent)
  |> add_caret(~caret, ~selection_anchor)
  |> String.concat("\n");

/* Use this to pretty-print zippers. See above comments on holes */
let of_zipper =
    (
      ~holes=?,
      ~concave_holes=?,
      ~indent=?,
      ~caret=?,
      ~selection_anchor=?,
      z: Zipper.t,
    )
    : string => {
  let segment = Zipper.seg_without_buffer(z);
  /* Note that we can't just pass in the measured from editor as
   * we must recalculate the measured after removing projectors */
  let measured = measured_no_projectors(segment);
  let caret =
    Option.map(char => (char, Zipper.Caret.point(measured, z)), caret);
  let selection_anchor =
    Option.bind(selection_anchor, char =>
      Zipper.selection_anchor_point(measured, z)
      |> Option.map(pt => (char, pt))
    );

  of_segment(
    ~holes?,
    ~concave_holes?,
    ~indent?,
    ~caret,
    ~selection_anchor,
    ~measured,
    segment,
  );
};

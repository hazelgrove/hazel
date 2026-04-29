open Util;

let remove_projector: Piece.t => Segment.t =
  fun
  | Projector(pr) => Triggers.projector_to_invoke(pr)
  | x => [x];

let measured_no_projectors = (segment: Segment.t) =>
  segment
  |> ZipperBase.MapPiece.of_segment(remove_projector)
  |> Measured.of_segment(_, ProjectorCore.Shape.Map.empty, Id.Map.empty);

let insert_string = (s: string, point: Point.t, rows: list(string)) => {
  switch (ListUtil.split_nth_opt(point.row, rows)) {
  | Some((pre, caret_row, suf)) =>
    let idx = Token.column_to_grapheme_index(caret_row, point.col);
    pre @ [Token.insert_nth(idx, s, caret_row)] @ suf;
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
  try(
    StringUtil.repeat(Measured.Rows.find(i, measured.rows).indent, indent)
    ++ r
  ) {
  | Not_found =>
    print_endline("Printer.add_indent: Not_found");
    r;
  };

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

/* Adjust a column from clean-string space to holed-string space.
 * Walk both rows byte-by-byte; matching bytes advance both positions,
 * mismatches (injected hole chars) advance only the holed position.
 * When the clean position reaches the target column, the holed
 * position is the adjusted column. */
let adjust_col = (clean_row: string, holed_row: string, col: int): int => {
  let len_c = String.length(clean_row);
  let len_h = String.length(holed_row);
  let rec walk = (ci, hi) =>
    if (ci >= col) {
      hi;
    } else if (ci >= len_c || hi >= len_h) {
      hi + (col - ci);
    } else if (clean_row.[ci] == holed_row.[hi]) {
      walk(ci + 1, hi + 1);
    } else {
      walk(ci, hi + 1);
    };
  walk(0, 0);
};

let adjust_point =
    (clean_rows: list(string), holed_rows: list(string), point: Point.t)
    : Point.t =>
  switch (
    List.nth_opt(clean_rows, point.row),
    List.nth_opt(holed_rows, point.row),
  ) {
  | (Some(cr), Some(hr)) when cr != hr => {
      ...point,
      col: adjust_col(cr, hr, point.col),
    }
  | _ => point
  };

/* Use this to pretty-print segments. Note that printing holes with
 * a space may result in extraneous whitespace, but printing without
 * a space may result in tokens getting glued together. You can't win */
let of_segment =
    (
      ~holes=" ",
      ~concave_holes=" ",
      ~projector_to_segment=Triggers.projector_to_invoke,
      ~indent="",
      ~refractors=[],
      ~refractor_seg_to_seg=Triggers.refractor_seg_to_seg,
      ~caret: option((string, Point.t))=None,
      ~selection_anchor: option((string, Point.t))=None,
      ~measured=?,
      ~is_single_line=false,
      segment: Segment.t,
    )
    : string => {
  let to_rows = (~h, ~ch) =>
    segment
    |> Segment.to_string(
         ~holes=h,
         ~concave_holes=ch,
         ~refractors,
         ~refractor_seg_to_seg,
         ~projector_to_segment,
       )
    |> String.split_on_char('\n')
    |> (is_single_line ? Fun.id : add_indents(segment, measured, indent));

  let holed_rows = to_rows(~h=holes, ~ch=concave_holes);

  /* When caret/selection_anchor are present, adjust their columns to
   * account for hole strings injected by conflict detection. Measured
   * positions are based on the segment without holes; the holed string
   * may have extra characters at conflict boundaries. */
  let (caret, selection_anchor) =
    switch (caret, selection_anchor) {
    | (None, None) => (None, None)
    | _ =>
      let clean_rows = to_rows(~h="", ~ch="");
      let adj = ((s, pt)) => (s, adjust_point(clean_rows, holed_rows, pt));
      (Option.map(adj, caret), Option.map(adj, selection_anchor));
    };

  holed_rows |> add_caret(~caret, ~selection_anchor) |> String.concat("\n");
};

/* Use this to pretty-print zippers. See above comments on holes */
let of_zipper =
    (
      ~holes=?,
      ~concave_holes=?,
      ~projector_to_segment=?,
      ~indent=?,
      ~caret=?,
      ~selection_anchor=?,
      z: Zipper.t,
    )
    : string => {
  let segment = Zipper.unselect_and_zip(~erase_buffer=true, z);
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
    ~projector_to_segment?,
    ~indent?,
    ~refractors=z.refractors.manuals,
    ~caret,
    ~selection_anchor,
    ~measured,
    segment,
  );
};

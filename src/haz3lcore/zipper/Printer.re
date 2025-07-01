open Util;

let remove_all_projectors: Segment.t => Segment.t =
  ZipperBase.MapPiece.of_segment(
    fun
    | Projector(pr) => Piece.unparenthesize(pr.syntax)
    | x => [x],
  );

let measured_no_projectors = (segment: Segment.t) =>
  segment
  |> remove_all_projectors
  |> Measured.of_segment(_, ProjectorCore.Shape.Map.empty);

/* This is a low-level function; use below entry point */
let rec of_segment = (~holes=" ", ~concave_holes=" ", seg: Segment.t): string =>
  seg |> List.map(of_piece(~holes, ~concave_holes)) |> String.concat("")
and of_piece = (~holes, ~concave_holes, p: Piece.t): string =>
  switch (p) {
  | Tile(t) => of_tile(~holes, ~concave_holes, t)
  | Grout({shape: Concave, _}) => concave_holes
  | Grout({shape: Convex, _}) => holes
  | Secondary(w) =>
    Secondary.is_linebreak(w) ? "\n" : Secondary.get_string(w.content)
  | Projector(p) =>
    of_segment(~holes, ~concave_holes, Piece.unparenthesize(p.syntax))
  }
and of_tile = (~holes, ~concave_holes, t: Tile.t): string =>
  Aba.mk(t.shards, t.children)
  |> Aba.join(of_delim(t), of_segment(~holes, ~concave_holes))
  |> String.concat("")
and of_delim = (t: Piece.tile, i: int): string => List.nth(t.label, i);

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

let mk_indent = (segment, measured, indent: string, rows: list(string)) =>
  if (indent == "") {
    /* If no indentation is needed, we don't need to bother calculating measures */
    rows;
  } else {
    let measured =
      switch (measured) {
      | Some(m) => m
      | None => measured_no_projectors(segment)
      };
    List.mapi(
      (i, r) =>
        StringUtil.repeat(
          Measured.Rows.find(i, measured.rows).indent,
          indent,
        )
        ++ r,
      rows,
    );
  };

/* Use this to pretty-print segments */
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
  |> of_segment(~holes, ~concave_holes)
  |> String.split_on_char('\n')
  |> mk_indent(segment, measured, indent)
  |> add_caret(caret)
  |> String.concat("\n");

/* Use this to pretty-print zippers */
let of_zipper =
    (
      ~holes=" ",
      ~concave_holes: string=" ",
      ~indent=" ",
      ~caret=?,
      ~measured=?,
      z: Zipper.t,
    )
    : string => {
  let seg_of_zip = Zipper.seg_without_buffer;
  let (caret, measured) =
    switch (caret) {
    | None => (None, measured)
    | Some(char) =>
      let measured =
        switch (measured) {
        | None => z |> seg_of_zip |> measured_no_projectors
        | Some(m) => m
        };
      (Some((char, Zipper.caret_point(measured, z))), Some(measured));
    };
  of_segment(
    ~holes,
    ~concave_holes,
    ~indent,
    ~caret,
    ~measured?,
    seg_of_zip(z),
  );
};

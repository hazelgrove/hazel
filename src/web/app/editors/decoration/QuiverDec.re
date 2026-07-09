/* QuiverDec: GUI decoration for canonical completion visualization.
 *
 * Shows "arrows" (delimiters) ready to be "fired" (inserted) to complete
 * incomplete syntax. Displays:
 *   - Small triangles at insertion points (below text baseline)
 *   - Offside boxes showing what delimiters will be inserted
 *
 * Named to complement "Backpack" - the quiver holds completion arrows.
 */

open Virtual_dom.Vdom;
open Node;
open Haz3lcore;
open Util;

/* An insertion with its resolved position */
type positioned_insertion = {
  row: int,
  col: int,
  delimiters: list(CanonicalCompletion.delimiter_info),
};

/* Resolve an insertion's position by looking up adjacent_id in Measured */
let resolve_position =
    (measured: Measured.t, ins: CanonicalCompletion.insertion)
    : option(positioned_insertion) =>
  switch (Measured.find_by_id(ins.adjacent_id, measured)) {
  | None => None
  | Some(m) =>
    let (row, col) =
      switch (ins.side) {
      | Right => (m.last.row, m.last.col)
      | Left => (m.origin.row, m.origin.col)
      };
    Some({
      row,
      col,
      delimiters: ins.delimiters,
    });
  };

/* Display nodes for delimiters with their holes. A delimiter
   completing a prefix-token witness renders its typed prefix bold and
   the completed remainder faded. */
let delimiter_nodes =
    (delimiters: list(CanonicalCompletion.delimiter_info)): list(Node.t) =>
  delimiters
  |> List.mapi((k, d: CanonicalCompletion.delimiter_info) => {
       let sep = k > 0 ? [Node.text(" ")] : [];
       let body =
         switch (d.typed_len) {
         | Some(n) when n > 0 && n < String.length(d.text) => [
             Node.span(
               ~attrs=[Attr.classes(["quiver-typed"])],
               [Node.text(String.sub(d.text, 0, n))],
             ),
             Node.span(
               ~attrs=[Attr.classes(["quiver-completed"])],
               [
                 Node.text(String.sub(d.text, n, String.length(d.text) - n)),
               ],
             ),
           ]
         | _ => [Node.text(d.text)]
         };
       let suffix = d.needs_hole ? [Node.text(" ?")] : [];
       sep @ body @ suffix;
     })
  |> List.concat;

/* Offset from end of line content to offside display (in characters)
   — the standard offside offset shared with probes (ProjectorView) */
let offside_offset = 4;

/* Render a small downward-pointing triangle at an insertion point.
 * Positioned at the top of the text line, between characters. */
let arrow_view = (~font_metrics: FontMetrics.t, ~row: int, ~col: int): Node.t => {
  /* Triangle pointing down, shorter (1/3 height), same width */
  let triangle_path =
    SvgUtil.Path.[
      M({
        x: 0.0,
        y: 0.0,
      }), /* Top left */
      L({
        x: 0.4,
        y: 0.0,
      }), /* Top right */
      L({
        x: 0.2,
        y: 0.13,
      }), /* Bottom center (tip) */
      Z,
    ];
  DecUtil.code_svg(
    ~font_metrics,
    ~origin={
      row,
      col,
    },
    ~base_cls=["quiver-arrow"],
    ~path_cls=["quiver-arrow-path"],
    ~scale=0.4,
    /* Position at top of row - arrow tip points down to insertion point */
    ~height_fudge=0.85 *. font_metrics.row_height,
    triangle_path,
  );
};

/* Render an offside box showing delimiter content */
let offside_view =
    (~font_metrics: FontMetrics.t, ~row: int, ~left: int, body: list(Node.t))
    : Node.t =>
  div(
    ~attrs=[
      Attr.classes(["quiver-offside"]),
      Attr.create(
        "style",
        Printf.sprintf(
          "position: absolute; top: %fpx; left: %fpx;",
          float_of_int(row) *. font_metrics.row_height,
          float_of_int(left) *. font_metrics.col_width,
        ),
      ),
    ],
    body,
  );

/* Plain-text length of the delimiter display (for box layout) */
let delimiters_len =
    (delimiters: list(CanonicalCompletion.delimiter_info)): int =>
  delimiters
  |> List.map((d: CanonicalCompletion.delimiter_info) =>
       String.length(d.text) + (d.needs_hole ? 2 : 0)
     )
  |> List.fold_left((+), 0)
  |> (n => n + max(0, List.length(delimiters) - 1));

/* Get the rightmost column of a row (for offside positioning) */
let row_max_col = (row: int, measured: Measured.t): int =>
  switch (IntMap.find_opt(row, measured.rows)) {
  | None => 0
  | Some({max_col, _}) => max_col
  };

/* Main view function: renders quiver decorations for a segment */
let view =
    (~measured: Measured.t, ~font_metrics: FontMetrics.t, seg: Segment.t)
    : Node.t => {
  /* Get completion result with insertions */
  let result = CanonicalCompletion.for_editor(seg);
  let insertions = result.insertions;

  if (List.length(insertions) == 0) {
    /* No completions needed */
    div([]);
  } else {
    /* Resolve positions for all insertions */
    let positioned = List.filter_map(resolve_position(measured), insertions);

    /* Sort by row then column for consistent rendering */
    let sorted =
      List.sort(
        (a, b) => {
          let row_cmp = Int.compare(a.row, b.row);
          if (row_cmp != 0) {
            row_cmp;
          } else {
            Int.compare(a.col, b.col);
          };
        },
        positioned,
      );

    /* claims from the previous render must not accumulate */
    RowOffsets.reset();
    /* Track offside positions per row to place boxes side by side */
    let (arrows, offsides, _) =
      List.fold_left(
        ((arrows_acc, offsides_acc, row_offsets), ins) => {
          /* Render arrow */
          let arrow = arrow_view(~font_metrics, ~row=ins.row, ~col=ins.col);

          /* Compute offside position */
          let base_left =
            switch (IntMap.find_opt(ins.row, row_offsets)) {
            | Some(offset) => offset + 0 /* 0 char gap between boxes */
            | None => row_max_col(ins.row, measured) + offside_offset
            };

          /* Render offside box */
          let offside =
            offside_view(
              ~font_metrics,
              ~row=ins.row,
              ~left=base_left,
              delimiter_nodes(ins.delimiters),
            );

          /* Update row offset for next box on same row, and publish
             the claimed extent so probe offside displays stack after
             the quiver instead of overlapping it */
          let text_width = delimiters_len(ins.delimiters) + 2; /* +2 padding */
          RowOffsets.claim(~row=ins.row, ~until_col=base_left + text_width);
          let new_offsets =
            IntMap.add(ins.row, base_left + text_width, row_offsets);

          ([arrow, ...arrows_acc], [offside, ...offsides_acc], new_offsets);
        },
        ([], [], IntMap.empty),
        sorted,
      );

    div(
      ~attrs=[Attr.classes(["quiver-decorations"])],
      List.rev(arrows) @ List.rev(offsides),
    );
  };
};

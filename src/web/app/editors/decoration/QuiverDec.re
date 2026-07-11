/* QuiverDec: GUI decoration for canonical completion visualization.
 *
 * Shows "arrows" (delimiters) ready to be "fired" (inserted) to complete
 * incomplete syntax. Displays:
 *   - Small triangles at insertion points (below text baseline)
 *   - Offside boxes showing what delimiters will be inserted
 *
 * The quiver holds completion arrows.
 */

open Virtual_dom.Vdom;
open Haz3lcore.QuiverLayout;
open Node;
open Haz3lcore;
open Util;

/* Does this chip hold the shard tab would put down right now? */
/* Chip text scale relative to the code font */
let chip_font_scale = 0.72;

let matches_droppable =
    (
      droppable: option((Id.t, int)),
      delimiters: list(CanonicalCompletion.delimiter_info),
    )
    : bool =>
  switch (droppable) {
  | None => false
  | Some((tid, k)) =>
    delimiters
    |> List.exists((d: CanonicalCompletion.delimiter_info) =>
         switch (d.of_shard) {
         | Some((tid', k')) => Id.equal(tid, tid') && k == k'
         | None => false
         }
       )
  };

/* Chip segments: the remainder is the payload (full contrast); the
   typed prefix and later coalesced segments fade. */
let delimiter_nodes =
    (
      ~font_metrics: FontMetrics.t,
      ~on_apply: option(Id.t => Ui_effect.t(unit)),
      delimiters: list(CanonicalCompletion.delimiter_info),
    )
    : list(Node.t) =>
  delimiters
  |> List.mapi((k, d: CanonicalCompletion.delimiter_info) => {
       /* F1: no space before commas/closers (", ?, ?)" not ", ? , ? )") */
       let sep =
         k > 0
         && !(
              String.length(d.text) > 0
              && (
                switch (d.text.[0]) {
                | ','
                | ')'
                | ']'
                | '}' => true
                | _ => false
                }
              )
            )
           ? [Node.text(" ")] : [];
       let seg_cls = k > 0 ? ["chip-seg", "chip-seg-later"] : ["chip-seg"];
       /* modifier-click completes this delimiter's tile; unmodified
          pointer events fall through to the editor */
       let apply_attrs =
         switch (on_apply, d.of_shard) {
         | (Some(f), Some((tid, _))) => [
             Attr.on_pointerdown(evt =>
               Js_of_ocaml.Js.to_bool(evt##.metaKey)
               || Js_of_ocaml.Js.to_bool(evt##.ctrlKey)
                 ? Effect.Many([
                     Effect.Stop_propagation,
                     Effect.Prevent_default,
                     f(tid),
                   ])
                 : Effect.Ignore
             ),
           ]
         | _ => []
         };
       let body =
         switch (d.typed_len) {
         | Some(n) when n > 0 && n < String.length(d.text) => [
             Node.span(
               ~attrs=[Attr.classes(["chip-frac-typed"])],
               [Node.text(String.sub(d.text, 0, n))],
             ),
             Node.span(
               ~attrs=[Attr.classes(["chip-frac-rest"])],
               [
                 Node.text(String.sub(d.text, n, String.length(d.text) - n)),
               ],
             ),
           ]
         | _ => [Node.text(d.text)]
         };
       let suffix =
         d.needs_hole
           ? [
             Node.text(" "),
             EmptyHoleDec.view(
               FontMetrics.{
                 col_width: font_metrics.col_width *. chip_font_scale,
                 row_height: font_metrics.row_height *. chip_font_scale,
               },
               Grout.Convex,
             ),
           ]
           : [];
       sep
       @ [Node.span(~attrs=[Attr.classes(seg_cls)] @ apply_attrs, body)]
       @ suffix;
     })
  |> List.concat;

/* One interline chip: bubble centered on the line boundary above
   the insertion point, pole below. */
let chip_view =
    (
      ~font_metrics: FontMetrics.t,
      ~row: int,
      ~col: int,
      ~shape: option(Direction.t),
      ~caret_form: option((Direction.t, option(Direction.t))),
      ~live: bool,
      ~at_caret: bool,
      ~body_shift: float=0.0,
      body: list(Node.t),
    )
    : Node.t => {
  let x = float_of_int(col) *. font_metrics.col_width;
  let y = float_of_int(row) *. font_metrics.row_height;
  /* the pole is a ghost caret: the path the real caret would draw
     here; hidden at coincidence */
  let pole =
    DecUtil.code_svg(
      ~font_metrics,
      ~origin={
        row,
        col,
      },
      ~base_cls=["quiver-chip-pole"],
      ~path_cls=["quiver-chip-pole-path"],
      ~scale=1.0,
      ~height_fudge=ShardDec.shadow_dy *. font_metrics.row_height,
      CaretDec.caret_base_path(Direction.Right, shape),
    );
  /* flag left edge = top-left corner of whichever caret stands at
     its foot: x = -(shape_adjust + caret_width/2) */
  let (dock_side, dock_shape) =
    switch (at_caret, caret_form) {
    | (true, Some((cs, csh))) => (cs, csh)
    | _ => (Direction.Right, shape)
    };
  let body_left =
    -. (
      ShardDec.shape_adjust(dock_side, dock_shape)
      +. 0.5
      *. CaretDec.caret_width
    )
    *. font_metrics.col_width
    +. body_shift;
  div(
    ~attrs=[
      Attr.classes(
        ["quiver-chip"]
        @ (
          switch (dock_shape) {
          | Some(Direction.Left) => ["chip-bend-left"]
          | Some(Right) => ["chip-bend-right"]
          | None => ["chip-straight"]
          }
        )
        @ (live ? ["chip-live"] : [])
        @ (at_caret ? ["chip-at-caret"] : []),
      ),
    ],
    [
      pole,
      div(
        ~attrs=[
          Attr.classes(["quiver-chip-anchor"]),
          Attr.create(
            "style",
            Printf.sprintf("left: %fpx; top: %fpx;", x, y),
          ),
        ],
        [
          div(
            ~attrs=[
              Attr.classes(["quiver-chip-body"]),
              Attr.create("style", Printf.sprintf("left: %fpx;", body_left)),
            ],
            body,
          ),
        ],
      ),
    ],
  );
};

/* Main view function: renders quiver decorations for a segment */
let view =
    (
      ~measured: Measured.t,
      ~font_metrics: FontMetrics.t,
      ~droppable: option((Id.t, int))=None,
      ~caret_pos: option((int, int))=None,
      ~caret_form: option((Direction.t, option(Direction.t)))=None,
      ~on_apply: option(Id.t => Ui_effect.t(unit))=None,
      ~assist: list(CanonicalCompletion.insertion),
      /* the engine must see the user's REAL program: the display
         segment (CachedSyntax) still contains the suggestion-buffer
         ghost, which perturbs placement (an in anchoring at line
         start while a ghost completes Bo -> Bool). Anchor pieces
         exist in both segments, so engine insertions resolve fine
         against the display's measured map. */
      ~engine_seg: Segment.t,
      seg: Segment.t,
    )
    : Node.t => {
  ignore(seg);
  let seg = engine_seg;
  /* A1: chips render THE assist stream (computed once in
     CachedStatics) — the same list the ghost and Tab consume */
  let insertions = assist;

  /* reset even when nothing draws: a vanished quiver must not leave
     stale row claims displacing probe offsides */
  RowOffsets.reset();

  if (List.length(insertions) == 0) {
    /* No completions needed */
    div([]);
  } else {
    let positioned =
      List.filter_map(
        resolve_position(~seg, ~caret_pos, measured),
        insertions,
      );
    let sorted =
      List.sort(
        (a, b) => {
          let row_cmp = Int.compare(a.row, b.row);
          row_cmp != 0 ? row_cmp : Int.compare(a.col, b.col);
        },
        positioned,
      );
    let chips =
      layout_overlaps(~col_width=font_metrics.col_width, sorted)
      |> List.map(((ins: positioned_insertion, body_shift)) =>
           chip_view(
             ~font_metrics,
             ~row=ins.row,
             ~col=ins.col,
             ~shape=ins.shape,
             ~caret_form,
             ~live=matches_droppable(droppable, ins.delimiters),
             ~at_caret=caret_pos == Some((ins.row, ins.col)),
             ~body_shift,
             delimiter_nodes(~font_metrics, ~on_apply, ins.delimiters),
           )
         );
    div(~attrs=[Attr.classes(["quiver-decorations"])], chips);
  };
};

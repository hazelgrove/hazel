/* QuiverDec: GUI decoration for canonical completion visualization.
 *
 * Shows "arrows" (delimiters) ready to be "fired" (inserted) to complete
 * incomplete syntax. Displays:
 *   - Small triangles at insertion points (below text baseline)
 *   - Offside boxes showing what delimiters will be inserted
 *
 * The quiver holds completion arrows.
 *
 * OWNERSHIP IS NOT DECIDED HERE: the caret's records come in as
 * `owned` (CompletionQuery.chips_among — the list Tab dispatches) and
 * QuiverLayout draws them as one bubble at the caret.
 *
 * Chip drawing follows completion-provenance (c546efcee1): a resting
 * chip points at its insertion site with a short CSS tail; the caret's
 * chip docks to the caret's top edge, or rides a flagpole above the
 * line when the Flag display mode is on. Placement (which chips, where,
 * coalescing) stays QuiverLayout's.
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

/* Chip text scaled the way the CSS scales the body. */
let chip_metrics = (font_metrics: FontMetrics.t): FontMetrics.t => {
  col_width: font_metrics.col_width *. chip_font_scale,
  row_height: font_metrics.row_height *. chip_font_scale,
};

let hole_glyph = (~font_metrics: FontMetrics.t, shape: Grout.shape): Node.t =>
  EmptyHoleDec.view((chip_metrics(font_metrics), shape, EmptyHoleDec.Boxed));

/* The implicit marker in a completion payload is rendered as a hole, never
   as source text. Literal spaces are retained by the chip's white-space CSS. */
let padding_nodes =
    (~font_metrics: FontMetrics.t, ~shape=Grout.Convex, text: string)
    : list(Node.t) =>
  Token.to_list(text)
  |> List.map(c =>
       c == Token.implicit_hole_marker
         ? hole_glyph(~font_metrics, shape) : Node.text(c)
     );

/* F1: no space before commas/closers (", ?, ?)" not ", ? , ? )") */
let hugs_left = (text: string): bool =>
  String.length(text) > 0
  && (
    switch (text.[0]) {
    | ','
    | ')'
    | ']'
    | '}' => true
    | _ => false
    }
  );

/* Chip segments: the remainder is the payload (full contrast); the
   typed prefix and later coalesced segments fade. The caret's chip may
   carry `head_padding` — the spacing and implicit hole Tab will type,
   from CompletionQuery.padding — so the preview shows what acceptance
   produces; other chips derive their padding from the record. */
let delimiter_nodes =
    (
      ~font_metrics: FontMetrics.t,
      ~on_apply: option(Id.t => Ui_effect.t(unit)),
      ~head_padding: option((string, string))=None,
      delimiters: list(CanonicalCompletion.delimiter_info),
    )
    : list(Node.t) =>
  delimiters
  |> List.mapi((k, d: CanonicalCompletion.delimiter_info) => {
       let (before, after) =
         switch (k, head_padding) {
         | (0, Some(padding)) => padding
         | _ => (
             (k > 0 && !hugs_left(d.text) ? " " : "")
             ++ (d.leading_hole ? Token.implicit_hole_marker ++ " " : ""),
             Option.is_some(d.trailing_hole) ? " " : "",
           )
         };
       let sep = padding_nodes(~font_metrics, before);
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
         padding_nodes(
           ~font_metrics,
           ~shape=Option.value(~default=Grout.Convex, d.trailing_hole),
           after
           ++ (
             Option.is_some(d.trailing_hole) ? Token.implicit_hole_marker : ""
           ),
         );
       sep
       @ [Node.span(~attrs=[Attr.classes(seg_cls)] @ apply_attrs, body)]
       @ suffix;
     })
  |> List.concat;

/* Backpack.main's top-edge rule (before cefc46bb86): rise at most four
   rows, with a special first-row position above the editor. */
let flagpole_top = (~row: int, ~row_height: float): float => {
  let displacement = min(row, 4);
  let baseline = float_of_int(row - displacement + (row == 0 ? 0 : 1));
  (baseline -. 1.33) *. row_height;
};

/* Match the actual caret's top edge: no overlap with its chevron and
   no fixed pixel width. Backpack used a side/shape-specific pixel offset;
   sharing CaretDec's edge also keeps this correct when font size changes. */
let flagpole_geometry =
    (
      ~font_metrics: FontMetrics.t,
      ~row,
      ~col,
      ~caret_form: option((Direction.t, option(Direction.t))),
    )
    : DecUtil.fdims => {
  let (side, shape) =
    Option.value(caret_form, ~default=(Direction.Right, None));
  let (edge_left, edge_width) = CaretDec.top_edge(side, shape);
  let top = flagpole_top(~row, ~row_height=font_metrics.row_height);
  {
    top,
    left: (float_of_int(col) +. edge_left) *. font_metrics.col_width,
    width: edge_width *. font_metrics.col_width,
    height: float_of_int(row) *. font_metrics.row_height -. top,
  };
};

let flagpole_view = (~font_metrics, ~row, ~col, ~caret_form, body) => {
  let {top, left, width, height}: DecUtil.fdims =
    flagpole_geometry(~font_metrics, ~row, ~col, ~caret_form);
  div(
    ~attrs=[
      Attr.classes([
        "quiver-chip",
        "chip-live",
        "quiver-flagpole",
        "floating-fixed",
      ]),
      Attr.create("data-float-anchor-class", "code-container"),
      Attr.create("data-float-local-top", Float.to_string(top)),
      Attr.create("data-float-local-left", Float.to_string(left)),
      Attr.create("data-float-min-top", "2"),
      Attr.create("data-float-local-bottom", Float.to_string(top +. height)),
      Attr.create(
        "style",
        "position: fixed; visibility: hidden; top: 0; left: 0;",
      ),
    ],
    [
      div(
        ~attrs=[
          Attr.classes(["quiver-flagpole-stem"]),
          Attr.create(
            "style",
            Printf.sprintf(
              "width: %fpx; height: max(0px, calc(%fpx - var(--float-top-shift, 0px)));",
              width,
              height,
            ),
          ),
        ],
        [],
      ),
      div(~attrs=[Attr.classes(["quiver-chip-body"])], body),
    ],
  );
};

/* One interline chip: bubble centered on the line boundary above the
   insertion point. The caret's chip docks to the caret's top edge;
   a resting chip points at its site with the CSS tail, offset by
   QuiverLayout's coalescing shift. */
let chip_view =
    (
      ~font_metrics: FontMetrics.t,
      ~row: int,
      ~col: int,
      ~caret_form: option((Direction.t, option(Direction.t))),
      ~live: bool,
      ~at_caret: bool,
      ~body_shift: float=0.0,
      body: list(Node.t),
    )
    : Node.t => {
  let x = float_of_int(col) *. font_metrics.col_width;
  let y = float_of_int(row) *. font_metrics.row_height;
  let body_left =
    (
      switch (at_caret, caret_form) {
      | (true, Some((side, shape))) =>
        fst(CaretDec.top_edge(side, shape)) *. font_metrics.col_width
      | _ => 0.
      }
    )
    +. body_shift;
  div(
    ~attrs=[
      Attr.classes(
        ["quiver-chip"]
        @ (live ? ["chip-live"] : [])
        @ (at_caret ? ["chip-at-caret"] : []),
      ),
    ],
    [
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
      ~flagpole=false,
      ~head_padding: option((string, string))=None,
      ~droppable: option((Id.t, int))=None,
      ~caret_pos: option((int, int))=None,
      ~caret_form: option((Direction.t, option(Direction.t)))=None,
      ~on_apply: option(Id.t => Ui_effect.t(unit))=None,
      ~assist: list(CanonicalCompletion.insertion),
      /* the caret's chips (CompletionQuery.chips_among over the same
         stream): what Tab acts on, drawn as the bubble at the caret */
      ~owned: list(CanonicalCompletion.insertion),
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

  switch (
    QuiverLayout.layout(
      ~measured,
      ~col_width=font_metrics.col_width,
      ~caret_pos,
      ~owned,
      ~seg,
      insertions,
    )
  ) {
  | [] =>
    /* No completions needed */
    div([])
  | bubbles =>
    let chips =
      bubbles
      |> List.map(((ins: positioned_insertion, body_shift)) => {
           let body =
             delimiter_nodes(
               ~font_metrics,
               ~on_apply,
               ~head_padding=ins.owned ? head_padding : None,
               ins.delimiters,
             );
           flagpole && ins.owned
             ? flagpole_view(
                 ~font_metrics,
                 ~row=ins.row,
                 ~col=ins.col,
                 ~caret_form,
                 body,
               )
             : chip_view(
                 ~font_metrics,
                 ~row=ins.row,
                 ~col=ins.col,
                 ~caret_form,
                 /* live = what Tab does: the caret's bubble when there is
                    one, else the chip holding Put_down's shard */
                 ~live=
                   ins.owned
                   || owned == []
                   && matches_droppable(droppable, ins.delimiters),
                 ~at_caret=ins.owned,
                 ~body_shift,
                 body,
               );
         });
    div(~attrs=[Attr.classes(["quiver-decorations"])], chips);
  };
};

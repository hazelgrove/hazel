/* QuiverDec: GUI decoration for canonical completion visualization.
 *
 * Shows "arrows" (delimiters) ready to be "fired" (inserted) to complete
 * incomplete syntax. Displays:
 *   - Small triangles at insertion points (below text baseline)
 *   - Offside boxes showing what delimiters will be inserted
 *
 * The quiver holds completion arrows.
 *
 * OWNERSHIP IS NOT DECIDED HERE. Which records the caret owns (the
 * bubble Tab acts on) comes in as `owned`, computed once by
 * CompletionQuery.chips_at_caret from the zipper; this layer draws
 * that list as ONE bubble at the caret and rests every other record
 * at its anchor. Deriving ownership again from measured coordinates
 * is how display and Tab drifted apart (a bubble reading "else ? end
 * in" while Tab typed end; zones leaking past a child segment). */

open Virtual_dom.Vdom;
open Node;
open Haz3lcore;
open Util;

/* An insertion with its resolved position; shape = the caret shape
   at the pin (the pole is a ghost caret). idx = the record's index in
   the engine's insertion list, which is landing-site order in the
   COMPLETED program — display decisions that need an order between
   resting chips use it, never pixel positions. owned = the caret's
   bubble (the only chip Tab acts on). */
type positioned_insertion = {
  idx: int,
  row: int,
  col: int,
  shape: option(Util.Direction.t),
  owned: bool,
  delimiters: list(CanonicalCompletion.delimiter_info),
};

/* Chip text scale relative to the code font */
let chip_font_scale = 0.72;

/* Does this chip hold the shard Put_down would drop right now? */
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

/* A record the caret does NOT own rests at its anchor: the left
   content edge when it shares the pin's line (the round-6 snap), the
   raw anchor point otherwise. Openers anchored Left on content sit
   at that content's origin. */
let rest_position =
    (
      ~idx: int,
      ~seg: Segment.t,
      measured: Measured.t,
      ins: CanonicalCompletion.insertion,
    )
    : option(positioned_insertion) =>
  switch (CanonicalCompletion.anchor_point(measured, ins)) {
  | None => None
  | Some(anchor) =>
    let (row, col) = (anchor.row, anchor.col);
    let is_free = Segment.skip_secondary_and_grout;
    switch (Segment.find_ctx(seg, ins.adjacent_id)) {
    | None =>
      Some({
        idx,
        row,
        col,
        shape: None,
        owned: false,
        delimiters: ins.delimiters,
      })
    | Some((sg, i, p)) =>
      let left_edge =
        is_free(p)
          ? Segment.prev_content(~skip=is_free, sg, i)
            |> Option.map(snd)
            |> Option.map(q => Measured.find_by_id(Piece.id(q), measured))
            |> Option.join
            |> Option.map((qm: Measured.measurement) =>
                 (qm.last.row, qm.last.col)
               )
          : None;
      let (row, col) =
        switch (left_edge) {
        | Some((lr, lc)) when lr == row => (row, min(lc, col))
        | _ => (row, col)
        };
      /* ghost-caret shape at the pin: the shared-nib facing between
         the pieces around the insertion point, mirroring
         Siblings.direction_between (right neighborhood first) */
      let shape = {
        let (before, after) = Util.ListUtil.split_n(i + 1, sg);
        switch (Segment.edge_direction_of(Left, after)) {
        | None => Segment.edge_direction_of(Right, before)
        | d => d
        };
      };
      Some({
        idx,
        row,
        col,
        shape,
        owned: false,
        delimiters: ins.delimiters,
      });
    };
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
       let sep = k > 0 ? [Node.text(" ")] : [];
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
    *. font_metrics.col_width;
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

/* Plain-text length of a chip's delimiters (for overlap coalescing) */
let delimiters_len =
    (delimiters: list(CanonicalCompletion.delimiter_info)): int =>
  delimiters
  |> List.map((d: CanonicalCompletion.delimiter_info) =>
       String.length(d.text) + (d.needs_hole ? 2 : 0)
     )
  |> List.fold_left((+), 0)
  |> (n => n + max(0, List.length(delimiters) - 1));

/* Overlapping same-row chips coalesce into ONE bubble. The merged
   bubble's text never follows pixel order (a wandering pin would
   flip it): the caret's OWNED bubble leads and the merge is drawn at
   the caret — Tab acts on its first delimiter, so a resting neighbor
   merged in by mere overlap trails; among resting chips, engine order
   (landing-site order in the completed program) and the engine-first
   member's pin. Overlap is judged on DRAWN extents and iterated to a
   fixpoint: a merge can move the bubble to a member's pin and onto a
   neighbor it was clear of before. */
let coalesce_overlaps =
    (~font_metrics: FontMetrics.t, chips: list(positioned_insertion))
    : list(positioned_insertion) => {
  let chip_w = (c: positioned_insertion) =>
    float_of_int(delimiters_len(c.delimiters) + 2)
    *. font_metrics.col_width
    *. chip_font_scale;
  let left_px = (c: positioned_insertion) =>
    float_of_int(c.col) *. font_metrics.col_width;
  let finalize = (members: list(positioned_insertion)) => {
    let by_idx =
      List.sort(
        (a: positioned_insertion, b: positioned_insertion) =>
          Int.compare(a.idx, b.idx),
        members,
      );
    let (owned, others) =
      List.partition((c: positioned_insertion) => c.owned, by_idx);
    let ordered = owned @ others;
    {
      ...List.hd(ordered),
      delimiters: List.concat_map(m => m.delimiters, ordered),
    };
  };
  let by_pos =
      (bs: list((positioned_insertion, list(positioned_insertion)))) =>
    List.sort(
      ((a: positioned_insertion, _), (b: positioned_insertion, _)) =>
        compare((a.row, a.col), (b.row, b.col)),
      bs,
    );
  let rec pass = (acc, rest) =>
    switch (acc, rest) {
    | (_, []) => List.rev(acc)
    | ([], [b, ...tl]) => pass([b], tl)
    | ([(prev, pm), ...acc_tl], [(c, cm), ...tl]) =>
      prev.row == c.row && left_px(c) < left_px(prev) +. chip_w(prev) +. 4.
        ? {
          let members = pm @ cm;
          pass([(finalize(members), members), ...acc_tl], tl);
        }
        : pass([(c, cm), (prev, pm), ...acc_tl], tl)
    };
  let rec fixpoint = bs => {
    let bs' = pass([], by_pos(bs));
    List.length(bs') < List.length(bs) ? fixpoint(bs') : bs';
  };
  chips |> List.map(c => (c, [c])) |> fixpoint |> List.map(fst);
};

/* The bubble list the view draws: the owned records as one bubble at
   the caret, every other record resting at its anchor, sorted by
   position, overlaps coalesced. Shared with the tests so what they
   pin is what renders. */
let bubbles =
    (
      ~measured: Measured.t,
      ~font_metrics: FontMetrics.t,
      ~caret_pos: option((int, int)),
      ~owned: list(CanonicalCompletion.insertion),
      seg: Segment.t,
    )
    : list(positioned_insertion) => {
  let insertions = CanonicalCompletion.for_editor(seg).insertions;
  /* records are unique per (anchor, side) after the engine's
     coalesce_insertions — match on that, not physical identity */
  let is_owned = (ins: CanonicalCompletion.insertion) =>
    List.exists(
      (o: CanonicalCompletion.insertion) =>
        Id.equal(o.adjacent_id, ins.adjacent_id) && o.side == ins.side,
      owned,
    );
  let resting =
    insertions
    |> List.mapi((idx, ins) =>
         is_owned(ins)
           ? (None: option(positioned_insertion))
           : rest_position(~idx, ~seg, measured, ins)
       )
    |> List.filter_map(x => x);
  let caret_bubble =
    switch (caret_pos, owned) {
    | (Some((row, col)), [_, ..._]) => [
        {
          idx: (-1),
          row,
          col,
          shape: None,
          owned: true,
          delimiters:
            List.concat_map(
              (ins: CanonicalCompletion.insertion) => ins.delimiters,
              owned,
            ),
        },
      ]
    | _ => []
    };
  let sorted =
    List.sort(
      (a: positioned_insertion, b: positioned_insertion) => {
        let row_cmp = Int.compare(a.row, b.row);
        row_cmp != 0 ? row_cmp : Int.compare(a.col, b.col);
      },
      caret_bubble @ resting,
    );
  coalesce_overlaps(~font_metrics, sorted);
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
      /* the caret's chips (CompletionQuery.chips_at_caret): what Tab
         acts on, drawn as the bubble at the caret */
      ~owned: list(CanonicalCompletion.insertion),
      /* the engine segment, not CachedSyntax's display segment: the
         display still contains the suggestion-buffer ghost, which
         perturbs placement. Anchor pieces exist in both, so engine
         insertions resolve fine against the display's measured map. */
      seg: Segment.t,
    )
    : Node.t => {
  /* reset even when nothing draws: a vanished quiver must not leave
     stale row claims displacing probe offsides */
  RowOffsets.reset();

  switch (bubbles(~measured, ~font_metrics, ~caret_pos, ~owned, seg)) {
  | [] =>
    /* No completions needed */
    div([])
  | bs =>
    let chips =
      bs
      |> List.map((ins: positioned_insertion) =>
           chip_view(
             ~font_metrics,
             ~row=ins.row,
             ~col=ins.col,
             ~shape=ins.shape,
             ~caret_form,
             /* live = what Tab does: the caret's bubble when there is
                one, else the chip holding Put_down's shard */
             ~live=
               ins.owned
               || owned == []
               && matches_droppable(droppable, ins.delimiters),
             ~at_caret=ins.owned,
             delimiter_nodes(~font_metrics, ~on_apply, ins.delimiters),
           )
         );
    div(~attrs=[Attr.classes(["quiver-decorations"])], chips);
  };
};

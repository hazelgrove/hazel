open Util;
open Util.WebUtil;
open Haz3lcore;

let shard_svg =
    (
      ~start_shape: ShardDec.tip,
      measurement: Measured.measurement,
      p: Piece.t,
    )
    : (Measured.measurement, (ShardDec.tip, ShardDec.tip)) => (
  measurement,
  switch (p) {
  | Tile(t) => t |> Tile.shapes |> ShardDec.tips_of_shapes
  | Grout(g) => g |> Grout.shapes |> ShardDec.tips_of_shapes
  | Secondary(_) => (
      Option.map(
        (s: Nib.Shape.t) =>
          switch (s) {
          | Concave(_) => Nib.Shape.Convex
          | Convex => Nib.Shape.Concave(0)
          },
        start_shape,
      ),
      None,
    )
  | Projector(p) => p |> ProjectorCore.shapes |> ShardDec.tips_of_shapes
  },
);

let multiline_shard =
    (
      num_lb: int,
      {origin, last}: Measured.measurement,
      tips: (option(Nib.Shape.t), option(Nib.Shape.t)),
    ) =>
  List.init(num_lb + 1, i =>
    [
      Some((
        Measured.{
          origin: {
            row: origin.row + i,
            col: origin.col,
          },
          last: {
            row: origin.row + i,
            col: last.col,
          },
        },
        (i == 0 ? fst(tips) : None, i == num_lb ? snd(tips) : None),
      )),
      None,
    ]
  )
  |> List.concat;

let of_segment =
    (
      ~measured: Measured.t,
      ~shape_map: ProjectorCore.Shape.Map.t,
      ~font_metrics: FontMetrics.t,
      ~shape_init: ShardDec.tip,
      ~clss: list(string),
      segment: Segment.t,
    ) => {
  let find_g = Measured.find_g(~msg="Highlight.of_piece", _, measured);
  let find_w = Measured.find_w(~msg="Highlight.of_piece", _, measured);
  let rec of_piece =
          (start_shape: ShardDec.tip, p: Piece.t)
          : (
              ShardDec.tip,
              list(
                option(
                  (Measured.measurement, (ShardDec.tip, ShardDec.tip)),
                ),
              ),
            ) => {
    let shard_data =
      switch (p) {
      | Tile(t) => of_tile(~start_shape, t)
      | Projector(p) => of_projector(~start_shape, p)
      | Grout(g) => [Some(shard_svg(~start_shape, find_g(g), p))]
      | Secondary(w) when Secondary.is_linebreak(w) => [None]
      | Secondary(w) => [
          Some((
            find_w(w),
            (start_shape |> Option.map(Nib.Shape.flip), start_shape),
          )),
        ]
      };
    let next_start_shape =
      switch (Piece.nibs(p)) {
      | None => start_shape
      | Some((_, {shape, _})) => Some(shape)
      };
    (next_start_shape, shard_data);
  }
  and of_tile = (~start_shape, t: Tile.t): list(option(_)) => {
    let shards = Measured.find_shards(~msg="sel_of_tile", t, measured);
    let tile_shards =
      shards
      |> List.filter_map(((i, m)) =>
           List.mem(i, t.shards) ? Some((i, m)) : None
         )
      |> List.map(((index, m)) => {
           let token = List.nth(t.label, index);
           let shard = Tile.shard_of(t, index);
           switch (StringUtil.num_linebreaks(token)) {
           | 0 => [Some(shard_svg(~start_shape, m, Tile(shard)))]
           | num_lb =>
             multiline_shard(num_lb, m, (Some(Convex), Some(Convex)))
           };
         });
    let shape_at = index => Some(snd(Mold.nibs(~index, t.mold)).shape);
    let children_shards =
      t.children |> List.mapi(index => of_segment(shape_at(index)));
    if (List.length(tile_shards) != List.length(children_shards) + 1) {
      failwith(
        "Highlight.of_tile: shard mismatch:"
        ++ "tile: "
        ++ Tile.show(t)
        ++ "tile_Shards:"
        ++ string_of_int(List.length(tile_shards))
        ++ ", children_Shards:"
        ++ string_of_int(List.length(children_shards)),
      );
    };
    ListUtil.interleave(tile_shards, children_shards) |> List.flatten;
  }
  and of_projector = (~start_shape, p: Base.projector): list(option(_)) =>
    switch (Measured.find_pr_opt(p, measured)) {
    | None => failwith("Highlight.of_projector: missing measurement")
    | Some(_m) =>
      let shape = ProjectorCore.Shape.Map.lookup(p.id, shape_map);
      /* Handling this internal to ProjectorsView at the moment because the
       * commented-out strategy doesn't work well, since the inserted str8-
       * edged lines vertical edge placement doesn't account for whether
       * the initial/final rows begin/end as concave/convex, and hence are
       * of slightly different lengths than is desirable */
      // multiline_shard(
      //   StringUtil.num_linebreaks(token),
      //   m,
      //   (Some(Convex), Some(Convex)),
      // );
      let num_lb =
        switch (shape.vertical) {
        | Inline => 0
        | Tab(num_lbs) => num_lbs
        | Block(num_lbs) => num_lbs
        };
      if (num_lb == 0) {
        [
          Some(
            shard_svg(
              ~start_shape,
              Measured.find_pr(p, measured),
              Projector(p),
            ),
          ),
        ];
      } else {
        List.init(num_lb + 1, _ => None);
      };
    }
  and of_segment =
      (start_shape: ShardDec.tip, seg: Segment.t): list(option(_)) =>
    seg |> List.fold_left_map(of_piece, start_shape) |> snd |> List.flatten
  and go = (segment: Segment.t, shape_init: ShardDec.tip, clss): list(Node.t) =>
    /* We draw a single deco per row by dividing partionining the shards
     * into linebreak-seperated segments, then combining the measurements
     * and shapes of the first and last shard of each segment. Ideally we
     * could just get this info from the row measurements, but we have no
     * current way of figuring out shapes for whitespace without traversing */
    of_segment(shape_init, segment)
    |> ListUtil.split_at_nones
    |> ListUtil.first_and_last
    |> List.map((((m1, (l1, _)), (m2, (_, r2)))) =>
         (
           Measured.{
             origin: m1.origin,
             last: m2.last,
           },
           (l1, r2),
         )
       )
    |> List.map(((measurement, tips)) =>
         ShardDec.simple(
           {
             font_metrics,
             measurement,
             tips,
           },
           clss,
         )
       );
  go(segment, shape_init, clss);
};

let selection =
    (
      ~measured: Measured.t,
      ~shape_map: ProjectorCore.Shape.Map.t,
      ~font_metrics: FontMetrics.t,
      ~statics: CachedStatics.t,
      z: Zipper.t,
    ) => {
  print_endline(Zipper.pp_zipper(z));
  print_endline("Selection content:");
  List.iter(
    piece => print_endline("  " ++ Zipper.pp_piece(piece)),
    z.selection.content,
  );
  let find_assoc_for_id = (id: Id.t): list(Id.t) => {
    /* Compute associative IDs for a given tile ID */
    // The idea here is that with left-associative operators, we
    // know where to find the "left" argument even if the subtree
    // is not a leaf node.
    //
    // For example, if we have the expression `1 + 2 + 3` and want to grab
    // 2 + 3, we know this is represented as (1 + 2) + 3 under the hood.
    // To grab the 2, we step left once from the +, and then step right
    // upon finding that we were looking at a +.
    // If, however, we had (1 * 2) + 3, we step left once, find *, and
    // return the ID of that subterm (1 * 2).
    let statics_opt = Language.Statics.Map.lookup(id, statics.info_map);
    switch (statics_opt) {
    | Some(InfoExp(exp)) =>
      switch (exp.term.term) {
      // | BinOp(op, left, right) when Language.Operators.is_associative_op(op) =>
      | BinOp(op, left, right) =>
        let left_id = left |> Language.Exp.rep_id;
        let right_id = right |> Language.Exp.rep_id;
        let left_assoc =
          switch (Language.Statics.Map.lookup(left_id, statics.info_map)) {
          | Some(InfoExp(left_contents)) =>
            switch (left_contents.term.term) {
            | BinOp(left_op, _, left_right) when left_op == op =>
              left_right |> Language.Exp.rep_id
            | _ => left_id
            }
          | _ => left_id
          };

        // Special selection snapping handling: if the associative operators we
        // select are BinOps, we grab the left/right child of that BinOp
        let left_left_id =
          switch (Language.Statics.Map.lookup(left_assoc, statics.info_map)) {
          | Some(InfoExp(left_contents)) =>
            switch (left_contents.term.term) {
            // only grab a child if it's a BinOp, otherwise return the original
            | BinOp(_, left_left, _) =>
              print_endline(
                "Left child of binOp: "
                ++ Language.Operators.show_binop(op)
                ++ " at tile "
                ++ Zipper.short_id(left_assoc)
                ++ " is "
                ++ Zipper.short_id(left_left |> Language.Exp.rep_id),
              );
              left_left |> Language.Exp.rep_id;
            | _ => left_assoc
            }
          | _ => left_assoc
          };
        let right_assoc =
          switch (Language.Statics.Map.lookup(right_id, statics.info_map)) {
          | Some(InfoExp(right_contents)) =>
            switch (right_contents.term.term) {
            // only grab a child if it's a BinOp, otherwise return the original
            | BinOp(_, _, right_right) => right_right |> Language.Exp.rep_id
            | _ => right_id
            }
          | _ => right_id
          };
        [left_left_id, right_assoc];
      | _ => [id]
      }
    | _ => [id]
    };
  };

  let associative_segment = (z: Zipper.t): Segment.t => {
    print_endline(Zipper.pp_zipper(z));
    print_endline("Selection content:");
    List.iter(
      piece => print_endline("  " ++ Zipper.pp_piece(piece)),
      z.selection.content,
    );

    /* Extract all Tile IDs from the selection segment */
    let tile_ids =
      z.selection.content
      |> List.filter_map(piece =>
           switch (piece) {
           | Piece.Tile(t) => Some(Tile.id(t))
           | Piece.Secondary(s) => Some(Secondary.id(s))
           | _ => None
           }
         );
    List.iter(
      id => print_endline("Tile ID: " ++ Zipper.short_id(id)),
      tile_ids,
    );
    /* Compute associative IDs for every selected tile */
    let assoc_ids = tile_ids |> List.concat_map(find_assoc_for_id);
    print_endline(
      "Assoc IDs: "
      ++ String.concat(", ", List.map(id => Zipper.short_id(id), assoc_ids)),
    );
    switch (assoc_ids) {
    | [] => z.selection.content
    | assoc_ids =>
      let unique_segment =
        Zipper.zip(z)
        |> List.fold_left(
             (acc, piece) => {
               let pid = Piece.id(piece);
               if (List.exists(id => id == pid, assoc_ids)
                   && !List.exists(p => Piece.id(p) == pid, acc)) {
                 acc @ [piece];
               } else {
                 acc;
               };
             },
             [],
           );
      print_endline(
        "IDs drawn in selection: "
        ++ String.concat(
             ", ",
             List.map(
               piece => Zipper.short_id(Piece.id(piece)),
               unique_segment,
             ),
           ),
      );
      unique_segment;
    };
  };
  div_c(
    "selects",
    of_segment(
      ~measured,
      ~shape_map,
      ~font_metrics,
      ~shape_init=Some(fst(Siblings.shapes(z.relatives.siblings))),
      ~clss=["selected", Selection.buffer_cls(z.selection)],
      associative_segment(z),
    ),
  );
};

let color =
    (
      ~syntax: CachedSyntax.t,
      ~font_metrics: FontMetrics.t,
      clss: list(string),
      id: Id.t,
    ) =>
  switch (TermData.segment(id, syntax.term_data)) {
  | Some(segment) =>
    of_segment(
      ~measured=syntax.measured,
      ~shape_map=syntax.shape_map,
      ~font_metrics,
      ~shape_init=Some(Convex),
      ~clss,
      segment,
    )
  | None => []
  };

let colors =
    (
      ~font_metrics: FontMetrics.t,
      ~syntax: CachedSyntax.t,
      color_highlights: option(ColorSteps.colorMap),
    ) =>
  div_c(
    "color-highlights",
    List.concat_map(
      ((id, c)) =>
        color(~syntax, ~font_metrics, ["highlight-code-" ++ c], id),
      switch (color_highlights) {
      | Some(colorMap) => ColorSteps.to_list(colorMap)
      | _ => []
      },
    ),
  );

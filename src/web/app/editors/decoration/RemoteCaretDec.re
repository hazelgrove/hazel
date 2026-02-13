open Util;
open WebUtil;
open Haz3lcore;

/* Remote caret rendering for collaborative cursor display.
   Similar to CaretDec but with custom color and no blinking.
   Includes optional name label above the caret. */

let caret_width = 0.2; /* Same width as main caret */

let caret_base_path = (side, shape): list(SvgUtil.Path.cmd) =>
  ShardDec.chonky_path_base(
    (shape, shape),
    ShardDec.shape_adjust(side, shape) +. 0.5 *. caret_width,
    caret_width,
    float_of_int(0),
  );

/* Truncate name for display:
   - If there's a space, use first word only
   - If longer than 8 chars, truncate with ellipsis */
let truncate_name = (name: string): string => {
  /* If there's a space, take first word */
  let name =
    switch (String.index_opt(name, ' ')) {
    | Some(i) => String.sub(name, 0, i)
    | None => name
    };
  /* If longer than 8 chars, truncate with ellipsis */
  String.length(name) > 8 ? String.sub(name, 0, 7) ++ "…" : name;
};

let main =
    (
      ~user_id: string,
      ~user_name: option(string),
      ~font_metrics: FontMetrics.t,
      ~color: string,
      ~origin: Point.t,
      ~side: Direction.t,
      ~shape: option(Direction.t),
    ) => {
  /* Create an SVG caret with custom color, no blink animation */
  let scale = 1.0;
  let height_fudge = ShardDec.shadow_dy *. font_metrics.row_height;
  let paths = caret_base_path(side, shape);

  let caret_svg =
    Node.create_svg(
      "svg",
      ~attrs=[
        Attr.classes(["remote-caret-svg"]),
        Attr.create("viewBox", Printf.sprintf("0 0 %f %f", scale, scale)),
        Attr.create("preserveAspectRatio", "none"),
      ],
      [
        SvgUtil.Path.view(
          ~attrs=[
            Attr.classes(["remote-caret-path"]),
            Attr.create("style", "fill: " ++ color ++ ";"),
          ],
          paths,
        ),
      ],
    );

  /* Optional name label above the caret */
  let label =
    switch (user_name) {
    | None => []
    | Some(name) =>
      let display_name = truncate_name(name);
      [
        Node.div(
          ~attrs=[
            Attr.classes(["remote-caret-label"]),
            Attr.create("style", "background-color: " ++ color ++ ";"),
          ],
          [Node.text(display_name)],
        ),
      ];
    };

  /* Container div holds both label and caret, gets the ID for animation */
  Node.div(
    ~attrs=[
      Attr.id("remote-caret-" ++ user_id),
      Attr.classes(["remote-caret"]),
      DecUtil.abs_position(~font_metrics, ~height_fudge, ~scale, origin),
    ],
    label @ [caret_svg],
  );
};

/* Look up measurement for a piece, using shard_index for multi-shard tiles.
   For tiles, shard_index identifies which specific shard's measurement to use.
   For non-tiles, shard_index is None and we use the standard lookup. */
let find_shard_measurement =
    (piece_id: Id.t, shard_index: option(int), measured: Measured.t)
    : option(Measured.measurement) => {
  switch (shard_index) {
  | None => Measured.find_by_id(piece_id, measured)
  | Some(idx) =>
    /* For tiles with shard_index, look up the specific shard */
    switch (Id.Map.find_opt(piece_id, measured.tiles)) {
    | Some(shards) =>
      switch (List.assoc_opt(idx, shards)) {
      | Some(m) => Some(m)
      | None => Measured.find_by_id(piece_id, measured) /* Fallback */
      }
    | None => Measured.find_by_id(piece_id, measured) /* Fallback */
    }
  };
};

/* Render a remote caret at the position of a piece.
   shard_index: For tiles, which shard (needed for multi-shard tiles like let/in)
   caret_offset: 0 = Outer (at piece's left edge), n = Inner(n-1) (n columns into the piece)
   shape: caret shape at piece boundaries (None when inside a piece)
   side: which edge of the piece the caret is on (Left = left edge, Right = right edge at end of segment) */
let view =
    (
      ~user_id: string,
      ~user_name: option(string),
      ~measured: Measured.t,
      ~font_metrics: FontMetrics.t,
      ~color: string,
      ~piece_id: Id.t,
      ~shard_index: option(int),
      ~caret_offset: int,
      ~shape: option(Direction.t),
      ~side: option(Direction.t),
    )
    : option(Node.t) => {
  switch (find_shard_measurement(piece_id, shard_index, measured)) {
  | None => None /* Piece not found in current layout */
  | Some(measurement) =>
    let origin = measurement.origin;
    /* Calculate position based on side:
       - Left side (normal): origin position + caret_offset
       - Right side (end of segment): last position (end of piece) */
    let position =
      switch (side) {
      | Some(Direction.Right) =>
        Point.{
          row: measurement.last.row,
          col: measurement.last.col,
        }
      | _ =>
        Point.{
          row: origin.row,
          col: origin.col + caret_offset,
        }
      };
    /* Visual side for caret shape rendering */
    let visual_side =
      switch (side) {
      | Some(Direction.Right) => Direction.Right /* At right edge */
      | Some(Direction.Left) => Direction.Left /* At left edge */
      | None => caret_offset == 0 ? Direction.Left : Direction.Right /* Fallback for inside */
      };
    Some(
      main(
        ~user_id,
        ~user_name,
        ~font_metrics,
        ~color,
        ~origin=position,
        ~side=visual_side,
        ~shape,
      ),
    );
  };
};

/* Render all remote carets */
let view_all =
    (~measured: Measured.t, ~font_metrics: FontMetrics.t): list(Node.t) => {
  PatchworkComm.get_remote_carets()
  |> List.filter_map(((user_id, rc: PatchworkComm.remote_caret)) =>
       view(
         ~user_id,
         ~user_name=rc.user_name,
         ~measured,
         ~font_metrics,
         ~color=rc.color,
         ~piece_id=rc.piece_id,
         ~shard_index=rc.shard_index,
         ~caret_offset=rc.caret_offset,
         ~shape=rc.shape,
         ~side=rc.side,
       )
     );
};

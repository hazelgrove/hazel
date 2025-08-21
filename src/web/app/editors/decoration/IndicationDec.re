open Util;
open Haz3lcorep;
open Virtual_dom.Vdom;
open SvgUtil;
open Measured;
open SvgUtil.Path;

/* This module is responsible for drawing the term indication decorations,
 * consisting of hexagonal delimiter decorations and the paths between them */

type path = list(Path.cmd);
type positioned_path = (Point.t, path);
type tile_data = list((Id.t, Mold.t, Shards.t));

let svg =
    (
      ~font_metrics: FontMetrics.t,
      ~path_cls: list(string),
      (origin, path): positioned_path,
    )
    : Node.t =>
  DecUtil.code_svg(~font_metrics, ~origin, ~path_cls, path);

let shards_of_tiles = (tiles: tile_data) =>
  tiles
  |> List.concat_map(((_, _, shards)) => shards)
  |> List.sort((m1, m2) => Point.compare(snd(m1).origin, snd(m2).origin));

let rep_tips = (tiles: tile_data) => {
  assert(tiles != []);
  let (_, rep_mold, _) = List.hd(tiles);
  let (l, r) = rep_mold.nibs;
  let (l, r) = ShardDec.tips_of_shapes((l.shape, r.shape));
  (
    Option.map(Nib.Shape.direction_of(Left), l),
    Option.map(Nib.Shape.direction_of(Right), r),
  );
};

let min_col = (~first: Point.t, ~last: Point.t, ~rows: Rows.t): int =>
  min(
    first.col,
    Rows.min_col(ListUtil.range(~lo=first.row, last.row + 1), rows),
  );

let m_horizontal = (~hx, ~first: Point.t, ~last: Point.t): path => [
  m(~x=0, ~y=1) |> cmdfudge(~x=hx),
  h(~x=last.col - first.col),
];

let hook = (hx, x, y) =>
  L_({
    dx: float_of_int(x) *. hx,
    dy: float_of_int(y) *. hx /. 2.,
  });

let l_horizontal = (~hx: float, ~first: Point.t, ~last: Point.t): path => [
  m(~x=0, ~y=1) |> cmdfudge(~x=-. hx, ~y=-. hx /. 2.),
  hook(hx, 1, 1),
  h(~x=last.col - first.col),
];

let r_horizontal = (~hx: float, ~first: Point.t, ~last: Point.t): path => [
  m(~x=0, ~y=1),
  h(~x=last.col - first.col),
  hook(hx, 1, -1),
];

/* This draws a C-shaped path with chamfered corners opening to the right.
 * The corners have radius `hx`, and the C is intended to be drawn starting
 * at the point `first`, with the initial (top) stroke of the the C going
 * towards the left, then down, then right. If the C would have no bottom
 * edge, that is, when the last point aligns with min_col, the C ends one
 * line early. */
let base_path =
    (~hx: float, ~min_col: int, ~first: Point.t, ~last: Point.t): path => {
  let v_delta = last.col == min_col ? 0 : 1;
  [
    h(~x=min_col - first.col),
    hook(hx, -1, 1),
    v(~y=last.row - first.row + v_delta) |> cmdfudge(~y=-. hx /. 2.),
    hook(hx, 1, 1),
    h(~x=last.col - first.col),
  ];
};

/* This draws a C-shaped path without edge hooks */
let m_vertical =
    (~hx: float, ~min_col: int, ~first: Point.t, ~last: Point.t): path => [
  m(~x=0, ~y=1) |> cmdfudge(~x=hx),
  ...base_path(~hx, ~min_col, ~first, ~last),
];

/* This draws a C-shaped path with a hook on the right */
let r_vertical =
    (~hx: float, ~min_col: int, ~first: Point.t, ~last: Point.t): path =>
  [
    m(~x=0, ~y=1) |> cmdfudge(~x=hx),
    ...base_path(~hx, ~min_col, ~first, ~last),
  ]
  @ [hook(hx, 1, -1)];

/* This draws a C-shaped path with a hook on the left */
let l_vertical =
    (~hx: float, ~min_col: int, ~first: Point.t, ~last: Point.t): path => {
  let vf_delta = first.col == min_col ? 0 : 1;
  let edge_case =
    last.row - first.row == 1 && last.col == min_col && first.col != min_col;
  [
    m(~x=0, ~y=vf_delta)
    |> cmdfudge(~y=float_of_int(vf_delta) *. (-. hx) /. 2.),
    hook(hx, - vf_delta, vf_delta) /* hacky; don't draw if vf_delta==0 */
  ]
  @ (
    edge_case
      ? [h(~x=min_col - first.col)]
      : base_path(~hx, ~min_col, ~first, ~last)
  );
};

/* This draws the inner lines between shards; that is, lines other than
 * those that have one end not touching a shard. These are drawn by considering
 * the shards divided into lists representing rows. Pairs of shards in the same list,
 * and hence onthe same row, get a horizontal line between them (`m_horizontal`).
 * Pairs of shards that span two lists, and hence rows, get C-shaped vertical paths
 * between them (`m_vertical`))  */
let inner_lines =
    (~shard_rows: list(Shards.t), ~hx: float, ~min_col: int)
    : list(positioned_path) => {
  let horizontals =
    shard_rows
    |> List.map(ListUtil.neighbors)
    |> List.concat_map(
         List.map(((l, r)) => {
           let first = snd(l).origin;
           let last = snd(r).origin;
           (first, m_horizontal(~hx, ~first, ~last));
         }),
       );
  let verticals =
    shard_rows
    |> ListUtil.neighbors
    |> List.filter_map(((l, r)) => {
         assert(l != []);
         assert(r != []);
         let first = snd(List.hd(l)).origin;
         let last = snd(List.hd(r)).origin;
         if (last.row > first.row) {
           Some((first, m_vertical(~first, ~last, ~min_col, ~hx)));
         } else {
           None;
         };
       });
  horizontals @ verticals;
};

/* Draws a path between the leftwards edge of the term and the tile's
 * first shard. If these are on the same line, this is just a horizontal
 * line with a hook on the left side ('l_horizontal`); otherwise, it's a
 * C-shaped path between the points, extending leftward to the minimum
 * enclosed leftward column containing program text. */
let l_path =
    (~min_col: int, ~last: Point.t, ~hx: float, ~first: Point.t)
    : list(positioned_path) =>
  if (last.row > first.row) {
    [(first, l_vertical(~hx, ~first, ~last, ~min_col))];
  } else if (Point.compare(last, first) > 0) {
    [(first, l_horizontal(~hx, ~first, ~last))];
  } else {
    [];
  };

/* See l-path */
let r_path =
    (~min_col: int, ~first: Point.t, ~hx: float, ~last: Point.t)
    : list(positioned_path) =>
  if (last.row > first.row) {
    [(first, r_vertical(~hx, ~first, ~last, ~min_col))];
  } else if (Point.compare(last, first) > 0) {
    [(first, r_horizontal(~hx, ~first, ~last))];
  } else {
    [];
  };

/* This draws the paths which connect the leftwards edge of the term to
 * the term's leftwardsmost shard, pairs of shards, and the rightwardsmost
 * shard to the rightwards edge of the term */
let paths =
    (
      tiles: tile_data,
      line_clss: list(string),
      font_metrics: FontMetrics.t,
      rows: Rows.t,
      (first, last): (Point.t, Point.t),
    )
    : list(Node.t) =>
  switch (tiles) {
  | [] => []
  | [(_, {out: sort, _}, _), ..._] =>
    let shards = shards_of_tiles(tiles);
    assert(shards != []);
    let path_cls = ["child-line", Sort.to_string(sort)] @ line_clss;
    let hx = abs_float(ShardDec.offset_of(fst(rep_tips(tiles))));
    let min_col = min_col(~first, ~last, ~rows);
    let shard_rows = Shards.split_by_row(shards);
    List.concat([
      l_path(~hx, ~min_col, ~first, ~last=snd(List.hd(shards)).origin),
      r_path(~hx, ~min_col, ~first=snd(ListUtil.last(shards)).last, ~last),
      inner_lines(~hx, ~min_col, ~shard_rows),
    ])
    |> List.map(svg(~font_metrics, ~path_cls));
  };

/* This draws the shards backing decorations,
 * i.e. the hexagons under the term's delimiters */
let shards =
    (
      ~attr: option(list(Attr.t))=?,
      ~font_metrics: FontMetrics.t,
      ~base_clss: option(string),
      tiles: tile_data,
    )
    : list(Node.t) =>
  List.concat_map(
    ((_, mold: Mold.t, shards: list(Shards.shard))) =>
      List.map(
        ((index: int, measurement: Measured.measurement)) =>
          ShardDec.simple(
            ~attr?,
            {
              font_metrics,
              measurement,
              tips: ShardDec.tips_of_shapes(Mold.nib_shapes(~index, mold)),
            },
            Option.to_list(base_clss)
            @ ["indicated", Sort.to_string(mold.out)],
          ),
        shards,
      ),
    tiles,
  );

/* This draws the indication decoration for a term, comprising shard
 * decorations and paths between the shards and the edges of the term */
let term =
    (
      ~attr: option(list(Attr.t))=?,
      ~font_metrics: FontMetrics.t,
      ~rows: Rows.t,
      ~tiles: tile_data,
      ~line_clss: list(string)=[],
      ~base_clss: option(string)=?,
      range: (Point.t, Point.t),
    )
    : list(Node.t) =>
  shards(~attr?, ~font_metrics, ~base_clss, tiles)
  @ paths(tiles, line_clss, font_metrics, rows, range);

let term =
    (
      ~term_data: TermData.t,
      ~terms: TermMap.t,
      ~measured: Measured.t,
      ~font_metrics: FontMetrics.t,
      ~attr: option(list(Attr.t))=?,
      tile: Tile.t,
    )
    : list(Node.t) => {
  let msg = "IndicationDec.term";
  let id = tile.id; //Language.Any.rep_id(Id.Map.find(tile.id, terms));
  let (p_l, p_r) = TermData.extremes(id, term_data);
  let l = Measured.find_p(~msg, p_l, measured).origin;
  let r = Measured.find_p(~msg, p_r, measured).last;
  let of_tile = (id: Id.t) => {
    let tile: Tile.t = TermData.root_tile(id, term_data);
    (id, tile.mold, Measured.find_shards(~msg, tile, measured));
  };
  let tiles =
    Id.Map.find(id, terms) |> Language.Any.ids |> List.map(of_tile);
  term(~font_metrics, ~rows=measured.rows, ~tiles, (l, r), ~attr?);
};

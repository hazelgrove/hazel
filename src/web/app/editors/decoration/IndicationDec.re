open Util;
open Haz3lcore;
open Virtual_dom.Vdom;
open SvgUtil;
open Measured;
open SvgUtil.Path;

type path = list(Path.cmd);
type positioned_path = (Point.t, path);
type tile_data = list((Id.t, Mold.t, Shards.t));

let shadowfudge = Path.cmdfudge(~y=ShardDec.shadow_dy /. 2.);

let svg =
    (
      ~font_metrics: FontMetrics.t,
      ~path_cls: list(string),
      (origin, path): positioned_path,
    )
    : Node.t =>
  DecUtil.code_svg(~font_metrics, ~origin, ~path_cls, path);

let shards_of_tiles = tiles =>
  tiles
  |> List.concat_map(((_, _, shards)) => shards)
  |> List.sort(((_, m1: measurement), (_, m2: measurement)) =>
       Point.compare(m1.origin, m2.origin)
     );

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

let first_of_last_row = (shards: Shards.t): measurement => {
  let shard_rows = Shards.split_by_row(shards);
  assert(shard_rows != []);
  let row = ListUtil.last(shard_rows);
  assert(row != []);
  snd(List.hd(row));
};

let l_horizontal = (~offset, ~first: Point.t, ~last: Point.t): path => [
  m(~x=0, ~y=1) |> cmdfudge(~x=abs_float(offset)) |> shadowfudge,
  h(~x=last.col - first.col),
];

let hook = (hx, x, y) =>
  L_({
    dx: float_of_int(x) *. hx,
    dy: float_of_int(y) *. hx /. 2.,
  });

let l_horizontal_hooked =
    (~offset: float, ~first: Point.t, ~last: Point.t): path => {
  let hx = abs_float(offset);
  [
    m(~x=0, ~y=1)
    |> cmdfudge(~y=-. hx /. 2.)
    |> cmdfudge(~x=-. hx)
    |> shadowfudge,
    hook(hx, 1, 1),
    h(~x=last.col - first.col),
  ];
};

let r_horizontal_hooked =
    (~offset: float, ~first: Point.t, ~last: Point.t): path => {
  let hx = abs_float(offset);
  [
    m(~x=0, ~y=1) |> shadowfudge,
    h(~x=last.col - first.col),
    hook(hx, 1, -1),
  ];
};

let core_path =
    (~v_delta, ~offset: float, ~min_col: int, ~first: Point.t, ~last: Point.t)
    : path => {
  let hx = abs_float(offset);
  [
    h(~x=min_col - first.col),
    hook(hx, -1, 1),
    v(~y=last.row - first.row + v_delta)
    |> cmdfudge(~y=-. hx /. 2.)
    |> shadowfudge,
    hook(hx, 1, 1),
    h(~x=last.col - first.col),
  ];
};

let m_path =
    (~offset: float, ~min_col: int, ~first: Point.t, ~last: Point.t): path => {
  let v_delta = last.col == min_col ? 0 : 1;
  let hx = abs_float(offset);
  [
    m(~x=0, ~y=1) |> cmdfudge(~x=hx) |> shadowfudge,
    ...core_path(~v_delta, ~offset, ~min_col, ~first, ~last),
  ];
};

let r_uni_path =
    (~offset: float, ~min_col: int, ~first: Point.t, ~last: Point.t): path => {
  let v_delta = last.col == min_col ? 0 : 1;
  let hx = abs_float(offset);
  [
    m(~x=0, ~y=1) |> cmdfudge(~x=hx) |> shadowfudge,
    ...core_path(~v_delta, ~offset, ~min_col, ~first, ~last),
  ]
  @ [hook(abs_float(offset), 1, -1)];
};

let l_uni_path =
    (~offset: float, ~min_col: int, ~first: Point.t, ~last: Point.t): path => {
  let v_delta = first.col == min_col ? 0 : 1;
  let hx = abs_float(offset);
  [
    m(~x=0, ~y=v_delta) |> cmdfudge(~y=hx /. 2.) |> shadowfudge,
    hook(hx, - v_delta, - v_delta), /* hacky; don't draw if v_delta==0 */
    ...core_path(~v_delta, ~offset, ~min_col, ~first, ~last),
  ];
};

let m_h_line = (~offset, ~first, ~last): positioned_path => (
  first,
  l_horizontal(~offset, ~first, ~last),
);

let m_v_line =
    (~rows: Rows.t, ~offset, ~first: Point.t, ~last: Point.t)
    : option(positioned_path) =>
  if (last.row - first.row == 1 && last.col == first.col) {
    None;
  } else if (last.row - first.row == 0) {
    None;
  } else {
    let min_col = min_col(~first, ~last, ~rows);
    Some((first, m_path(~first, ~last, ~min_col, ~offset)));
  };

let l_line =
    (~rows: Rows.t, ~offset: float, ~first: Point.t, ~last: Point.t)
    : option(positioned_path) =>
  if (Point.compare(last, first) > 0 && first.row == last.row) {
    Some((first, l_horizontal_hooked(~offset, ~first, ~last)));
  } else if (Point.compare(last, first) > 0) {
    let min_col = min_col(~first, ~last, ~rows);
    Some((first, l_uni_path(~offset, ~min_col, ~first, ~last)));
  } else {
    None;
  };

let r_line =
    (
      ~rows: Rows.t,
      ~shards: Shards.t,
      ~offset: float,
      ~first: measurement,
      ~last: Point.t,
    )
    : option(positioned_path) =>
  if (last.row == first.last.row && last.col > first.last.col) {
    Some((
      first.last,
      r_horizontal_hooked(~offset, ~first=first.last, ~last),
    ));
  } else if (last.row > first.last.row) {
    let first = first_of_last_row(shards).origin;
    let min_col = min_col(~first, ~last, ~rows);
    Some((first, r_uni_path(~first, ~last, ~offset, ~min_col)));
  } else {
    None;
  };

let inner_lines =
    (~rows: Rows.t, ~offset: float, ~shards: Shards.t): list(positioned_path) => {
  let shard_rows = Shards.split_by_row(shards);
  let horizontals =
    shard_rows
    |> List.map(ListUtil.neighbors)
    |> List.concat_map(
         List.map((((_, l: measurement), (_, r: measurement))) =>
           m_h_line(~offset, ~first=l.origin, ~last=r.origin)
         ),
       );
  let verticals =
    shard_rows
    |> ListUtil.neighbors
    |> List.filter_map(((l, r)) => {
         assert(l != []);
         assert(r != []);
         m_v_line(
           ~rows,
           ~offset,
           ~first=snd(List.hd(l)).origin,
           ~last=snd(List.hd(r)).origin,
         );
       });
  horizontals @ verticals;
};

let outer_lines =
    (
      ~rows: Rows.t,
      ~shards: Shards.t,
      ~offset: float,
      (first: Point.t, last: Point.t),
    )
    : list(positioned_path) => {
  assert(shards != []);
  let l_line =
    l_line(~rows, ~offset, ~first, ~last=snd(List.hd(shards)).origin)
    |> Option.to_list;
  let r_line =
    r_line(
      ~rows,
      ~offset,
      ~last,
      ~shards,
      ~first=snd(ListUtil.last(shards)),
    )
    |> Option.to_list;
  l_line @ r_line;
};

let lines =
    (
      tiles: tile_data,
      line_clss: list(string),
      font_metrics: FontMetrics.t,
      rows: Rows.t,
      range: (Point.t, Point.t),
    )
    : list(Node.t) =>
  switch (tiles) {
  | [] => []
  | [(_, {out: sort, _}, _), ..._] =>
    let path_cls = ["child-line", Sort.to_string(sort)] @ line_clss;
    let offset = -. ShardDec.offset_of(fst(rep_tips(tiles)));
    let shards = shards_of_tiles(tiles);
    List.concat([
      outer_lines(~rows, ~offset, ~shards, range),
      inner_lines(~rows, ~offset, ~shards),
    ])
    |> List.map(svg(~font_metrics, ~path_cls));
  };

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

let term =
    (
      ~attr: option(list(Attr.t))=?,
      ~font_metrics: FontMetrics.t,
      ~rows: Rows.t,
      ~tiles: tile_data,
      ~line_clss: list(string),
      ~base_clss: option(string)=?,
      range: (Point.t, Point.t),
    )
    : list(Node.t) =>
  shards(~attr?, ~font_metrics, ~base_clss, tiles)
  @ lines(tiles, line_clss, font_metrics, rows, range);

let error_term =
    (
      ~font_metrics: FontMetrics.t,
      ~rows: Rows.t,
      range: (Point.t, Point.t),
      tiles: tile_data,
    ) => {
  let shard_of = (mold, (index, measurement)) =>
    ShardDec.simple(
      {
        font_metrics,
        measurement,
        tips: ShardDec.tips_of_shapes(Mold.nib_shapes(~index, mold)),
      },
      ["error"],
    );
  let shard_decos =
    List.concat_map(
      ((_, mold, shards)) => List.map(shard_of(mold), shards),
      tiles,
    );
  shard_decos @ lines(tiles, [], font_metrics, rows, range);
};

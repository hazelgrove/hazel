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

let horizontal_path = (l: measurement, r: measurement, offset: float): path => [
  M({
    x: offset,
    y: 1.0,
  })
  |> shadowfudge,
  H({x: float_of_int(r.origin.col - l.origin.col)}),
];

let horizontal_line =
    (~offset, ((_, l: measurement), (_, r: measurement))): positioned_path => (
  l.origin,
  SvgUtil.Path.[
    shadowfudge(
      M({
        x: offset,
        y: 1.0,
      }),
    ),
    H({x: float_of_int(r.origin.col - l.origin.col)}),
  ],
);

let hook = (hx, x, y) =>
  L_({
    dx: float_of_int(x) *. hx,
    dy: float_of_int(y) *. hx /. 2.,
  });

let vertical_path = (l: Point.t, r: Point.t, indent: int, offset: float): path => {
  let v_delta = r.col == indent ? (-1) : 0;
  let hx = abs_float(offset);
  let hy = hx /. 2.;
  r.row - l.row <= 1
    ? []
    : [
      M({
        x: hx +. float_of_int(indent - l.col),
        y: 1.0,
      })
      |> shadowfudge,
      hook(hx, -1, 1),
      V_({dy: float_of_int(r.row - l.row + v_delta) -. 2. *. hy})
      |> shadowfudge,
      hook(hx, 1, 1),
      H_({dx: float_of_int(r.col - indent)}),
    ];
};

let vertical_line =
    (~rows: Rows.t, ~offset, (l: Shards.t, r: Shards.t)): positioned_path => {
  assert(l != []);
  assert(r != []);
  let origin_l = snd(List.hd(l)).origin;
  let origin_r = snd(List.hd(r)).origin;
  let indent = Rows.find(origin_l.row, rows).indent;
  (origin_l, vertical_path(origin_l, origin_r, indent, offset));
};

let inner_lines =
    (~rows: Rows.t, ~offset: float, ~shards: Shards.t): list(positioned_path) => {
  let shard_rows = Shards.split_by_row(shards);
  let horizontals =
    shard_rows
    |> List.map(ListUtil.neighbors)
    |> List.concat_map(List.map(horizontal_line(~offset)));
  let verticals =
    shard_rows
    |> ListUtil.neighbors
    |> List.map(vertical_line(~rows, ~offset));
  horizontals @ verticals;
};

let l_horizontal_hooked =
    (~offset: float, ~first: Point.t, ~last: Point.t): path => [
  shadowfudge(m(~x=0, ~y=1)),
  h(~x=first.col - last.col),
  hook(abs_float(offset), -1, -1),
];

let r_horizontal_hooked =
    (~offset: float, ~first: measurement, ~last: Point.t): path => [
  m(
    ~x=first.last.col - first.origin.col,
    ~y=first.last.row - first.origin.row + 1,
  )
  |> shadowfudge,
  h(~x=last.col - first.origin.col),
  hook(abs_float(offset), 1, -1),
];

let l_uni_path =
    (~offset: float, ~indent: int, ~first: Point.t, ~last: Point.t): path => {
  let hx = abs_float(offset);
  last.row - first.row <= 1
    ? []
    : [
      m(~x=0, ~y=last.col == indent ? 0 : 1) |> shadowfudge,
      H_({dx: float_of_int(indent - last.col) +. hx}),
      hook(hx, -1, -1),
      V({y: float_of_int(first.row - last.row + 1)}),
      hook(hx, 1, -1),
      H_({dx: float_of_int(first.col - indent)}),
      hook(hx, 1, 1),
    ];
};

let r_uni_path =
    (~offset: float, ~min_col: int, ~first: measurement, ~last: Point.t): path => {
  let hx = abs_float(offset);
  [
    M({
      x: hx,
      y: float_of_int(first.last.row - first.origin.row + 1),
    })
    |> shadowfudge,
    H({x: float_of_int(min_col - first.origin.col) +. hx}),
    hook(hx, -1, 1),
    V({y: float_of_int(last.row - first.origin.row + 1)}),
    hook(hx, 1, 1),
    H({x: float_of_int(last.col - first.origin.col)}),
    hook(hx, 1, -1),
  ];
};

let l_line =
    (~rows: Rows.t, ~first: Point.t, ~last: Point.t, ~offset)
    : list(positioned_path) =>
  if (Point.compare(first, last) < 0) {
    let indent = Rows.find(last.row, rows).indent;
    [
      (
        last,
        first.row == last.row
          ? l_horizontal_hooked(~offset, ~first, ~last)
          : l_uni_path(~offset, ~indent, ~first, ~last),
      ),
    ];
  } else {
    [];
  };

let r_line =
    (
      ~rows: Rows.t,
      ~shards: Shards.t,
      ~offset: float,
      ~first: measurement,
      ~last: Point.t,
    )
    : list(positioned_path) =>
  if (last.row == first.last.row && last.col > first.last.col) {
    [(first.origin, r_horizontal_hooked(~offset, ~first, ~last))];
  } else if (last.row > first.last.row) {
    let min_col = min_col(~first=first.last, ~last, ~rows);
    let first = first_of_last_row(shards);
    [(first.origin, r_uni_path(~first, ~last, ~offset, ~min_col))];
  } else {
    [];
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
    l_line(~rows, ~offset, ~first, ~last=snd(List.hd(shards)).origin);
  let r_line =
    r_line(
      ~rows,
      ~offset,
      ~last,
      ~shards,
      ~first=snd(ListUtil.last(shards)),
    );
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

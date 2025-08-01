open Util;
open Haz3lcore;
open Virtual_dom.Vdom;
open SvgUtil;
open Measured;
open SvgUtil.Path;

let shadowfudge = Path.cmdfudge(~y=ShardDec.shadow_dy /. 2.);

let shards_of_tiles = tiles =>
  tiles
  |> List.concat_map(((_, _, shards)) => shards)
  |> List.sort(
       ((_, m1: Measured.measurement), (_, m2: Measured.measurement)) =>
       Measured.Point.compare(m1.origin, m2.origin)
     );

let rep_tips = (tiles: list((Id.t, Mold.t, Measured.Shards.t))) => {
  assert(tiles != []);
  let (_, rep_mold, _) = List.hd(tiles);
  let (l, r) = rep_mold.nibs;
  let (l, r) = ShardDec.tips_of_shapes((l.shape, r.shape));
  (
    Option.map(Nib.Shape.direction_of(Left), l),
    Option.map(Nib.Shape.direction_of(Right), r),
  );
};

let horizontal_path =
    (l: Measured.measurement, r: Measured.measurement, offset) =>
  SvgUtil.Path.[
    shadowfudge(
      M({
        x: offset,
        y: 1.0,
      }),
    ),
    H({x: float_of_int(r.origin.col - l.origin.col)}),
  ];

let horizontal_line =
    (offset, ((_, l: Measured.measurement), (_, r: Measured.measurement)))
    : (Measured.Point.t, list(Path.cmd)) => {
  (
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
};

let vertical_path =
    (l: Measured.Point.t, r: Measured.Point.t, indent: int, offset: float)
    : list(Path.cmd) => {
  let v_delta = r.col == indent ? (-1) : 0;
  let hx = abs_float(offset);
  let hy = hx /. 2.;
  r.row - l.row <= 1
    ? []
    : [
      shadowfudge(
        M({
          x: hx +. float_of_int(indent - l.col),
          y: 1.0,
        }),
      ),
      L_({
        dx: -. hx,
        dy: hy,
      }),
      shadowfudge(
        V_({dy: float_of_int(r.row - l.row + v_delta) -. 2. *. hy}),
      ),
      L_({
        dx: hx,
        dy: hy,
      }),
      H_({dx: float_of_int(r.col - indent)}),
    ];
};

let vertical_line =
    (
      rows: Measured.Rows.t,
      offset,
      (l: Measured.Shards.t, r: Measured.Shards.t),
    )
    : (Measured.Point.t, list(Path.cmd)) => {
  assert(l != []);
  assert(r != []);
  let origin_l = snd(List.hd(l)).origin;
  let origin_r = snd(List.hd(r)).origin;
  let indent = Measured.Rows.find(origin_l.row, rows).indent;
  (origin_l, vertical_path(origin_l, origin_r, indent, offset));
};

let inner_lines =
    (
      ~font_metrics: FontMetrics.t,
      ~path_cls: list(string),
      ~rows: Measured.Rows.t,
      ~offset: float,
      ~shards: Measured.Shards.t,
    )
    : list(Node.t) => {
  let shard_rows = Measured.Shards.split_by_row(shards);
  let horizontals =
    shard_rows
    |> List.map(ListUtil.neighbors)
    |> List.concat_map(List.map(horizontal_line(offset)));
  let verticals =
    shard_rows |> ListUtil.neighbors |> List.map(vertical_line(rows, offset));
  horizontals
  @ verticals
  |> List.map(((origin, path)) =>
       DecUtil.code_svg(~font_metrics, ~origin, ~path_cls, path)
     );
};

let l_horizontal_hooked =
    (~offset, ~first: Measured.Point.t, ~last: Measured.Point.t) => [
  shadowfudge(m(~x=0, ~y=1)),
  h(~x=first.col - last.col),
  L_({
    dx: -. abs_float(offset),
    dy: -. abs_float(offset) /. 2.,
  }),
];

let r_horizontal_hooked =
    (~offset, ~m_last: Measured.measurement, ~last: Measured.Point.t) => [
  m(
    ~x=m_last.last.col - m_last.origin.col,
    ~y=m_last.last.row - m_last.origin.row + 1,
  )
  |> shadowfudge,
  h(~x=last.col - m_last.origin.col),
  L_({
    dx: abs_float(offset),
    dy: -. abs_float(offset) /. 2.,
  }),
];

let l_uni_path =
    (
      ~offset: float,
      ~indent: int,
      ~first: Measured.Point.t,
      ~last: Measured.Point.t,
    )
    : list(Path.cmd) => {
  let hx = abs_float(offset);
  let hy = hx /. 2.;
  last.row - first.row <= 1
    ? []
    : [
      m(~x=0, ~y=last.col == indent ? 0 : 1) |> shadowfudge,
      H_({dx: float_of_int(indent - last.col) +. hx}),
      L_({
        dx: -. hx,
        dy: -. hy,
      }),
      V({y: float_of_int(first.row - last.row + 1)}),
      L_({
        dx: hx,
        dy: -. hy,
      }),
      H_({dx: float_of_int(first.col - indent)}),
      L_({
        dx: hx,
        dy: -. hy,
      }),
    ];
};

let r_uni_path =
    (
      ~offset: float,
      ~min_col: int,
      ~first: Measured.measurement,
      ~last: Measured.Point.t,
    )
    : list(Path.cmd) => {
  let hx = abs_float(offset);
  let hy = hx /. 2.;
  [
    M({
      x: hx,
      y: float_of_int(first.last.row - first.origin.row + 1),
    })
    |> shadowfudge,
    H({x: float_of_int(min_col - first.origin.col) +. hx}),
    L_({
      dx: -. hx,
      dy: +. hy,
    }),
    V({y: float_of_int(last.row - first.origin.row + 1)}),
    L_({
      dx: +. hx,
      dy: +. hy,
    }),
    H({x: float_of_int(last.col - first.origin.col)}),
    L_({
      dx: hx,
      dy: -. hy,
    }),
  ];
};

let l_line =
    (
      ~rows: Measured.Rows.t,
      ~first: Measured.Point.t,
      ~last: Measured.Point.t,
      ~offset,
    )
    : list((Measured.Point.t, list(Path.cmd))) =>
  if (Measured.Point.compare(first, last) < 0) {
    let indent = Measured.Rows.find(last.row, rows).indent;
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

let min_col =
    (
      ~m_last: Measured.measurement,
      ~last: Measured.Point.t,
      ~rows: Measured.Rows.t,
    ) =>
  Measured.Rows.min_col(
    ListUtil.range(~lo=m_last.last.row, last.row + 1),
    rows,
  )
  |> min(m_last.last.col);

let first_of_last_row =
    (shard_rows: list(list((int, Measured.measurement)))) => {
  let row = ListUtil.last(shard_rows);
  assert(row != []);
  snd(List.hd(row));
};

let r_line =
    (
      ~rows: Measured.Rows.t,
      ~last: Measured.Point.t,
      ~offset: float,
      ~shards: Measured.Shards.t,
      ~m_last: Measured.measurement,
    )
    : list((Measured.Point.t, list(Path.cmd))) => {
  let shard_rows = Measured.Shards.split_by_row(shards);
  if (last.row == m_last.last.row && last.col > m_last.last.col) {
    [(m_last.origin, r_horizontal_hooked(~offset, ~m_last, ~last))];
  } else if (last.row > m_last.last.row) {
    let min_col = min_col(~m_last, ~last, ~rows);
    let first_of_last_row = first_of_last_row(shard_rows);
    [
      (
        first_of_last_row.origin,
        r_uni_path(~first=first_of_last_row, ~last, ~offset, ~min_col),
      ),
    ];
  } else {
    [];
  };
};

let outer_lines =
    (
      ~font_metrics: FontMetrics.t,
      ~rows: Measured.Rows.t,
      ~path_cls: list(string),
      (first: Measured.Point.t, last: Measured.Point.t),
      ~offset,
      shards,
    )
    : list(Node.t) => {
  assert(shards != []);
  let l_line =
    l_line(~rows, ~offset, ~first, ~last=snd(List.hd(shards)).origin);
  let r_line =
    r_line(
      ~rows,
      ~offset,
      ~last,
      ~shards,
      ~m_last=snd(ListUtil.last(shards)),
    );
  List.concat([l_line, r_line])
  |> List.map(((origin, path)) =>
       DecUtil.code_svg(~font_metrics, ~origin, ~path_cls, path)
     );
};

let lines =
    (
      tiles: list((Id.t, Mold.t, Measured.Shards.t)),
      line_clss: list(string),
      font_metrics: FontMetrics.t,
      rows: Measured.Rows.t,
      range: (Measured.Point.t, Measured.Point.t),
    )
    : list(Node.t) =>
  switch (tiles) {
  | [] => []
  | [(_, {out: sort, _}, _), ..._] =>
    let path_cls = ["child-line", Sort.to_string(sort)] @ line_clss;
    let offset = -. ShardDec.offset_of(fst(rep_tips(tiles)));
    let shards = shards_of_tiles(tiles);
    outer_lines(~path_cls, ~font_metrics, ~rows, ~offset, range, shards)
    @ inner_lines(~path_cls, ~font_metrics, ~rows, ~offset, ~shards);
  };

let shards = (~attr=?, ~font_metrics, ~base_clss, tiles): list(Node.t) =>
  List.concat_map(
    ((_, mold: Mold.t, shards: list((int, Measured.measurement)))) =>
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
      ~attr=?,
      ~font_metrics: FontMetrics.t,
      ~rows: Measured.Rows.t,
      ~tiles,
      ~line_clss: list(string),
      ~show_lines as _=true,
      ~base_clss=?,
      range,
    )
    : list(Node.t) => {
  shards(~attr?, ~font_metrics, ~base_clss, tiles)
  @ lines(tiles, line_clss, font_metrics, rows, range);
};

let error_term =
    (
      ~font_metrics,
      ~rows: Measured.Rows.t,
      range: (Measured.Point.t, Measured.Point.t),
      tiles: list((Id.t, Mold.t, Measured.Shards.t)),
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

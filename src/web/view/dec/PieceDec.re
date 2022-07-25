open Util;
open Core;
open Virtual_dom.Vdom;
open Node;
open SvgUtil;

module Profile = {
  type style =
    | Root(Measured.point, Measured.point)
    | Selected(int, int);

  type t = {
    shards: Measured.Shards.t,
    mold: Mold.t,
    style,
    index: int,
  };
};

let run: Nib.Shape.t => float =
  fun
  | Convex => +. DecUtil.short_tip_width
  | Concave(_) => -. DecUtil.short_tip_width;

let adj: Nib.Shape.t => float =
  fun
  | Convex => DecUtil.convex_adj
  | Concave(_) => DecUtil.concave_adj;

let l_hook = (l: Nib.Shape.t): list(Path.cmd) => [
  H_({dx: -. adj(l)}),
  L_({dx: -. run(l), dy: (-0.5)}),
  L_({dx: +. run(l), dy: (-0.5)}),
  H_({dx: +. adj(l)}),
];

let r_hook = (r: Nib.Shape.t): list(Path.cmd) => [
  H_({dx: +. adj(r)}),
  L_({dx: +. run(r), dy: 0.5}),
  L_({dx: -. run(r), dy: 0.5}),
  H_({dx: -. adj(r)}),
];

let simple_shard_path = ((l, r): Nibs.shapes, length: int): list(Path.cmd) =>
  List.flatten(
    Path.[
      [m(~x=0, ~y=0), h(~x=length)],
      r_hook(r),
      [h(~x=0)],
      l_hook(l),
    ],
  );

let chunky_shard_path =
    (
      {origin, last}: Measured.measurement,
      (l, r): Nibs.shapes,
      indent_col: int,
      max_col: int,
    )
    : list(Path.cmd) =>
  List.flatten(
    Path.[
      [
        m(~x=0, ~y=0),
        h(~x=max_col - origin.col + 1),
        v(~y=last.row - origin.row),
        h(~x=last.col - origin.col),
      ],
      r_hook(r),
      [h(~x=indent_col - origin.col), v(~y=1), h(~x=0)],
      l_hook(l),
    ],
  );

let simple_shard =
    (
      ~font_metrics: FontMetrics.t,
      ~mold: Mold.t,
      ~index: int,
      (this_index, {origin, last}: Measured.measurement),
    )
    : t => {
  let nib_shapes = Mold.nib_shapes(~index=this_index, mold);
  let path = simple_shard_path(nib_shapes, last.col - origin.col);
  let path_cls =
    ["tile-path", "raised", Sort.to_string(mold.out)]
    @ (index == this_index ? ["indicated-caret"] : ["indicated"]);
  let base_cls = ["tile-indicated"];
  DecUtil.code_svg(~font_metrics, ~origin, ~base_cls, ~path_cls, path);
};

let simple_shard_child =
    (
      ~font_metrics: FontMetrics.t,
      (mold: Mold.t, {origin, last}: Measured.measurement),
    )
    : t => {
  let nib_shapes = Mold.nib_shapes(mold);
  let path = simple_shard_path(nib_shapes, last.col - origin.col);
  let clss = ["indicated-child", Sort.to_string(mold.out)];
  DecUtil.code_svg(
    ~font_metrics,
    ~origin,
    ~path_cls=clss,
    ~base_cls=["child-backing"],
    path,
  );
};

let chunky_shard =
    (
      ~font_metrics: FontMetrics.t,
      ~rows: Measured.Rows.t,
      (i, j): (int, int),
      mold: Mold.t,
      shards: Measured.Shards.t,
    ) => {
  let origin = List.assoc(i, shards).origin;
  let last = List.assoc(j, shards).last;
  let (nib_l, _) = Mold.nib_shapes(~index=i, mold);
  let (_, nib_r) = Mold.nib_shapes(~index=j, mold);
  let indent_col = Measured.Rows.find(origin.row, rows).indent;
  let max_col =
    ListUtil.range(~lo=origin.row, last.row + 1)
    |> List.map(r => Measured.Rows.find(r, rows).max_col)
    |> List.fold_left(max, 0);
  let path =
    chunky_shard_path({origin, last}, (nib_l, nib_r), indent_col, max_col);
  let clss = ["tile-path", "selected", "raised", Sort.to_string(mold.out)];
  DecUtil.code_svg(~font_metrics, ~origin, ~path_cls=clss, path);
};

let shadowfudge = Path.cmdfudge(~y=DecUtil.shadow_adj);

let bi_lines =
    (
      ~font_metrics: FontMetrics.t,
      ~rows: Measured.Rows.t,
      mold: Mold.t,
      shards: Measured.Shards.t,
    )
    : list(t) => {
  let shard_rows = Measured.Shards.split_by_row(shards);
  let intra_lines =
    shard_rows
    |> List.map(ListUtil.neighbors)
    |> List.map(
         List.map(
           (((_, l: Measured.measurement), (_, r: Measured.measurement))) =>
           (
             l.origin,
             SvgUtil.Path.[
               shadowfudge(m(~x=0, ~y=1)),
               h(~x=r.last.col - l.origin.col),
             ],
           )
         ),
       )
    |> List.concat;
  let inter_lines =
    ListUtil.neighbors(shard_rows)
    |> List.map(
         ((row_shards: Measured.Shards.t, row_shards': Measured.Shards.t)) => {
         assert(row_shards != []);
         assert(row_shards' != []);
         let origin = snd(List.hd(row_shards)).origin;
         let origin' = snd(List.hd(row_shards')).origin;
         let indent = Measured.Rows.find(origin.row, rows).indent;
         let v_delta = origin'.col == indent ? (-1) : 0;
         (
           origin,
           SvgUtil.Path.[
             shadowfudge(m(~x=0, ~y=1)),
             h_(~dx=indent - origin.col),
             shadowfudge(v_(~dy=origin'.row - origin.row + v_delta)),
             h_(~dx=origin'.col - indent),
           ],
         );
       });
  let clss = ["child-line", Sort.to_string(mold.out)];
  intra_lines
  @ inter_lines
  |> List.map(((origin, path)) =>
       DecUtil.code_svg(~font_metrics, ~origin, ~path_cls=clss, path)
     );
};

let uni_lines =
    (
      ~font_metrics: FontMetrics.t,
      ~rows: Measured.Rows.t,
      (l: Measured.point, r: Measured.point),
      mold: Mold.t,
      shards: Measured.Shards.t,
    ) => {
  open SvgUtil.Path;
  let l_line = {
    let (_, m_first) = List.hd(shards);
    let (_, m_last_of_first) = {
      let shard_rows = Measured.Shards.split_by_row(shards);
      assert(shard_rows != []);
      let row = List.hd(shard_rows);
      assert(row != []);
      ListUtil.last(row);
    };
    if (l != m_first.origin) {
      let max_col =
        Measured.Rows.max_col(
          ListUtil.range(~lo=l.row, m_first.origin.row),
          rows,
        )
        |> max(m_first.origin.col);
      let indent = Measured.Rows.find(m_first.origin.row, rows).indent;
      [
        l.row == m_first.origin.row
          ? (
            m_first.origin,
            [
              shadowfudge(m(~x=0, ~y=1)),
              h(~x=l.col - m_first.origin.col),
              L_({
                dx: -. DecUtil.short_tip_width,
                dy: -. DecUtil.short_tip_height /. 2. //hack
              }),
              //L_({
              //  dx: DecUtil.short_tip_width,
              //  dy: -. DecUtil.short_tip_height,
              //}),
            ],
          )
          : (
            m_first.origin,
            (
              m_first.origin.col == indent
                ? [
                  m(~x=m_last_of_first.last.col - m_first.origin.col, ~y=0),
                  // TODO(d) need to take max of all rows, not just top
                  h(~x=max_col - m_first.origin.col),
                  shadowfudge(v(~y=l.row - m_last_of_first.origin.row)),
                ]
                : [
                  shadowfudge(m(~x=0, ~y=1)),
                  h(~x=indent - m_first.origin.col),
                  shadowfudge(v(~y=l.row + 1 - m_first.origin.row)),
                  h(~x=max_col - m_first.origin.col),
                  shadowfudge(v(~y=l.row - m_first.origin.row)),
                ]
            )
            @ [
              h(~x=l.col - m_first.origin.col),
              L_({
                dx: -. DecUtil.short_tip_width,
                dy: DecUtil.short_tip_height /. 2. //hack
              }),
              //L_({dx: DecUtil.short_tip_width, dy: DecUtil.short_tip_height}),
            ],
          ),
      ];
    } else {
      [];
    };
  };
  let r_line = {
    let (_, m_last) = ListUtil.last(shards);
    let hook = [
      L_({
        dx: DecUtil.short_tip_width,
        dy: -. DecUtil.short_tip_height /. 2. //hack
      }),
      //L_({dx: -. DecUtil.short_tip_width, dy: -. DecUtil.short_tip_height}),
    ];
    if (r.row == m_last.last.row && r.col != m_last.last.col) {
      [
        (
          m_last.origin,
          [
            shadowfudge(
              m(
                ~x=m_last.last.col - m_last.origin.col,
                ~y=m_last.last.row - m_last.origin.row + 1,
              ),
            ),
            h(~x=r.col - m_last.origin.col),
            ...hook,
          ],
        ),
      ];
    } else if (r.row != m_last.last.row) {
      let indent =
        shards
        |> List.map(((_, m): Measured.Shards.shard) =>
             Measured.Rows.find(m.origin.row, rows).indent
           )
        |> List.fold_left(min, Measured.Rows.find(r.row, rows).indent);
      // let r_indent = Measured.Rows.find(r.row, rows).indent;
      let (_, m_flast) = {
        let shard_rows = Measured.Shards.split_by_row(shards);
        assert(shard_rows != []);
        let row = ListUtil.last(shard_rows);
        assert(row != []);
        List.hd(row);
      };
      // let flast_indent = Measured.Rows.find(m_flast.origin.row, rows).indent;
      [
        (
          m_flast.origin,
          [
            shadowfudge(
              m(~x=0, ~y=m_flast.last.row - m_flast.origin.row + 1),
            ),
            h(~x=indent - m_flast.origin.col),
            shadowfudge(v(~y=r.row - m_flast.origin.row + 1)),
            h(~x=r.col - m_flast.origin.col),
            ...hook,
          ],
        ),
      ];
    } else {
      [];
    };
  };
  let clss = ["child-line", Sort.to_string(mold.out)];
  l_line
  @ r_line
  |> List.map(((origin, path)) =>
       DecUtil.code_svg(~font_metrics, ~origin, ~path_cls=clss, path)
     );
};

let view =
    (
      ~font_metrics: FontMetrics.t,
      ~rows: Measured.Rows.t,
      ~segs: list((Mold.t, Measured.measurement))=[],
      {mold, shards, index, _} as profile: Profile.t,
    )
    : list(Node.t) =>
  switch (profile.style) {
  | Selected(i, j) => [
      chunky_shard(~font_metrics, ~rows, (i, j), mold, shards),
    ]
  | Root(l, r) =>
    List.map(simple_shard(~font_metrics, ~mold, ~index), shards)
    @ List.map(simple_shard_child(~font_metrics), segs)
    @ uni_lines(~font_metrics, ~rows, (l, r), mold, shards)
    @ bi_lines(~font_metrics, ~rows, mold, shards)
  };

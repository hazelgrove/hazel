open Util;
open Haz3lcore;
open Virtual_dom.Vdom;
open Node;
open SvgUtil;

module Profile = {
  type shard = (Id.t, int);
  type tiles = list((Id.t, Mold.t, Measured.Shards.t));

  type style =
    | Root(Measured.Point.t, Measured.Point.t)
    | Selected(shard, shard);

  type t = {
    style,
    caret: shard,
    tiles,
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
      ~font_metrics,
      ~has_caret,
      ~shapes,
      ~sort,
      ~measurement: Measured.measurement,
    )
    : t => {
  let path =
    simple_shard_path(shapes, measurement.last.col - measurement.origin.col);
  let path_cls =
    ["tile-path", "raised", Sort.to_string(sort)]
    @ (has_caret ? ["indicated-caret"] : ["indicated"]);
  let base_cls = ["tile-indicated"];
  DecUtil.code_svg_sized(
    ~font_metrics,
    ~measurement,
    ~base_cls,
    ~path_cls,
    path,
  );
};

let simple_shards =
    (~font_metrics: FontMetrics.t, ~caret: (Id.t, int), (id, mold, shards))
    : list(t) =>
  List.map(
    ((index, measurement)) =>
      simple_shard(
        ~font_metrics,
        ~has_caret=caret == (id, index),
        ~shapes=Mold.nib_shapes(~index, mold),
        ~sort=mold.out,
        ~measurement,
      ),
    shards,
  );

let simple_shard_child =
    (
      ~font_metrics: FontMetrics.t,
      (mold: Mold.t, {origin, last}: Measured.measurement),
    )
    : t => {
  let nib_shapes = Mold.nib_shapes(mold);
  let path = simple_shard_path(nib_shapes, last.col - origin.col);
  let clss = ["indicated-child", Sort.to_string(mold.out)];
  DecUtil.code_svg_sized(
    ~font_metrics,
    ~measurement={origin, last},
    ~path_cls=clss,
    ~base_cls=["child-backing"],
    path,
  );
};

let chunky_shard =
    (
      ~font_metrics: FontMetrics.t,
      ~rows: Measured.Rows.t,
      (i, j): (Profile.shard, Profile.shard),
      tiles: Profile.tiles,
    ) => {
  let (nib_l, origin) = {
    let (id, index) = i;
    let (_, mold, shards) =
      switch (List.find_opt(((id', _, _)) => id' == id, tiles)) {
      | Some(x) => x
      | None => failwith("chunky_shard 1")
      };
    (
      fst(Mold.nib_shapes(~index, mold)),
      ListUtil.assoc_err(index, shards, "chunky_shard").origin,
    );
  };
  let (nib_r, last) = {
    let (id, index) = j;
    let (_, mold, shards) =
      switch (List.find_opt(((id', _, _)) => id' == id, tiles)) {
      | Some(x) => x
      | None => failwith("chunky_shard 2")
      };
    (
      snd(Mold.nib_shapes(~index, mold)),
      ListUtil.assoc_err(index, shards, "chunky_shard").last,
    );
  };
  let indent_col = Measured.Rows.find(origin.row, rows).indent;
  let max_col =
    ListUtil.range(~lo=origin.row, last.row + 1)
    |> List.map(r => Measured.Rows.find(r, rows).max_col)
    |> List.fold_left(max, 0);
  let path =
    chunky_shard_path({origin, last}, (nib_l, nib_r), indent_col, max_col);
  let clss = ["tile-path", "selected", "raised"];
  DecUtil.code_svg_sized(
    ~font_metrics,
    ~measurement={origin, last},
    ~base_cls=["tile-selected"],
    ~path_cls=clss,
    path,
  );
};

let shadowfudge = Path.cmdfudge(~y=DecUtil.shadow_adj);

let shards_of_tiles = tiles =>
  tiles
  |> List.concat_map(((_, _, shards)) => shards)
  |> List.sort(
       ((_, m1: Measured.measurement), (_, m2: Measured.measurement)) =>
       Measured.Point.compare(m1.origin, m2.origin)
     );

let bi_lines =
    (
      ~font_metrics: FontMetrics.t,
      ~rows: Measured.Rows.t,
      tiles: Profile.tiles,
    )
    : list(t) => {
  let shards = shards_of_tiles(tiles);
  let shard_rows = Measured.Shards.split_by_row(shards);
  let intra_lines =
    shard_rows
    |> List.map(ListUtil.neighbors)
    |> List.concat_map(
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
       );
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
  // TODO(d) clean up Profile datatype
  let s =
    switch (tiles) {
    | [] => failwith("empty tile")
    | [(_, mold, _), ..._] => mold.out
    };
  let clss = ["child-line", Sort.to_string(s)];
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
      (l: Measured.Point.t, r: Measured.Point.t),
      tiles: list((Id.t, Mold.t, Measured.Shards.t)),
    ) => {
  open SvgUtil.Path;
  let shards = shards_of_tiles(tiles);
  let l_line = {
    let (_, m_first) = List.hd(shards);
    let (_, m_last_of_first) = {
      let shard_rows = Measured.Shards.split_by_row(shards);
      assert(shard_rows != []);
      let row = List.hd(shard_rows);
      assert(row != []);
      ListUtil.last(row);
    };
    if (Measured.Point.compare(l, m_first.origin) < 0) {
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
    if (r.row == m_last.last.row && r.col > m_last.last.col) {
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
    } else if (r.row > m_last.last.row) {
      let min_col =
        Measured.Rows.min_col(
          ListUtil.range(~lo=m_last.last.row, r.row + 1),
          rows,
        )
        |> min(m_last.last.col);
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
            h(~x=min_col - m_flast.origin.col),
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
  // TODO(d) clean up Profile datatype
  let s =
    switch (tiles) {
    | [] => failwith("empty tile")
    | [(_, mold, _), ..._] => mold.out
    };
  let clss = ["child-line", Sort.to_string(s)];
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
      {style, caret, tiles}: Profile.t,
    )
    : list(Node.t) =>
  switch (style) {
  | Selected(i, j) => [chunky_shard(~font_metrics, ~rows, (i, j), tiles)]
  | Root(l, r) =>
    List.concat_map(simple_shards(~font_metrics, ~caret), tiles)
    @ List.map(simple_shard_child(~font_metrics), segs)
    @ uni_lines(~font_metrics, ~rows, (l, r), tiles)
    @ bi_lines(~font_metrics, ~rows, tiles)
  };

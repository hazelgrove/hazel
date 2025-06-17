open Util;
open Haz3lcorep;
open Virtual_dom.Vdom;
open Node;
open SvgUtil;

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

let bi_lines =
    (
      ~font_metrics: FontMetrics.t,
      ~rows: Measured.Rows.t,
      ~line_clss: list(string),
      tiles: list((Id.t, Mold.t, Measured.Shards.t)),
    )
    : list(t) => {
  let (dl, dr) = rep_tips(tiles);
  let shards = shards_of_tiles(tiles);
  let shard_rows = Measured.Shards.split_by_row(shards);
  let intra_lines =
    shard_rows
    |> List.map(ListUtil.neighbors)
    |> List.concat_map(
         List.mapi(
           (
             i,
             ((_, l: Measured.measurement), (_, r: Measured.measurement)),
           ) => {
           let offset = i == 0 ? -. ShardDec.offset_of(dl) : 0.;
           let length =
             i == 0
               ? ShardDec.length_of(r.origin.col - l.origin.col, dl, dr)
                 +. 0.2
               : float_of_int(r.origin.col - l.origin.col) +. 0.2;
           (
             l.origin,
             SvgUtil.Path.[
               shadowfudge(
                 M({
                   x: offset,
                   y: 1.0,
                 }),
               ),
               H({x: length}),
             ],
           );
         }),
       );
  let inter_lines =
    ListUtil.neighbors(shard_rows)
    |> List.mapi(
         (i, (row_shards: Measured.Shards.t, row_shards': Measured.Shards.t)) => {
         assert(row_shards != []);
         assert(row_shards' != []);
         let origin = snd(List.hd(row_shards)).origin;
         let origin' = snd(List.hd(row_shards')).origin;
         let indent = Measured.Rows.find(origin.row, rows).indent;
         let v_delta = origin'.col == indent ? (-1) : 0;
         let offset = i == 0 ? -. ShardDec.offset_of(dl) : 0.;
         (
           origin,
           SvgUtil.Path.[
             shadowfudge(
               M({
                 x: offset,
                 y: 1.0,
               }),
             ),
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
  let clss = ["child-line", Sort.to_string(s)] @ line_clss;
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
      ~line_clss: list(string),
      (l: Measured.Point.t, r: Measured.Point.t),
      tiles: list((Id.t, Mold.t, Measured.Shards.t)),
    ) => {
  open SvgUtil.Path;
  let shards = shards_of_tiles(tiles);
  let (dl, _) = rep_tips(tiles);
  let offset = -. ShardDec.offset_of(dl);
  let hook_dx = ShardDec.tip_width /. 2.;
  let hook_dy = ShardDec.tip_height /. 4.;
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
              shadowfudge(
                M({
                  x: 0.,
                  y: 1.0,
                }),
              ),
              h(~x=l.col - m_first.origin.col),
              L_({
                dx: -. hook_dx,
                dy: -. hook_dy,
              }),
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
                  shadowfudge(
                    M({
                      x: offset,
                      y: 1.0,
                    }),
                  ),
                  h(~x=indent - m_first.origin.col),
                  shadowfudge(v(~y=l.row + 1 - m_first.origin.row)),
                  h(~x=max_col - m_first.origin.col),
                  shadowfudge(v(~y=l.row - m_first.origin.row)),
                ]
            )
            @ [
              h(~x=l.col - m_first.origin.col),
              L_({
                dx: -. hook_dx,
                dy: hook_dy,
              }),
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
        dx: hook_dx,
        dy: -. hook_dy,
      }),
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
      let (_, m_flast) = {
        let shard_rows = Measured.Shards.split_by_row(shards);
        assert(shard_rows != []);
        let row = ListUtil.last(shard_rows);
        assert(row != []);
        List.hd(row);
      };
      [
        (
          m_flast.origin,
          [
            shadowfudge(
              M({
                x: offset,
                y: float_of_int(m_flast.last.row - m_flast.origin.row + 1),
              }),
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
  let clss = ["child-line", Sort.to_string(s)] @ line_clss;
  l_line
  @ r_line
  |> List.map(((origin, path)) =>
       DecUtil.code_svg(~font_metrics, ~origin, ~path_cls=clss, path)
     );
};

let term =
    (
      ~attr=?,
      ~font_metrics: FontMetrics.t,
      ~rows: Measured.Rows.t,
      ~caret,
      ~tiles,
      ~line_clss: list(string),
      ~base_clss=?,
      range,
    )
    : list(Node.t) => {
  let shard_of = (id, mold, (index, measurement)) =>
    ShardDec.simple(
      ~attr?,
      {
        font_metrics,
        measurement,
        tips: ShardDec.tips_of_shapes(Mold.nib_shapes(~index, mold)),
      },
      Option.to_list(base_clss)
      @ ["indicated", Sort.to_string(mold.out)]
      @ (caret == (id, index) ? ["caret"] : []),
    );
  let shard_decos =
    List.concat_map(
      ((id, mold, shards)) => List.map(shard_of(id, mold), shards),
      tiles,
    );
  shard_decos
  @ uni_lines(~line_clss, ~font_metrics, ~rows, range, tiles)
  @ bi_lines(~line_clss, ~font_metrics, ~rows, tiles);
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
  shard_decos
  @ uni_lines(~font_metrics, ~rows, range, tiles, ~line_clss=[])
  @ bi_lines(~font_metrics, ~rows, tiles, ~line_clss=[]);
};

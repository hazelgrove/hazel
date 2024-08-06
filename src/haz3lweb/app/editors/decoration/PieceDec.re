open Util;
open Haz3lcore;
open Virtual_dom.Vdom;
open Node;
open SvgUtil;

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

let simple_shard =
    (
      ~font_metrics,
      ~shapes,
      ~path_cls,
      ~base_cls,
      ~attr=[],
      ~fudge=DecUtil.fzero,
      measurement: Measured.measurement,
    )
    : t =>
  DecUtil.code_svg_sized(
    ~font_metrics,
    ~measurement,
    ~base_cls,
    ~path_cls,
    ~attr,
    ~fudge,
    simple_shard_path(shapes, measurement.last.col - measurement.origin.col),
  );

let simple_shard_selected =
    (~font_metrics, ~shapes, ~measurement: Measured.measurement, ~buffer): t => {
  let path_cls = [
    "tile-path",
    "raised",
    buffer ? "selected-buffer" : "selected",
  ];
  let base_cls = ["tile-selected"];
  simple_shard(
    /* Increase height slightly to avoid leaving spaces between selected lines */
    ~fudge={height: 0.3, top: 0., width: 0., left: 0.},
    ~font_metrics,
    ~shapes,
    ~path_cls,
    ~base_cls,
    measurement,
  );
};

let simple_shard_indicated =
    (
      ~font_metrics,
      ~has_caret,
      ~shapes,
      ~sort,
      ~measurement: Measured.measurement,
    )
    : t => {
  let path_cls =
    ["tile-path", "raised", Sort.to_string(sort)]
    @ (has_caret ? ["indicated-caret"] : ["indicated"]);
  let base_cls = ["tile-indicated"];
  simple_shard(~font_metrics, ~shapes, ~path_cls, ~base_cls, measurement);
};

let simple_shards_indicated =
    (~font_metrics: FontMetrics.t, ~caret: (Id.t, int), (id, mold, shards))
    : list(t) =>
  List.map(
    ((index, measurement)) =>
      simple_shard_indicated(
        ~font_metrics,
        ~has_caret=caret == (id, index),
        ~shapes=Mold.nib_shapes(~index, mold),
        ~sort=mold.out,
        ~measurement,
      ),
    shards,
  );

let next_step_indicated =
    (
      ~inject,
      ~font_metrics,
      ~has_caret,
      ~shapes,
      ~sort,
      ~measurement: Measured.measurement,
    )
    : t => {
  let path_cls =
    ["tile-path", "raised", Sort.to_string(sort)]
    @ (has_caret ? ["indicated-caret"] : ["indicated"]);
  let base_cls = ["tile-next-step"];
  simple_shard(
    ~font_metrics,
    ~shapes,
    ~path_cls,
    ~base_cls,
    ~attr=[Attr.on_mousedown(_ => {inject()})],
    measurement,
  );
};

let next_step_shards_indicated =
    (
      ~inject,
      ~font_metrics: FontMetrics.t,
      ~caret: (Id.t, int),
      (id, mold, shards),
    )
    : list(t) =>
  List.map(
    ((index, measurement)) =>
      next_step_indicated(
        ~inject,
        ~font_metrics,
        ~has_caret=caret == (id, index),
        ~shapes=Mold.nib_shapes(~index, mold),
        ~sort=mold.out,
        ~measurement,
      ),
    shards,
  );

let taken_step_indicated =
    (
      ~font_metrics,
      ~has_caret,
      ~shapes,
      ~sort,
      ~measurement: Measured.measurement,
    )
    : t => {
  let path_cls =
    ["tile-path", "raised", Sort.to_string(sort)]
    @ (has_caret ? ["indicated-caret"] : ["indicated"]);
  let base_cls = ["tile-taken-step"];
  simple_shard(~font_metrics, ~shapes, ~path_cls, ~base_cls, measurement);
};

let taken_step_shards_indicated =
    (~font_metrics: FontMetrics.t, ~caret: (Id.t, int), (id, mold, shards))
    : list(t) =>
  List.map(
    ((index, measurement)) =>
      taken_step_indicated(
        ~font_metrics,
        ~has_caret=caret == (id, index),
        ~shapes=Mold.nib_shapes(~index, mold),
        ~sort=mold.out,
        ~measurement,
      ),
    shards,
  );

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
      tiles: list((Id.t, Mold.t, Measured.Shards.t)),
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

let indicated =
    (
      ~simple_shards,
      ~font_metrics: FontMetrics.t,
      ~rows: Measured.Rows.t,
      ~caret,
      ~tiles,
      range,
    )
    : list(Node.t) => {
  List.concat_map(simple_shards(~font_metrics, ~caret), tiles)
  @ uni_lines(~font_metrics, ~rows, range, tiles)
  @ bi_lines(~font_metrics, ~rows, tiles);
};

let next_step_indicated = (~inject) =>
  indicated(~simple_shards=next_step_shards_indicated(~inject));
let taken_step_indicated =
  indicated(~simple_shards=taken_step_shards_indicated);
let indicated = indicated(~simple_shards=simple_shards_indicated);

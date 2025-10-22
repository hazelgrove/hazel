open Util;
open Haz3lcore;
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
    (~flip: bool, ~hx: float, ~min_col: int, ~first: Point.t, ~last: Point.t)
    : path => {
  let vf_delta = first.col == min_col ? 0 : 1;
  let edge_case =
    last.row - first.row == 1 && last.col == min_col && first.col != min_col;
  [
    m(~x=0, ~y=vf_delta)
    |> cmdfudge(~y=float_of_int(vf_delta) *. (-. hx) /. 2.),
    hook(hx, flip ? vf_delta : - vf_delta, vf_delta) /* hacky; don't draw if vf_delta==0 */
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
    (~flip: bool, ~min_col: int, ~last: Point.t, ~hx: float, ~first: Point.t)
    : option(positioned_path) =>
  if (last.row > first.row) {
    Some((first, l_vertical(~flip, ~hx, ~first, ~last, ~min_col)));
  } else if (Point.compare(last, first) > 0) {
    Some((first, l_horizontal(~hx, ~first, ~last)));
  } else {
    None;
  };

/* See l-path */
let r_path =
    (~min_col: int, ~first: Point.t, ~hx: float, ~last: Point.t)
    : option(positioned_path) =>
  if (last.row > first.row) {
    Some((first, r_vertical(~hx, ~first, ~last, ~min_col)));
  } else if (Point.compare(last, first) > 0) {
    Some((first, r_horizontal(~hx, ~first, ~last)));
  } else {
    None;
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
      l_path(
        ~flip=false,
        ~hx,
        ~min_col,
        ~first,
        ~last=snd(List.hd(shards)).origin,
      )
      |> Option.to_list,
      r_path(~hx, ~min_col, ~first=snd(ListUtil.last(shards)).last, ~last)
      |> Option.to_list,
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

let tiles_data =
    (
      ~term_data: TermData.t,
      ~terms: TermMap.t,
      ~measured: Measured.t,
      tile: Tile.t,
    ) => {
  let msg = "Arms.tiles_data";
  let id = Language.Any.rep_id(Id.Map.find(tile.id, terms));
  let of_tile = (id: Id.t) => {
    open OptUtil.Syntax;
    let+ tile = TermData.root_tile(id, term_data);
    (id, tile.mold, Measured.find_shards(~msg, tile, measured));
  };
  Id.Map.find(id, terms) |> Language.Any.ids |> List.filter_map(of_tile);
};

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
  let id = Language.Any.rep_id(Id.Map.find(tile.id, terms));
  switch (TermData.extreme_measures(id, term_data, measured)) {
  | Some((l, r)) =>
    let tiles = tiles_data(~term_data, ~terms, ~measured, tile);
    term(~font_metrics, ~rows=measured.rows, ~tiles, (l, r), ~attr?);
  | _ => []
  };
};

let term = (~syntax: CachedSyntax.t, ~font_metrics: FontMetrics.t) =>
  term(
    ~term_data=syntax.term_data,
    ~terms=syntax.terms,
    ~measured=syntax.measured,
    ~font_metrics,
  );

let term_range = (~syntax: CachedSyntax.t, p: Piece.t) => {
  switch (p) {
  | Secondary(_) => None
  | Grout(_)
  | Projector(_)
  | Tile(_) =>
    let id = Language.Any.rep_id(Id.Map.find(Piece.id(p), syntax.terms));
    TermData.extreme_measures(id, syntax.term_data, syntax.measured);
  };
};

open Util.WebUtil;

module Errors = {
  let of_id =
      (~font_metrics: FontMetrics.t, ~syntax: CachedSyntax.t, id: Id.t) =>
    div_c(
      "errors-piece",
      switch (Id.Map.find_opt(id, syntax.projectors)) {
      | Some(p) =>
        /* Special case for projectors as they are not in tile map */
        switch (Id.Map.find_opt(id, syntax.measured.projectors)) {
        | Some(measurement) => [
            ShardDec.simple(
              {
                font_metrics,
                tips: p |> ProjectorCore.shapes |> ShardDec.tips_of_shapes,
                measurement,
              },
              ["error"],
            ),
          ]
        | None =>
          /* This is caused by the statics overloading for exercise mode. The overriding
           * Exercise mode statics maps are calculated based on splicing together multiple
           * editors, but error_ids are extracted generically from the statics map, so
           * there may be error holes that don't occur in the editor being rendered */
          []
        }
      | None =>
        switch (TermData.root_tile(id, syntax.term_data)) {
        | Some(t) => term(~syntax, ~font_metrics, t)
        | None => []
        }
      },
    );

  let of_ids =
      (~font_metrics: FontMetrics.t, ~syntax: CachedSyntax.t, error_ids) =>
    div_c("errors", List.map(of_id(~font_metrics, ~syntax), error_ids));
};

module Indicated = {
  let of_piece =
      (~font_metrics: FontMetrics.t, ~syntax: CachedSyntax.t, p: Piece.t)
      : list(Node.t) => {
    switch (p) {
    | Grout(_)
    | Secondary(_) => []
    | Projector(p) =>
      switch (Measured.find_pr_opt(p, syntax.measured)) {
      | Some(measurement) => [
          ShardDec.simple(
            {
              font_metrics,
              measurement,
              tips: p |> ProjectorCore.shapes |> ShardDec.tips_of_shapes,
            },
            [
              p.syntax |> Piece.sort |> fst |> Sort.to_string,
              "caret",
              "indicated",
            ],
          ),
        ]
      | None => []
      }
    | Tile(t) as p =>
      if (Piece.is_infix_delimiter_op_prefix(p)) {
        [];
      } else {
        term(~font_metrics, ~syntax, t);
      }
    };
  };

  let indicated_piece =
      (~font_metrics: FontMetrics.t, ~syntax: CachedSyntax.t, z: Zipper.t)
      : list(Node.t) =>
    switch (Indicated.piece(z)) {
    | _ when z.selection.content != [] => []
    | Some((p, _, _)) => of_piece(~font_metrics, ~syntax, p)
    | _ => []
    };

  let term =
      (~font_metrics: FontMetrics.t, ~syntax: CachedSyntax.t, z: Zipper.t) =>
    div_c("indication", indicated_piece(~font_metrics, ~syntax, z));
};

module Refractors = {
  let paths =
      (
        hx: float,
        ~dashed: bool,
        sort: Sort.t,
        font_metrics: FontMetrics.t,
        rows: Rows.t,
        ~cls: string,
        (first, last): (Point.t, Point.t),
      )
      : list(Node.t) => {
    let min_col = min_col(~first, ~last, ~rows);
    let (orig, path) =
      l_path(~flip=true, ~hx, ~min_col, ~first, ~last) |> Option.get;
    //TODO(andrew): unhardcode magic 4 offset
    let dashed_length = IntMap.find(last.row, rows).max_col - last.col + 4;
    [
      svg(
        ~font_metrics,
        ~path_cls=["child-line", cls, Sort.to_string(sort)],
        (orig, path @ [hook(hx, 1, -1)]),
      ),
    ]
    @ (
      dashed
        ? [
          svg(
            ~font_metrics,
            ~path_cls=["child-line", Sort.to_string(sort), "dashed"],
            (last, [m(~x=0, ~y=1), h(~x=dashed_length)]),
          ),
        ]
        : []
    );
  };

  let refractor_arms =
      (
        ~id: Id.t,
        ~syntax: CachedSyntax.t,
        ~font_metrics: FontMetrics.t,
        ~cls: string,
        ~dynamics: Language.Dynamics.Map.t,
      ) =>
    switch (Id.Map.find_opt(id, syntax.term_data)) {
    | Some(t) =>
      switch (term_range(~syntax, t.root_piece)) {
      | Some(range) =>
        let hx = abs_float(ShardDec.offset_of(Some(Left))); // Always left-convex
        let sort = Piece.sort(t.root_piece) |> fst;
        paths(
          hx,
          ~dashed=Id.Map.mem(Id.transform_variant(id), dynamics),
          ~cls,
          sort,
          font_metrics,
          syntax.measured.rows,
          range,
        );
      | _ => []
      }
    | None => []
    };

  let of_zipper =
      (
        ~font_metrics: FontMetrics.t,
        ~syntax: CachedSyntax.t,
        ~dynamics: Language.Dynamics.Map.t,
        z: Zipper.t,
      )
      : list(Node.t) =>
    (
      z.refractors.manuals
      |> Id.Map.to_list
      |> List.concat_map(((id, _p)) =>
           refractor_arms(
             ~id,
             ~syntax,
             ~font_metrics,
             ~cls="manual",
             ~dynamics,
           )
         )
    )
    @ (
      z.refractors.ephemerals
      |> Id.Map.to_list
      |> List.concat_map(((id, _p)) =>
           refractor_arms(
             ~id,
             ~syntax,
             ~font_metrics,
             ~cls=
               Haz3lcore.Indicated.index(z) == Some(id)
                 ? "repl-indicated" : "repl",
             ~dynamics,
           )
         )
    );

  let all =
      (
        ~font_metrics: FontMetrics.t,
        ~syntax: CachedSyntax.t,
        ~dynamics: Language.Dynamics.Map.t,
        z: Zipper.t,
      ) =>
    div_c("refractors", of_zipper(~font_metrics, ~syntax, ~dynamics, z));
};

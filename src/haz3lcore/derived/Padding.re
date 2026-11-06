open Util;

[@deriving show({with_path: false})]
type edge = {
  col: int,
  rank: int,
};

[@deriving show({with_path: false})]
type block_row = {
  row: int,
  left: edge,
  right: edge,
  rank: int,
};

[@deriving show({with_path: false})]
type block = {
  block: Language.Blocks.t,
  rows: list(block_row),
};

[@deriving show({with_path: false})]
type gap = {
  row: int,
  before: option(int),
  hi: int,
  demand: int,
};

[@deriving show({with_path: false})]
type leading = {
  above: int,
  below: int,
  demand: int,
};

[@deriving show({with_path: false})]
type t = {
  blocks: list(block),
  gaps: list(gap),
  leading: list(leading),
};

/* A block on one of its rows. */
type span = {
  level: int,
  extent: TermData.row_extent,
};

let count = (p: 'a => bool, xs: list('a)): int =>
  List.length(List.filter(p, xs));

/* Lists keyed by row, in order. */
let by_row = (row: 'a => int, xs: list('a)): IntMap.t(list('a)) =>
  List.fold_left(
    (acc, x) =>
      IntMap.update(
        row(x),
        fun
        | None => Some([x])
        | Some(l) => Some([x, ...l]),
        acc,
      ),
    IntMap.empty,
    xs,
  )
  |> IntMap.map(List.rev);

let on = (map: IntMap.t(list('a)), row: int): list('a) =>
  Option.value(IntMap.find_opt(row, map), ~default=[]);

let at = (rows: array(list('a)), row: int): list('a) =>
  row >= 0 && row < Array.length(rows) ? rows[row] : [];

/* Ranks against the other spans on the same row: an edge counts the deeper
   spans sharing its column, the box counts the deepest span nested in it. */
let row_of_span = (peers: list(span), s: span): block_row => {
  let e = s.extent;
  let deeper = (s': span) => s'.level > s.level;
  let nested = (s': span) =>
    deeper(s')
    && s'.extent.start_col >= e.start_col
    && s'.extent.end_col <= e.end_col;
  let deepest =
    peers
    |> List.filter(nested)
    |> List.fold_left((acc, s': span) => max(acc, s'.level), s.level);
  {
    row: e.row,
    left: {
      col: e.start_col,
      rank:
        1
        + count(
            s' => deeper(s') && s'.extent.start_col == e.start_col,
            peers,
          ),
    },
    right: {
      col: e.end_col,
      rank:
        1 + count(s' => deeper(s') && s'.extent.end_col == e.end_col, peers),
    },
    rank: deepest - s.level + 1,
  };
};

/* The gaps of one row, from the columns of its content pieces in order. */
let gaps_of_row =
    (~content: list((int, int)), row: int, spans: list(span)): list(gap) => {
  let starts_at = col =>
    count((s: span) => s.extent.start_col == col, spans);
  let ends_at = col => count((s: span) => s.extent.end_col == col, spans);
  let (_, gaps) =
    List.fold_left(
      ((before, acc), (start, end_)) => {
        let demand =
          Option.fold(~none=0, ~some=ends_at, before) + starts_at(start);
        let acc =
          demand > 0
            ? [
              {
                row,
                before,
                hi: start,
                demand,
              },
              ...acc,
            ]
            : acc;
        (Some(end_), acc);
      },
      (None, []),
      content,
    );
  List.rev(gaps);
};

/* A stretch of a block's top or bottom edge on a row: the columns it runs
   over, the pads standing out of its ends included. */
type ledge = {
  row: int,
  lo: float,
  hi: float,
  rank: int,
};

let stretch = (l: ledge, lo: float, hi: float): list(ledge) =>
  lo < hi
    ? [
      {
        ...l,
        lo,
        hi,
      },
    ]
    : [];

/* `l` less the columns from `lo` to `hi`. */
let minus = (l: ledge, (lo, hi): (float, float)): list(ledge) =>
  stretch(l, l.lo, min(l.hi, lo)) @ stretch(l, max(l.lo, hi), l.hi);

let overlaps = (l: ledge, (lo, hi): (float, float)) =>
  l.lo < hi && lo < l.hi;

/* A block row's columns, its pads included. */
let columns = (~pad_cols: float, r: block_row): (float, float) => {
  let out = (e: edge) => pad_cols *. float_of_int(e.rank);
  (
    float_of_int(r.left.col) -. out(r.left),
    float_of_int(r.right.col) +. out(r.right),
  );
};

/* A row's top or bottom edge, less the block's own row across the leading
   where it carries on there: a block is never in its own way. */
let ledge =
    (~pad_cols: float, r: block_row, across: option(block_row)): list(ledge) => {
  let (lo, hi) = columns(~pad_cols, r);
  let l = {
    row: r.row,
    lo,
    hi,
    rank: r.rank,
  };
  switch (across) {
  | None => [l]
  | Some(a) => minus(l, columns(~pad_cols, a))
  };
};

/* The top edges and the bottom edges of a block's rows. */
let ledges =
    (~pad_cols: float, rows: list(block_row)): (list(ledge), list(ledge)) => {
  let next_to = (r: block_row, o: option(block_row), d: int) =>
    switch (o) {
    | Some(o) when o.row == r.row + d => Some(o)
    | _ => None
    };
  let rec go = (above: option(block_row), rows: list(block_row)) =>
    switch (rows) {
    | [] => ([], [])
    | [r, ...rest] =>
      let (tops, bottoms) = go(Some(r), rest);
      (
        ledge(~pad_cols, r, next_to(r, above, -1)) @ tops,
        ledge(~pad_cols, r, next_to(r, ListUtil.hd_opt(rest), 1)) @ bottoms,
      );
    };
  go(None, rows);
};

let of_blocks =
    (
      ~pad_cols: float,
      ~measured: Measured.t,
      ~term_data: TermData.t,
      blocks: list(Language.Blocks.t),
    )
    : t => {
  let extents =
    blocks
    |> List.filter_map((b: Language.Blocks.t) =>
         switch (TermData.row_extents(b.id, term_data, measured)) {
         | None
         | Some([]) => None
         | Some(extents) => Some((b, extents))
         }
       );
  let spans =
    extents
    |> List.concat_map(((b: Language.Blocks.t, extents)) =>
         List.map(
           extent =>
             {
               level: b.level,
               extent,
             },
           extents,
         )
       )
    |> by_row((s: span) => s.extent.row);
  let blocks =
    extents
    |> List.map(((b: Language.Blocks.t, extents)) =>
         {
           block: b,
           rows:
             List.map(
               (extent: TermData.row_extent) =>
                 row_of_span(
                   on(spans, extent.row),
                   {
                     level: b.level,
                     extent,
                   },
                 ),
               extents,
             ),
         }
       );
  let content =
    Measured.row_pieces(measured)
    |> Array.map(List.filter_map(Measured.content_span(_, measured)));
  let gaps =
    IntMap.fold(
      (row, spans, acc) =>
        List.rev_append(
          gaps_of_row(~content=at(content, row), row, spans),
          acc,
        ),
      spans,
      [],
    )
    |> List.rev;
  let text =
    Array.map(
      List.map(((s, e)) => (float_of_int(s), float_of_int(e))),
      content,
    );
  let (tops, bottoms) =
    blocks |> List.map((b: block) => ledges(~pad_cols, b.rows)) |> List.split;
  let (tops, bottoms) = (List.concat(tops), List.concat(bottoms));
  let (tops_on, bottoms_on) = (
    by_row((l: ledge) => l.row, tops),
    by_row((l: ledge) => l.row, bottoms),
  );
  /* What an edge's stretches face, a row at a time, `step` rows on: over
     text or an edge facing back, a stretch asks for the leading between
     the two rows, its rank plus the deepest such edge's; the rest looks on
     to the next row. */
  let rec faces =
          (
            ~step: int,
            ~facing: IntMap.t(list(ledge)),
            row: int,
            stretches: list(ledge),
            acc: list(leading),
          )
          : list(leading) =>
    switch (stretches) {
    | [] => acc
    | _ when row < 0 || row >= Array.length(text) => acc
    | _ =>
      let (acc, rest) =
        List.fold_left(
          ((acc, rest), s: ledge) => {
            let over =
              List.filter(
                (o: ledge) => overlaps(s, (o.lo, o.hi)),
                on(facing, row),
              );
            let over_text = List.filter(overlaps(s), at(text, row));
            switch (over, over_text) {
            | ([], []) => (acc, [s, ...rest])
            | _ =>
              let deepest =
                List.fold_left(
                  (acc, o: ledge) => max(acc, o.rank),
                  0,
                  over,
                );
              let asks = {
                above: min(s.row, row),
                below: max(s.row, row),
                demand: s.rank + deepest,
              };
              let hits =
                List.map((o: ledge) => (o.lo, o.hi), over) @ over_text;
              let rest =
                List.fold_left(
                  (stretches, hit) =>
                    List.concat_map(minus(_, hit), stretches),
                  [s],
                  hits,
                )
                @ rest;
              ([asks, ...acc], rest);
            };
          },
          (acc, []),
          stretches,
        );
      faces(~step, ~facing, row + step, rest, acc);
    };
  let asked =
    List.fold_left(
      (acc, l: ledge) =>
        faces(~step=1, ~facing=tops_on, l.row + 1, [l], acc),
      [],
      bottoms,
    );
  let asked =
    List.fold_left(
      (acc, l: ledge) =>
        faces(~step=-1, ~facing=bottoms_on, l.row - 1, [l], acc),
      asked,
      tops,
    );
  /* the most asked between any two rows */
  let leading =
    List.fold_left(
      (acc, {above, below, demand}) =>
        IntMap.update(
          above,
          fun
          | None => Some(IntMap.singleton(below, demand))
          | Some(belows) =>
            Some(
              IntMap.update(
                below,
                fun
                | None => Some(demand)
                | Some(d) => Some(max(d, demand)),
                belows,
              ),
            ),
          acc,
        ),
      IntMap.empty,
      asked,
    )
    |> IntMap.bindings
    |> List.concat_map(((above, belows)) =>
         IntMap.bindings(belows)
         |> List.map(((below, demand)) =>
              {
                above,
                below,
                demand,
              }
            )
       );
  {
    blocks,
    gaps,
    leading,
  };
};

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
  block: Language.TypeBlocks.block,
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

let overlap = (a: block_row, b: block_row) =>
  a.left.col <= b.right.col && b.left.col <= a.right.col;

let of_blocks =
    (
      ~measured: Measured.t,
      ~term_data: TermData.t,
      blocks: list(Language.TypeBlocks.block),
    )
    : t => {
  let extents =
    blocks
    |> List.filter_map((b: Language.TypeBlocks.block) =>
         switch (TermData.row_extents(b.id, term_data, measured)) {
         | None
         | Some([]) => None
         | Some(extents) => Some((b, extents))
         }
       );
  let by_row: IntMap.t(list(span)) =
    extents
    |> List.concat_map(((b: Language.TypeBlocks.block, extents)) =>
         List.map(
           extent =>
             {
               level: b.level,
               extent,
             },
           extents,
         )
       )
    |> List.fold_left(
         (acc, s: span) =>
           IntMap.update(
             s.extent.row,
             fun
             | None => Some([s])
             | Some(l) => Some([s, ...l]),
             acc,
           ),
         IntMap.empty,
       );
  let peers = row => Option.value(IntMap.find_opt(row, by_row), ~default=[]);
  let blocks =
    extents
    |> List.map(((b: Language.TypeBlocks.block, extents)) =>
         {
           block: b,
           rows:
             List.map(
               (extent: TermData.row_extent) =>
                 row_of_span(
                   peers(extent.row),
                   {
                     level: b.level,
                     extent,
                   },
                 ),
               extents,
             ),
         }
       );
  let pieces = Measured.row_pieces(measured);
  let content = row =>
    row >= 0 && row < Array.length(pieces)
      ? List.filter_map(Measured.content_span(_, measured), pieces[row]) : [];
  let gaps =
    IntMap.fold(
      (row, spans, acc) =>
        List.rev_append(
          gaps_of_row(~content=content(row), row, spans),
          acc,
        ),
      by_row,
      [],
    )
    |> List.rev;
  /* a block's first and last rows, keyed by row */
  let ends = (pick: list(block_row) => option(block_row)) =>
    blocks
    |> List.filter_map(b => pick(b.rows))
    |> List.fold_left(
         (acc, r: block_row) =>
           IntMap.update(
             r.row,
             fun
             | None => Some([r])
             | Some(l) => Some([r, ...l]),
             acc,
           ),
         IntMap.empty,
       );
  let bottoms = ends(ListUtil.last_opt);
  let tops = ends(ListUtil.hd_opt);
  let on = (map, row) =>
    Option.value(IntMap.find_opt(row, map), ~default=[]);
  /* An edge against the row across the leading: its rank plus that of the
     deepest edge there it overlaps, or its rank alone over that row's text;
     nothing where there is neither, since nothing is in the way. */
  let against = (edge: block_row, others: list(block_row), text) => {
    let other =
      List.fold_left(
        (acc, o: block_row) => overlap(edge, o) ? max(acc, o.rank) : acc,
        0,
        others,
      );
    let over_text =
      List.exists(
        ((start, end_)) => edge.left.col <= end_ && start <= edge.right.col,
        text,
      );
    other > 0 || over_text ? edge.rank + other : 0;
  };
  let demand_between = (row: int): int => {
    let (bs, ts) = (on(bottoms, row), on(tops, row + 1));
    let most = (edges, others, text) =>
      List.fold_left(
        (acc, e) => max(acc, against(e, others, text)),
        0,
        edges,
      );
    max(most(bs, ts, content(row + 1)), most(ts, bs, content(row)));
  };
  let leading =
    List.map(fst, IntMap.bindings(bottoms))
    @ List.map(((row, _)) => row - 1, IntMap.bindings(tops))
    |> List.sort_uniq(Int.compare)
    |> List.filter_map(row =>
         switch (demand_between(row)) {
         | 0 => None
         | demand =>
           Some({
             above: row,
             demand,
           })
         }
       );
  {
    blocks,
    gaps,
    leading,
  };
};

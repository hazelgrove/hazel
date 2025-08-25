open Util;
open OptUtil.Syntax;

[@deriving (show({with_path: false}), sexp, yojson)]
type data = {
  skel: Skel.t,
  sort: Sort.t,
  base_seg: Segment.t,
  root_piece: Piece.t,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = Id.Map.t(data);

let mk = (p: Piece.t, sort: Sort.t, skel: Skel.t, base_seg: Segment.t): data => {
  skel,
  sort,
  base_seg,
  root_piece: p,
};

let root_tile = (id: Id.t, data: t): option(Tile.t) =>
  switch (Id.Map.find_opt(id, data)) {
  | Some({root_piece: Tile(t), _}) => Some(t)
  | _ => None
  };

let sort = (id: Id.t, data: t): Sort.t =>
  switch (Id.Map.find_opt(id, data)) {
  | Some({sort, _}) => sort
  | None => Any
  };

let extremes_opt = (id: Id.t, data: t) =>
  /* This currently fails for singleton labelled tuples due
     to their maketerm hack, otherwise the extreme functions
     could be failwiths instead of options */
  switch (Id.Map.find_opt(id, data)) {
  | Some({skel, base_seg, _}) =>
    let (l, r) = Skel.range(skel);
    switch (List.nth(base_seg, l), List.nth(base_seg, r)) {
    | exception _ => None
    | (l, r) => Some((l, r))
    };
  | None => None
  };

let extreme_ids = (id: Id.t, data: t): option((Id.t, Id.t)) =>
  switch (extremes_opt(id, data)) {
  | Some((l, r)) => Some((Piece.id(l), Piece.id(r)))
  | None => None
  };

let extreme_measures = (id: Id.t, data: t, measured: Measured.t) =>
  switch (extremes_opt(id, data)) {
  | Some((l, r)) =>
    switch (
      Measured.find_p(l, measured).origin,
      Measured.find_p(r, measured).last,
    ) {
    | exception _ => None
    | (l, r) => Some((l, r))
    }
  | None => None
  };

/* The segment corresponding to the `id` term */
let segment = (id: Id.t, data: t): option(Segment.t) => {
  let+ {base_seg, skel, _} = Id.Map.find_opt(id, data);
  let (l, r) = Skel.range(skel);
  ListUtil.sublist((l, r + 1), base_seg);
};

let get_term_rows =
    (id: Id.t, data: t, measured: Measured.t)
    : option((int, list(Segment.t))) => {
  let+ (start, final) = extreme_measures(id, data, measured);
  let term_rows =
    measured.piece_rows
    |> List.rev
    |> Util.ListUtil.sublist((start.row, final.row + 1))
    |> List.map(List.rev);
  (start.row, term_rows);
};

/*
 TODO: handle cases where the first term found is actually a subterm
 of another term on that line

 TODO: tuples special case? (try a 3-tuple of args, each on its own line)
 (it would be nice if last one was last arg instead of whole tuple)
 (prob want to so the sam with list literals)
 */

let get_largest_terminal_term_ids = (id: Id.t, data: t, measured: Measured.t) => {
  let+ (start_row_idx, term_rows) = get_term_rows(id, data, measured);

  let get_final_col = (current_row: int, piece: Piece.t): option(int) =>
    /* Find the rightmost piece that is part of a term finishing on this line.
     * We definitely want a term sharing the final position of this term, but
     * not necessarily this term itself, if this term is a subterm of a term
     * with the same final position */
    switch (extreme_measures(Piece.id(piece), data, measured)) {
    | Some((_, final)) when final.row == current_row => Some(final.col)
    | _ => None
    };

  term_rows
  |> List.mapi((row_index: int, row: Segment.t) => {
       let current_row = start_row_idx + row_index;
       let* target_col =
         row |> List.rev |> List.find_map(get_final_col(current_row));
       /* Search from beginning of row to find largest terms first */
       List.find_map(
         piece =>
           switch (get_final_col(current_row, piece)) {
           | Some(col) when col == target_col => Some(Piece.id(piece))
           | _ => None
           },
         row,
       );
     });
};

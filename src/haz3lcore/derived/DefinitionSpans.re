/* Shared piece-level definition boundaries for focus, editing and presentation.
   No printing or reparsing: stable syntax ids survive each splice. */
let ends_with_in = (t: Base.tile): bool =>
  switch (List.rev(Tile.label(t))) {
  | ["in", ..._] => true
  | _ => false
  };
let is_semi = (p: Piece.t): bool =>
  switch (p) {
  | Tile(t) => Tile.is_semi(t)
  | _ => false
  };
/* Edge-whitespace handling: the raw pat/def slices carry the
   master's padding (spaces around the pat, the linebreak+indent
   before a def) — in an isolated cell that reads as stray
   whitespace begging to be deleted. Cells hold the TRIMMED core;
   the splice re-wraps with whatever edge whitespace the (stale)
   master copy still carries, so padding round-trips without being
   stored. Comments are content, not padding — they stay. */
let is_edge_ws = (p: Piece.t): bool =>
  switch (p) {
  | Secondary({content: Whitespace(_), _}) => true
  | _ => false
  };

let trim_ws = (seg: Segment.t): (Segment.t, Segment.t, Segment.t) => {
  let rec take = ps =>
    switch (ps) {
    | [p, ...rest] when is_edge_ws(p) =>
      let (pre, core) = take(rest);
      ([p, ...pre], core);
    | _ => ([], ps)
    };
  let (pre, rest) = take(seg);
  let (fus, eroc) = take(List.rev(rest));
  (pre, List.rev(eroc), List.rev(fus));
};

let core_ws = (seg: Segment.t): Segment.t => {
  let (_, core, _) = trim_ws(seg);
  core;
};

/* re-wrap [content] in the edge whitespace of the segment [find]
   locates in [seg] (the master's copy, untouched while focused) */
let rewrap_ws =
    (find: (Id.t, Segment.t) => option(Segment.t), fid, seg, content)
    : Segment.t =>
  switch (find(fid, seg)) {
  | Some(old) =>
    let (pre, _, suf) = trim_ws(old);
    pre @ content @ suf;
  | None => content
  };

/* does [seg] contain a piece with id [target] (recursively)? */
let rec seg_contains_id = (target: Id.t, seg: Segment.t): bool =>
  List.exists(
    (p: Piece.t) =>
      Piece.id(p) == target
      || (
        switch (p) {
        | Tile(t) => List.exists(seg_contains_id(target), t.children)
        | _ => false
        }
      ),
    seg,
  );

let rec take = (n, xs) =>
  switch (n, xs) {
  | (0, _)
  | (_, []) => []
  | (n, [x, ...xs]) => [x, ...take(n - 1, xs)]
  };
let rec drop = (n, xs) =>
  switch (n, xs) {
  | (0, _)
  | (_, []) => xs
  | (n, [_, ...xs]) => drop(n - 1, xs)
  };
let slice = (a, b, xs) => take(b - a, drop(a, xs));

/* split [ps] at the first `;` piece: (def run, separator + rest) */
let split_at_semi = (ps: list(Piece.t)): (list(Piece.t), list(Piece.t)) => {
  let rec go = (acc, ps) =>
    switch (ps) {
    | [] => (List.rev(acc), [])
    | [p, ..._] when is_semi(p) => (List.rev(acc), ps)
    | [p, ...rest] => go([p, ...acc], rest)
    };
  go([], ps);
};

/* --- top-level item spans, BY PIECE STRUCTURE (no parse) ---
   Boundaries are `…in`-tiles (def items: the tile + trailing ws)
   and top-level `;`s (statement items: the run since the previous
   boundary through the `;` + trailing ws); whatever remains is the
   trailing expression. Spans partition the top-level piece list, so
   restructure ops and headerless cells slice/splice without ever
   parsing the program. */
type item_kind =
  | IDef /* let / type / module: header+body cells */
  | IStmt /* a `…;` statement: headerless cell */
  | ITail; /* the trailing expression: headerless cell */

type item_span = {
  sp_id: option(Id.t), /* the boundary tile's id; None for the tail */
  sp_start: int,
  sp_stop: int, /* exclusive */
  sp_kind: item_kind,
};

let item_spans = (~divided_only_tail=false, seg: Segment.t): list(item_span) => {
  let arr = Array.of_list(seg);
  let len = Array.length(arr);
  let rec ws_end = i => i < len && is_edge_ws(arr[i]) ? ws_end(i + 1) : i;
  let is_in_tile = (p: Piece.t) =>
    switch (p) {
    | Tile(t) => ends_with_in(t)
    | _ => false
    };
  /* MODULE BODIES have 2-shard member defs terminated by `;`: a
     `;`-run whose first tile is a def head is a DEF item, not a
     statement (its cell takes the header/body path) */
  let run_def_head = (start: int, stop: int): option(Id.t) => {
    let rec first_tile = i =>
      i >= stop
        ? None
        : (
          switch (arr[i]) {
          | Piece.Tile(t) => Some(t)
          | _ => first_tile(i + 1)
          }
        );
    switch (first_tile(start)) {
    | Some(t) =>
      switch (Tile.label(t)) {
      | ["let", ..._]
      | ["type", ..._]
      | ["module", ..._] => Some(t.id)
      | _ => None
      }
    | None => None
    };
  };
  let rec walk = (i, start, acc) =>
    if (i >= len) {
      /* the remainder (if it has content) is the trailing expr —
         or a trailing 2-shard member def */
      let has_content = {
        let rec go = j => j < len && (!is_edge_ws(arr[j]) || go(j + 1));
        start < len && go(start);
      };
      let tail_ok =
        switch (run_def_head(start, len)) {
        | Some(_) => true
        /* a boundary-less segment is an EXPRESSION, not a block: its
           content must not read as a trailing item (deep containment
           would otherwise swallow arbitrary ids). The program's own
           top level keeps unconditional tails (the ⇒ row). */
        | None => !divided_only_tail || acc != []
        };
      List.rev(
        has_content && tail_ok
          ? [
            switch (run_def_head(start, len)) {
            | Some(id) => {
                sp_id: Some(id),
                sp_start: start,
                sp_stop: len,
                sp_kind: IDef,
              }
            | None => {
                sp_id: None,
                sp_start: start,
                sp_stop: len,
                sp_kind: ITail,
              }
            },
            ...acc,
          ]
          : acc,
      );
    } else if (is_in_tile(arr[i])) {
      let stop = ws_end(i + 1);
      walk(
        stop,
        stop,
        [
          {
            sp_id: Some(Piece.id(arr[i])),
            sp_start: i,
            sp_stop: stop,
            sp_kind: IDef,
          },
          ...acc,
        ],
      );
    } else if (is_semi(arr[i])) {
      let stop = ws_end(i + 1);
      let sp =
        switch (run_def_head(start, i)) {
        | Some(id) => {
            sp_id: Some(id),
            sp_start: start,
            sp_stop: stop,
            sp_kind: IDef,
          }
        | None => {
            sp_id: Some(Piece.id(arr[i])),
            sp_start: start,
            sp_stop: stop,
            sp_kind: IStmt,
          }
        };
      walk(stop, stop, [sp, ...acc]);
    } else {
      walk(i + 1, start, acc);
    };
  walk(0, 0, []);
};

/* CanonicalCompletion: Complete incomplete syntax to enable term creation
 *
 * Partition heuristics (to determine where to insert missing delimiters):
 * 1. BLANK LINE: Two consecutive linebreaks always partition
 * 2. RELATIVE INDENT: Content at same-or-lesser indent than incomplete tile partitions
 *
 * Algorithm:
 * 1. Partition segment based on heuristics above
 * 2. Collect trailing shards from all incomplete tiles (inner first, outer last)
 * 3. Insert shards at end of each partition
 * 4. Regrout the whole segment to fix shape inconsistencies
 * 5. Reassemble to combine same-ID shards into complete tiles
 *
 * Performance note: The syntax cache tracks global_missing_shards (cached_backpack).
 * If cached_backpack is empty, we can skip completion entirely since there are
 * no incomplete tiles. This check should be done at the call site (e.g., MakeTerm)
 * before invoking completion.
 */

open Util;

/* Record of which shards were originally present in an incomplete tile */
[@deriving (show({with_path: false}), sexp, yojson)]
type shard_record = {
  tile_id: Id.t,
  original_shards: list(int),
};

/* A single delimiter to be inserted, with hole info */
[@deriving (show({with_path: false}), sexp, yojson)]
type delimiter_info = {
  text: string, /* The delimiter token (e.g., "in", "->", ")") */
  needs_hole: bool /* Whether a "?" follows this delimiter */
};

/* Information about a single insertion point for visualization.
 * Positions are looked up later using the adjacent piece ID. */
[@deriving (show({with_path: false}), sexp, yojson)]
type insertion = {
  adjacent_id: Id.t, /* ID of piece adjacent to insertion point */
  side: Direction.t, /* Which side of the adjacent piece (Left or Right) */
  delimiters: list(delimiter_info) /* The delimiter tokens with hole info */
};

/* Result of completing a segment */
[@deriving (show({with_path: false}), sexp, yojson)]
type completion_result = {
  completed_seg: Segment.t,
  shard_records: list(shard_record),
  insertions: list(insertion) /* For visualization: where and what to insert */
};

/* Get trailing missing shard indices for a tile */
let trailing_shards = (t: Tile.t): list(Piece.t) =>
  Tile.right_missing_shards(t)
  |> List.map(s => Piece.Tile(Tile.shard_of(t, Tile.r_shard(s))));

/* Create shard pieces from incomplete tiles.
 * Takes tiles in left-to-right order, returns shards inner-first (reversed). */
let shards_from_incomplete = (incomplete: list(Tile.t)): list(Piece.t) =>
  List.rev(incomplete) |> List.concat_map(trailing_shards);

/* Leading missing shard pieces for a tile (openers), natural order */
let leading_shards = (t: Tile.t): list(Piece.t) =>
  Tile.left_missing_shards(t) |> List.map(st => Piece.Tile(st));

/* Middle-missing shards (`let x in 2`, `if true else 2` — targeted
 * put-down can strand an interior delimiter in the backpack). The
 * missing shard cannot be appended to the segment like leading/trailing
 * ones: reassemble requires shard order. Instead the tile is completed
 * in place — each original child stays in the slot opening at its
 * original left shard; newly created slots get a convex grout (a hole),
 * so `let x in 2` completes to `let x = ? in 2`. Grout ids derive
 * deterministically from the tile id. */
let complete_middle_shards = (t: Tile.t): Tile.t => {
  let lo = Tile.l_shard(t);
  let hi = Tile.r_shard(t);
  if (List.length(t.shards) == hi - lo + 1) {
    t; /* no interior gaps */
  } else {
    let index_in_shards = (i: int): option(int) => {
      let rec go = (k, xs) =>
        switch (xs) {
        | [] => None
        | [x, ..._] when x == i => Some(k)
        | [_, ...rest] => go(k + 1, rest)
        };
      go(0, t.shards);
    };
    let slot_id = ref(t.id);
    let children =
      List.init(
        hi - lo,
        j => {
          let slot_lo = lo + j;
          switch (index_in_shards(slot_lo)) {
          | Some(k) when k < List.length(t.children) =>
            List.nth(t.children, k)
          | _ =>
            slot_id := Id.next(slot_id^);
            [
              Piece.Grout({
                id: slot_id^,
                shape: Convex,
              }),
            ];
          };
        },
      );
    {
      ...t,
      shards: List.init(hi - lo + 1, i => lo + i),
      children,
    };
  };
};

/* Fallback: all openers at partition start, later-closer outermost. */
let leading_from_incomplete = (incomplete: list(Tile.t)): list(Piece.t) =>
  List.rev(incomplete) |> List.concat_map(leading_shards);

/* === Opener placement ===
 * An opener's position is the start of its closer's LEFT-OPERAND SPAN in
 * the partition skel: the maximal span the completed form absorbs
 * without crossing enclosing structure (closer shards have permissively
 * loose concave-left nibs, so skel left kids are maximal chains, but a
 * containing prefix form like `let a = ...` bounds them — `let a = 1,2]`
 * must complete to `let a = [1,2] in ?`, not hoist `[` above the let).
 * All positions are computed against the ORIGINAL skel and materialized
 * simultaneously; insertion order can never mispair delimiters (shards
 * pair by tile id at reassembly) — it only decides absorption spans and
 * nesting. Same-position ties open the later closer outermost
 * (`1) + 2)` -> ((1) + 2)). */
let rec skel_leftmost = (sk: Skel.t): int =>
  switch (sk) {
  | Op(r)
  | Pre(r, _) => Aba.first_a(r)
  | Post(k, _)
  | Bin(k, _, _) => skel_leftmost(k)
  };

let rec opener_insertion_index = (sk: Skel.t, idx: int): option(int) => {
  let in_root = (r: Skel.root) => Aba.get_as(r) |> List.mem(idx);
  let first_some = opts =>
    List.fold_left(
      (acc, o) =>
        switch (acc) {
        | Some(_) => acc
        | None => o
        },
      None,
      opts,
    );
  let search_kids = (r: Skel.root) =>
    Aba.get_bs(r)
    |> List.map(k => opener_insertion_index(k, idx))
    |> first_some;
  switch (sk) {
  | Op(r) => in_root(r) ? None : search_kids(r)
  | Pre(r, k) =>
    in_root(r)
      ? None  /* prefix shape: no left operand to absorb */
      : first_some([search_kids(r), opener_insertion_index(k, idx)])
  | Post(k, r) =>
    in_root(r)
      ? Some(skel_leftmost(k))
      : first_some([opener_insertion_index(k, idx), search_kids(r)])
  | Bin(l, r, rr) =>
    in_root(r)
      ? Some(skel_leftmost(l))
      : first_some([
          opener_insertion_index(l, idx),
          search_kids(r),
          opener_insertion_index(rr, idx),
        ])
  };
};

/* Splice each leading-incomplete tile's openers at its computed index.
 * Ties: later tile (later closer) first at the same index = outermost. */
let insert_openers = (subseg: Segment.t, incomplete: list(Tile.t)): Segment.t => {
  let leading_incomplete =
    incomplete |> List.filter((t: Tile.t) => Tile.l_shard(t) > 0);
  if (leading_incomplete == []) {
    subseg;
  } else {
    switch (Segment.skel(subseg)) {
    | exception _ => leading_from_incomplete(leading_incomplete) @ subseg
    | skel =>
      let index_of = (t: Tile.t) => {
        let rec go = (i, ps) =>
          switch (ps) {
          | [] => None
          | [Piece.Tile(t'), ..._] when t'.id == t.id => Some(i)
          | [_, ...rest] => go(i + 1, rest)
          };
        go(0, subseg);
      };
      let scheduled =
        leading_incomplete
        |> List.filter_map(t =>
             index_of(t)
             |> Option.map(idx =>
                  (
                    opener_insertion_index(skel, idx)
                    |> Option.value(~default=0),
                    idx,
                    leading_shards(t),
                  )
                )
           )
        /* position asc; ties: later closer first (outermost) */
        |> List.sort(((a1, i1, _), (a2, i2, _)) =>
             a1 == a2 ? compare(i2, i1) : compare(a1, a2)
           );
      let rec splice = (i, ps, sched) =>
        switch (sched) {
        | [] => ps
        | [(at, _, openers), ...rest] when at == i =>
          openers @ splice(i, ps, rest)
        | _ =>
          switch (ps) {
          | [] => List.concat_map(((_, _, o)) => o, sched)
          | [p, ...ptl] => [p, ...splice(i + 1, ptl, sched)]
          }
        };
      splice(0, subseg, scheduled);
    };
  };
};

/* Check if a shard needs a hole after it (has concave right side).
 *
 * Delimiters with concave right expect something after them:
 *   - `in`   : concave right (expects body expression)
 *   - `->`   : concave right (expects function body)
 *   - `then` : concave right (expects consequent)
 *   - `else` : concave right (expects alternative)
 *
 * Delimiters with convex right are self-terminating:
 *   - `)`    : convex right
 *   - `]`    : convex right
 *   - `end`  : convex right
 *
 * Note: When multiple delimiters are inserted at the same position,
 * later delimiters cannot fill holes from earlier ones. This is because
 * all trailing/closing delimiters have CONCAVE LEFT (they receive what
 * came before them in the tile structure):
 *   - `in`   : concave left (accepts the definition)
 *   - `->`   : concave left (accepts the pattern)
 *   - `)`    : concave left (accepts inner expression)
 *   - `else` : concave left (accepts the "then" branch)
 *   - `end`  : concave left (accepts case arms)
 *
 * So for `let f = fun x` → `-> ? in ?`, the `in` cannot fill the hole
 * after `->` because `in` has concave left, not convex left. */
let shard_needs_hole = (t: Tile.t, shard_idx: int): bool => {
  let (_, right_nib) = Mold.nibs(~index=shard_idx, t.mold);
  switch (right_nib.shape) {
  | Concave(_) => true
  | Convex => false
  };
};

/* Get delimiter info for missing shards of incomplete tiles.
 * For visualization: shows what text will be inserted and whether holes needed.
 * Takes tiles inner-first (reversed from left-to-right order). */
let delimiters_from_incomplete =
    (incomplete: list(Tile.t)): list(delimiter_info) =>
  List.rev(incomplete)
  |> List.concat_map((t: Tile.t) => {
       let label = t.label;
       Tile.right_missing_shards(t)
       |> List.map((s: Tile.t) => {
            let shard_idx = List.hd(s.shards);
            {
              text: List.nth(label, shard_idx),
              needs_hole: shard_needs_hole(t, shard_idx),
            };
          });
     });

/* Count leading space pieces in a segment */
let count_leading_spaces = (seg: Segment.t): int => {
  let rec count = (seg, n) =>
    switch (seg) {
    | [Piece.Secondary(s), ...rest] when Secondary.is_space(s) =>
      count(rest, n + 1)
    | _ => n
    };
  count(seg, 0);
};

/* Single-pass partitioning based on indentation heuristics.
 * Returns list of (subsegment, incomplete_tiles_in_subsegment).
 *
 * Partition heuristics (when incomplete_before is true):
 * 1. BLANK LINE: Two consecutive linebreaks (always enabled)
 * 2. RELATIVE INDENT: After a linebreak, if the content's indentation is
 *    less than or equal to the incomplete tile's indentation, partition.
 *    (only when ~use_indent_heuristic=true)
 *
 * The relative indent heuristic interprets same-or-lesser indented content
 * after incomplete syntax as user intent to start something new.
 * This subsumes the old "zero indent" heuristic (incomplete at col 0,
 * content at col 0 means 0 <= 0 -> partition).
 *
 * This should be disabled for indentation calculation to avoid circular
 * dependency (indentation uses completion, completion uses indentation). */
let partition_segment =
    (~use_indent_heuristic=true, seg: Segment.t)
    : list((Segment.t, list(Tile.t))) => {
  let rec go =
          (
            seg: Segment.t,
            acc: Segment.t,
            incomplete_acc: list(Tile.t),
            incomplete_before: bool,
            line_indent: int, /* spaces since last linebreak */
            past_indent: bool, /* have we seen non-space on this line? */
            incomplete_indent: option(int),
          ) /* indent of first incomplete tile */
          : list((Segment.t, list(Tile.t))) => {
    switch (seg) {
    | [] =>
      /* End of segment - return accumulated subsegment with its incomplete tiles */
      [(List.rev(acc), List.rev(incomplete_acc))]

    /* Heuristic 1: Blank line (two consecutive linebreaks) */
    | [Secondary(w1), Secondary(w2), ...rest]
        when Secondary.is_linebreak(w1) && Secondary.is_linebreak(w2) =>
      if (incomplete_before) {
        /* Split here: finish current subsegment, start new one */
        let current = List.rev([Piece.Secondary(w1), ...acc]);
        let current_incomplete = List.rev(incomplete_acc);
        let remaining =
          go(rest, [Secondary(w2)], [], false, 0, false, None);
        [(current, current_incomplete), ...remaining];
      } else {
        /* No split - continue accumulating */
        go(
          rest,
          [Secondary(w2), Secondary(w1), ...acc],
          incomplete_acc,
          false,
          0,
          false,
          incomplete_indent,
        );
      }

    /* Heuristic 2: Relative indent comparison */
    | [Secondary(w), ...rest]
        when use_indent_heuristic && Secondary.is_linebreak(w) =>
      let spaces_after = count_leading_spaces(rest);
      switch (incomplete_indent) {
      | Some(inc_ind) when incomplete_before && spaces_after <= inc_ind =>
        /* Partition: content at same/lesser indent than incomplete tile */
        let current = List.rev(acc);
        let current_incomplete = List.rev(incomplete_acc);
        let remaining = go(rest, [Secondary(w)], [], false, 0, false, None);
        [(current, current_incomplete), ...remaining];
      | _ =>
        /* No partition - continue accumulating */
        go(
          rest,
          [Secondary(w), ...acc],
          incomplete_acc,
          incomplete_before,
          0,
          false,
          incomplete_indent,
        )
      };

    /* Space at start of line - increment indent */
    | [Secondary(s) as p, ...rest] when Secondary.is_space(s) && !past_indent =>
      go(
        rest,
        [p, ...acc],
        incomplete_acc,
        incomplete_before,
        line_indent + 1,
        false,
        incomplete_indent,
      )

    /* Space after content - doesn't affect indent tracking */
    | [Secondary(_) as p, ...rest] =>
      go(
        rest,
        [p, ...acc],
        incomplete_acc,
        incomplete_before,
        line_indent,
        past_indent,
        incomplete_indent,
      )

    /* Incomplete tile - record its indent level */
    | [Piece.Tile(t) as p, ...rest] when !Tile.is_complete(t) =>
      let new_incomplete_indent =
        switch (incomplete_indent) {
        | None => Some(line_indent)
        | some => some
        };
      go(
        rest,
        [p, ...acc],
        [t, ...incomplete_acc],
        true,
        line_indent,
        true,
        new_incomplete_indent,
      );

    /* Other pieces (complete tiles, grout, projectors) */
    | [p, ...rest] =>
      go(
        rest,
        [p, ...acc],
        incomplete_acc,
        incomplete_before,
        line_indent,
        true,
        incomplete_indent,
      )
    };
  };
  go(seg, [], [], false, 0, false, None);
};

/* Find the last piece in a segment for insertion position.
 * For blank-line partitions, this will be the trailing linebreak.
 * For column-0 partitions, this will be the last content piece. */
let last_piece_for_insertion = (seg: Segment.t): option(Piece.t) =>
  ListUtil.last_opt(seg);

/* === Orphaned rule chains ===
 * Complete `| p => e` rule tiles appearing outside any case (Exp/Any
 * sort context) are wrapped in a synthesized case/end tile so the rules
 * receive full statics. The wrap is recorded as a shard_record with NO
 * original shards (fully synthetic); printing deletes the tile and
 * splices its content back out (see ExpToSegment strip pass). The tile
 * id derives deterministically from the first rule tile so reparses are
 * stable across keystrokes. Incomplete rule tiles (missing =>) are not
 * wrapped in v1: wrap detection runs before trailing completion. */
let rule_label = ["|", "=>"];

/* Rule-chain nodes anywhere in the partition skel: nodes whose root
 * pieces are complete ["|","=>"] rule tiles. Each yields the index span
 * (leftmost..rightmost, kids included: scrutinee + clauses) to wrap in a
 * synthesized case/end, plus a deterministic wrap-tile id derived from
 * the first rule tile. Robust to enclosing junk (leading/trailing grout
 * or juxtaposed content): the chain need not be the partition root. */
let rec skel_rightmost = (sk: Skel.t): int =>
  switch (sk) {
  | Op(r)
  | Post(_, r) => Aba.last_a(r)
  | Pre(_, k)
  | Bin(_, _, k) => skel_rightmost(k)
  };

let rule_chain_spans =
    (subseg: Segment.t, sk: Skel.t): list((int, int, Id.t)) => {
  let root_rule_id = (r: Skel.root): option(Id.t) =>
    switch (Aba.get_as(r) |> List.map(List.nth(subseg))) {
    | [] => None
    | ps =>
      let all_rules =
        ps
        |> List.for_all((p: Piece.t) =>
             switch (p) {
             | Tile(t) => t.label == rule_label && Tile.is_complete(t)
             | _ => false
             }
           );
      all_rules
        ? switch (List.hd(ps)) {
          | Piece.Tile(t) => Some(Id.next(t.id))
          | _ => None
          }
        : None;
    };
  let rec go = (sk: Skel.t): list((int, int, Id.t)) => {
    let kids_of_root = r => Aba.get_bs(r) |> List.concat_map(go);
    let here = r =>
      root_rule_id(r)
      |> Option.map(id => [(skel_leftmost(sk), skel_rightmost(sk), id)]);
    switch (sk) {
    | Op(r) => here(r) |> Option.value(~default=kids_of_root(r))
    | Pre(r, k) =>
      here(r) |> Option.value(~default=kids_of_root(r) @ go(k))
    | Post(k, r) =>
      here(r) |> Option.value(~default=go(k) @ kids_of_root(r))
    | Bin(l, r, rr) =>
      here(r) |> Option.value(~default=go(l) @ kids_of_root(r) @ go(rr))
    };
  };
  go(sk);
};

/* Insert pieces before the given indices (computed against the
 * original segment), materialized in one pass. */
let splice_at_indices =
    (seg: Segment.t, inserts: list((int, Piece.t))): Segment.t => {
  let sorted = List.sort(((a, _), (b, _)) => compare(a, b), inserts);
  let rec go = (i, ps, sched) =>
    switch (sched) {
    | [] => ps
    | [(at, piece), ...rest] when at == i => [piece, ...go(i, ps, rest)]
    | _ =>
      switch (ps) {
      | [] => List.map(snd, sched)
      | [p, ...ptl] => [p, ...go(i + 1, ptl, sched)]
      }
    };
  go(0, seg, sorted);
};

let case_wrap_shards = (id: Id.t): (Piece.t, Piece.t) => {
  let form: Form.t = Form.get(Case);
  switch (Tile.split_shards(id, form.label, form.mold, [0, 1])) {
  | [l, r] => (Piece.Tile(l), Piece.Tile(r))
  | _ => failwith("CanonicalCompletion.case_wrap_shards")
  };
};

let complete_segment =
    (~use_indent_heuristic=true, sort: Sort.t, seg: Segment.t)
    : completion_result => {
  /* Single pass: partition AND collect incomplete tiles */
  let partitioned = partition_segment(~use_indent_heuristic, seg);

  /* Orphaned rule chains: per-partition case/end wrap spans (Exp/Any
     sort only; drv has its own rule forms) */
  let wraps_of = subseg =>
    switch (sort) {
    | Exp
    | Any =>
      switch (Segment.skel(subseg)) {
      | exception _ => []
      | skel => rule_chain_spans(subseg, skel)
      }
    | _ => []
    };
  let partitioned =
    partitioned
    |> List.map(((subseg, incomplete)) =>
         (subseg, incomplete, wraps_of(subseg))
       );

  /* Extract all incomplete tiles for shard_records */
  let all_incomplete = List.concat_map(((_, inc, _)) => inc, partitioned);
  let wrap_records =
    partitioned
    |> List.concat_map(((_, _, wraps)) =>
         wraps
         |> List.map(((_, _, id)) =>
              {
                tile_id: id,
                original_shards: [],
              }
            )
       );
  let shard_records =
    List.map(
      (t: Tile.t) =>
        {
          tile_id: t.id,
          original_shards: t.shards,
        },
      all_incomplete,
    )
    @ wrap_records;

  if (List.length(all_incomplete) == 0 && wrap_records == []) {
    {
      /* No changes needed */
      completed_seg: seg,
      shard_records,
      insertions: [],
    };
  } else {
    /* Compute insertions: for each partition with incomplete tiles,
     * record the adjacent piece ID for later position lookup */
    let insertions =
      partitioned
      |> List.filter_map(((subseg, incomplete, _)) =>
           if (List.length(incomplete) == 0) {
             None;
           } else {
             /* Find the last piece in the subsegment.
              * Insertion happens on the RIGHT side of this piece.
              * For blank-line partitions, this is the trailing linebreak.
              * For column-0 partitions, this is the last content piece. */
             switch (last_piece_for_insertion(subseg)) {
             | None => None
             | Some(last_p) =>
               let delimiters = delimiters_from_incomplete(incomplete);
               Some({
                 adjacent_id: Piece.id(last_p),
                 side: Right,
                 delimiters,
               });
             };
           }
         );

    /* Phase 1: splice wrap shards (case/end around each rule-chain
       span) and missing openers at their computed indices, fill interior
       gaps in place, and append missing closers at the partition end */
    let seg_with_shards =
      partitioned
      |> List.concat_map(((subseg, incomplete, wraps)) => {
           let wrap_inserts =
             wraps
             |> List.concat_map(((l_idx, r_idx, id)) => {
                  let (l, r) = case_wrap_shards(id);
                  [(l_idx, l), (r_idx + 1, r)];
                });
           let subseg = splice_at_indices(subseg, wrap_inserts);
           /* interior gaps are filled in place before shard insertion */
           let subseg =
             subseg
             |> List.map((pc: Piece.t) =>
                  switch (pc) {
                  | Tile(t) when !Tile.is_complete(t) =>
                    Piece.Tile(complete_middle_shards(t))
                  | pc => pc
                  }
                );
           insert_openers(subseg, incomplete)
           @ shards_from_incomplete(incomplete);
         });

    /* Phase 2: Regrout to make segment well-formed for reassemble */
    let regrouted =
      seg_with_shards
      |> Segment.regrout((Nib.Shape.concave(), Nib.Shape.concave()), _);

    /* Phase 3: Reassemble to combine same-ID shards; remold to get correct molds */
    let reassembled =
      Segment.reassemble(regrouted) |> Segment.remold(_, sort);

    /* Phase 4: Regrout again based on NEW molds (remold may have changed shapes) */
    let completed_seg =
      Segment.regrout(
        (Nib.Shape.concave(), Nib.Shape.concave()),
        reassembled,
      );

    {
      completed_seg,
      shard_records,
      insertions,
    };
  };
};

/* Complete a segment recursively (descends into tile children).
 * Collects insertions from all levels for visualization. */
let rec complete_segment_deep =
        (~use_indent_heuristic=true, ~sort, seg: Segment.t): completion_result => {
  /* Helper: complete all children of a tile, collecting insertions
     and shard_records */
  let complete_tile_children =
      (t: Tile.t): (list(Segment.t), list(insertion), list(shard_record)) => {
    Tile.sorted_children(t)
    |> List.fold_left(
         ((segs_acc, ins_acc, rec_acc), (child_sort, child)) => {
           let result =
             complete_segment_deep(
               ~use_indent_heuristic,
               ~sort=child_sort,
               child,
             );
           (
             segs_acc @ [result.completed_seg],
             ins_acc @ result.insertions,
             rec_acc @ result.shard_records,
           );
         },
         ([], [], []),
       );
  };

  /* Complete children of all tiles, collecting insertions and records */
  let (seg_with_completed_children, child_insertions, child_records) =
    List.fold_left(
      ((seg_acc, ins_acc, rec_acc), piece) =>
        switch (piece) {
        | Piece.Tile(t) =>
          let (completed_children, tile_insertions, tile_records) =
            complete_tile_children(t);
          let new_tile =
            Piece.Tile({
              ...t,
              children: completed_children,
            });
          (
            seg_acc @ [new_tile],
            ins_acc @ tile_insertions,
            rec_acc @ tile_records,
          );
        | p => (seg_acc @ [p], ins_acc, rec_acc)
        },
      ([], [], []),
      seg,
    );

  /* Complete the segment at this level */
  let top_result =
    complete_segment(
      ~use_indent_heuristic,
      sort,
      seg_with_completed_children,
    );

  /* Merge child insertions and shard_records with top-level ones */
  {
    ...top_result,
    insertions: child_insertions @ top_result.insertions,
    shard_records: child_records @ top_result.shard_records,
  };
};

/* === Integration Points === */

let for_make_term = (seg: Segment.t): (Segment.t, list(shard_record)) => {
  let result = complete_segment_deep(~sort=Sort.Exp, seg);
  (result.completed_seg, result.shard_records);
};

let for_editor = (seg: Segment.t): completion_result => {
  complete_segment_deep(~sort=Sort.Exp, seg);
};

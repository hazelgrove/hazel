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
  /* partially-typed shards absorbed from prefix tokens */
  prefixes: list(Language.IdTagged.IdTag.shard_prefix),
  tile_id: Id.t,
  original_shards: list(int),
};

/* Build the MakeTerm masks map from shard records */
let masks_of_records =
    (records: list(shard_record))
    : Id.Map.t(Language.IdTagged.IdTag.incomplete_mask) =>
  List.fold_left(
    (m, r: shard_record) =>
      Id.Map.add(
        r.tile_id,
        Language.IdTagged.IdTag.{
          present: r.original_shards,
          prefixes: r.prefixes,
        },
        m,
      ),
    Id.Map.empty,
    records,
  );

/* A single delimiter to be inserted, with hole info */
[@deriving (show({with_path: false}), sexp, yojson)]
type delimiter_info = {
  text: string, /* The delimiter token (e.g., "in", "->", ")") */
  needs_hole: bool, /* Whether a "?" follows this delimiter */
  /* When completing a prefix-token witness: how many chars of the
     delimiter the user already typed (viz bolds the typed prefix and
     fades the completed remainder) */
  typed_len: option(int),
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

/* Leading missing shard pieces for a tile (openers), natural order */
let leading_shards = (t: Tile.t): list(Piece.t) =>
  Tile.left_missing_shards(t) |> List.map(st => Piece.Tile(st));

/* === Sort-fit (form table) ===
 * "Can this piece inhabit sort S" judged from the FORM TABLE (possible
 * molds by label), NOT the piece's current mold — label-level sort
 * inventories are edit-stable, so mid-edit remolding can't destabilize
 * placement decisions built on this. No-evidence cases (secondary,
 * grout, undefined tokens) fit everything: absence of a mold table
 * entry must never trigger clipping. */
let sort_fits = (out: Sort.t, s: Sort.t): bool =>
  out == s || out == Sort.Any || s == Sort.Any;

/* Monotone possible-sorts frontier scan: a tile fits if some
 * form-table mold outs at a sort in the current set, and fitting
 * molds' concave-right sorts JOIN the set — `x : ...` in a Pat slot
 * legally continues with Typ material after the ascription. The set
 * only grows, so the scan is permissive rightward: over-absorbing
 * falls back to the status quo (no clip), never to a wrong clip.
 * Returns the index (relative to the given pieces) of the first
 * unfittable tile, or None. */
let scan_frontier = (~start: Sort.t, pieces: list(Piece.t)): option(int) => {
  let rec go = (j, sorts: list(Sort.t), ps) =>
    switch (ps) {
    | [] => None
    | [pc, ...rest] =>
      switch ((pc: Piece.t)) {
      | Secondary(_)
      | Grout(_)
      | Projector(_) => go(j + 1, sorts, rest)
      | Tile(t) =>
        switch (Form.Molds.get_base(t.label)) {
        | [] => go(j + 1, sorts, rest) /* no evidence: don't clip */
        | molds =>
          let fitting =
            molds
            |> List.filter((m: Mold.t) =>
                 List.exists(sr => sort_fits(m.out, sr), sorts)
               );
          if (fitting == []) {
            Some(j);
          } else {
            let opened =
              fitting
              |> List.filter_map((m: Mold.t) => {
                   let (_, r) =
                     Mold.nibs(~index=List.length(t.label) - 1, m);
                   switch (r.shape) {
                   | Concave(_) => Some(r.sort)
                   | Convex => None
                   };
                 });
            let sorts =
              List.fold_left(
                (acc, sr) => List.mem(sr, acc) ? acc : [sr, ...acc],
                sorts,
                opened,
              );
            go(j + 1, sorts, rest);
          };
        }
      }
    };
  go(0, [start], pieces);
};

/* A prefix-token witness for a missing shard: a token molded as an
 * infix-delimiter prefix (operator position — a genuine variable in
 * operand position molds Var and never qualifies) whose text is a
 * proper prefix of the expected shard's text. The tile independently
 * EXPECTS the delimiter; the token only witnesses WHERE. */
let is_prefix_witness = (p: Piece.t, shard_text: Token.t): bool =>
  Piece.is_infix_delimiter_op_prefix(p)
  && (
    switch (p) {
    | Tile({label: [tok], _}) =>
      Token.length(tok) < Token.length(shard_text)
      && String.sub(shard_text, 0, Token.length(tok)) == tok
    | _ => false
    }
  );

let prefix_of_witness =
    (p: Piece.t, shard: int): option(Language.IdTagged.IdTag.shard_prefix) =>
  switch (p) {
  | Tile({label: [tok], id, _}) =>
    Some({
      shard,
      len: Token.length(tok),
      token_id: id,
      debris: None,
    })
  | _ => None
  };

/* A whole span can inhabit a slot if the frontier scan never fires */
let span_fits_sort = (ps: Segment.t, s: Sort.t): bool =>
  scan_frontier(~start=s, ps) == None;

/* Only non-Exp slots clip: nearly every label has an Exp mold, so an
 * Exp frontier is vacuous. Rul defers to the case-wrap machinery. */
let clippable_sort = (s: Sort.t): bool =>
  switch (s) {
  | Pat
  | TPat
  | Typ => true
  | _ => false
  };

/* === Junction drop (middle shards) ===
 * When exactly one interior shard is missing and the displaced child
 * contains exactly ONE sort-legal concave-grout junction, the child
 * splits there: `let x 1 in x` -> `let x = 1 in x` (the junction marks
 * where the deleted delimiter sat, its operands left juxtaposed).
 * Multiple legal junctions mean the deletion site is ambiguous
 * (`let x y 1 in`) — fall back to the stable everything-left
 * completion rather than guess. */
let middle_split_plan =
    (t: Tile.t)
    : option(
        (
          int,
          Segment.t,
          Segment.t,
          option(Language.IdTagged.IdTag.shard_prefix),
        ),
      ) => {
  let lo = Tile.l_shard(t);
  let hi = Tile.r_shard(t);
  let missing =
    List.init(max(hi - lo + 1, 0), i => lo + i)
    |> List.filter(i => !List.mem(i, t.shards));
  switch (missing) {
  | [m] when m > lo && m < hi =>
    let k = List.length(List.filter(sh => sh < m, t.shards)) - 1;
    switch (List.nth_opt(t.children, k)) {
    | None => None
    | Some(child) =>
      let (l_nib, r_nib) = Mold.nibs(~index=m, t.mold);
      let has_content =
        List.exists(
          fun
          | Piece.Tile(_) => true
          | _ => false,
        );
      let legal = (j: int): option((Segment.t, Segment.t)) => {
        let (left, rest) = ListUtil.split_n(j, child);
        let right = List.tl(rest);
        has_content(left)
        && has_content(right)
        && span_fits_sort(left, l_nib.sort)
        && span_fits_sort(right, r_nib.sort)
          ? Some((left, right)) : None;
      };
      let indexed = child |> List.mapi((j, pc) => (j, pc));
      let token_sites =
        indexed
        |> List.filter_map(((j, pc): (int, Piece.t)) =>
             is_prefix_witness(pc, List.nth(t.label, m))
               ? legal(j) |> Option.map(lr => (pc, lr)) : None
           );
      let junctions =
        indexed
        |> List.filter_map(((j, pc): (int, Piece.t)) =>
             switch (pc) {
             | Grout({shape: Concave, _}) => legal(j)
             | _ => None
             }
           );
      switch (token_sites, junctions) {
      | ([(pc, (left, right))], _) =>
        Some((m, left, right, prefix_of_witness(pc, m)))
      | ([], [(left, right)]) => Some((m, left, right, None))
      | _ => None
      };
    };
  | _ => None
  };
};

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
    let plan = middle_split_plan(t);
    let slot_id = ref(t.id);
    let children =
      List.init(
        hi - lo,
        j => {
          let slot_lo = lo + j;
          switch (plan) {
          | Some((m, left, _, _)) when slot_lo == m - 1 => left
          | Some((m, _, right, _)) when slot_lo == m => right
          | _ =>
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
            }
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
/* Per leading-incomplete tile: (insertion index, closer index, tile),
 * position asc, same-position ties later-closer-first (outermost). */
/* How a scheduled opener lands at its position */
type opener_action =
  | Splice /* insert before the piece at the position (default) */
  | ReplaceJunction /* replace the junction grout in place */
  | ReplaceWitness(Language.IdTagged.IdTag.shard_prefix); /* complete a prefix token */

/* (position, tile index, tile, action) */
let opener_schedule =
    (subseg: Segment.t, incomplete: list(Tile.t))
    : list((int, int, Tile.t, opener_action)) => {
  let leading_incomplete =
    incomplete |> List.filter((t: Tile.t) => Tile.l_shard(t) > 0);
  if (leading_incomplete == []) {
    [];
  } else {
    let index_of = (t: Tile.t) => {
      let rec go = (i, ps) =>
        switch (ps) {
        | [] => None
        | [Piece.Tile(t'), ..._] when t'.id == t.id => Some(i)
        | [_, ...rest] => go(i + 1, rest)
        };
      go(0, subseg);
    };
    let at_of = (idx: int) =>
      switch (Segment.skel(subseg)) {
      | exception _ => 0
      | skel => opener_insertion_index(skel, idx) |> Option.value(~default=0)
      };
    /* A synthesized opener must not cross an unmatched opener (whose
       closer is appended at partition end): `( ]` completes to
       `([?])`, not the crossed `[(])`. */
    let trailing_positions =
      incomplete
      |> List.filter((t: Tile.t) => Tile.l_shard(t) == 0)
      |> List.filter_map(index_of);
    let clamp = (at, idx) =>
      trailing_positions
      |> List.filter(p => p < idx)
      |> List.fold_left((acc, p) => max(acc, p + 1), at);
    /* Rule walls: an opener span must not absorb across a naked rule
       tile (a `|` for a later rule stops after the previous rule; a
       stray opener can't swallow an arm) — except case's own opener,
       whose whole job is to adopt the rule chain. */
    let rule_walls =
      subseg
      |> List.mapi((i, p: Piece.t) => (i, p))
      |> List.filter_map(((i, p)) =>
           switch (p) {
           | Piece.Tile(t) when t.mold.out == Sort.Rul => Some(i)
           | _ => None
           }
         );
    let case_label = Form.get(Case).label;
    let clamp_walls = (t: Tile.t, at, idx) =>
      t.label == case_label
        ? at
        : rule_walls
          |> List.filter(w => w >= at && w < idx)
          |> List.fold_left((acc, w) => max(acc, w + 1), at);
    /* Leading junction drops: a CONCAVE-LEFT leading shard (rules —
       bin-molded tiles) is shape-qualified to fill an operator hole,
       so it takes a unique sort-legal junction within its span over
       maximal-left placement. Convex-left openers ((, [, case) can't
       fill operator holes; maximal-left is correct for them
       (`let a = 1,2]` -> `[1,2]`). */
    let junction_for = (t: Tile.t, at: int, idx: int): option(int) =>
      if (Tile.l_shard(t) != 1) {
        None; /* exactly one missing leading shard */
      } else {
        let (l_nib, r_nib) = Mold.nibs(~index=0, t.mold);
        switch (l_nib.shape) {
        | Convex => None
        | Concave(_) =>
          let slice = (a, b) =>
            ListUtil.split_n(b, subseg) |> fst |> ListUtil.split_n(a) |> snd;
          let has_content =
            List.exists(
              fun
              | Piece.Tile(_) => true
              | _ => false,
            );
          let candidates =
            List.init(max(idx - at, 0), k => at + k)
            |> List.filter(j =>
                 switch (List.nth(subseg, j)) {
                 | Piece.Grout({shape: Concave, _}) => true
                 | _ => false
                 }
               )
            |> List.filter(j => {
                 let left = slice(at, j);
                 let right = slice(j + 1, idx);
                 has_content(left)
                 && has_content(right)
                 && span_fits_sort(left, l_nib.sort)
                 && span_fits_sort(right, r_nib.sort);
               });
          switch (candidates) {
          | [j] => Some(j)
          | _ => None
          };
        };
      };
    /* Leading witness: the first content piece of the opener span is
       a bare token that proper-prefixes the missing opener (`cas` for
       a broken case whose end survived; `typ`, `le`, `fu` likewise) —
       complete the token in place. No mold gate exists in operand
       position, so the residual protections are: the tile's own
       EXPECTATION (we're completing this tile's opener), the token
       sitting at the opener's own position, and PREFIX LENGTH >= 2 —
       single chars are overwhelmingly genuine operands (`if i then`:
       a condition named i must not be eaten as an if-witness). */
    let witness_for =
        (t: Tile.t, at: int, idx: int)
        : option((int, Language.IdTagged.IdTag.shard_prefix)) =>
      if (Tile.l_shard(t) != 1) {
        None;
      } else {
        let opener_text = List.nth(t.label, 0);
        let rec first_content = (j: int) =>
          j >= idx
            ? None
            : (
              switch (List.nth(subseg, j)) {
              | Piece.Secondary(_)
              | Piece.Grout(_) => first_content(j + 1)
              | pc => Some((j, pc))
              }
            );
        /* single-char candidates need CORROBORATION: a broken keyword
           leaves junction debris (its former neighbors juxtaposed) or
           sits against a hole, while a genuine variable filling the
           slot cleanly has structure, not grout, beside it. So
           deleting the f of `if x < 3 then` (i next to the junction)
           absorbs, while `i then v else x` from a whole-if deletion
           (clean single operand) is preserved as the condition.
           Accepted trade (pinned in tests): whole-form deletion where
           the slot held a multihole CONTAINING a var named like the
           prefix eats the var. */
        let corroborated = (j: int) => {
          let rec next_content = k =>
            k >= idx
              ? None
              : (
                switch (List.nth(subseg, k)) {
                | Piece.Secondary(_) => next_content(k + 1)
                | pc => Some(pc)
                }
              );
          switch (next_content(j + 1)) {
          | Some(Piece.Grout(_)) => true
          | _ => false
          };
        };
        switch (first_content(at)) {
        | Some((j, Piece.Tile({label: [tok], id, children: [], _})))
            when
              (Token.length(tok) >= 2 || corroborated(j))
              && Token.length(tok) < Token.length(opener_text)
              && String.sub(opener_text, 0, Token.length(tok)) == tok =>
          let debris =
            switch (List.nth_opt(subseg, j + 1)) {
            | Some(Piece.Grout({id, shape: Concave})) => Some(id)
            | _ => None
            };
          Some((
            j,
            {
              shard: 0,
              len: Token.length(tok),
              token_id: id,
              debris,
            },
          ));
        | _ => None
        };
      };
    leading_incomplete
    |> List.filter_map(t =>
         index_of(t)
         |> Option.map(idx => {
              let at = clamp(clamp_walls(t, at_of(idx), idx), idx);
              switch (witness_for(t, at, idx)) {
              | Some((j, sp)) => (j, idx, t, ReplaceWitness(sp))
              | None =>
                switch (junction_for(t, at, idx)) {
                | Some(j) => (j, idx, t, ReplaceJunction)
                | None => (at, idx, t, Splice)
                }
              };
            })
       )
    |> List.sort(((a1, i1, _, _), (a2, i2, _, _)) =>
         a1 == a2 ? compare(i2, i1) : compare(a1, a2)
       );
  };
};

let insert_openers =
    (subseg: Segment.t, incomplete: list(Tile.t))
    : (Segment.t, list((Id.t, Language.IdTagged.IdTag.shard_prefix))) => {
  let scheduled =
    opener_schedule(subseg, incomplete)
    |> List.map(((at, idx, t: Tile.t, act)) =>
         (at, idx, leading_shards(t), t.id, act)
       );
  let absorbed = ref([]);
  let rec splice = (i, ps, sched) =>
    switch (sched) {
    | [] => ps
    | [(at, _, openers, tid, act), ...rest] when at == i =>
      switch (act) {
      | ReplaceJunction
      | ReplaceWitness(_) =>
        /* the opener replaces the site piece in place (junction grout
           or the prefix token it completes). A witness also consumes
           the adjacent concave junction debris its brokenness left —
           RECORDED in the prefix mask (debris id), so the reprint
           reproduces the buffer's exact layout while the completed
           form stays clean. */
        switch (act) {
        | ReplaceWitness(sp) => absorbed := [(tid, sp), ...absorbed^]
        | _ => ()
        };
        switch (act, ps) {
        | (
            ReplaceWitness({debris: Some(_), _}),
            [_, Piece.Grout({shape: Concave, _}), ...ptl],
          ) =>
          openers @ splice(i + 2, ptl, rest)
        | (_, [_, ...ptl]) => openers @ splice(i + 1, ptl, rest)
        | (_, []) => openers @ splice(i, ps, rest)
        };
      | Splice => openers @ splice(i, ps, rest)
      }
    | _ =>
      switch (ps) {
      | [] => List.concat_map(((_, _, o, _, _)) => o, sched)
      | [p, ...ptl] => [p, ...splice(i + 1, ptl, sched)]
      }
    };
  let seg = splice(0, subseg, scheduled);
  (seg, absorbed^);
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

/* Viz records for leading openers: anchored Left of the piece at the
 * computed insertion index. Openers absorb existing content rightward,
 * so no hole follows. */
let leading_insertions =
    (subseg: Segment.t, incomplete: list(Tile.t)): list(insertion) =>
  opener_schedule(subseg, incomplete)
  |> List.filter_map(((at, _, t: Tile.t, act)) =>
       List.nth_opt(subseg, at)
       |> Option.map(p =>
            {
              adjacent_id: Piece.id(p),
              side: Direction.Left,
              delimiters:
                Tile.left_missing_shards(t)
                |> List.map((sh: Tile.t) => {
                     let i = List.hd(sh.shards);
                     {
                       text: List.nth(t.label, i),
                       needs_hole: false,
                       typed_len:
                         switch (act) {
                         | ReplaceWitness(sp) when sp.shard == i =>
                           Some(sp.len)
                         | _ => None
                         },
                     };
                   }),
            }
          )
     );

/* Viz records for interior gaps: each missing shard anchors Right of the
 * last piece of the child content preceding the gap (`let x in` shows
 * the pending = after the x). */
let middle_insertions = (incomplete: list(Tile.t)): list(insertion) =>
  incomplete
  |> List.concat_map((t: Tile.t) => {
       let lo = Tile.l_shard(t);
       let hi = Tile.r_shard(t);
       let plan = middle_split_plan(t);
       let interior =
         List.init(hi - lo + 1, i => lo + i)
         |> List.filter(i => !List.mem(i, t.shards));
       interior
       |> List.filter_map(m => {
            switch (plan) {
            | Some((pm, left, _, psp)) when pm == m =>
              /* junction/witness drop: shard lands inside the child,
                 no hole */
              ListUtil.last_opt(left)
              |> Option.map(p =>
                   {
                     adjacent_id: Piece.id(p),
                     side: Direction.Right,
                     delimiters: [
                       {
                         text: List.nth(t.label, m),
                         needs_hole: false,
                         typed_len:
                           Option.map(
                             (sp: Language.IdTagged.IdTag.shard_prefix) =>
                               sp.len,
                             psp,
                           ),
                       },
                     ],
                   }
                 )
            | _ =>
              let k = List.length(List.filter(sh => sh < m, t.shards)) - 1;
              switch (List.nth_opt(t.children, k)) {
              | Some(child) =>
                ListUtil.last_opt(child)
                |> Option.map(p =>
                     {
                       adjacent_id: Piece.id(p),
                       side: Direction.Right,
                       delimiters: [
                         {
                           text: List.nth(t.label, m),
                           needs_hole: shard_needs_hole(t, m),
                           typed_len: None,
                         },
                       ],
                     }
                   )
              | None => None
              };
            }
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

/* A concave grout whose operand fell into the neighboring partition is
 * dangling: only secondary sits between it and the partition edge. It
 * makes the subsegment non-convex, so Segment.skel throws and opener
 * placement / wrap detection silently degrade to their fallbacks
 * (openers land at partition start — cf. `let x = 1, 2]` + newline +
 * unindented `x`). Drop it; the final regrout re-derives whatever the
 * completed boundary needs. */
let drop_dangling_grout = (subseg: Segment.t): Segment.t => {
  let drop_edge = ps => {
    let rec go = (secs, ps) =>
      switch (ps) {
      | [Piece.Secondary(_) as w, ...rest] => go([w, ...secs], rest)
      | [Piece.Grout({shape: Concave, _}), ...rest] =>
        List.rev_append(secs, rest)
      | _ => List.rev_append(secs, ps)
      };
    go([], ps);
  };
  subseg |> drop_edge |> List.rev |> drop_edge |> List.rev;
};

/* === Trailing-shard placement (sort-frontier clipping) ===
 * A missing trailing delimiter closes a specific child slot (the
 * shard's left-nib sort). For clippable (non-Exp) slots, the shard
 * lands at the frontier: before the first following top-level piece
 * that cannot inhabit that sort — `fun x` followed by a `let` line
 * completes `fun x -> let ...`, not by absorbing the let into the
 * pattern. When everything fits (or the slot is Exp-like), degrades
 * to the partition-end append. Tiles process inner-first, preserving
 * the old nesting order; an inner tile's own label rarely fits an
 * outer non-Exp slot, so outer shards naturally clip before inner
 * tiles. Returns viz records: clipped shards anchor at the piece
 * before their landing site; unclipped ones aggregate at the
 * partition's last piece as before. */
let place_trailing_shards =
    (~aggregate_anchor: option(Piece.t), seg: Segment.t, incomplete)
    : (
        Segment.t,
        list(insertion),
        list((Id.t, Language.IdTagged.IdTag.shard_prefix)),
      ) => {
  let insert_at = (i: int, pc: Piece.t, seg: Segment.t) => {
    let (a, b) = ListUtil.split_n(i, seg);
    a @ [pc] @ b;
  };
  let clip_position = (seg: Segment.t, ~from: int, slot: Sort.t): option(int) => {
    let n = List.length(seg);
    if (from >= n) {
      None;
    } else {
      let (_, tail) = ListUtil.split_n(from, seg);
      scan_frontier(~start=slot, tail) |> Option.map(j => from + j);
    };
  };
  /* back over whitespace and junction grout so the shard lands
     against the content it closes (concave grout at the boundary is
     junction debris the final regrout re-derives; convex grout is a
     real operand and stays absorbed) */
  let rec back_over_boundary = (seg, j, floor) =>
    if (j > floor) {
      switch (List.nth_opt(seg, j - 1)) {
      | Some(Piece.Secondary(_))
      | Some(Piece.Grout({shape: Concave, _})) =>
        back_over_boundary(seg, j - 1, floor)
      | _ => j
      };
    } else {
      j;
    };
  /* A NAKED rule tile at placement level marks broken case
     structure (healthy rules live inside their case tile's child).
     Rules wall off placement for any shard whose slot isn't
     Rul-sorted — a `)` must not absorb a rule chain — while case's
     own `end` (Rul slot) absorbs rules as its content. */
  let is_rule_piece = (p: Piece.t): bool =>
    switch (p) {
    | Tile(t) => t.mold.out == Sort.Rul
    | _ => false
    };
  let wall_position = (seg: Segment.t, ~from: int): option(int) => {
    let n = List.length(seg);
    let rec go = j =>
      if (j >= n) {
        None;
      } else if (is_rule_piece(List.nth(seg, j))) {
        Some(j);
      } else {
        go(j + 1);
      };
    go(from);
  };
  let place_one = ((seg, ins, agg, abs), t: Tile.t) => {
    let entries =
      Tile.right_missing_shards(t)
      |> List.map((sh: Tile.t) => {
           let i = Tile.r_shard(sh);
           (i, Piece.Tile(Tile.shard_of(t, i)));
         });
    let rec find_pos = (j, ps) =>
      switch (ps) {
      | [] => None
      | [pc, ...rest] =>
        Piece.id(pc) == t.id ? Some(j) : find_pos(j + 1, rest)
      };
    switch (find_pos(0, seg)) {
    | None => (
        seg @ List.map(snd, entries),
        ins,
        agg
        @ List.map(
            ((i, _)) =>
              {
                text: List.nth(t.label, i),
                needs_hole: shard_needs_hole(t, i),
                typed_len: None,
              },
            entries,
          ),
        abs,
      )
    | Some(pos) =>
      let (seg, ins, agg, abs, _) =
        List.fold_left(
          ((seg, ins, agg, abs, cursor), (i, piece)) => {
            let (l_nib, r_nib) = Mold.nibs(~index=i, t.mold);
            let clip = {
              let sort_clip =
                clippable_sort(l_nib.sort)
                  ? clip_position(seg, ~from=cursor, l_nib.sort) : None;
              let wall =
                l_nib.sort == Sort.Rul
                  ? None : wall_position(seg, ~from=cursor);
              switch (sort_clip, wall) {
              | (Some(a), Some(b)) => Some(min(a, b))
              | (Some(a), None) => Some(a)
              | (None, w) => w
              };
            };
            /* junction drop: a concave-right shard (one that FILLS an
               operator-position hole; closers only relocate it) lands
               at a unique sort-legal concave-grout junction within the
               legal span, taking precedence over clip/append */
            let region_end =
              switch (clip) {
              | Some(stop) => stop
              | None => List.length(seg)
              };
            let slice = (a, b, sg) =>
              ListUtil.split_n(b, sg) |> fst |> ListUtil.split_n(a) |> snd;
            let has_content =
              List.exists(
                fun
                | Piece.Tile(_) => true
                | _ => false,
              );
            /* a prefix-token witness (backspaced `in` leaving `i` in
               operator position) outranks a bare junction: it names
               the delimiter, not just the position */
            let witness =
              switch (r_nib.shape) {
              | Convex => None
              | Concave(_) =>
                let shard_text = List.nth(t.label, i);
                let sites =
                  List.init(max(region_end - cursor, 0), k => cursor + k)
                  |> List.filter(j =>
                       is_prefix_witness(List.nth(seg, j), shard_text)
                     )
                  |> List.filter(j => {
                       let left = slice(cursor, j, seg);
                       let right = slice(j + 1, region_end, seg);
                       /* no has_content(right): the witness names the
                          delimiter, and line-final witnesses (`let a =
                          1 i` — in-prefixes end let-chain lines) have
                          their content in the next partition */
                       has_content(left)
                       && span_fits_sort(left, l_nib.sort)
                       && span_fits_sort(right, r_nib.sort);
                     });
                switch (sites) {
                | [j] => Some(j)
                | _ => None
                };
              };
            let junction =
              switch (r_nib.shape) {
              | Convex => None
              | _ when witness != None => None
              | Concave(_) =>
                let legal =
                  List.init(max(region_end - cursor, 0), k => cursor + k)
                  |> List.filter(j =>
                       switch (List.nth(seg, j)) {
                       | Piece.Grout({shape: Concave, _}) => true
                       | _ => false
                       }
                     )
                  |> List.filter(j => {
                       let left = slice(cursor, j, seg);
                       let right = slice(j + 1, region_end, seg);
                       has_content(left)
                       && has_content(right)
                       && span_fits_sort(left, l_nib.sort)
                       && span_fits_sort(right, r_nib.sort);
                     });
                switch (legal) {
                | [j] => Some(j)
                | _ => None
                };
              };
            switch (witness, junction) {
            | (Some(j), _)
            | (None, Some(j)) =>
              /* the shard replaces the site piece (junction grout or
                 witness token) in place, inheriting its spacing */
              let site = List.nth(seg, j);
              let (before, after) = ListUtil.split_n(j, seg);
              let anchor = j > 0 ? List.nth_opt(seg, j - 1) : None;
              let seg = before @ [piece] @ List.tl(after);
              let witness_prefix =
                switch (witness) {
                | Some(_) => prefix_of_witness(site, i)
                | None => None
                };
              let abs =
                switch (witness_prefix) {
                | Some(sp) => [(t.id, sp), ...abs]
                | None => abs
                };
              let ins =
                switch (anchor) {
                | Some(a) => [
                    {
                      adjacent_id: Piece.id(a),
                      side: Direction.Right,
                      delimiters: [
                        {
                          text: List.nth(t.label, i),
                          needs_hole: false,
                          typed_len:
                            Option.map(
                              (sp: Language.IdTagged.IdTag.shard_prefix) =>
                                sp.len,
                              witness_prefix,
                            ),
                        },
                      ],
                    },
                    ...ins,
                  ]
                | None => ins
                };
              (seg, ins, agg, abs, j + 1);
            | (None, None) =>
              switch (clip) {
              | Some(stop) =>
                let stop = back_over_boundary(seg, stop, cursor);
                let anchor = stop > 0 ? List.nth_opt(seg, stop - 1) : None;
                let seg = insert_at(stop, piece, seg);
                let ins =
                  switch (anchor) {
                  | Some(a) => [
                      {
                        adjacent_id: Piece.id(a),
                        side: Direction.Right,
                        delimiters: [
                          {
                            text: List.nth(t.label, i),
                            needs_hole: false,
                            typed_len: None,
                          },
                        ],
                      },
                      ...ins,
                    ]
                  | None => ins
                  };
                (seg, ins, agg, abs, stop + 1);
              | None => (
                  seg @ [piece],
                  ins,
                  agg
                  @ [
                    {
                      text: List.nth(t.label, i),
                      needs_hole: shard_needs_hole(t, i),
                      typed_len: None,
                    },
                  ],
                  abs,
                  List.length(seg) + 1,
                )
              }
            };
          },
          (seg, ins, agg, abs, pos + 1),
          entries,
        );
      (seg, ins, agg, abs);
    };
  };
  let (seg, ins, agg, abs) =
    List.fold_left(place_one, (seg, [], [], []), List.rev(incomplete));
  let ins =
    switch (agg, aggregate_anchor) {
    | ([], _)
    | (_, None) => ins
    | (delimiters, Some(pc)) =>
      ins
      @ [
        {
          adjacent_id: Piece.id(pc),
          side: Direction.Right,
          delimiters,
        },
      ]
    };
  (seg, ins, abs);
};

let complete_segment =
    (~use_indent_heuristic=true, sort: Sort.t, seg: Segment.t)
    : completion_result => {
  /* Single pass: partition AND collect incomplete tiles */
  let partitioned = partition_segment(~use_indent_heuristic, seg);
  /* boundary sanitation only matters once a split actually happened */
  let partitioned =
    List.length(partitioned) <= 1
      ? partitioned
      : partitioned
        |> List.map(((subseg, inc)) => (drop_dangling_grout(subseg), inc));

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
    |> List.map(((subseg, incomplete)) => {
         /* Arbitration: an incomplete case tile in this partition
            (orphan end / broken case) will absorb the rule chain
            through its own opener/closer completion — wrapping the
            same rules would double-complete (two cases + stray
            end). The wrap machinery is for TRULY orphaned rules
            (case AND end both gone). */
         let case_label = Form.get(Case).label;
         let has_incomplete_case =
           List.exists((t: Tile.t) => t.label == case_label, incomplete);
         (subseg, incomplete, has_incomplete_case ? [] : wraps_of(subseg));
       });

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
                prefixes: [],
              }
            )
       );
  let shard_records =
    List.map(
      (t: Tile.t) =>
        {
          tile_id: t.id,
          original_shards: t.shards,
          prefixes: [],
        },
      all_incomplete,
    )
    @ wrap_records;

  if (List.length(all_incomplete) == 0 && wrap_records == []) {
    {
      /* No structural changes — but still regrout: edits can leave
         stray grout (glom ( onto an orphan )) and Segment.skel
         silently drops pieces on shape-invalid segments */
      completed_seg:
        Segment.regrout((Nib.Shape.concave(), Nib.Shape.concave()), seg),
      shard_records,
      insertions: [],
    };
  } else {
    /* Per partition, one pass: phase-1 shard splicing AND the viz
     * records (trailing placement computes clip positions used by
     * both). Wraps splice first, interior gaps fill in place, openers
     * land at their computed indices, then trailing shards place at
     * their sort frontiers or the partition end. */
    let completed_parts =
      partitioned
      |> List.map(((subseg, incomplete, wraps)) => {
           let wrap_ins =
             wraps
             |> List.concat_map(((l_idx, r_idx, _)) =>
                  switch (
                    List.nth_opt(subseg, l_idx),
                    List.nth_opt(subseg, r_idx),
                  ) {
                  | (Some(lp), Some(rp)) => [
                      {
                        adjacent_id: Piece.id(lp),
                        side: Direction.Left,
                        delimiters: [
                          {
                            text: "case",
                            needs_hole: false,
                            typed_len: None,
                          },
                        ],
                      },
                      {
                        adjacent_id: Piece.id(rp),
                        side: Direction.Right,
                        delimiters: [
                          {
                            text: "end",
                            needs_hole: false,
                            typed_len: None,
                          },
                        ],
                      },
                    ]
                  | _ => []
                  }
                );
           let static_ins =
             leading_insertions(subseg, incomplete)
             @ middle_insertions(incomplete)
             @ wrap_ins;
           let aggregate_anchor = last_piece_for_insertion(subseg);
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
           let (subseg, opener_abs) = insert_openers(subseg, incomplete);
           let (subseg, trail_ins, abs) =
             place_trailing_shards(~aggregate_anchor, subseg, incomplete);
           (subseg, trail_ins @ static_ins, opener_abs @ abs);
         });
    let insertions = List.concat_map(((_, ins, _)) => ins, completed_parts);
    let seg_with_shards =
      List.concat_map(((sg, _, _)) => sg, completed_parts);
    /* Prefix absorptions: trailing witnesses from placement, middle
       witnesses re-derived from the (pure, deterministic) split plan */
    let absorbed =
      List.concat_map(((_, _, ab)) => ab, completed_parts)
      @ List.filter_map(
          (t: Tile.t) =>
            switch (middle_split_plan(t)) {
            | Some((_, _, _, Some(sp))) => Some((t.id, sp))
            | _ => None
            },
          all_incomplete,
        );
    let shard_records =
      shard_records
      |> List.map((r: shard_record) =>
           {
             ...r,
             prefixes:
               List.filter_map(
                 ((tid, sp)) => Id.equal(tid, r.tile_id) ? Some(sp) : None,
                 absorbed,
               ),
           }
         );

    /* Phase 2: Regrout to make segment well-formed for reassemble */
    let regrouted =
      seg_with_shards
      |> Segment.regrout((Nib.Shape.concave(), Nib.Shape.concave()), _);

    /* Phase 3: Reassemble to combine same-ID shards; remold to get
       correct molds. Must recurse: an opener splice can capture
       still-unmerged shard pairs inside a fresh tile's child, which a
       top-level pass never revisits. */
    let rec deep_reassemble = (seg: Segment.t): Segment.t =>
      seg
      |> Segment.reassemble
      |> List.map((p: Piece.t) =>
           switch (p) {
           | Tile(t) =>
             Piece.Tile({
               ...t,
               children: List.map(deep_reassemble, t.children),
             })
           | p => p
           }
         );
    let reassembled = deep_reassemble(regrouted) |> Segment.remold(_, sort);

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

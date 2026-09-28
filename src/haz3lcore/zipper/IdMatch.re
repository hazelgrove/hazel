open Util;

/* Structural (AST) diffing over segments, used to preserve piece ids across
 * an edit whose replacement text is re-parsed from a string.
 *
 * WHY. The incremental evaluator keys its cache on expression ids, and
 * MakeTerm derives expression ids from the ids of the tiles the term was built
 * from (see MakeTerm.ids / ids_of_tiles). A structure-editor keystroke
 * preserves the ids of everything it did not touch, so the cache survives. An
 * agent- or paste-style edit does not: the replacement goes through
 * Parser.to_segment, which replays keystrokes into a fresh zipper and mints a
 * brand-new id for every piece. Everything under the edit point becomes
 * uncacheable, including the parts that are textually identical --- editing
 * `(21, 22)` to `(20, 22)` throws away the id of `22` along with `21`.
 *
 * WHAT THIS DOES. Given the old segment being replaced and the freshly parsed
 * replacement, find corresponding pieces and transplant the old piece's id
 * onto the new one. Unmatched new pieces keep their fresh ids.
 *
 * LAYER. This works on Segment.t / Piece.t, not on Exp.t. The zipper is the
 * persistent structure; preserving tile ids is what actually reaches the
 * evaluator.
 *
 * CORRECTNESS. Transplanting an id must never change the program. Everything
 * here is checked before it is returned (see `transplant`): the result must be
 * shape-identical to the un-diffed segment, its ids must be unique, and every
 * transplanted id must come from the region actually being replaced. Any check
 * that fails discards the whole match and returns the fresh-id segment, so the
 * worst case is the status quo rather than a corrupted program.
 *
 * WHAT A WRONG MATCH COSTS, precisely --- the value/state asymmetry is easy to
 * get wrong and an earlier version of this comment did. For a cached VALUE: a
 * wasted lookup, not a wrong answer. IncrEval.reuse_check re-checks
 * `Exp.fast_equal(entry.prev_elab, info.elab_term)` before using an entry, and
 * that comparison is id-blind, so an id landing on a different expression
 * simply misses.
 *
 * For the cached STATE that is false. An entry also carries the evaluator-state
 * slice its subtree produced; probe samples in that slice are keyed by syntax
 * id and carry call-stack frames holding application ids. `Exp.fast_equal`
 * compares no ids at all, so an entry can be re-used when the elaboration is
 * structurally identical but the identities inside it changed, and the replayed
 * slice then carries the stale ids. A probe audit reproduced three such
 * divergences: a timeline transposed by permuting equal-valued siblings; a
 * sample attributed to a syntax id absent from the program while the id that is
 * present gets none; and samples carrying the previous run's application id.
 *
 * All three came from SYNTHETIC id-preserving inputs. None was reachable
 * through a real editor action, and the reason is this pass: it assigns ids
 * that match the structure they name, so the bad input does not arise. That
 * makes the checks below load-bearing for more than performance, and it makes
 * the asymmetry a live constraint on anything new that keys on ids --- not a
 * settled non-issue.
 *
 * Duplicate ids are a separate exception --- those break an invariant the rest
 * of the editor (Measured, Info map, selection) depends on --- hence the
 * explicit check. */

module Policy = {
  /* Every knob that decides HOW AGGRESSIVELY to match lives here, so that
     changing the policy is a one-edit change. The defaults are deliberately
     conservative: matching is order-preserving and requires exact structural
     agreement, so no threshold is ever consulted to decide a match is "close
     enough".

     One matching decision is not expressible as a parameter and lives in
     `align_gap` instead: how pieces are paired up inside a gap between
     anchors. That function is the other place to look when revisiting how
     aggressive this pass should be. */
  type t = {
    /* Phase 1 (GumTree top-down): at each segment level, anchor on a longest
       common subsequence of structural hashes and match whole isomorphic
       subtrees. */
    isomorphic: bool,
    /* Descent: between those anchors, pair pieces positionally and descend
       into pairs that are the SAME CONTAINER --- identical label, mold,
       shards and arity, differing only inside their children. This is what
       lets the pass reach inside a changed parent at all; without it a
       changed root means nothing below it is ever examined. */
    descend_containers: bool,
    /* Whether descending into such a container also transplants the
       container's own id. Separate from `descend_containers` so the two
       effects can be measured apart. */
    claim_containers: bool,
    /* Transplant ids of whitespace/comment pieces. */
    match_secondary: bool,
    /* Transplant ids of grout. Mostly cosmetic: insert_segment regrouts
       afterwards and may replace these anyway. */
    match_grout: bool,
    /* Skip the LCS (and so the whole isomorphic phase at that level) when
       the two segments are big enough that the O(n*m) table is not worth
       it. Segments at one level are tens of pieces in practice. */
    lcs_budget: int,
  };

  let default = {
    isomorphic: true,
    descend_containers: true,
    claim_containers: true,
    match_secondary: true,
    match_grout: true,
    lcs_budget: 10000,
  };

  /* GumTree phase 1 with no descent: matches only isomorphic subtrees that
     are already siblings at the top level of the replaced region. Kept for
     measurement --- it is what "top-down pass alone" actually buys. */
  let isomorphic_only = {
    ...default,
    descend_containers: false,
    claim_containers: false,
  };

  /* No matching at all: `transplant` becomes the identity. The un-diffed
     baseline to measure against. */
  let none = {
    ...default,
    isomorphic: false,
    descend_containers: false,
    claim_containers: false,
  };

  /* The policy the editor actually runs. A ref rather than a constant so
     that a measurement harness can replay the same edit under `none` and
     under `default` and report the difference, and so that changing what
     the editor does stays a one-line change in one place. */
  let current: ref(t) = ref(default);
};

/* ---------------------------------------------------------------- shapes */

/* Structural equality ignoring ids. Also the "program unchanged" check:
   `same_piece(before, after)` holding means the transplant renamed nodes and
   nothing else. Written out rather than reusing the derived equality on
   Base.piece, because that one ignores tile ids by an attribute but compares
   grout and secondary ids, which is the opposite of what is wanted here. */
let rec same_piece = (p: Piece.t, q: Piece.t): bool =>
  switch (p, q) {
  | (Tile(t), Tile(u)) =>
    Tile.label(t) == Tile.label(u)
    && Tile.mold(t) == Tile.mold(u)
    && t.shards == u.shards
    && List.length(t.children) == List.length(u.children)
    && List.for_all2(same_seg, t.children, u.children)
  | (Grout(g), Grout(h)) => g.shape == h.shape
  | (Secondary(s), Secondary(w)) => s.content == w.content
  | (Projector(a), Projector(b)) =>
    a.kind == b.kind && a.model == b.model && same_piece(a.syntax, b.syntax)
  | (Tile(_) | Grout(_) | Secondary(_) | Projector(_), _) => false
  }
and same_seg = (a: Segment.t, b: Segment.t): bool =>
  List.length(a) == List.length(b) && List.for_all2(same_piece, a, b);

/* Structural hash, ignoring ids, agreeing with `same_piece` on equal inputs.
     Collisions are possible (Hashtbl.hash is bounded) and harmless: every LCS
     anchor is re-checked with `same_piece` before it is believed.
   *
   * Memoized on piece id, which is unique within a segment. Without the memo
   * the descent re-hashes each surviving sibling subtree once per level it
   * descends, which is quadratic on a deep tree; with it the whole match is
   * linear in the number of pieces. The memo cannot make a match WRONG --- at
   * worst a stale entry costs a missed or wasted pairing, and `same_piece`
   * still has the final word. */
type memo = Hashtbl.t(Id.t, int);

let rec hash_piece = (~memo: memo, p: Piece.t): int => {
  let id = Piece.id(p);
  switch (Hashtbl.find_opt(memo, id)) {
  | Some(h) => h
  | None =>
    let h =
      switch (p) {
      | Tile(t) =>
        Hashtbl.hash_param(
          256,
          256,
          (
            0,
            Tile.label(t),
            Tile.mold(t),
            t.shards,
            List.map(hash_seg(~memo), t.children),
          ),
        )
      | Grout(g) => Hashtbl.hash_param(256, 256, (1, g.shape))
      | Secondary(s) => Hashtbl.hash_param(256, 256, (2, s.content))
      | Projector(p) =>
        Hashtbl.hash_param(
          256,
          256,
          (3, p.kind, p.model, hash_piece(~memo, p.syntax)),
        )
      };
    Hashtbl.replace(memo, id, h);
    h;
  };
}
and hash_seg = (~memo: memo, seg: Segment.t): int =>
  Hashtbl.hash_param(256, 256, List.map(hash_piece(~memo), seg));

/* Does this pair of pieces name the same container --- same shape everywhere
   except inside the children? Descending is only ever allowed through a pair
   that passes this, which is why descent cannot mis-associate two different
   forms (an `if` with a `let`, say). */
let same_container = (p: Piece.t, q: Piece.t): bool =>
  switch (p, q) {
  | (Tile(t), Tile(u)) =>
    Tile.label(t) == Tile.label(u)
    && Tile.mold(t) == Tile.mold(u)
    && t.shards == u.shards
    && List.length(t.children) == List.length(u.children)
  | (Grout(g), Grout(h)) => g.shape == h.shape
  | (Secondary(s), Secondary(w)) => s.content == w.content
  | (Projector(a), Projector(b)) => a.kind == b.kind && a.model == b.model
  | (Tile(_) | Grout(_) | Secondary(_) | Projector(_), _) => false
  };

/* ------------------------------------------------------------------- ids */

/* Every id in a segment, each node counted once. A projector shares its id
   with the piece it wraps by construction (Segment.IDs.replace keeps them in
   sync), so that pair counts as one node --- otherwise every projector would
   read as a duplicate. */
let rec ids_seg = (seg: Segment.t): list(Id.t) =>
  List.concat_map(ids_piece, seg)
and ids_piece = (p: Piece.t): list(Id.t) =>
  switch (p) {
  | Tile(t) => [t.id, ...List.concat_map(ids_seg, t.children)]
  | Grout(g) => [g.id]
  | Secondary(s) => [s.id]
  | Projector(pr) =>
    switch (ids_piece(pr.syntax)) {
    | [inner, ...rest] when Id.equal(inner, pr.id) => [pr.id, ...rest]
    | inner => [pr.id, ...inner]
    }
  };

/* Ids appearing more than once. Empty is the invariant; anything else is a
   well-formedness bug, not a missed optimization. */
let duplicate_ids = (seg: Segment.t): list(Id.t) => {
  let seen = ref(Id.Set.empty);
  let dups = ref(Id.Set.empty);
  List.iter(
    id =>
      if (Id.Set.mem(id, seen^)) {
        dups := Id.Set.add(id, dups^);
      } else {
        seen := Id.Set.add(id, seen^);
      },
    ids_seg(seg),
  );
  Id.Set.elements(dups^);
};

/* ------------------------------------------------------------- the match */

type state = {
  /* new id -> old id. Keyed by the NEW id so a projector and the piece it
     wraps, which share an id, are rewritten together for free. */
  mutable pairs: Id.Map.t(Id.t),
  /* old ids already spoken for, so no old id is transplanted twice */
  mutable claimed: Id.Set.t,
  /* old ids that must not be transplanted because the syntax carrying them
     still exists elsewhere in the program */
  forbidden: Id.Set.t,
  /* structural hashes, computed once per piece and shared by every level of
     the descent */
  hashes: memo,
};

let claim = (st: state, ~old_id: Id.t, ~new_id: Id.t): unit =>
  if (!Id.Map.mem(new_id, st.pairs)
      && !Id.Set.mem(old_id, st.claimed)
      && !Id.Set.mem(old_id, st.forbidden)) {
    st.claimed = Id.Set.add(old_id, st.claimed);
    st.pairs = Id.Map.add(new_id, old_id, st.pairs);
  };

let claim_piece = (~policy: Policy.t, st: state, o: Piece.t, n: Piece.t): unit =>
  switch (o, n) {
  | (Secondary(_), Secondary(_)) when !policy.match_secondary => ()
  | (Grout(_), Grout(_)) when !policy.match_grout => ()
  | _ => claim(st, ~old_id=Piece.id(o), ~new_id=Piece.id(n))
  };

/* Two pieces already known to be isomorphic: transplant every id in the
   subtree, walking both sides in lockstep. */
let rec claim_iso =
        (~policy: Policy.t, st: state, o: Piece.t, n: Piece.t): unit => {
  claim_piece(~policy, st, o, n);
  switch (o, n) {
  | (Tile(t), Tile(u)) =>
    List.iter2(claim_iso_seg(~policy, st), t.children, u.children)
  | (Projector(a), Projector(b)) =>
    claim_iso(~policy, st, a.syntax, b.syntax)
  | _ => ()
  };
}
and claim_iso_seg =
    (~policy: Policy.t, st: state, os: Segment.t, ns: Segment.t): unit =>
  List.iter2(claim_iso(~policy, st), os, ns);

/* Longest common subsequence of two hash sequences, as index pairs in
   increasing order. O(n*m); callers gate on `lcs_budget`. */
let lcs = (a: array(int), b: array(int)): list((int, int)) => {
  let n = Array.length(a);
  let m = Array.length(b);
  let dp = Array.make_matrix(n + 1, m + 1, 0);
  for (i in n - 1 downto 0) {
    for (j in m - 1 downto 0) {
      let v =
        if (a[i] == b[j]) {
          dp[i + 1][j + 1] + 1;
        } else {
          max(dp[i + 1][j], dp[i][j + 1]);
        };
      dp[i][j] = v;
    };
  };
  let out = ref([]);
  let i = ref(0);
  let j = ref(0);
  while (i^ < n && j^ < m) {
    if (a[i^] == b[j^]) {
      out := [(i^, j^), ...out^];
      incr(i);
      incr(j);
    } else if (dp[i^ + 1][j^] >= dp[i^][j^ + 1]) {
      incr(i);
    } else {
      incr(j);
    };
  };
  List.rev(out^);
};

/* The regions of both sides that the anchors did not cover, as
   (old_lo, old_hi, new_lo, new_hi) half-open index ranges. */
let gaps =
    (anchors: list((int, int)), no: int, nn: int)
    : list((int, int, int, int)) => {
  let rec go = (oi, ni, rest) =>
    switch (rest) {
    | [] => [(oi, no, ni, nn)]
    | [(i, j), ...tl] => [(oi, i, ni, j), ...go(i + 1, j + 1, tl)]
    };
  go(0, 0, anchors) |> List.filter(((a, b, c, d)) => a < b && c < d);
};

let rec align =
        (~policy: Policy.t, st: state, olds: Segment.t, news: Segment.t): unit => {
  let o = Array.of_list(olds);
  let n = Array.of_list(news);
  let no = Array.length(o);
  let nn = Array.length(n);
  if (no > 0 && nn > 0) {
    let anchors =
      if (policy.isomorphic && no * nn <= policy.lcs_budget) {
        lcs(
          Array.map(hash_piece(~memo=st.hashes), o),
          Array.map(hash_piece(~memo=st.hashes), n),
        )
        |> List.filter(((i, j)) => same_piece(o[i], n[j]));
      } else {
        [];
      };
    List.iter(((i, j)) => claim_iso(~policy, st, o[i], n[j]), anchors);
    if (policy.descend_containers) {
      List.iter(align_gap(~policy, st, o, n), gaps(anchors, no, nn));
    };
  };
}
/* Inside a gap neither side is isomorphic to the other, so there is nothing
   to anchor on. Pair positionally from both ends --- a leading run and a
   trailing run --- and descend through whichever of those pairs are the same
   container. Anchoring from both ends is what makes an insertion or deletion
   in the middle of a gap not shift everything after it. */
and align_gap =
    (
      ~policy: Policy.t,
      st: state,
      o: array(Piece.t),
      n: array(Piece.t),
      (lo_o, hi_o, lo_n, hi_n): (int, int, int, int),
    )
    : unit => {
  let cap = min(hi_o - lo_o, hi_n - lo_n);
  let lead = ref(0);
  while (lead^ < cap
         && try_descend(~policy, st, o[lo_o + lead^], n[lo_n + lead^])) {
    incr(lead);
  };
  let trail = ref(0);
  while (lead^
         + trail^ < cap
         && try_descend(
              ~policy,
              st,
              o[hi_o - 1 - trail^],
              n[hi_n - 1 - trail^],
            )) {
    incr(trail);
  };
}
/* Returns whether the pair was the same container. Has no effect when it
   returns false, which is what lets the callers above use it as a loop
   condition. */
and try_descend = (~policy: Policy.t, st: state, o: Piece.t, n: Piece.t): bool =>
  if (!same_container(o, n)) {
    false;
  } else {
    if (policy.claim_containers) {
      claim_piece(~policy, st, o, n);
    };
    switch (o, n) {
    | (Tile(t), Tile(u)) =>
      List.iter2(align(~policy, st), t.children, u.children)
    | (Projector(a), Projector(b)) =>
      ignore(try_descend(~policy, st, a.syntax, b.syntax))
    | _ => ()
    };
    true;
  };

/* ------------------------------------------------------------- rewriting */

let rec rewrite_seg = (m: Id.Map.t(Id.t), seg: Segment.t): Segment.t =>
  List.map(rewrite_piece(m), seg)
and rewrite_piece = (m: Id.Map.t(Id.t), p: Piece.t): Piece.t => {
  let sub = (id: Id.t) =>
    switch (Id.Map.find_opt(id, m)) {
    | Some(id') => id'
    | None => id
    };
  switch (p) {
  | Tile(t) =>
    Tile({
      ...t,
      id: sub(t.id),
      children: List.map(rewrite_seg(m), t.children),
    })
  | Grout(g) =>
    Grout({
      ...g,
      id: sub(g.id),
    })
  | Secondary(s) =>
    Secondary({
      ...s,
      id: sub(s.id),
    })
  | Projector(pr) =>
    Projector({
      ...pr,
      id: sub(pr.id),
      syntax: rewrite_piece(m, pr.syntax),
    })
  };
};

/* ----------------------------------------------------------------- entry */

type stats = {
  /* nodes in the region being replaced */
  old_nodes: int,
  /* nodes in the freshly parsed replacement */
  new_nodes: int,
  /* new nodes that ended up carrying an old id */
  preserved: int,
  /* the match was computed but a safety check rejected it */
  rejected: bool,
};

let no_stats = (old_seg: Segment.t, new_seg: Segment.t) => {
  old_nodes: List.length(ids_seg(old_seg)),
  new_nodes: List.length(ids_seg(new_seg)),
  preserved: 0,
  rejected: false,
};

/* Transplant ids from `old_seg` (the syntax being replaced) onto `new_seg` (the
 * freshly parsed replacement). `forbidden` holds ids that are still live
 * elsewhere in the program and so must not be taken.
 *
 * Returns `new_seg` untouched if anything is off. */
let transplant_with_stats =
    (
      ~policy: Policy.t=Policy.default,
      ~forbidden: Id.Set.t=Id.Set.empty,
      ~old_seg: Segment.t,
      ~new_seg: Segment.t,
      (),
    )
    : (Segment.t, stats) =>
  if (old_seg == [] || new_seg == []) {
    (new_seg, no_stats(old_seg, new_seg));
  } else {
    let st = {
      pairs: Id.Map.empty,
      claimed: Id.Set.empty,
      forbidden,
      hashes: Hashtbl.create(64),
    };
    align(~policy, st, old_seg, new_seg);
    let out = rewrite_seg(st.pairs, new_seg);
    let old_ids = Id.Set.of_list(ids_seg(old_seg));
    let fresh_ids = Id.Set.of_list(ids_seg(new_seg));
    let out_ids = ids_seg(out);
    let ok =
      /* the program is unchanged: same pieces, same shapes, same text */
      same_seg(new_seg, out)
      /* ids stay unique within the replacement */
      && List.length(out_ids) == Id.Set.cardinal(Id.Set.of_list(out_ids))
      /* every id came either from the fresh parse or from the region being
         replaced --- nothing was invented, nothing was stolen */
      && List.for_all(
           id => Id.Set.mem(id, fresh_ids) || Id.Set.mem(id, old_ids),
           out_ids,
         )
      && Id.Set.is_empty(Id.Set.inter(st.claimed, forbidden));
    if (ok) {
      (
        out,
        {
          old_nodes: List.length(ids_seg(old_seg)),
          new_nodes: List.length(ids_seg(new_seg)),
          preserved: Id.Map.cardinal(st.pairs),
          rejected: false,
        },
      );
    } else {
      (
        new_seg,
        {
          ...no_stats(old_seg, new_seg),
          rejected: true,
        },
      );
    };
  };

let transplant =
    (
      ~policy: Policy.t=Policy.default,
      ~forbidden: Id.Set.t=Id.Set.empty,
      ~old_seg: Segment.t,
      ~new_seg: Segment.t,
      (),
    )
    : Segment.t =>
  fst(transplant_with_stats(~policy, ~forbidden, ~old_seg, ~new_seg, ()));

/* The zipper-facing entry point, called from CompositionGo.introduce.
 *
 * The region being replaced is exactly the zipper's selection: insert_segment
 * overwrites it and drops it. `forbidden` is everything OUTSIDE the selection,
 * which under the zipper's own uniqueness invariant is already disjoint from
 * the selection --- computing it anyway is cheap next to the statics pass this
 * edit is about to trigger, and it is the check that rules out the one failure
 * mode that would be a correctness bug rather than a missed speedup. */
let for_selection =
    (~policy: option(Policy.t)=?, z: ZipperBase.t, new_seg: Segment.t)
    : Segment.t => {
  let policy =
    switch (policy) {
    | Some(p) => p
    | None => Policy.current^
    };
  switch (z.selection.content) {
  | [] => new_seg
  | old_seg =>
    let forbidden =
      Id.Set.of_list(ids_seg(Relatives.zip(~sel=[], z.relatives)));
    transplant(~policy, ~forbidden, ~old_seg, ~new_seg, ());
  };
};

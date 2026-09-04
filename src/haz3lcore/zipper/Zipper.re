open Util_web;
open OptUtil.Syntax;
include ZipperBase;

let init: unit => t =
  () => {
    selection: Selection.mk([]),
    relatives: {
      siblings: (
        [],
        [
          Grout({
            id: Id.mk(),
            shape: Convex,
          }),
        ],
      ),
      ancestors: [],
    },
    caret: Outer,
    refractors: Refractor.init,
  };

let zip = (z: t): Segment.t =>
  Relatives.zip(~sel=z.selection.content, z.relatives);

let unzip = (~direction: Direction.t=Right, seg: Segment.t): t => {
  selection: Selection.mk([]),
  relatives: {
    siblings:
      switch (direction) {
      | Right => (seg, [])
      | Left => ([], seg)
      },
    ancestors: [],
  },
  caret: Outer,
  refractors: Refractor.init,
};

let regrout = (d: Direction.t, z: t): t => {
  assert(Selection.is_empty(z.selection));
  let relatives = Relatives.regrout(d, z.relatives);
  {
    ...z,
    relatives,
  };
};

let remold = (z: t, ~root): t => {
  assert(Selection.is_empty(z.selection));
  {
    ...z,
    relatives: Relatives.remold(z.relatives, root),
  };
};

let remold_regrout = (d: Direction.t, z: t, ~root): t =>
  z |> remold(~root) |> regrout(d);

/* Rescan ancestor-level siblings: converts standalone monotiles that
 * match a parent ancestor's missing shards, giving them the parent's
 * ID, then absorbs them into the parent via reassemble_parent-style
 * logic. This handles delimiters (e.g. =) re-inserted via paste with
 * fresh IDs that don't match their ancestor tile. */
let rescan_parent_shards = (z: t): t => {
  /* For each ancestor, compute its missing shards as (token, index) pairs */
  let ancestor_missing = (a: Ancestor.t): list((string, int)) => {
    let all_shards = fst(a.shards) @ snd(a.shards);
    List.init(List.length(a.label), Fun.id)
    |> List.filter(i => !List.mem(i, all_shards))
    |> List.map(i => (List.nth(a.label, i), i));
  };

  let convert_piece =
      (a: Ancestor.t, missing: list((string, int)), p: Piece.t): Piece.t =>
    switch (p) {
    | Tile(t) when List.length(t.shards) == 1 && t.id != a.Ancestor.id =>
      let tok = List.hd(Tile.effective_label(t));
      switch (List.assoc_opt(tok, missing)) {
      | Some(idx) =>
        Tile({
          ...t,
          id: a.Ancestor.id,
          label: a.Ancestor.label,
          mold: a.Ancestor.mold,
          shards: [idx],
        })
      | None => p
      };
    | _ => p
    };

  /* Try converting sibs against target ancestor's missing shards.
   * If conversion happens, absorb converted shards into the target
   * ancestor using reassemble_parent-style logic. Returns updated
   * (sibs, target) or None if no conversion happened. */
  let try_absorb =
      (sibs: Siblings.t, target: Ancestor.t)
      : option((Siblings.t, Ancestor.t)) => {
    let missing = ancestor_missing(target);
    if (missing == []) {
      None;
    } else {
      let convert = convert_piece(target, missing);
      let (l, r) = sibs;
      let new_sibs = (List.map(convert, l), List.map(convert, r));
      if (new_sibs == sibs) {
        None;
      } else {
        /* Absorb converted shards into target using split_by_matching */
        let flatten_match =
          Aba.fold_right(
            (t: Tile.t, kid, (shards, kids)) =>
              Aba.mk(t.shards @ shards, t.children @ [kid, ...kids]),
            (t: Tile.t) => Aba.mk(t.shards, t.children),
          );
        let (l_match, r_match) =
          new_sibs
          |> Siblings.split_by_matching(target.Ancestor.id)
          |> TupleUtil.map2(Aba.trim);
        let (target, new_l) =
          switch (l_match) {
          | None => (target, fst(new_sibs))
          | Some((outer_l, match_l, inner_l)) =>
            let (shards_l, kids_l) = flatten_match(match_l);
            let target = {
              ...target,
              shards: target.shards |> PairUtil.map_fst(ss => ss @ shards_l),
              children:
                target.children
                |> PairUtil.map_fst(kids =>
                     Segment.inner_regrout(kids @ [outer_l, ...kids_l])
                   ),
            };
            (target, inner_l);
          };
        let (target, new_r) =
          switch (r_match) {
          | None => (target, snd(new_sibs))
          | Some((inner_r, match_r, outer_r)) =>
            let (shards_r, kids_r) = flatten_match(match_r);
            let target = {
              ...target,
              shards: target.shards |> PairUtil.map_snd(ss => shards_r @ ss),
              children:
                target.children
                |> PairUtil.map_snd(kids =>
                     Segment.inner_regrout([outer_r, ...kids_r] @ kids)
                   ),
            };
            (target, inner_r);
          };
        Some(((new_l, new_r), target));
      };
    };
  };

  /* Walk ancestor chain. For each (inner, sibs), try to absorb
   * converted shards from `sibs` into the parent ancestor.
   * Also try direct siblings against the immediate ancestor. */
  let rec go = (ancestors: Ancestors.t): Ancestors.t =>
    switch (ancestors) {
    | [] => []
    | [(a, sibs)] => [(a, sibs)]
    | [(a, sibs), (parent, parent_sibs), ...rest] =>
      let rest = go([(parent, parent_sibs), ...rest]);
      switch (rest) {
      | [] => [(a, sibs)] /* shouldn't happen */
      | [(parent, parent_sibs), ...rest_tail] =>
        switch (try_absorb(sibs, parent)) {
        | None => [(a, sibs), (parent, parent_sibs), ...rest_tail]
        | Some((new_sibs, new_parent)) => [
            (a, new_sibs),
            (new_parent, parent_sibs),
            ...rest_tail,
          ]
        }
      };
    };

  let ancestors = go(z.relatives.ancestors);
  let (siblings, ancestors) =
    switch (ancestors) {
    | [] => (z.relatives.siblings, ancestors)
    | [(a, a_sibs), ...rest] =>
      switch (try_absorb(z.relatives.siblings, a)) {
      | None => (z.relatives.siblings, ancestors)
      | Some((new_sibs, new_a)) => (new_sibs, [(new_a, a_sibs), ...rest])
      }
    };

  if (ancestors == z.relatives.ancestors && siblings == z.relatives.siblings) {
    z;
  } else {
    {
      ...z,
      relatives: {
        siblings,
        ancestors,
      },
    };
  };
};

/* Rescan siblings for label-based shard conversion, then
 * reassemble + remold + regrout. This handles the case where
 * a standalone monotile should retroactively become a shard
 * of an incomplete tile (e.g. standalone `->` matching `fun`).
 * Should be called after edits, not during cursor movement. */
let rescan_reassemble = (~with_parent=false, d: Direction.t, z: t, ~root): t => {
  let siblings = Siblings.rescan(z.relatives.siblings);
  let z =
    if (siblings == z.relatives.siblings) {
      z;
    } else {
      let relatives =
        {
          ...z.relatives,
          siblings,
        }
        |> Relatives.reassemble
        |> (r => Relatives.remold(r, root))
        |> Relatives.regrout(d);
      {
        ...z,
        relatives,
      };
    };
  /* After normal rescan+reassemble, try matching shard tiles in
   * ancestor-level siblings against their ancestor's missing shards.
   * This handles delimiters (e.g. =) re-inserted via paste with fresh
   * IDs that don't match the ancestor tile. After converting IDs,
   * we flatten through the first ancestor so reassemble can rebuild
   * the tile at the correct scope. */
  if (with_parent) {
    let z' = rescan_parent_shards(z);
    if (z'.relatives.ancestors != z.relatives.ancestors
        || z'.relatives.siblings != z.relatives.siblings) {
      /* Parent rescan converted+absorbed shards. Remold and regrout
       * the updated relatives (no need to flatten/delete_parent since
       * try_absorb already restructured the ancestors in place). */
      let relatives =
        Relatives.remold(z'.relatives, root) |> Relatives.regrout(d);
      {
        ...z',
        relatives,
      };
    } else {
      z;
    };
  } else {
    z;
  };
};

let clear_unparsed_buffer = (z: t) =>
  switch (z.selection.mode) {
  | Buffer(Unparsed) => {
      ...z,
      selection: Selection.empty,
    }
  | _ => z
  };

let unselect = (~erase_buffer=false, z: t): t => {
  /* NOTE(andrew): Erase buffer flag only applies to unparsed buffer,
   * that is, the buffer style that just contains a single flat token.
   * Erasing a buffer that contains arbitrary tiles would be more complex
   * as we can't just empty the selection without regrouting */
  let z = erase_buffer ? clear_unparsed_buffer(z) : z;
  let relatives =
    z.relatives
    |> Relatives.prepend(z.selection.focus, z.selection.content)
    |> Relatives.reassemble;
  let selection = Selection.empty;
  {
    ...z,
    selection,
    relatives,
  };
};

/* Create a monotile piece from a token string with a generic mold.
 * Used for remainder pieces when splitting partial tokens during
 * char-level selection destruction. Callers' remold_regrout will
 * assign the correct mold. */
let mk_remainder_piece = (tok: Token.t): Piece.t =>
  if (Token.is_secondary(tok)) {
    Secondary(Secondary.mk(Id.mk(), tok));
  } else {
    Tile({
      id: Id.mk(),
      label: [tok],
      mold: Mold.mk_op(Sort.Any, []),
      shards: [0],
      children: [],
    });
  };

let destroy_selection: t => t =
  z =>
    unselect({
      ...z,
      selection: Selection.empty,
    });

/* Inner offsets of a char selection's boundaries, in the frame of the first
 * and last selected pieces; `None` means that side reaches the piece's outer
 * boundary. smart_rounded reads as Outer: the anchor is displayed at its
 * piece's edge, so the intent is "whole starting token". */
let char_selection_offsets = (z: t): (option(int), option(int)) => {
  let inner = (c: CaretBase.t): option(int) =>
    switch (c) {
    | CaretBase.Inner(n) => Some(n)
    | CaretBase.Outer => None
    };
  let anchor: CaretBase.t =
    z.selection.smart_rounded ? Outer : z.selection.anchor_caret;
  switch (z.selection.focus) {
  | Right => (inner(anchor), inner(z.caret))
  | Left => (inner(z.caret), inner(anchor))
  };
};

/* Whether the selection has at least one boundary strictly inside a piece,
 * i.e. `selection.content` holds more than what is actually selected. */
let has_char_selection = (z: t): bool =>
  !Selection.is_empty(z.selection)
  && (
    switch (char_selection_offsets(z)) {
    | (None, None) => false
    | _ => true
    }
  );

/* Splitting a string/comment literal would strand its delimiters, so those
 * boundary tokens are kept whole; everything else splits on the offset. */
let splittable_token = (p: Piece.t): option(Token.t) =>
  switch (Piece.token_of(p)) {
  | Some(tok) when !Token.is_string_or_comment(tok) => Some(tok)
  | _ => None
  };

/* Fragments inherit the source tile's mold, because they can land outside
 * the following remold pass (wrap_balanced moves them into the ancestor
 * frame) where an Any-sorted placeholder attracts spurious grout. Shards
 * have no mold to reuse and fall back to the generic monotile. */
let split_piece = (p: Piece.t, tok: Token.t): Piece.t =>
  switch (p) {
  | Tile({label: [_], shards: [0], _} as t) =>
    Tile({
      ...t,
      id: Id.mk(),
      label: [tok],
    })
  | _ => mk_remainder_piece(tok)
  };

/* (unselected head, selected pieces, unselected tail). Nothing is deleted or
 * placed — callers decide where the remainders go, unlike
 * normalize_char_selection, which drops the selection and rejoins them.
 * Returns `([], content, [])` when nothing is partially selected. */
let split_char_selection = (z: t): (Segment.t, Segment.t, Segment.t) =>
  if (!has_char_selection(z)) {
    ([], z.selection.content, []);
  } else {
    let content = z.selection.content;
    let (left_offset, right_offset) = char_selection_offsets(z);
    let seg = (p: Piece.t, tok: Token.t): Segment.t =>
      tok == "" ? [] : [split_piece(p, tok)];
    /* Offsets are token positions of the caret, one past the char they name */
    let cut = (tok: Token.t, n: int) =>
      Token.split_nth(tok, max(0, min(n, Token.length(tok))));
    switch (content) {
    | [] => ([], content, [])
    | [p] =>
      /* Both boundaries in one token: head and tail are separated by the
       * selection, so unlike deletion they never need rejoining. */
      switch (splittable_token(p)) {
      | None => ([], content, [])
      | Some(tok) =>
        let len = Token.length(tok);
        let lo = Option.fold(~none=0, ~some=n => n + 1, left_offset);
        let lo = max(0, min(lo, len));
        let hi = Option.fold(~none=len, ~some=n => n + 1, right_offset);
        let hi = max(lo, min(hi, len));
        let (head, rest) = cut(tok, lo);
        let (mid, tail) = cut(rest, hi - lo);
        (seg(p, head), seg(p, mid), seg(p, tail));
      }
    | [first, ...rest] =>
      let (middle, last) = ListUtil.split_last(rest);
      let (left_rem, first_sel) =
        switch (left_offset, splittable_token(first)) {
        | (Some(n), Some(tok)) =>
          let (head, sel) = cut(tok, n + 1);
          (seg(first, head), seg(first, sel));
        | _ => ([], [first])
        };
      let (last_sel, right_rem) =
        switch (right_offset, splittable_token(last)) {
        | (Some(n), Some(tok)) =>
          let (sel, tail) = cut(tok, n + 1);
          (seg(last, sel), seg(last, tail));
        | _ => ([last], [])
        };
      (left_rem, first_sel @ middle @ last_sel, right_rem);
    };
  };

/* Normalize a char-level selection before destruction.
 * Splits partial boundary tokens and keeps the exterior (unselected)
 * portions, setting caret appropriately. Must be called explicitly
 * by top-level actions (Destruct, Insert) — NOT from internal helpers
 * like replace_shard which set Inner caret for other purposes. */
let normalize_char_selection = (z: t): t =>
  if (!has_char_selection(z)) {
    z;
  } else {
    let content = z.selection.content;
    let (left_offset, right_offset) = char_selection_offsets(z);

    /* Compute left remainder (exterior chars before left boundary) */
    let left_remainder =
      switch (left_offset) {
      | None => None
      | Some(n) =>
        switch (content) {
        | [] => None
        | [p, ..._] =>
          switch (Piece.token_of(p)) {
          | Some(tok) =>
            let (rest, _) = Token.split_nth(tok, n + 1);
            rest == "" ? None : Some(rest);
          | None => None
          }
        }
      };

    /* Compute right remainder (exterior chars after right boundary) */
    let right_remainder =
      switch (right_offset) {
      | None => None
      | Some(n) =>
        switch (ListUtil.last_opt(content)) {
        | None => None
        | Some(p) =>
          switch (Piece.token_of(p)) {
          | Some(tok) =>
            let (_, rest) = Token.split_nth(tok, n + 1);
            rest == "" ? None : Some(rest);
          | None => None
          }
        }
      };

    /* For strings and comments, preserve delimiters that would be
     * destroyed. If the selection includes an opening/closing delimiter,
     * add it back to the corresponding remainder. */
    let (left_remainder, right_remainder) =
      switch (content) {
      | [p] when Piece.token_of(p) != None =>
        let tok = Option.get(Piece.token_of(p));
        if (Token.is_string_or_comment(tok)) {
          let tok_len = Token.length(tok);
          let (opening, _) = Token.split_nth(tok, 1);
          let (_, closing) = Token.split_nth(tok, tok_len - 1);
          /* Check if opening delimiter is in the selected (deleted) range */
          let left_remainder =
            switch (left_offset) {
            | None when tok_len > 0 =>
              /* Selection starts at piece boundary → includes opening delimiter */
              switch (left_remainder) {
              | None => Some(opening)
              | Some(r) => Some(opening ++ r)
              }
            | _ => left_remainder
            };
          /* Check if closing delimiter is in the selected (deleted) range */
          let right_remainder =
            switch (right_offset) {
            | None when tok_len > 0 =>
              /* Selection ends at piece boundary → includes closing delimiter */
              switch (right_remainder) {
              | None => Some(closing)
              | Some(r) => Some(r ++ closing)
              }
            | _ => right_remainder
            };
          (left_remainder, right_remainder);
        } else {
          (left_remainder, right_remainder);
        };
      | _ => (left_remainder, right_remainder)
      };

    let left_str = Option.value(left_remainder, ~default="");
    let right_str = Option.value(right_remainder, ~default="");
    let is_single_piece = List.length(content) == 1;

    if (is_single_piece) {
      /* Single-piece: combine remainders into one token to avoid
       * creating two adjacent pieces that would need grout between them.
       * Caret is set to Inner at the seam position so callers (like
       * Insert.go) can insert at the correct spot within the token. */
      let combined = left_str ++ right_str;
      let z = {
        ...z,
        selection: Selection.empty,
        caret: Outer,
      };
      let z = unselect(z);
      if (combined == "") {
        z;
      } else {
        let piece = mk_remainder_piece(combined);
        let seam_pos = Token.length(left_str);
        let combined_len = Token.length(combined);
        let max_idx = combined_len - 2;
        if (seam_pos == 0) {
          /* No left remainder: piece on right, caret before it */
          let siblings =
            Siblings.prepend(Right, [piece], z.relatives.siblings);
          let relatives =
            Relatives.reassemble({
              ...z.relatives,
              siblings,
            });
          {
            ...z,
            caret: Outer,
            relatives,
          };
        } else if (seam_pos >= combined_len) {
          /* No right remainder: piece on left, caret after it */
          let siblings =
            Siblings.prepend(Left, [piece], z.relatives.siblings);
          let relatives =
            Relatives.reassemble({
              ...z.relatives,
              siblings,
            });
          {
            ...z,
            caret: Outer,
            relatives,
          };
        } else {
          /* Seam in middle: piece on right, Inner caret at seam */
          let siblings =
            Siblings.prepend(Right, [piece], z.relatives.siblings);
          let relatives =
            Relatives.reassemble({
              ...z.relatives,
              siblings,
            });
          {
            ...z,
            caret: Inner(min(seam_pos - 1, max_idx)),
            relatives,
          };
        };
      };
    } else {
      /* Multi-piece: separate remainders. Callers (Destruct.go) run
       * merge_or_noop which will merge adjacent compatible tokens. */
      let siblings = z.relatives.siblings;
      let siblings =
        switch (left_remainder) {
        | Some(tok) =>
          Siblings.prepend(Left, [mk_remainder_piece(tok)], siblings)
        | None => siblings
        };
      let siblings =
        switch (right_remainder) {
        | Some(tok) =>
          Siblings.prepend(Right, [mk_remainder_piece(tok)], siblings)
        | None => siblings
        };
      let relatives =
        Relatives.reassemble({
          ...z.relatives,
          siblings,
        });
      {
        ...z,
        caret: Outer,
        selection: Selection.empty,
        relatives,
      };
    };
  };

let unselect_and_zip = (~erase_buffer=false, z: t): Segment.t =>
  z |> unselect(~erase_buffer) |> zip;

let replace_selection = (focus, segment, z: t): t => {
  ...z,
  selection: Selection.mk(~focus, segment),
};

let grow_selection = (z: t): option(t) => {
  let+ (p, relatives) = Relatives.pop(z.selection.focus, z.relatives);
  let selection = Selection.push(p, z.selection);
  {
    ...z,
    selection,
    relatives,
  };
};

/* Like grow_selection but skips reassembly in push. Used during
 * char-level selection to prevent shard merging. */
let grow_selection_raw = (z: t): option(t) => {
  let+ (p, relatives) = Relatives.pop(z.selection.focus, z.relatives);
  let selection = Selection.push_raw(p, z.selection);
  {
    ...z,
    selection,
    relatives,
  };
};

// toggles focus and grows if selection is empty
let shrink_selection = (z: t): option(t) => {
  switch (Selection.pop(z.selection)) {
  | None =>
    let selection = Selection.toggle_focus(z.selection);
    grow_selection({
      ...z,
      selection,
    });
  | Some((p, selection)) =>
    let relatives =
      z.relatives
      |> Relatives.push(selection.focus, p)
      |> Relatives.reassemble;
    Some({
      ...z,
      selection,
      relatives,
    });
  };
};

let toggle_focus = (z: t): t => {
  /* Swap caret and anchor_caret so each end retains its position.
   * Both are CaretBase.t so no conversion needed. */
  let new_anchor_caret = z.caret;
  let new_caret = z.selection.anchor_caret;
  {
    ...z,
    caret: new_caret,
    selection: {
      ...Selection.toggle_focus(z.selection),
      anchor_caret: new_anchor_caret,
    },
  };
};

let set_focus = (z: t, d: Direction.t): t => {
  let selection = {
    ...z.selection,
    focus: d,
  };
  {
    ...z,
    selection,
  };
};

let directional_unselect = (d: Direction.t, z: t): t => {
  let landing_at_anchor = d != z.selection.focus;
  /* Determine the target caret after unselect.
   * Both caret and anchor_caret are CaretBase.t so no conversion needed. */
  let target_caret =
    if (landing_at_anchor) {
      z.selection.anchor_caret;
    } else {
      z.caret;
    };
  let selection = {
    ...z.selection,
    focus: Direction.toggle(d),
  };
  let z =
    unselect({
      ...z,
      selection,
    });
  let z = {
    ...z,
    caret: target_caret,
  };
  /* Inner(n) references the right neighbor. After unselect, if the
   * referenced piece ended up in left siblings instead, move it right. */
  switch (target_caret) {
  | Inner(_) when Siblings.neighbor(Right, z.relatives.siblings) == None =>
    switch (Relatives.pop(Left, z.relatives)) {
    | Some((p, relatives)) =>
      let relatives = Relatives.push(Right, p, relatives);
      {
        ...z,
        relatives,
      };
    | None => z
    }
  | _ => z
  };
};

let unselect = (z: t): t =>
  z.selection.content == [] ? z : directional_unselect(z.selection.focus, z);

let move = (d: Direction.t, z: t): option(t) =>
  if (Selection.is_empty(z.selection)) {
    let+ (p, relatives) = Relatives.pop(d, z.relatives);
    let relatives =
      relatives
      |> Relatives.push(Direction.toggle(d), p)
      |> Relatives.reassemble;
    {
      ...z,
      relatives,
    };
  } else {
    Some(directional_unselect(d, z));
  };

let select = (d: Direction.t, z: t): option(t) =>
  d == z.selection.focus ? grow_selection(z) : shrink_selection(z);

/* As opposed to the Siblings.neighbor functions, which simply returns
 * the adjacent piece (if any) in the focal segment, this function is a
 * more general notion of 'the token to the left/right' of the cursor'.
 * It agrees with Sibling.neighbor whenever you are in the middle of
 * the focal segment; it returns None only if you are at the start/end
 * of the entire program, and if you are at an extreme of the focal
 * segment it returns the ADJACENT SHARD of the containing parent.
 * Note that this last case necessarily returns an incomplete tile and
 * thus does not retain knowledge of the tile's in-situ completeness */
let generalized_neighbor = (d: Direction.t, z: t): option(Piece.t) => {
  let uz = unselect(z);
  let* z = select(d, uz);
  switch (z.selection.content) {
  | [p] => Some(p)
  | _ => None
  };
};

type neighbors = (option(Piece.t), option(Piece.t));

let generalized_neighbors = (z: t): neighbors => (
  generalized_neighbor(Left, z),
  generalized_neighbor(Right, z),
);

let neighbor_token = (d: Direction.t, z: t): option(Token.t) => {
  let* p = generalized_neighbor(d, z);
  Piece.token_of(p);
};

let neighbor_tokens = (z: t): (option(Token.t), option(Token.t)) => (
  neighbor_token(Left, z),
  neighbor_token(Right, z),
);

/* Iterative version to avoid stack overflow on large programs */
let do_until_piece =
    (action: t => option(t), p_n: neighbors => bool, z: t): option(t) => {
  let current = ref(action(z));
  let result = ref(None);
  let done_ = ref(false);
  while (! done_^) {
    switch (current^) {
    | None =>
      result := None;
      done_ := true;
    | Some(z) =>
      if (p_n(Siblings.neighbors(z.relatives.siblings))) {
        result := Some(z);
        done_ := true;
      } else {
        current := action(z);
      }
    };
  };
  result^;
};

/* Do `action` until the predicate on the generalized neigbors of the
   caret becomes true. A generalized neighbor is the neighboring piece, unless
   the neighbor is a polytile, in which case it's the relevant shard, or
   we are at the edge of a segment, in which case it's the relevant shard
   of the parent. The None case strictly means the beginning/end of the program.
   If no such piece is found, don't move. Does not check predicate before
   moving; caller should handle that case if necessary.

   NOTE: This is implemented iteratively to avoid stack overflow on large
   programs. The previous recursive implementation would overflow when
   traversing documents with thousands of tokens. */
let do_until =
    (action: t => option(t), p_n: neighbors => bool, z: t): option(t) => {
  let current = ref(action(z));
  let result = ref(None);
  let done_ = ref(false);
  while (! done_^) {
    switch (current^) {
    | None =>
      result := None;
      done_ := true;
    | Some(z) =>
      if (p_n(generalized_neighbors(z))) {
        result := Some(z);
        done_ := true;
      } else {
        current := action(z);
      }
    };
  };
  result^;
};

let do_to_extreme = (action: t => option(t), z: t): t =>
  do_until(
    action,
    (neighbors: neighbors) =>
      switch (neighbors) {
      | (None, _) => true
      | (_, None) => true
      | _ => false
      },
    z,
  )
  |> Option.value(~default=z);

let linebreak_on = (d: Direction.t, neighbors: neighbors): bool =>
  switch (neighbors) {
  | (_, Some(Secondary(s))) when d == Right && Secondary.is_linebreak(s) =>
    true
  | (_, None) when d == Right => true
  | (Some(Secondary(s)), _) when d == Left && Secondary.is_linebreak(s) =>
    true
  | (None, _) when d == Left => true
  | _ => false
  };

let do_until_linebreak =
    (f: t => option(t), d: Direction.t, z: t): option(t) =>
  linebreak_on(d, generalized_neighbors(z))
    ? Some(z) : do_until(f, linebreak_on(d), z);

let local_backpack = (z: t): list(Tile.t) =>
  Relatives.local_missing_shards(z.relatives);

let backpack_hd = (z: t): option(Tile.t) =>
  z |> local_backpack |> ListUtil.hd_opt;

let backpack_find = (tok: Token.t, z: t): option(Tile.t) =>
  if (Form.is_ambiguous_polymorph(tok)) {
    /* Special case for ambiguous polymorphs. These tokens
       occur both on their own as infix ops and as delimiters of
       multi-delimiter forms. To give the singleton form a chance, we
       only match these to incomplete tiles to form their multi forms
       when they're on the top of the stack */
    backpack_hd(z) |> Option.map(Tile.effective_label) == Some([tok])
      ? backpack_hd(z) : None;
  } else {
    List.find_map(
      t => Tile.effective_label(t) == [tok] ? Some(t) : None,
      local_backpack(z),
    );
  };

let insert_segment = (z: t, seg: Segment.t, ~root): t =>
  z
  |> replace_selection(Right, seg)
  |> unselect
  |> remold_regrout(Right, ~root);

let adj_pos = (d: Direction.t, z: t): t =>
  switch (d) {
  | Left => z
  | Right =>
    switch (move(Left, z)) {
    | None => z
    | Some(z) => z
    }
  };

/* Unselect with the caret Outer: directional_unselect would step an Inner
 * caret one piece right, and the caller's adj_pos(Right) already does that,
 * leaving Inner(n) indexing the wrong token. */
let put_down_core = (seg: Segment.t, z: t): t => {
  let caret = z.caret;
  let z =
    {
      ...z,
      caret: Outer,
    }
    |> replace_selection(Right, seg)
    |> unselect;
  {
    ...z,
    caret,
  };
};

/* Like put_down_core but skips Relatives.reassemble.
 * Used for Inner-caret edits where the replaced token would
 * otherwise be absorbed back into an ancestor tile during
 * reassembly, leaving the caret pointing at the wrong piece. */
let put_down_no_reassemble = (seg: Segment.t, z: t): t => {
  let z = z |> replace_selection(Right, seg);
  let relatives =
    z.relatives |> Relatives.prepend(z.selection.focus, z.selection.content);
  let selection = Selection.empty;
  {
    ...z,
    selection,
    relatives,
  };
};

let put_down_seg = (d: Direction.t, seg: Segment.t, z: t): t =>
  z |> put_down_core(seg) |> adj_pos(d);

let can_put_down = z =>
  switch (local_backpack(z)) {
  | [] => false
  | _ => z.caret == Outer
  };

let put_down_target = (d: Direction.t, target: Tile.t, z: t, ~root): t =>
  z
  |> put_down_core([Tile(target)])
  |> remold_regrout(Left, ~root)
  |> adj_pos(d);

let put_down = (z: t, ~root): option(t) =>
  z.caret == Outer
    ? {
      let+ target = backpack_hd(z);
      put_down_target(Left, target, z, ~root);
    }
    : None;

let delete = (d: Direction.t, z: t): option(t) =>
  z |> select(d) |> Option.map(destroy_selection);

let adjacent_monotile_id = (d: Direction.t, z: t): option(Id.t) =>
  switch (Siblings.neighbors(z.relatives.siblings)) {
  | (Some(Tile({id, label: [_], _})), _) when d == Left => Some(id)
  | (_, Some(Tile({id, label: [_], _}))) when d == Right => Some(id)
  | _ => None
  };

let adjacent_monotile_or_new_id = (d, z) =>
  switch (adjacent_monotile_id(d, z)) {
  | Some(id) => id
  | None => Id.mk()
  };

let representative_piece = (z: t): option((Piece.t, Direction.t)) => {
  /* The piece to the left of the caret, or if none exists, the piece to the right */
  switch (Siblings.neighbors(sibs_with_sel(z))) {
  | (Some(l), _) => Some((l, Left))
  | (_, Some(r)) => Some((r, Right))
  | _ => None
  };
};

let base_point = (measured: Measured.t, z: t): Point.t => {
  switch (representative_piece(z)) {
  | Some((p, d)) =>
    let seg = Piece.disassemble(p);
    switch (d) {
    | Left =>
      let p = ListUtil.last(seg);
      let m = Measured.find_p(~msg="base_point", p, measured);
      m.last;
    | Right =>
      let p = List.hd(seg);
      let m = Measured.find_p(~msg="base_point", p, measured);
      m.origin;
    };
  | None => {
      row: 0,
      col: 0,
    }
  };
};

module Caret = {
  /* Any shard can span more columns than it has graphemes, because emoji and
     CJK render two columns wide. Translate an inner caret index into measured
     columns by consulting the token width table. */
  let token_offset = (token: Token.t, idx: int): int =>
    Token.prefix_columns(token, idx + 1);

  /* Columns to advance for an Inner caret. `Inner(n)` is
     right-neighbor-relative (see Move: the right generalized neighbor is the
     piece it indexes into), so the RIGHT token decides the offset; the left
     token is only consulted at the very end of the program, where there is
     no right neighbor. */
  let inner_offset = (idx: int, z: t): int =>
    switch (neighbor_token(Right, z)) {
    | Some(token) => token_offset(token, idx)
    | None =>
      switch (neighbor_token(Left, z)) {
      | Some(token) => token_offset(token, idx)
      | None => idx + 1
      }
    };

  let offset = (z: t): int =>
    switch (z.caret) {
    | Outer => 0
    | Inner(idx) => inner_offset(idx, z)
    };

  let set = (caret: caret, z: t): t => {
    ...z,
    caret,
  };

  /* Max internal index of the shard the caret is adjacent to */
  let nhbr_max_idx = (d: Direction.t, z: t): option(int) => {
    let* t =
      switch (d, neighbor_tokens(z)) {
      | (Left, (Some(t), _)) => Some(t)
      | (Right, (_, Some(t))) => Some(t)
      | _ => None
      };
    let max_idx = Token.length(t) - 2;
    max_idx < 0 ? None : Some(max_idx);
  };

  /* Direction the caret is facing in */
  let direction = (z: t): option(Direction.t) =>
    switch (z.caret) {
    | Inner(_) => None
    | Outer =>
      switch (Siblings.neighbors(sibs_with_sel(z))) {
      | (Some(l), Some(r))
          when
            Piece.is_secondary(l)
            && Piece.is_secondary(r)
            && Selection.is_empty(z.selection) =>
        None
      | _ => Siblings.direction_between(sibs_with_sel(z))
      }
    };

  /* Compute inner offset using a known token (avoids generalized_neighbor
   * which unselects and gives wrong results during char-level selection). */
  let inner_offset_for_token = (idx: int, token: Token.t): int =>
    token_offset(token, idx);

  /* Like inner_offset_for_token but counts GRAPHEMES, not display columns: a
     wide char (e.g. an emoji) is one grapheme but two columns. Used for
     clipboard text slicing, where the column count would over-trim. */
  let inner_grapheme_offset = (idx: int): int => idx + 1;

  /* Grid position of the caret */
  /* Convert a caret to a concrete grid point for rendering and hit testing. */
  let point = (measured: Measured.t, z: t): Point.t =>
    switch (z.caret, z.selection.content) {
    | (Inner(idx), [_, ..._]) =>
      /* Char-level selection: caret is inside the focus-side boundary
       * piece of the selection. Inner(n) always indexes left-to-right
       * from the token's origin, regardless of focus direction. */
      let focus_piece =
        switch (Selection.focus_piece(z.selection)) {
        | Some(p) => p
        | None => failwith("Caret.point: Inner caret with empty selection")
        };
      let seg = Piece.disassemble(focus_piece);
      /* Always use the first shard to get origin */
      let p = List.hd(seg);
      let m = Measured.find_p(~msg="caret_point_charsel", p, measured);
      let offset =
        switch (Piece.token_of(focus_piece)) {
        | Some(tok) => inner_offset_for_token(idx, tok)
        | None => idx + 1
        };
      {
        row: m.origin.row,
        col: m.origin.col + offset,
      };
    | _ =>
      let Point.{row, col} = base_point(measured, z);
      {
        row,
        col: col + offset(z),
      };
    };

  type t = ZipperBase.caret;
};

/* Compute character offsets to trim from the left and right ends
 * of the printed selection content string. Returns (left_chars_to_skip,
 * right_chars_to_skip). */
let selection_trim_offsets = (z: t): (int, int) => {
  let left_trim = (inner_n, content, focus) => {
    let p =
      switch ((focus: Direction.t)) {
      | Right => List.hd(content)
      | Left => ListUtil.last(content)
      };
    let shard = List.hd(Piece.disassemble(p));
    switch (Piece.token_of(shard)) {
    | Some(_) => Caret.inner_grapheme_offset(inner_n)
    | None => 0
    };
  };
  let right_trim = (inner_n, content, focus) => {
    let p =
      switch ((focus: Direction.t)) {
      | Right => ListUtil.last(content)
      | Left => List.hd(content)
      };
    let seg = Piece.disassemble(p);
    let last_shard = ListUtil.last(seg);
    switch (Piece.token_of(last_shard)) {
    | Some(tok) =>
      let tok_len = Unicode.length(tok);
      tok_len - Caret.inner_grapheme_offset(inner_n);
    | None => 0
    };
  };
  let content = z.selection.content;
  /* When smart_rounded, the anchor displays at its piece's outer
   * boundary, so no trim from that side. */
  let effective_anchor_caret: CaretBase.t =
    z.selection.smart_rounded ? Outer : z.selection.anchor_caret;
  switch (z.selection.focus) {
  | Right => (
      switch (effective_anchor_caret) {
      | CaretBase.Inner(n) => left_trim(n, content, Right)
      | CaretBase.Outer => 0
      },
      switch (z.caret) {
      | Inner(n) => right_trim(n, content, Right)
      | Outer => 0
      },
    )
  | Left => (
      switch (z.caret) {
      | Inner(n) => left_trim(n, content, Left)
      | Outer => 0
      },
      switch (effective_anchor_caret) {
      | CaretBase.Inner(n) => right_trim(n, content, Left)
      | CaretBase.Outer => 0
      },
    )
  };
};

/* Trim a printed selection string to account for char-level
 * boundaries. Takes the full printed text of selection.content
 * and trims characters from both ends as needed. */
let trim_selected_text = (z: t, full: string): string =>
  if (Selection.is_empty(z.selection)) {
    "";
  } else {
    let (l, r) = selection_trim_offsets(z);
    let total = Unicode.length(full);
    let len = total - l - r;
    if (len <= 0) {
      "";
    } else {
      let (_, after_left) = Token.split_nth(full, l);
      let (selected, _) = Token.split_nth(after_left, len);
      selected;
    };
  };

let do_towards_point =
    (
      ~anchor: option(Measured.Point.t)=?,
      ~measured: Measured.t,
      ~force_progress: bool=false,
      f: (Direction.t, t) => option(t),
      goal: Measured.Point.t,
      z: t,
    )
    : option(t) => {
  let caret_point = Caret.point(measured);

  let is_at_side_of_row = (d: Direction.t, z: t) => {
    let Point.{row, col} = caret_point(z);
    switch (move(d, z)) {
    | None => true
    | Some(z) =>
      let Point.{row: rowp, col: colp} = caret_point(z);
      row != rowp || col == colp;
    };
  };

  let direction_to_from = (p1: Point.t, p2: Point.t): Direction.t => {
    let before_row = p1.row < p2.row;
    let at_row = p1.row == p2.row;
    let before_col = p1.col < p2.col;
    before_row || at_row && before_col ? Left : Right;
  };

  let closer_to_prev = (curr, prev, goal: Point.t) =>
    /* Default to true if equal */
    abs(caret_point(prev).col - goal.col)
    < abs(caret_point(curr).col - goal.col);

  let init = caret_point(z);
  let d_to_goal = direction_to_from(goal, init);
  let max_iter = 100_000;
  let rec go = (iter: int, prev: t, curr: t) => {
    if (iter > max_iter) {
      failwith(
        "do_towards_point: exceeded "
        ++ string_of_int(max_iter)
        ++ " iterations (goal="
        ++ Point.show(goal)
        ++ ", init="
        ++ Point.show(init)
        ++ ", curr="
        ++ Point.show(caret_point(curr))
        ++ ")",
      );
    };
    let curr_p = caret_point(curr);
    let x_progress = Point.dcomp(d_to_goal, curr_p.col, goal.col);
    let y_progress = Point.dcomp(d_to_goal, curr_p.row, goal.row);
    switch (y_progress, x_progress) {
    /* If we're not there yet, keep going */
    | (Under, Over | Exact | Under)
    | (Exact, Under) =>
      switch (f(d_to_goal, curr)) {
      | Some(next) =>
        /* Guard: if f didn't advance the caret, stop to prevent
         * infinite loops (e.g. zero-width pieces, measured edge cases) */
        let next_p = caret_point(next);
        Point.equals(next_p, curr_p) ? curr : go(iter + 1, curr, next);
      | None => curr /* Should only occur at start/end of program */
      }
    /* If we're there, stop */
    | (Exact, Exact) => curr
    /* If we've overshot, meaning the exact goal is inaccessible,
     * we choose between current and previous (undershot) positions */
    | (Over, Over | Exact | Under) =>
      switch (force_progress) {
      /* Ideally we would use the same logic as from the below
       * anchor case here; however that results in strange
       * behavior when accidentally starting a drag at the end
       * of a line, which triggers the (invisible) selection of
       * a linebreak, making it appear that the caret has jumped
       * to the next line. The downside of leaving this as-is is
       * that multiline tokens (projectors) do not become part of
       * the selection when dragging until you're all the way
       * over them, which is slightly visually jarring */
      | false => prev
      /* Up/down kb movement works by setting a goal one row
       * below the current. When adjacent to a multiline token,
       * the nearest next caret position may be multiple lines down.
       * We must allow this overshoot in order to make progress. */
      | true => caret_point(prev) == init ? curr : prev
      }
    | (Exact, Over) =>
      switch (anchor) {
      | None =>
        /* If you're trying to (eg) move down at the end of a row
         * but the first position of the next row is further right
         * than the currentrow's end, we want to make progress
         * regardless of whether the new position would be closer
         * or further from the goal.  Otherwise, we try to just
         * get as close as we can  */
        is_at_side_of_row(Direction.toggle(d_to_goal), curr)
          ? curr : closer_to_prev(curr, prev, goal) ? prev : curr
      | Some(anchor) =>
        /* If we're dragging to make a selection, decide whether or
         * not to force progress based on the relative position of the
         * anchor (the position where the drag was started) */
        direction_to_from(goal, anchor) == d_to_goal ? curr : prev
      }
    };
  };
  let res = go(0, z, z);
  Measured.Point.equals(caret_point(res), caret_point(z))
    ? None : Some(res);
};

let selection_anchor_point = (measured, z: t): option(Point.t) => {
  switch (Selection.anchor_piece(z.selection)) {
  | None => None
  | Some(anchor_piece) =>
    /* In smart-rounded mode, render the anchor at the outer boundary
     * of the anchor piece regardless of anchor_caret's inner position.
     * This lets the partial-token anchor be preserved internally (so
     * dragging back in restores it) while the current visible
     * selection rounds up to the whole token. */
    let anchor_caret: CaretBase.t =
      z.selection.smart_rounded ? Outer : z.selection.anchor_caret;
    let seg = Piece.disassemble(anchor_piece);
    switch (z.selection.focus) {
    | Right =>
      /* Anchor is at the LEFT end */
      let p = List.hd(seg);
      let m = Measured.find_p(~msg="selection_anchor_point", p, measured);
      switch (anchor_caret) {
      | CaretBase.Outer => Some(m.origin)
      | CaretBase.Inner(idx) =>
        let offset =
          switch (Piece.token_of(anchor_piece)) {
          | Some(tok) => Caret.inner_offset_for_token(idx, tok)
          | None => idx + 1
          };
        Some({
          row: m.origin.row,
          col: m.origin.col + offset,
        });
      };
    | Left =>
      /* Anchor is at the RIGHT end */
      let p = ListUtil.last(seg);
      let m = Measured.find_p(~msg="selection_anchor_point", p, measured);
      switch (anchor_caret) {
      | CaretBase.Outer => Some(m.last)
      | CaretBase.Inner(idx) =>
        let offset =
          switch (Piece.token_of(anchor_piece)) {
          | Some(tok) => Caret.inner_offset_for_token(idx, tok)
          | None => idx + 1
          };
        let p_first = List.hd(seg);
        let m_first =
          Measured.find_p(
            ~msg="selection_anchor_point_origin",
            p_first,
            measured,
          );
        Some({
          row: m_first.origin.row,
          col: m_first.origin.col + offset,
        });
      };
    };
  };
};

let set_buffer = (z: t, ~mode: Selection.buffer, ~content: Segment.t): t => {
  ...z,
  selection: Selection.mk_buffer(mode, content),
};

let is_linebreak_to_right_of_caret =
    ({relatives: {siblings: (_, r), _}, _}: t): bool => {
  switch (r) {
  | [Secondary(s), ..._] when Secondary.is_linebreak(s) => true
  | _ => false
  };
};

open Util.OptUtil.Syntax;
open TyDiSuggestion;
open Language;

/* Minimum number of characters required before showing completions.
 * Adjust this value to control when suggestions first appear. */
let min_prefix_len = 2;

/* Suggest the token at the top of the backpack, if we can put it down */
let suggest_backpack = (z: Zipper.t): list(t) => {
  /* Note: Sort check unnecessary here as wouldn't be able to put down */
  switch (Zipper.local_backpack(z)) {
  | [] => []
  | [t, ..._] =>
    switch (t) {
    | {label, shards: [idx], _} when Zipper.can_put_down(z) => [
        {
          content: List.nth(label, idx),
          strategy: Any(FromBackpack),
        },
      ]
    | _ => []
    }
  };
};

/* Check if the expected type is unknown (no type annotation context) */
let has_unknown_expectation = (ci: Info.t): bool =>
  switch (ci) {
  | InfoExp({ana, _})
  | InfoPat({ana, _}) =>
    switch (Typ.term_of(ana)) {
    | Unknown(_) => true
    | _ => false
    }
  | _ => false
  };

let suggest = (ci: Info.t, z: Zipper.t): list(t) => {
  /* NOTE: Sorting ensures that if we have an exact match already,
   * we won't suggest extending it, but straight-up lexical sorting
   * may not be desirable in other ways, for example maybe we want
   * recency bias in ctx. Revisit this later. I'm sorting before
   * combination because we want backpack candidates to show up first */
  switch (ci) {
  | InfoExp({dot_labels, _}) when dot_labels != [] =>
    List.map(
      label =>
        TyDiSuggestion.{
          content: label,
          strategy: Exp(Common(FromCtx(Label(label) |> Typ.fresh))),
        },
      dot_labels,
    )
  | InfoTyp({expects: LabelProjectionExpected(Some(labels)), _})
      when labels != [] =>
    List.map(
      label =>
        TyDiSuggestion.{
          content: label,
          strategy: Typ(FromCtx),
        },
      labels,
    )
  | InfoExp({label_sort: true, _})
  | InfoPat({label_sort: true, _})
  | InfoExp({cls: Exp(Label), _})
  | InfoPat({cls: Pat(Label), _})
  | InfoTyp({cls: Typ(Label), _})
  | InfoExp({cls: Exp(TupLabel), _})
  | InfoPat({cls: Pat(TupLabel), _})
  | InfoTyp({cls: Typ(TupLabel), _}) => []
  | _ =>
    /* When the expected type is unknown (e.g., no type annotation),
     * prioritize keywords/forms over context variables. This prevents
     * e.g. 'f' completing to 'false' when the user likely wants 'fun'. */
    let forms =
      TyDiForms.suggest_leading(ci)
      @ TyDiForms.suggest_operand(ci)
      |> List.sort(TyDiSuggestion.compare);
    let ctx_suggestions =
      TyDiCtx.suggest_variable(ci)
      @ TyDiCtx.suggest_lookahead_variable(ci)
      |> List.sort(TyDiSuggestion.compare);
    let operators =
      TyDiForms.suggest_operator(ci) |> List.sort(TyDiSuggestion.compare);
    if (has_unknown_expectation(ci)) {
      /* Unknown type: keywords first, then context, then operators */
      suggest_backpack(z) @ forms @ ctx_suggestions @ operators;
    } else {
      /* Known type: context variables first (type-directed), then forms */
      suggest_backpack(z) @ ctx_suggestions @ forms @ operators;
    };
  };
};

/* If there is a monotile to the left of the caret, return it. We
 * currently only make suggestions in such situations */
let token_to_left = (z: Zipper.t): option(string) =>
  switch (
    z.caret,
    z.relatives.siblings |> fst |> List.rev,
    z.relatives.siblings |> snd,
  ) {
  | (Outer, [Tile({label: [tok_to_left], _}), ..._], _) =>
    Some(tok_to_left)
  | _ => None
  };

/* The selection buffer used by TyDi is currently unstructured; it simply
 * holds an unparsed string, which is parsed via the same mechanism as
 * Paste only when a suggestion is accepted. */
let mk_unparsed_buffer = (t: Token.t): Segment.t => {
  [
    Secondary({
      id: Id.mk(),
      content: Comment(t),
    }),
  ];
};

/* If 'current' is a proper prefix of 'candidate', return the
 * suffix such that current ++ suffix == candidate */
let suffix_of = (candidate: Token.t, current: Token.t): option(Token.t) => {
  let candidate_suffix =
    String.sub(
      candidate,
      String.length(current),
      String.length(candidate) - String.length(current),
    );
  candidate_suffix == "" ? None : Some(candidate_suffix);
};

/* Convert buffer segment pieces to a display string.
 * Comment text is kept as-is, Grout becomes ○. */
let buffer_to_string = (seg: Segment.t): string =>
  String.concat(
    "",
    List.map(
      (p: Piece.t) =>
        switch (p) {
        | Secondary({content: Comment(s), _}) => s
        | Grout(_) => "○" /* ○ U+25CB */
        | _ => ""
        },
      seg,
    ),
  );

/* Returns the text content of the suggestion buffer.
 * For scaffold buffers (mixed Comment + Grout), reconstructs the
 * display string. For completion buffers (single Comment), returns text. */
let get_unparsed_buffer = (z: Zipper.t): option(Token.t) =>
  switch (z.selection.mode) {
  | Buffer(Unparsed) when z.selection.content != [] =>
    Some(buffer_to_string(z.selection.content))
  | _ => None
  };

/* Unicode circle used as hole placeholder in scaffold display strings.
 * Stripped before insertion. */
let scaffold_hole = "\xe2\x97\x8b"; /* ○ U+25CB, 3 bytes in UTF-8 */

/* Check if an unparsed buffer contains scaffold content.
 * Scaffolds contain Grout pieces; completions are pure Comment text. */
let is_scaffold_buffer = (z: Zipper.t): bool =>
  switch (z.selection.mode) {
  | Buffer(Unparsed) =>
    List.exists(
      (p: Piece.t) =>
        switch (p) {
        | Grout(_) => true
        | _ => false
        },
      z.selection.content,
    )
  | _ => false
  };

/* Legacy string-based scaffold check (for backward compat with tests) */
let is_scaffold = (text: Token.t): bool => {
  let len = String.length(text);
  let rec check = (i: int): bool =>
    if (i + 2 >= len) {
      false;
    } else if (Char.code(text.[i]) == 0xe2
               && Char.code(text.[i + 1]) == 0x97
               && Char.code(text.[i + 2]) == 0x8b) {
      true;
    } else {
      check(i + 1);
    };
  check(0);
};

/* Strip scaffold display chars to get insertable text.
 * Keeps commas and label prefixes (e.g. "x=") — both are
 * syntactically meaningful for tuple structure.
 * Strips: ○ placeholders and spaces.
 * e.g. ", ○" → ","  or  ", y=○, z=○" → ",y=,z="
 *      "x=○, " → "x=," */
let strip_scaffold_display = (text: Token.t): Token.t => {
  /* Use Stdlib.Buffer to avoid conflict with haz3lcore Buffer module */
  let buf = Stdlib.Buffer.create(String.length(text));
  let i = ref(0);
  while (i^ < String.length(text)) {
    let c = text.[i^];
    if (i^
        + 2 < String.length(text)
        && Char.code(text.[i^]) == 0xe2
        && Char.code(text.[i^ + 1]) == 0x97
        && Char.code(text.[i^ + 2]) == 0x8b) {
      /* Skip ○ (3-byte UTF-8 sequence: E2 97 8B) */
      i := i^ + 3;
    } else if (c == ' ') {
      /* Skip spaces */
      incr(i);
    } else {
      /* Keep everything else: commas, label chars, = signs */
      Stdlib.Buffer.add_char(buf, c);
      incr(i);
    };
  };
  Stdlib.Buffer.contents(buf);
};

/* Extract label from a Prod element type, if present.
 * e.g. TupLabel(Label("x"), Int) → Some("x"), Int → None */
let label_of_prod_elem = (ty: Typ.t): option(string) =>
  switch (Typ.match_tup_label(ty)) {
  | Some((name, _)) => Some(name)
  | None => None
  };

/* Build the scaffold buffer segment for remaining tuple elements.
 * Uses actual Grout pieces for holes instead of text placeholders,
 * with Comment secondaries for commas and label prefixes.
 *
 * holes_first: controls whether holes precede or follow commas.
 *   true:  [○, ", "]^n  — e.g. f(|? or f(|1 (left boundary is empty)
 *   false: [", ", ○]^n  — e.g. f(1|  or f(1|) (left has content)
 *
 * trailing_hole: when false and holes_first=false, the final hole is
 *   omitted because a convex tile to the right already fills that
 *   position.  e.g. f(1|~ 1 → ", " instead of ", ○"
 *
 * Labels appear before their hole: [", ", "y=", ○] */
let mk_scaffold_segment =
    (
      ~holes_first: bool,
      ~trailing_hole: bool,
      ~labels: list(option(string)),
      remaining: int,
    )
    : Segment.t => {
  let mk_comment = (s: string): Piece.t =>
    Secondary({
      id: Id.mk(),
      content: Comment(s),
    });
  let mk_hole = (): Piece.t =>
    Grout({
      id: Id.mk(),
      shape: Convex,
    });
  let mk_label_prefix = (i: int): list(Piece.t) =>
    switch (List.nth_opt(labels, i)) {
    | Some(Some(name)) => [mk_comment(name ++ "=")]
    | _ => []
    };
  if (holes_first) {
    List.concat(
      List.init(remaining, i =>
        mk_label_prefix(i) @ [mk_hole(), mk_comment(", ")]
      ),
    );
  } else {
    List.concat(
      List.init(
        remaining,
        i => {
          let is_last = i == remaining - 1;
          let hole =
            is_last && !trailing_hole
              ? [] : mk_label_prefix(i) @ [mk_hole()];
          [mk_comment(", ")] @ hole;
        },
      ),
    );
  };
};

/* Alias for debug logging. */
let scaffold_segment_to_string = buffer_to_string;

/* Count comma tiles in sibling segments */
let is_comma = (p: Piece.t): bool =>
  switch (p) {
  | Tile({label: [","], _}) => true
  | _ => false
  };

let count_commas = ((l, r): Siblings.t): int =>
  List.length(List.filter(is_comma, l))
  + List.length(List.filter(is_comma, r));

/* Count commas in a list of pieces (single-sided). */
let count_commas_in = (pieces: list(Piece.t)): int =>
  List.length(List.filter(is_comma, pieces));

/* Get the left siblings between the caret and the nearest ( shard
 * (excluding the ( shard itself). Returns in the same order as the
 * original left siblings (left-to-right / farthest-first).
 * For ancestor case (no ( shard in siblings), returns all left siblings.
 *
 * Left siblings are stored in left-to-right order (farthest from caret
 * first). We reverse to walk nearest-first, collect until hitting a
 * ( shard, then reverse back. */
let inner_left_siblings = (z: Zipper.t): list(Piece.t) => {
  let l_nearest = List.rev(fst(z.relatives.siblings));
  let rec take_until_paren = (acc, pieces) =>
    switch (pieces) {
    | [] => acc
    | [Piece.Tile({label: ["(", ")"], shards: [0], _}), ..._] => acc
    | [p, ...rest] => take_until_paren([p, ...acc], rest)
    };
  /* acc is built by consing, so it ends up in farthest-first order */
  take_until_paren([], l_nearest);
};

/* Check if we're inside parentheses. Three cases:
 * 1. Ancestor has label ["(", ")"] — both parens placed, caret inside child
 * 2. Backpack has a ")" shard — open paren placed, close paren deferred
 * 3. Left sibling is a "(" shard — caret right after open paren
 * The union of these covers all parenthesized contexts. */
let inside_parens = (z: Zipper.t): bool => {
  /* Case 1: ancestors */
  let ancestor_check =
    switch (z.relatives.ancestors) {
    | [(ancestor, _), ..._] => ancestor.label == ["(", ")"]
    | _ => false
    };
  /* Case 2: backpack has ")" */
  let backpack_check =
    List.exists(
      (t: Tile.t) =>
        switch (t) {
        | {label: ["(", ")"], shards: [1], _} => true
        | _ => false
        },
      Zipper.local_backpack(z),
    );
  /* Case 3: left siblings contain "(" shard */
  let left_paren_check = {
    let (l, _) = z.relatives.siblings;
    List.exists(
      (p: Piece.t) =>
        switch (p) {
        | Tile({label: ["(", ")"], shards: [0], _}) => true
        | _ => false
        },
      l,
    );
  };
  ancestor_check || backpack_check || left_paren_check;
};

/* Determine the expected tuple type inside parentheses.
 *
 * Two cases:
 * 1. Both parens placed (ancestor case): the paren tile is the nearest
 *    ancestor. Its ana reflects the expected type from context (function
 *    argument type via Ap analysis, or type annotation).
 * 2. Only open paren placed (shard case): the `(` shard is in left
 *    siblings. For function application we look at the function token's
 *    type and extract matched_arrow. For explicit parens we use the
 *    `(` shard's ana.
 *
 * Returns the expected argument type (which should be checked for Prod). */
let scaffold_expected_type =
    (z: Zipper.t, info_map: Statics.Map.t): option(Typ.t) => {
  /* Case 1: Both parens placed — paren tile is nearest ancestor.
   * For function application, MakeTerm assigns the paren tile's ID
   * to the Ap node (cls=Application, ana=Unknown). We need to find
   * the function token in the ancestor's left context and use
   * matched_arrow to get the argument type.
   * For explicit parens, the paren tile maps to a Parens node
   * whose ana is the expected type from context. */
  switch (z.relatives.ancestors) {
  | [(ancestor, (anc_left, _)), ..._] when ancestor.label == ["(", ")"] =>
    switch (Id.Map.find_opt(ancestor.id, info_map)) {
    | Some(InfoExp({cls: Exp(Ap), _})) =>
      /* Function application: find function in ancestor left context */
      let l = List.rev(anc_left);
      let rec find_fn = (
        fun
        | [] => None
        | [p, ..._] when !Piece.is_secondary(p) && !Piece.is_grout(p) =>
          switch (Id.Map.find_opt(Piece.id(p), info_map)) {
          | Some(InfoExp({ty, ctx, _})) =>
            let (arg_ty, _) = Typ.matched_arrow(ctx, ty);
            Some(arg_ty);
          | _ => None
          }
        | [_, ...rest] => find_fn(rest)
      );
      find_fn(l);
    | Some(InfoExp({ana, ctx, _})) =>
      Some(Typ.weak_head_normalize(ctx, ana))
    | Some(InfoPat({ana, ctx, _})) =>
      Some(Typ.weak_head_normalize(ctx, ana))
    | _ => None
    }
  | _ =>
    /* Case 2: Only open paren placed — shard in left siblings.
     * Find the nearest ( shard to the caret and determine the expected
     * type inside it. For function application parens (cls=Ap), find
     * the function token and use matched_arrow. For other parens
     * (grouping, nested), use the paren shard's ana type directly. */
    /* Left siblings are in left-to-right order (farthest first).
     * Reverse to search nearest-first for the innermost ( shard. */
    let l_nearest = List.rev(fst(z.relatives.siblings));
    /* For a ( shard, determine the expected type inside it.
     * For Ap: find the function and use matched_arrow.
     * For Parens with known ana: use ana directly.
     * For Parens with Unknown ana: the inner paren is nested inside
     * an outer context whose type hasn't propagated. Find the outer
     * paren, determine which element position the inner paren occupies
     * (by counting commas between outer and inner), and index into
     * the outer type. */
    let type_for_paren =
        (paren_piece: Piece.t, pieces_after: list(Piece.t)): option(Typ.t) =>
      switch (Id.Map.find_opt(Piece.id(paren_piece), info_map)) {
      | Some(InfoExp({cls: Exp(Ap), _})) =>
        /* Function application: find function token beyond paren */
        let rec find_fn = (
          fun
          | [] => None
          | [p, ...rest] =>
            if (Piece.is_secondary(p)) {
              find_fn(rest);
            } else {
              Some(p);
            }
        );
        switch (find_fn(pieces_after)) {
        | Some(fn_piece) =>
          switch (Id.Map.find_opt(Piece.id(fn_piece), info_map)) {
          | Some(InfoExp({ty, ctx, _})) =>
            let (arg_ty, _) = Typ.matched_arrow(ctx, ty);
            Some(arg_ty);
          | _ => None
          }
        | None => None
        };
      | Some(InfoExp({ana, ctx, _})) =>
        let ana = Typ.weak_head_normalize(ctx, ana);
        switch (Typ.term_of(ana)) {
        | Unknown(_) => None /* Will trigger fallback below */
        | _ => Some(ana)
        };
      | Some(InfoPat({ana, ctx, _})) =>
        Some(Typ.weak_head_normalize(ctx, ana))
      | _ => None
      };
    /* Walk nearest-first from some position to find the next ( shard.
     * Returns (paren_piece, pieces_after_paren, commas_skipped). */
    let rec find_next_paren = (pieces, commas) =>
      switch (pieces) {
      | [] => None
      | [Piece.Tile({label: ["(", ")"], shards: [0], _}) as p, ...rest] =>
        Some((p, rest, commas))
      | [p, ...rest] =>
        find_next_paren(rest, is_comma(p) ? commas + 1 : commas)
      };
    /* Find the nearest ( shard */
    switch (find_next_paren(l_nearest, 0)) {
    | None => None
    | Some((inner_paren, after_inner, _)) =>
      switch (type_for_paren(inner_paren, after_inner)) {
      | Some(ty) => Some(ty)
      | None =>
        /* Inner paren has Unknown ana — try the outer paren.
         * Count commas between inner and outer to find element index. */
        switch (find_next_paren(after_inner, 0)) {
        | None => None
        | Some((outer_paren, after_outer, commas_between)) =>
          switch (type_for_paren(outer_paren, after_outer)) {
          | None => None
          | Some(outer_ty) =>
            let rec unwrap_parens = (ty: Typ.t): Typ.t =>
              switch (Typ.term_of(ty)) {
              | Parens(inner) => unwrap_parens(inner)
              | _ => ty
              };
            let outer_ty = unwrap_parens(outer_ty);
            switch (Typ.term_of(outer_ty)) {
            | Prod(tys) =>
              /* The inner paren is at element index (commas_between).
               * This is 0-indexed: 0 commas = first element, etc. */
              let idx = commas_between;
              switch (List.nth_opt(tys, idx)) {
              | Some(elem_ty) => Some(elem_ty)
              | None => None
              };
            | _ => None
            };
          }
        }
      }
    };
  };
};

/* Scaffold generation: produces a display string like ", ○" when
 * the caret is inside parentheses and the expected type is a Prod
 * (tuple) with more elements than commas already present.
 *
 * Unlike text completion (set_buffer), scaffold:
 * - Triggers on empty holes (no min_prefix_len)
 * - Only triggers inside parentheses
 * - Uses the ana type to determine tuple arity
 * - Takes info_map directly (not a pre-computed ci) */
/* Check if a complete paren tile has any inner content pieces with
 * type errors. Used to detect nested tuple false suppression:
 * when Tuple self types are contaminated by bidirectional checking
 * (element ty falls back to ana on error), the overall self type
 * can match the expected Prod even when inner elements don't. */
let has_inner_errors =
    (children: list(Segment.t), info_map: Statics.Map.t): bool => {
  switch (children) {
  | [inner_seg] =>
    List.exists(
      (p: Piece.t) =>
        switch (p) {
        | Tile(_) when !Piece.is_secondary(p) && !Piece.is_grout(p) =>
          switch (Id.Map.find_opt(Piece.id(p), info_map)) {
          | Some(InfoExp({status: InHole(_), _})) => true
          | _ => false
          }
        | _ => false
        },
      inner_seg,
    )
  | _ => false
  };
};

/* Check if the piece to the left of the caret already satisfies the
 * expected Prod type. If so, suppress scaffold generation.
 *
 * Uses Self.typ_of_exp (synthesized type) when available, falling back
 * to ty (derived type) when self is Unknown. For complete paren tiles
 * wrapping tuples, also checks inner element statuses to avoid false
 * suppression from bidirectional type contamination. */
let should_suppress =
    (l: list(Piece.t), expected_ty: Typ.t, info_map: Statics.Map.t): bool =>
  switch (l) {
  | [p, ..._] when Piece.is_convex(p) =>
    /* For complete paren tiles, check inner elements for errors.
     * Tuple self types are built from element ty fields which fall
     * back to ana on inconsistency, making self = expected even when
     * inner elements don't match. Checking inner statuses catches this. */
    let inner_ok =
      switch (p) {
      | Tile({label: ["(", ")"], children, _})
          when List.length(children) > 0 =>
        !has_inner_errors(children, info_map)
      | _ => true
      };
    if (!inner_ok) {
      false;
    } else {
      switch (Id.Map.find_opt(Piece.id(p), info_map)) {
      | Some(InfoExp({self, ctx, _})) =>
        /* Use ONLY the synthesized type for suppression. Never fall
         * back to the reconciled `ty` field, which can be stale
         * (e.g., when the piece inherits a grout's ID via
         * preserve_grout_id, the stale info_map entry has ty = ana
         * from the previous grout hole, not the current piece). */
        switch (Self.typ_of_exp(self)) {
        | Some(syn_ty) =>
          switch (Typ.term_of(syn_ty)) {
          | Unknown(_) => false
          | _ => Typ.is_consistent(ctx, syn_ty, expected_ty)
          }
        | None => false
        }
      | _ => false
      };
    };
  | _ => false
  };

/* Compute the scaffold buffer segment without modifying the zipper.
 * Returns None if no scaffold applies. The segment contains Comment
 * secondaries for text (commas, labels) and Grout for hole placeholders. */
let scaffold_display =
    (~info_map: Statics.Map.t, z: Zipper.t): option(Segment.t) =>
  if (z.caret != Outer) {
    None;
  } else if (!inside_parens(z)) {
    None;
  } else {
    switch (z.selection.mode) {
    | Buffer(Parsed | Unparsed) => None
    | Normal when !Selection.is_empty(z.selection) => None
    | _ =>
      switch (scaffold_expected_type(z, info_map)) {
      | None => None
      | Some(expected_ty) =>
        let rec unwrap_parens = (ty: Typ.t): Typ.t =>
          switch (Typ.term_of(ty)) {
          | Parens(inner) => unwrap_parens(inner)
          | _ => ty
          };
        let expected_ty = unwrap_parens(expected_ty);
        switch (Typ.term_of(expected_ty)) {
        | Prod(tys) when List.length(tys) >= 2 =>
          /* Scope to the innermost paren: only consider siblings
           * between the nearest ( shard and the caret. For the
           * ancestor case (no ( shard), this is all left siblings. */
          let scoped_l = inner_left_siblings(z);
          let l = List.rev(scoped_l);
          if (should_suppress(l, expected_ty, info_map)) {
            None;
          } else {
            let arity = List.length(tys);
            let existing_commas =
              count_commas_in(scoped_l)
              + count_commas_in(snd(z.relatives.siblings));
            let remaining = arity - 1 - existing_commas;
            if (remaining <= 0) {
              None;
            } else {
              /* holes_first: true when the left boundary is structurally
               * empty — the nearest tile (skipping grout and secondary)
               * is a delimiter (open paren, comma) or absent.
               *
               * When the left has content (e.g., f(1|, f(x=|), the user
               * is mid-edit on an element, so commas come first. */
              let holes_first = {
                let l_nearest = List.rev(scoped_l);
                let rec check = (
                  fun
                  | [] => true
                  | [Piece.Secondary(_), ...rest]
                  | [Piece.Grout(_), ...rest] => check(rest)
                  | [Piece.Tile({label: [","], _}), ..._] => true
                  | [Piece.Tile({label: ["(", ")"], shards: [0], _}), ..._] =>
                    true
                  | _ => false
                );
                check(l_nearest);
              };
              /* trailing_hole: false when a convex tile (not grout) exists
               * to the right of the buffer. That tile already fills the
               * last element position, so no trailing hole is needed.
               * Grout is skipped because it gets absorbed when the comma
               * is accepted (comma is concave, replaces concave grout). */
              let trailing_hole = {
                let rec check = (
                  fun
                  | [] => true
                  | [Piece.Secondary(_), ...rest]
                  | [Piece.Grout(_), ...rest] => check(rest)
                  | [p, ..._] => !Piece.is_convex(p)
                );
                check(snd(z.relatives.siblings));
              };
              let label_start =
                holes_first ? existing_commas : existing_commas + 1;
              let remaining_tys =
                tys
                |> List.filteri((i, _) => i >= label_start)
                |> (lst => List.filteri((i, _) => i < remaining, lst));
              let labels = List.map(label_of_prod_elem, remaining_tys);
              Some(
                mk_scaffold_segment(
                  ~holes_first,
                  ~trailing_hole,
                  ~labels,
                  remaining,
                ),
              );
            };
          };
        | _ => None
        };
      }
    };
  };

let set_scaffold = (~info_map: Statics.Map.t, z: Zipper.t): Zipper.t =>
  switch (scaffold_display(~info_map, z)) {
  | None => z
  | Some(content) => Zipper.set_buffer(z, ~content, ~mode=Unparsed)
  };

/* Reify the scaffold buffer into the zipper by inserting the
 * stripped scaffold text. Called before dumping for statics so
 * that statics sees the tuple structure. */
let reify_scaffold = (z: Zipper.t): Zipper.t =>
  switch (get_unparsed_buffer(z)) {
  | Some(text) when is_scaffold(text) =>
    let insertable = strip_scaffold_display(text);
    let z = Zipper.clear_unparsed_buffer(z);
    switch (Parser.to_zipper(~zipper_init=z, insertable)) {
    | Some(z) => z
    | None => z
    };
  | _ => z
  };

/* Populates the suggestion buffer with a type-directed suggestion */
let set_buffer = (~ci: option(Info.t), z: Zipper.t): option(Zipper.t) => {
  let* ci = ci;
  let* _ =
    switch (z.selection.mode) {
    /* Make sure not to populate the completion buffer if there is a non-empty
     * selection, otherwise it will get clobbered by the buffer */
    | Buffer(Unparsed | Parsed) => Some()
    | Normal when Selection.is_empty(z.selection) => Some()
    | Normal => None
    };
  let* tok_to_left = token_to_left(z);
  let prefix_len = String.length(tok_to_left);
  let* _ = prefix_len >= 1 ? Some() : None;
  let suggestions = suggest(ci, z);
  let suggestions =
    suggestions
    |> List.filter(({content, _}: TyDiSuggestion.t) =>
         String.starts_with(~prefix=tok_to_left, content)
       );
  /* Require min_prefix_len characters before showing completions,
   * unless there is exactly one match (unambiguous). */
  let* _ =
    prefix_len >= min_prefix_len || List.length(suggestions) == 1
      ? Some() : None;
  /* If any suggestion is an exact match for the current token, suppress
   * all suggestions. This check must scan the full list, not just the
   * top suggestion, because exact-match variables and keyword suggestions
   * come from different pipelines and may be ordered differently. */
  let has_exact_match =
    List.exists(
      ({content, _}: TyDiSuggestion.t) => content == tok_to_left,
      suggestions,
    );
  let* _ = has_exact_match ? None : Some();
  let* top_suggestion = suggestions |> Util.ListUtil.hd_opt;
  let* suggestion_suffix = suffix_of(top_suggestion.content, tok_to_left);
  let content = mk_unparsed_buffer(suggestion_suffix);
  let z = Zipper.set_buffer(z, ~content, ~mode=Unparsed);
  Some(z);
};

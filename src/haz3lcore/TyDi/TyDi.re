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

/* Returns the text content of the suggestion buffer */
let get_unparsed_buffer = (z: Zipper.t): option(Token.t) =>
  switch (z.selection.mode, z.selection.content) {
  | (Buffer(Unparsed), [Secondary({content: Comment(completion), _})]) =>
    Some(completion)
  | _ => None
  };

/* Unicode circle used as hole placeholder in scaffold display strings.
 * Stripped before insertion. */
let scaffold_hole = "\xe2\x97\x8b"; /* ○ U+25CB, 3 bytes in UTF-8 */

/* Check if an unparsed buffer contains scaffold content (has ○ placeholder) */
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

/* Build the scaffold display string for remaining commas.
 *
 * When there's grout (a real hole) to the right of the caret, the scaffold
 * places the hole placeholder BEFORE the comma so it reads naturally:
 *   grout_right=true:  remaining=1 → "○, "   remaining=2 → "○, ○, "
 *   e.g. f(○, ?)  — scaffold ○ for current position, real ? for next
 *
 * When the caret follows a convex piece (no grout to right), commas lead:
 *   grout_right=false: remaining=1 → ", ○"   remaining=2 → ", ○, ○"
 *   e.g. f(1, ○)  — value already typed, scaffold shows what's next
 *
 * When labels are provided, they appear before the hole placeholder:
 *   e.g. ", y=○" for a labeled element */
let mk_scaffold_display =
    (~grout_right: bool, ~labels: list(option(string)), remaining: int)
    : string => {
  let mk_hole = (i: int): string => {
    let label_prefix =
      switch (List.nth_opt(labels, i)) {
      | Some(Some(name)) => name ++ "="
      | _ => ""
      };
    label_prefix ++ scaffold_hole;
  };
  if (grout_right) {
    let parts = List.init(remaining, i => mk_hole(i) ++ ", ");
    String.concat("", parts);
  } else {
    let parts = List.init(remaining, i => ", " ++ mk_hole(i));
    String.concat("", parts);
  };
};

/* Count comma tiles in sibling segments */
let count_commas = ((l, r): Siblings.t): int => {
  let is_comma = (p: Piece.t): bool =>
    switch (p) {
    | Tile({label: [","], _}) => true
    | _ => false
    };
  List.length(List.filter(is_comma, l))
  + List.length(List.filter(is_comma, r));
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
    | Some(InfoExp({ana, _})) => Some(ana)
    | Some(InfoPat({ana, _})) => Some(ana)
    | _ => None
    }
  | _ =>
    /* Case 2: Only open paren placed — shard in left siblings.
     * Handles nested ( shards: for f((¦, the innermost ( finds the
     * outer ( as neighbor, which finds f as the function piece.
     * Each nesting level peels one Prod element (first element). */
    let l = fst(z.relatives.siblings) |> List.rev;
    /* Find the chain of ( shards and the piece beyond them.
     * Returns (nesting_depth, maybe_fn_piece, innermost_paren_piece). */
    let rec find_paren_chain =
            (pieces: list(Piece.t), depth: int)
            : option((int, option(Piece.t), Piece.t)) =>
      switch (pieces) {
      | [] => None
      | [Tile({label: ["(", ")"], shards: [0], _}) as p] =>
        Some((depth, None, p))
      | [
          Tile({label: ["(", ")"], shards: [0], _}) as p,
          Tile({label: ["(", ")"], shards: [0], _}) as _outer,
          ...rest,
        ] =>
        /* Nested paren: recurse to find function beyond the chain */
        find_paren_chain([_outer, ...rest], depth + 1)
        |> Option.map(((d, fn, _)) => (d, fn, p))
      | [Tile({label: ["(", ")"], shards: [0], _}) as p, left, ..._] =>
        Some((depth, Some(left), p))
      | [_, ...rest] => find_paren_chain(rest, depth)
      };
    /* Peel nested Prod elements: for each nesting level, take the
     * first element of the Prod type (the inner paren groups it). */
    let rec peel_prod = (ty: Typ.t, depth: int): Typ.t =>
      if (depth <= 0) {
        ty;
      } else {
        switch (Typ.term_of(ty)) {
        | Prod([first, ..._]) => peel_prod(first, depth - 1)
        | _ => ty /* Not a Prod — inner paren is just grouping */
        };
      };
    switch (find_paren_chain(l, 0)) {
    | None => None
    | Some((depth, maybe_fn, paren_piece)) =>
      let base_ty =
        switch (maybe_fn) {
        | Some(fn_piece) when !Piece.is_secondary(fn_piece) =>
          switch (Id.Map.find_opt(Piece.id(fn_piece), info_map)) {
          | Some(InfoExp({ty, ctx, _})) =>
            let (arg_ty, _) = Typ.matched_arrow(ctx, ty);
            Some(arg_ty);
          | _ => None
          }
        | _ =>
          switch (Id.Map.find_opt(Piece.id(paren_piece), info_map)) {
          | Some(InfoExp({ana, _})) => Some(ana)
          | Some(InfoPat({ana, _})) => Some(ana)
          | _ => None
          }
        };
      Option.map(ty => peel_prod(ty, depth), base_ty);
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

/* Compute the scaffold display string without modifying the zipper.
 * Returns None if no scaffold applies. */
let scaffold_display =
    (~info_map: Statics.Map.t, z: Zipper.t): option(string) =>
  if (!inside_parens(z)) {
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
          let l = fst(z.relatives.siblings) |> List.rev;
          if (should_suppress(l, expected_ty, info_map)) {
            None;
          } else {
            let arity = List.length(tys);
            let existing_commas = count_commas(z.relatives.siblings);
            let remaining = arity - 1 - existing_commas;
            if (remaining <= 0) {
              None;
            } else {
              /* grout_right: true when the caret is on an empty hole
               * (grout to right, no typed content to left). This controls
               * display style — grout_right puts ○ before commas.
               * After typing content (e.g., x= at f(x=¦?), grout_right
               * must be false so scaffold shows remaining elements
               * after the current one, not a duplicate of it.
               *
               * Left is "empty" if nothing between caret and the nearest
               * structural delimiter (comma or ( shard) is content. */
              let grout_right = {
                let right_is_grout = {
                  let rec skip_secondary = (
                    fun
                    | [Piece.Secondary(_), ...rest] => skip_secondary(rest)
                    | [p, ..._] => Piece.is_grout(p)
                    | [] => false
                  );
                  skip_secondary(snd(z.relatives.siblings));
                };
                let left_has_no_content = {
                  /* Left siblings are stored farthest-first. Reverse to
                   * check from caret outward. True if the first
                   * non-secondary piece is a delimiter (comma, ( shard),
                   * grout, or nothing — meaning no content at current
                   * element position. */
                  let l = List.rev(fst(z.relatives.siblings));
                  let rec check = (
                    fun
                    | [] => true
                    | [Piece.Secondary(_), ...rest] => check(rest)
                    | [Piece.Grout(_), ..._] => true
                    | [Piece.Tile({label: [","], _}), ..._] => true
                    | [Piece.Tile({label: ["(", ")"], shards, _}), ..._]
                        when List.mem(0, shards) =>
                      true
                    | _ => false
                  );
                  check(l);
                };
                right_is_grout && left_has_no_content;
              };
              let label_start =
                grout_right ? existing_commas : existing_commas + 1;
              let remaining_tys =
                tys
                |> List.filteri((i, _) => i >= label_start)
                |> (lst => List.filteri((i, _) => i < remaining, lst));
              let labels = List.map(label_of_prod_elem, remaining_tys);
              Some(mk_scaffold_display(~grout_right, ~labels, remaining));
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
  | Some(display) =>
    let content = mk_unparsed_buffer(display);
    Zipper.set_buffer(z, ~content, ~mode=Unparsed);
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
  /* Only show completions after typing enough characters */
  let* _ = String.length(tok_to_left) >= min_prefix_len ? Some() : None;
  let suggestions = suggest(ci, z);
  let suggestions =
    suggestions
    |> List.filter(({content, _}: TyDiSuggestion.t) =>
         String.starts_with(~prefix=tok_to_left, content)
       );
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

open Language;
open Util.OptUtil.Syntax;

/* ---- Shared helpers ---- */

/* Unwrap Parens type wrappers to get the inner type. */
let rec unwrap_parens = (ty: Typ.t): Typ.t =>
  switch (Typ.term_of(ty)) {
  | Parens(inner) => unwrap_parens(inner)
  | _ => ty
  };

/* Look up a piece in the info map and extract the function
 * argument type via matched_arrow. */
let fn_arg_type = (info_map: Statics.Map.t, piece: Piece.t): option(Typ.t) =>
  switch (Id.Map.find_opt(Piece.id(piece), info_map)) {
  | Some(InfoExp({ty, ctx, _})) =>
    let (arg_ty, _) = Typ.matched_arrow(ctx, ty);
    Some(arg_ty);
  | _ => None
  };

let is_comma = (p: Piece.t): bool =>
  switch (p) {
  | Tile({label: [","], _}) => true
  | _ => false
  };

/* ---- Buffer identification ---- */

/* Scaffolds contain Grout and/or Tile pieces (commas);
 * completions are pure Comment text. */
let is_scaffold = (z: Zipper.t): bool =>
  switch (z.selection.mode) {
  | Buffer(Unparsed) =>
    List.exists(
      (p: Piece.t) =>
        switch (p) {
        | Grout(_)
        | Tile(_) => true
        | _ => false
        },
      z.selection.content,
    )
  | _ => false
  };

/* Extract insertable text from a scaffold buffer segment.
 * Keeps commas, label names, label = operators, and whitespace —
 * all are meaningful for producing well-formatted code.
 * Skips only Grout (holes) and non-label/non-comma Tiles.
 * e.g. [,, " ", ?]  => ", "
 *      [" ", ?, ,, " "] => " , "
 *      [,, " ", x, =, ?] => ", x="  */
let insertable = (content: Segment.t): Token.t =>
  String.concat(
    "",
    List.filter_map(
      (p: Piece.t) =>
        switch (p) {
        | Secondary({content: Comment(s), _}) => Some(s)
        | Secondary({content: Whitespace(s), _}) => Some(s)
        | Tile({label: [","], _}) => Some(",")
        | Tile({label: ["="], _}) => Some("=")
        | Tile({
            label: [tok],
            mold: {nibs: ({shape: Convex, _}, {shape: Convex, _}), _},
            _,
          }) =>
          /* Operand tile: label name token */
          Some(tok)
        | _ => None
        },
      content,
    ),
  );

/* ---- Type and label extraction ---- */

/* Extract label from a Prod element type, if present.
 * e.g. TupLabel(Label("x"), Int) => Some("x"), Int => None */
let label_of_prod_elem = (ty: Typ.t): option(string) => {
  let+ (name, _) = Typ.match_tup_label(ty);
  name;
};

/* ---- Segment construction ---- */

/* Build the scaffold buffer segment for remaining tuple elements.
 * Uses actual Grout pieces for holes instead of text placeholders,
 * with Comment secondaries for commas and label prefixes.
 *
 * holes_first: controls whether holes precede or follow commas.
 *   true:  [?, ", "]^n  -- e.g. f(|? or f(|1 (left boundary is empty)
 *   false: [", ", ?]^n  -- e.g. f(1|  or f(1|) (left has content)
 *
 * trailing_hole: when false and holes_first=false, the final hole is
 *   omitted because a convex tile to the right already fills that
 *   position.  e.g. f(1|~ 1 => ", " instead of ", ?"
 *
 * Labels appear before their hole: [", ", "y=", ?] */
let mk_segment =
    (
      ~holes_first: bool,
      ~trailing_hole: bool,
      ~labels: list(option(string)),
      remaining: int,
    )
    : Segment.t => {
  let mk_space = (): Piece.t =>
    Secondary({
      id: Id.mk(),
      content: Whitespace(" "),
    });
  let mk_hole = (): Piece.t =>
    Grout({
      id: Id.mk(),
      shape: Convex,
    });
  let mk_comma = (): Piece.t => Piece.mk_tile(Form.get(CommaExp), []);
  let mk_label_prefix = (i: int): list(Piece.t) =>
    switch (List.nth_opt(labels, i)) {
    | Some(Some(name)) => [
        Tile({
          id: Id.mk(),
          label: [Token.quote_label_when_necessary(name)],
          mold: Mold.mk_op(Sort.Exp, []),
          shards: [0],
          children: [],
        }),
        Piece.mk_tile(Form.get(TupleLabeledExp), []),
      ]
    | _ => []
    };
  if (holes_first) {
    List.concat(
      List.init(remaining, i =>
        mk_label_prefix(i) @ [mk_hole(), mk_comma(), mk_space()]
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
          [mk_comma(), mk_space()] @ hole;
        },
      ),
    );
  };
};

let segment_to_string = TyDiComplete.buffer_to_string;

/* ---- Comma counting ---- */

let count_commas_in = (pieces: list(Piece.t)): int =>
  List.length(List.filter(is_comma, pieces));

/* ---- Paren context detection ---- */

/* Get the left siblings between the caret and the nearest ( shard
 * (excluding the ( shard itself). Returns in left-to-right order
 * (farthest-first). For ancestor case, returns all left siblings. */
let inner_left_siblings = (z: Zipper.t): list(Piece.t) => {
  let l_nearest = List.rev(fst(z.relatives.siblings));
  let rec take_until_paren = (acc, pieces) =>
    switch (pieces) {
    | [] => acc
    | [Piece.Tile({label: ["(", ")"], shards: [0], _}), ..._] => acc
    | [p, ...rest] => take_until_paren([p, ...acc], rest)
    };
  take_until_paren([], l_nearest);
};

/* Check if we're inside parentheses. Three cases:
 * 1. Ancestor has label ["(", ")"] -- both parens placed
 * 2. Backpack has a ")" shard -- open paren placed, close deferred
 * 3. Left sibling is a "(" shard -- right after open paren */
let inside_parens = (z: Zipper.t): bool => {
  let ancestor_parens =
    switch (z.relatives.ancestors) {
    | [(ancestor, _), ..._] => ancestor.label == ["(", ")"]
    | _ => false
    };
  let backpack_close_paren =
    List.exists(
      (t: Tile.t) =>
        switch (t) {
        | {label: ["(", ")"], shards: [1], _} => true
        | _ => false
        },
      Zipper.local_backpack(z),
    );
  let left_open_paren = {
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
  ancestor_parens || backpack_close_paren || left_open_paren;
};

/* ---- Expected type inference ---- */

/* For a ( shard or paren tile, determine the expected type inside it.
 * For Ap: find the function and use matched_arrow.
 * For Parens with known ana: use ana directly.
 * For Parens with Unknown ana: return None (triggers outer fallback). */
let type_for_paren =
    (info_map: Statics.Map.t, paren: Piece.t, beyond: list(Piece.t))
    : option(Typ.t) =>
  switch (Id.Map.find_opt(Piece.id(paren), info_map)) {
  | Some(InfoExp({cls: Exp(Ap), _})) =>
    /* Function application: skip whitespace to find function token */
    let rec first_non_secondary = (
      fun
      | [] => None
      | [Piece.Secondary(_), ...rest] => first_non_secondary(rest)
      | [p, ..._] => Some(p)
    );
    let* fn_piece = first_non_secondary(beyond);
    fn_arg_type(info_map, fn_piece);
  | Some(InfoExp({ana, ctx, _})) =>
    let ana = Typ.weak_head_normalize(ctx, ana);
    switch (Typ.term_of(ana)) {
    | Unknown(_) => None
    | _ => Some(ana)
    };
  | Some(InfoPat({ana, ctx, _})) => Some(Typ.weak_head_normalize(ctx, ana))
  | _ => None
  };

/* Walk nearest-first to find the next ( shard.
 * Returns (paren_piece, pieces_beyond, commas_skipped). */
let rec find_next_paren = (pieces, commas) =>
  switch (pieces) {
  | [] => None
  | [Piece.Tile({label: ["(", ")"], shards: [0], _}) as p, ...rest] =>
    Some((p, rest, commas))
  | [p, ...rest] => find_next_paren(rest, is_comma(p) ? commas + 1 : commas)
  };

/* Determine the expected tuple type inside parentheses.
 *
 * Two cases:
 * 1. Both parens placed (ancestor): paren tile is nearest ancestor.
 *    For Ap, find the function in ancestor left context and use
 *    matched_arrow. For Parens, use the ana type directly.
 * 2. Only open paren (shard): find the nearest ( shard in left siblings.
 *    Try its type first; if Unknown, fall back to an outer paren and
 *    index into its Prod type by counting commas between them. */
let expected_type = (z: Zipper.t, info_map: Statics.Map.t): option(Typ.t) =>
  switch (z.relatives.ancestors) {
  | [(ancestor, (anc_left, _)), ..._] when ancestor.label == ["(", ")"] =>
    /* Ancestor case: look up the paren tile in info_map */
    let* info = Id.Map.find_opt(ancestor.id, info_map);
    switch (info) {
    | InfoExp({cls: Exp(Ap), _}) =>
      /* For Ap, find the function token in ancestor left context.
       * Skip secondary AND grout (ancestor context can have padding). */
      let rec first_content = (
        fun
        | [] => None
        | [p, ..._] when !Piece.is_secondary(p) && !Piece.is_grout(p) =>
          Some(p)
        | [_, ...rest] => first_content(rest)
      );
      let* fn_piece = first_content(List.rev(anc_left));
      fn_arg_type(info_map, fn_piece);
    | InfoExp({ana, ctx, _}) => Some(Typ.weak_head_normalize(ctx, ana))
    | InfoPat({ana, ctx, _}) => Some(Typ.weak_head_normalize(ctx, ana))
    | _ => None
    };
  | _ =>
    /* Shard case: search left siblings (nearest-first) for ( shards */
    let l_nearest = List.rev(fst(z.relatives.siblings));
    let* (inner_paren, after_inner, _) = find_next_paren(l_nearest, 0);
    /* Try inner paren directly */
    switch (type_for_paren(info_map, inner_paren, after_inner)) {
    | Some(ty) => Some(ty)
    | None =>
      /* Inner has Unknown ana -- fall back to the outer paren.
       * Count commas between inner and outer to find element index. */
      let* (outer_paren, after_outer, commas_between) =
        find_next_paren(after_inner, 0);
      let* outer_ty = type_for_paren(info_map, outer_paren, after_outer);
      let outer_ty = unwrap_parens(outer_ty);
      let* tys =
        switch (Typ.term_of(outer_ty)) {
        | Prod(tys) => Some(tys)
        | _ => None
        };
      List.nth_opt(tys, commas_between);
    };
  };

/* ---- Suppression checks ---- */

/* Check if a complete paren tile has inner content with type errors.
 * Used to detect false suppression from bidirectional contamination:
 * element ty falls back to ana on error, so the overall self type
 * can match expected Prod even when inner elements are wrong. */
let has_inner_errors =
    (children: list(Segment.t), info_map: Statics.Map.t): bool =>
  switch (children) {
  | [inner_seg] =>
    List.exists(
      (p: Piece.t) =>
        switch (Id.Map.find_opt(Piece.id(p), info_map)) {
        | Some(InfoExp({status: InHole(_), _})) => true
        | _ => false
        },
      inner_seg,
    )
  | _ => false
  };

/* Check if the piece to the left of the caret already satisfies the
 * expected Prod type. If so, suppress scaffold generation.
 * Uses synthesized type only -- never the reconciled `ty` field
 * (which can be stale from preserve_grout_id). For complete paren
 * tiles, also checks inner element statuses. */
let should_suppress =
    (l: list(Piece.t), expected_ty: Typ.t, info_map: Statics.Map.t): bool => {
  /* Skip whitespace and grout to find the nearest content piece.
   * In multi-line contexts, newline secondaries can appear between
   * the variable and the caret. */
  let rec skip_non_content =
    fun
    | [Piece.Secondary(_), ...rest]
    | [Piece.Grout(_), ...rest] => skip_non_content(rest)
    | l => l;
  switch (skip_non_content(l)) {
  | [p, ..._] when Piece.is_convex(p) =>
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
};

/* Check if accepting a text completion would produce a variable whose
 * type satisfies the expected Prod. Used by set_assist_buffer to
 * decide whether to combine completion + scaffold or show completion
 * alone. completion_text is the suffix (e.g., "args" for bl→blargs). */
let completion_would_suppress =
    (~completion_text: string, ~info_map: Statics.Map.t, z: Zipper.t): bool =>
  switch (TyDiComplete.token_to_left(z)) {
  | None => false
  | Some(current_token) =>
    let completed = current_token ++ completion_text;
    /* Look up the expected Prod type */
    switch (expected_type(z, info_map)) {
    | None => false
    | Some(expected_ty) =>
      let expected_ty = unwrap_parens(expected_ty);
      switch (Typ.term_of(expected_ty)) {
      | Prod(_) =>
        /* Look up the completed variable in the context */
        let ctx = Info.ctx_of(Indicated.ci_of(z, info_map) |> Option.get);
        switch (Ctx.lookup_var(ctx, completed)) {
        | None => false
        | Some({typ, _}) =>
          let typ = Typ.weak_head_normalize(ctx, typ);
          switch (Typ.term_of(typ)) {
          | Unknown(_) => false
          | _ => Typ.is_consistent(ctx, typ, expected_ty)
          };
        };
      | _ => false
      };
    };
  };

/* ---- Display computation ---- */

/* Is the left boundary structurally empty? True when the nearest
 * tile (skipping grout and secondary) is a delimiter or absent.
 * Controls whether holes come before commas in the scaffold. */
let left_boundary_empty = (scoped_l: list(Piece.t)): bool => {
  let rec check =
    fun
    | [] => true
    | [Piece.Secondary(_), ...rest]
    | [Piece.Grout(_), ...rest] => check(rest)
    | [Piece.Tile({label: [","], _}), ..._] => true
    | [Piece.Tile({label: ["(", ")"], shards: [0], _}), ..._] => true
    /* After label= (TupleLabeledExp), the = is concave-right and
     * needs an operand. Treat as empty boundary so scaffold starts
     * with a hole, not a comma. */
    | [Piece.Tile({label: ["="], _}), ..._] => true
    | _ => false;
  check(List.rev(scoped_l));
};

/* Is the immediate left context a comma without trailing space?
 * Used to add a leading space to the scaffold for formatting:
 * f(1,¦ should show " ?, " not "?, ".
 * f(1, ¦ already has a space so no extra needed. */
let left_needs_space = (l: list(Piece.t)): bool =>
  switch (l) {
  | [Piece.Grout(_), ...rest] =>
    /* Skip grout then check for bare comma */
    switch (rest) {
    | [Piece.Tile({label: [","], _}), ..._] => true
    | _ => false
    }
  | [Piece.Tile({label: [","], _}), ..._] => true
  | _ => false
  };

/* Does the right side have a convex tile (not grout) that fills the
 * last element position? If so, no trailing hole is needed. */
let right_has_convex = (r: list(Piece.t)): bool => {
  let rec check =
    fun
    | [] => false
    | [Piece.Secondary(_), ...rest]
    | [Piece.Grout(_), ...rest] => check(rest)
    | [p, ..._] => Piece.is_convex(p);
  check(r);
};

/* Compute the scaffold buffer segment without modifying the zipper.
 * Returns None if no scaffold applies.
 *
 * Preconditions: outer caret, inside parens, no active buffer/selection.
 * Then: look up expected Prod type, check suppression, compute arity. */
let display = (~info_map: Statics.Map.t, z: Zipper.t): option(Segment.t) => {
  /* Preconditions */
  let* () = z.caret == Outer ? Some() : None;
  let* () = inside_parens(z) ? Some() : None;
  let* () =
    switch (z.selection.mode) {
    | Buffer(Parsed | Unparsed) => None
    | Normal when !Selection.is_empty(z.selection) => None
    | _ => Some()
    };

  /* Get the expected Prod type */
  let* expected_ty = expected_type(z, info_map);
  let expected_ty = unwrap_parens(expected_ty);
  let* tys =
    switch (Typ.term_of(expected_ty)) {
    | Prod(tys) when List.length(tys) >= 2 => Some(tys)
    | _ => None
    };

  /* Scope to innermost paren and check suppression */
  let scoped_l = inner_left_siblings(z);
  let l = List.rev(scoped_l);
  if (should_suppress(l, expected_ty, info_map)) {
    None;
  } else {
    let arity = List.length(tys);
    let existing_commas =
      count_commas_in(scoped_l) + count_commas_in(snd(z.relatives.siblings));
    let remaining = arity - 1 - existing_commas;
    let* () = remaining > 0 ? Some() : None;

    let holes_first = left_boundary_empty(scoped_l);
    let trailing_hole = !right_has_convex(snd(z.relatives.siblings));
    /* label_start: which element index do labels start from?
     * When holes_first and the boundary is a delimiter like ( or ,,
     * the scaffold fills from the current position: label_start = existing_commas.
     * When the boundary is = (user typed a label prefix), the current
     * element's label is already provided: label_start = existing_commas + 1. */
    let after_equals = {
      let rec check =
        fun
        | [Piece.Secondary(_), ...rest]
        | [Piece.Grout(_), ...rest] => check(rest)
        | [Piece.Tile({label: ["="], _}), ..._] => true
        | _ => false;
      check(List.rev(scoped_l));
    };
    let label_start =
      holes_first && !after_equals ? existing_commas : existing_commas + 1;
    let remaining_tys =
      tys
      |> List.filteri((i, _) => i >= label_start)
      |> (lst => List.filteri((i, _) => i < remaining, lst));
    let labels = List.map(label_of_prod_elem, remaining_tys);
    let seg =
      if (after_equals) {
        /* After = (user typed label prefix): [hole, comma, space]
         * for current value, then [comma, space, label, =, hole]
         * for each remaining element. */
        let mk_space = (): Piece.t =>
          Secondary({
            id: Id.mk(),
            content: Whitespace(" "),
          });
        let mk_hole = (): Piece.t =>
          Piece.Grout({
            id: Id.mk(),
            shape: Convex,
          });
        let mk_comma = (): Piece.t => Piece.mk_tile(Form.get(CommaExp), []);
        let mk_label = (i: int): list(Piece.t) =>
          switch (List.nth_opt(labels, i)) {
          | Some(Some(name)) => [
              Tile({
                id: Id.mk(),
                label: [Token.quote_label_when_necessary(name)],
                mold: Mold.mk_op(Sort.Exp, []),
                shards: [0],
                children: [],
              }),
              Piece.mk_tile(Form.get(TupleLabeledExp), []),
            ]
          | _ => []
          };
        /* Value hole for current element, then comma-separated
         * labeled entries for remaining elements */
        let n_remaining_labels = List.length(labels);
        List.concat(
          List.init(n_remaining_labels + 1, i =>
            if (i == 0) {
              [
                /* First: bare hole for current value + comma */
                mk_hole(),
                mk_comma(),
                mk_space(),
              ];
            } else {
              /* Subsequent: label + hole, preceded by comma (except
               * first which already has comma from previous entry) */
              let label_idx = i - 1;
              let is_last = i == n_remaining_labels;
              let entry = mk_label(label_idx) @ [mk_hole()];
              if (is_last) {
                /* Last entry: no trailing comma */
                entry;
              } else {
                entry @ [mk_comma(), mk_space()];
              };
            }
          ),
        );
      } else {
        mk_segment(~holes_first, ~trailing_hole, ~labels, remaining);
      };
    /* If caret immediately follows a comma without a trailing space,
     * prepend a space so the scaffold reads "f(1, ?" not "f(1,?". */
    let seg =
      left_needs_space(l)
        ? [
          Piece.Secondary({
            id: Id.mk(),
            content: Whitespace(" "),
          }),
          ...seg,
        ]
        : seg;
    Some(seg);
  };
};

/* ---- Grout stripping ---- */

/* Strip the first concave grout from a piece list, skipping over
 * whitespace. Used to resolve concave-concave shape conflicts
 * between buffer edges and adjacent sibling grout at buffer-set time. */
let strip_first_concave_grout = (pieces: list(Piece.t)): list(Piece.t) => {
  let rec go =
    fun
    | [Piece.Secondary(_) as s, ...rest] => [s, ...go(rest)]
    | [Piece.Grout({shape: Concave, _}), ...rest] => rest
    | pieces => pieces;
  go(pieces);
};

let strip_last_concave_grout = (pieces: list(Piece.t)): list(Piece.t) =>
  List.rev(strip_first_concave_grout(List.rev(pieces)));

/* ---- Public API ---- */

/* Does a segment contain structural pieces (Tiles/Grout)?
 * Used to determine if content needs grout stripping when set as buffer. */
let has_structural = (content: Segment.t): bool =>
  List.exists(
    (p: Piece.t) =>
      switch (p) {
      | Grout(_)
      | Tile(_) => true
      | _ => false
      },
    content,
  );

/* Set buffer with grout stripping for structural content.
 * When content has concave edges (comma tiles), strips adjacent
 * sibling grout to avoid shape conflicts.
 * If ~content is not provided, computes it via display(). */
let set = (~info_map: Statics.Map.t, ~content=?, z: Zipper.t): Zipper.t => {
  let content =
    switch (content) {
    | Some(c) => Some(c)
    | None => display(~info_map, z)
    };
  switch (content) {
  | None => z
  | Some(content) =>
    let concave_edge = (pieces: list(Piece.t)): bool => {
      let rec check = (
        fun
        | [] => false
        | [Piece.Secondary(_), ...rest] => check(rest)
        | [p, ..._] =>
          switch (Piece.shapes(p)) {
          | Some((Concave(_), _)) => true
          | _ => false
          }
      );
      check(pieces);
    };
    let has_concave_left_edge = concave_edge(content);
    let has_concave_right_edge = concave_edge(List.rev(content));
    let (l, r) = z.relatives.siblings;
    let l = has_concave_left_edge ? strip_last_concave_grout(l) : l;
    let r = has_concave_right_edge ? strip_first_concave_grout(r) : r;
    let z = {
      ...z,
      relatives: {
        ...z.relatives,
        siblings: (l, r),
      },
    };
    Zipper.set_buffer(z, ~content, ~mode=Unparsed);
  };
};

/* Split buffer content into leading completion text (Comment pieces)
 * and the structural scaffold remainder (commas, labels, grout). */
let split_leading_comments = (content: Segment.t): (string, Segment.t) => {
  let rec go = (acc_text, pieces) =>
    switch (pieces) {
    | [Piece.Secondary({content: Comment(s), _}), ...rest] =>
      go(acc_text ++ s, rest)
    | _ => (acc_text, pieces)
    };
  go("", content);
};

let reify = (z: Zipper.t): Zipper.t =>
  if (is_scaffold(z)) {
    let (leading_text, structural) =
      split_leading_comments(z.selection.content);
    /* Clear the buffer */
    let z = Zipper.clear_unparsed_buffer(z);
    /* Handle any leading completion text via Parser (appends to left token) */
    let z =
      switch (leading_text) {
      | "" => z
      | text =>
        switch (Parser.to_zipper(~zipper_init=z, text)) {
        | Some(z) => z
        | None => z
        }
      };
    /* Splice structural scaffold pieces directly, preserving IDs.
     * Set as a Normal selection with focus=Left (buffer convention)
     * so unselect places content to the right of the caret. */
    switch (structural) {
    | [] => z
    | _ =>
      let z = {
        ...z,
        selection: {
          content: structural,
          mode: Normal,
          focus: Left,
        },
      };
      Zipper.directional_unselect(Left, z);
    };
  } else {
    z;
  };

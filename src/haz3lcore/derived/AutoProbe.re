open Util.OptUtil.Syntax;
/* AUTOMATIC PROBE PLACEMENT FOR REPL MODE
 * This module determines which term on each line should receive an automatic probe.
 *
 * The core strategy is to analyze each line's candidate terms (ordered by priority:
 * find the most rightwards last position of a term ending on that line, then find the
 * largest term sharing that last position. This is the primary candidate, which we'll
 * refer to as the default below. The 'ordered candidates' consist of the default
 * candidate, followed by other terms sharing that same ending position (ordered by size),
 * then other terms ending on that line (first by rightwardsness, disambiguating by size).
 * We then apply a series of predicates and special case handlers which may result in
 * another term ending on that line, or no term at all, being indicated instead.
 * Ultimate we select 0 or 1 terms to display for each line in the probed range.
 *
 * DEFAULT BEHAVIOR:
 *
 * Step 1: Find rightmost ending position on the line
 * Step 2: Among terms ending at that position, pick the largest
 *
 * Only one term at rightmost position:
 *   let x = 1 in             // rightmost ending: where '1' ends
 *                            // terms ending there: just '1'
 *                            // probe: '1' (largest of 1 term)
 *
 * Multiple terms at rightmost position - largest wins:
 *   let x = 2 + 1 in         // rightmost ending: where '2 + 1' ends (same as where '1' ends)
 *                            // terms ending there: '1' and '2 + 1'
 *                            // probe: '2 + 1' (larger than '1')
 *
 * Rightmost position beats larger terms ending earlier:
 *   let (x, y) = v in  probe: 'v' (NOT '(x, y)' even though it's larger)
 *
 * DEFAULT MULTILINE BEHAVIOR:
 *
 * For function applications and other elimination forms, use the default logic, i.e.
 * favor probing the form itself at the end, instead of e.g. a subterm:
 *     fn(arg1,       // probe on reference 'arg1' (default)
 *        arg2        // probe on reference 'arg2' (default)
 *     ) in           // probe on application 'fn(arg1, arg2)' (default)
 *
  * HOLE AVOIDANCE EXAMPLES:
  *
  * Only holes on line:
  *   let incomplete =
  *     ? in               // probe on '?' (probe holes when they're the only option)
  *
  * FUNCTION TYPE FILTERING EXAMPLES:
  *
  * RULE: Function values have no good value display so avoid probing them.
  *       We approximate this statically, by not probing terms with function type.
  *
  * Example: Function literal:
  *   let adder =
  *     fun x -> x + 1 in  // probe on 'x + 1', NOT on 'fun x -> x + 1' (avoid function values)
  *
  * Example: Variable with function type:
  *   let f = get_fn(true) in   // probe on 'get_fn(true)', NOT on 'f' if f has function type
  *   f(42);                    // probe on 'f(42)' (default)
  *
  * RULE: Parens probes are redundant:
  *   (      // no probe (no terms end on this line)
  *     1    // probe on '1'  (default)
  *   )      // no probe (avoid redundant values from parens)
  *
  * CONTAINER SPECIAL CASES:
  *
  * RULE: Multi-line containers prefer elements over the whole container:
  *   let triple = (
  *     first_value,     // probe on 'first_value' (default)
  *     second_value,    // probe on 'second_value' (default)
  *     third_value      // probe on 'third_value' (default)
  *   ) in               // probe nothing (avoid redundant parens probing)
  *
  *   let items = [
  *     item1,           // probe on 'item1' (default)
  *     item2,           // probe on 'item2' (default)
  *     item3            // probe on 'item3' (default)
  *   ] in               // probe nothing (avoid redundant list literal)
  *
  * RULE: Single-line containers use normal default behavior (no special case):
  *   let pair = (a, b) in            // probe on '(a, b)' (rightmost ending term)
  *   let list = [1, 2, 3] in         // probe on '[1, 2, 3]' (rightmost ending term)
  *   let (y, z) = expr in            // probe on 'expr' (rightmost ending term, NOT 'z')
  *   let items = [item1, item2] in   // probe on '[item1, item2]' (rightmost ending, NOT 'item2')
  *
 * LET EXPRESSION SPECIAL CASES:
 *
 * RULE: Avoid probing lets with hole bodies (semantically uninformative):
 *
 * Basic case - prefer meaningful binding over uninformative let:
 *   let x = 2 + 1 in ?    // probe on '2 + 1', NOT on whole let (which evaluates to ?)
 *
 * No non-hole alternatives - stick with original behavior:
 *   let x = ? in ?        // probe on 'x' (no better alternatives available)
 *   let ? = 1 in ?        // probe on '1'
 *
 * Normal let (no hole body) - no special case:
 *   let x = 2 + 1 in x    // probe on whole let (normal default behavior)
 *
 * IF EXPRESSION SPECIAL CASES:
 *
 * Single-line if (default behavior):
 *   let result = if cond then a else b in  // probe on 'if cond then a else b'
 *
 * RULE: For multi-line ifs: don't probe whole if at end
 *   let result =
 *     if condition     // probe on 'condition' (default)
 *     then branch1     // probe on 'branch1' (largest on line)
 *     else branch2 in  // probe on 'branch2', NOT on whole if (prefer trailing branch)
 *
  * VARIABLE REFERENCE REDUNDANCY EXAMPLES:
  * (Record what variables have been seen to avoid redundant probes, respecting scope)
  *
  * Pattern binding tracking:
  *   let (a, b) =        // probe on '(a, b)' (default); track variables a, b as seen
  *     get_pair(1) in     // probe on 'get_pair(1)' (default)
  *   let x = 1 + a in    // probe on 'x', NOT on 'a' (already seen via pattern probe)
  *
 * Behavior is aligned with the scenarios documented in `test/Test_AutoProbe.re`.
 * The implementation follows a simple pipeline:
 *   1. Derive ordered candidate IDs per source row from `TermData`.
 *   2. Compute lightweight row metadata (e.g. whether only holes appear).
 *   3. Apply rule-based adjustments (currently only the if/else preference).
 *   4. Scan the ordered candidates and pick the first one accepted by the predicates.
 *   5. Update cross-row state (pattern tracking) when a probe is selected.
 */

type selection_state = {seen_patterns: list(Id.t)};

let empty_state: selection_state = {seen_patterns: []};

type selection_env = {
  terms: TermMap.t,
  data: TermData.t,
  measured: Measured.t,
  info_map: Language.Statics.Map.t,
};

type row_context = {
  row_index: int,
  ids: list(Id.t),
  hole_only: bool,
};

let get_term = (id: Id.t, terms: TermMap.t): option(Language.Any.t) =>
  Id.Map.find_opt(id, terms);

let term_is_hole = (term: Language.Any.t): bool =>
  switch (term) {
  | Exp({term: EmptyHole | MultiHole(_), _})
  | Pat({term: EmptyHole | MultiHole(_), _})
  | Typ({term: Unknown(Hole(EmptyHole | MultiHole(_))), _})
  | TPat({term: EmptyHole | MultiHole(_), _}) => true
  | _ => false
  };

let term_is_parens = (term: Language.Any.t): bool =>
  switch (term) {
  | Exp({term: Parens(_), _})
  | Pat({term: Parens(_), _})
  | Typ({term: Parens(_), _}) => true
  | _ => false
  };

let term_is_tuple = (term: Language.Any.t): bool =>
  switch (term) {
  | Exp({term: Tuple(_), _})
  | Pat({term: Tuple(_), _}) => true
  | _ => false
  };

let term_is_list_literal = (term: Language.Any.t): bool =>
  switch (term) {
  | Exp({term: ListLit(_), _})
  | Pat({term: ListLit(_), _}) => true
  | _ => false
  };

let term_is_variable = (term: Language.Any.t): bool =>
  switch (term) {
  | Exp({term: Var(_), _})
  | Pat({term: Var(_), _}) => true
  | _ => false
  };

let term_is_if = (term: Language.Any.t): bool =>
  switch (term) {
  | Exp({term: If(_, _, _), _}) => true
  | _ => false
  };

let term_is_let = (term: Language.Any.t): bool =>
  switch (term) {
  | Exp({term: Let(_, _, _), _}) => true
  | _ => false
  };

/* Module declarations (ModLet, ModType, ModuleMod) don't have runtime
 * values — they're declarations, not expressions. Auto-probe should
 * never probe them; it should probe their definition subexpression
 * instead (which appears as a separate candidate on the same line).
 * ModExp (bare expression in module body) is excluded — it wraps
 * an expression that does have a value. */
let term_is_mod_declaration = (term: Language.Any.t): bool =>
  switch (term) {
  | Mod({term: ModLet(_, _) | ModType(_, _) | ModuleMod(_, _), _}) => true
  | _ => false
  };

let let_body_is_hole = (term: Language.Any.t): bool =>
  switch (term) {
  | Exp({term: Let(_, _, body), _}) => term_is_hole(Exp(body))
  | _ => false
  };

let has_function_type = (id: Id.t, info_map: Language.Statics.Map.t): bool =>
  switch (Language.Statics.Map.lookup(id, info_map)) {
  | Some(InfoExp({ty, _}))
  | Some(InfoPat({ty, _})) => Language.Typ.is_arrow(ty)
  | _ => false
  };

let term_spans_multiple_rows =
    (id: Id.t, data: TermData.t, measured: Measured.t): bool =>
  switch (TermData.extreme_measures(id, data, measured)) {
  | Some((start_loc, end_loc)) => start_loc.row < end_loc.row
  | None => false
  };

let term_end_position =
    (candidate_id: Id.t, env: selection_env): option((int, int)) =>
  switch (TermData.extreme_measures(candidate_id, env.data, env.measured)) {
  | Some((_, end_loc)) => Some((end_loc.row, end_loc.col))
  | None => None
  };

let rec collect_row_terms =
        (ids: list(Id.t), terms: TermMap.t, acc: list(Language.Any.t))
        : list(Language.Any.t) =>
  switch (ids) {
  | [] => List.rev(acc)
  | [id, ...rest] =>
    let next_acc =
      switch (get_term(id, terms)) {
      | Some(term) => [term, ...acc]
      | None => acc
      };
    collect_row_terms(rest, terms, next_acc);
  };

let row_is_hole_only = (ids: list(Id.t), terms: TermMap.t): bool =>
  switch (collect_row_terms(ids, terms, [])) {
  | [] => false
  | collected_terms => List.for_all(term_is_hole, collected_terms)
  };

let rec collect_terms_for_row =
        (
          pieces: list(Piece.t),
          current_row: int,
          data: TermData.t,
          measured: Measured.t,
          acc: list((Id.t, int)),
        )
        : list((Id.t, int)) =>
  switch (pieces) {
  | [] => List.rev(acc)
  | [piece, ...rest] =>
    let maybe_position =
      TermData.extreme_measures(Piece.id(piece), data, measured);
    let next_acc =
      switch (maybe_position) {
      | Some((_, end_loc)) when end_loc.row == current_row => [
          (Piece.id(piece), end_loc.col),
          ...acc,
        ]
      | _ => acc
      };
    collect_terms_for_row(rest, current_row, data, measured, next_acc);
  };

let compare_by_col_desc = (pair1: (Id.t, int), pair2: (Id.t, int)): int =>
  switch (pair1, pair2) {
  | ((_, col1), (_, col2)) => Int.compare(col2, col1)
  };

let rec ids_from_pairs =
        (pairs: list((Id.t, int)), acc: list(Id.t)): list(Id.t) =>
  switch (pairs) {
  | [] => List.rev(acc)
  | [(id, _), ...rest] => ids_from_pairs(rest, [id, ...acc])
  };

let rec build_row_id_lists =
        (
          rows: list(Segment.t),
          start_row: int,
          index: int,
          data: TermData.t,
          measured: Measured.t,
          acc: list(list(Id.t)),
        )
        : list(list(Id.t)) =>
  switch (rows) {
  | [] => List.rev(acc)
  | [row, ...rest] =>
    let current_row = start_row + index;
    let terms_with_cols =
      collect_terms_for_row(row, current_row, data, measured, []);
    let sorted_pairs = List.sort(compare_by_col_desc, terms_with_cols);
    let ids = ids_from_pairs(sorted_pairs, []);
    build_row_id_lists(
      rest,
      start_row,
      index + 1,
      data,
      measured,
      [ids, ...acc],
    );
  };

let get_row_term_ids_prioritized =
    (id: Id.t, data: TermData.t, measured: Measured.t)
    : option(list(list(Id.t))) => {
  let+ (start_row_idx, term_rows) =
    TermData.get_term_rows(id, data, measured);
  build_row_id_lists(term_rows, start_row_idx, 0, data, measured, []);
};

let rec build_row_contexts =
        (
          rows: list(list(Id.t)),
          terms: TermMap.t,
          index: int,
          acc: list(row_context),
        )
        : list(row_context) =>
  switch (rows) {
  | [] => List.rev(acc)
  | [ids, ...rest] =>
    let context = {
      row_index: index,
      ids,
      hole_only: row_is_hole_only(ids, terms),
    };
    build_row_contexts(rest, terms, index + 1, [context, ...acc]);
  };

/* Check if a candidate corresponds to an incomplete tile.
 * Incomplete tiles become MultiHoles in the term structure,
 * but we don't want to filter them out like regular holes. */
let is_incomplete_tile = (candidate_id: Id.t, data: TermData.t): bool =>
  switch (TermData.root_tile(candidate_id, data)) {
  | Some(t) => !Tile.is_complete(t)
  | None => false
  };

/* Keywords that introduce a "body" determining the form's value.
 * When an incomplete tile is missing a shard with one of these,
 * the form's value is hole-like (body not yet typed). */
let body_introducing_keywords = ["in", "else", "end"];

/* Check if an incomplete tile is missing a body-determining shard.
 * E.g., `let a = expr` (missing "in") - the body determines the value.
 * Such forms should be deprioritized in favor of their subexpressions. */
let is_incomplete_binding_form = (candidate_id: Id.t, data: TermData.t): bool =>
  switch (TermData.root_tile(candidate_id, data)) {
  | Some(t) when !Tile.is_complete(t) =>
    /* Check if any missing shard is a body-introducing keyword */
    let missing_shards = Tile.missing_shards(t);
    List.exists(
      (shard: Tile.t) =>
        switch (shard.shards) {
        | [i] =>
          switch (List.nth_opt(shard.label, i)) {
          | Some(token) => List.mem(token, body_introducing_keywords)
          | None => false
          }
        | _ => false
        },
      missing_shards,
    );
  | _ => false
  };

/* Check if an id represents a "meaningful" alternative (not a hole,
 * or an incomplete tile that isn't a binding form). */
let is_meaningful_alternative =
    (id: Id.t, terms: TermMap.t, data: TermData.t): bool =>
  /* Incomplete non-binding tiles are meaningful alternatives */
  if (is_incomplete_tile(id, data) && !is_incomplete_binding_form(id, data)) {
    true;
  } else {
    switch (get_term(id, terms)) {
    | Some(term) => !term_is_hole(term)
    | None => false
    };
  };

let rec row_has_non_hole_alternative =
        (
          candidate_id: Id.t,
          ids: list(Id.t),
          terms: TermMap.t,
          data: TermData.t,
        )
        : bool =>
  switch (ids) {
  | [] => false
  | [id, ...rest] =>
    if (id == candidate_id) {
      row_has_non_hole_alternative(candidate_id, rest, terms, data);
    } else if (is_meaningful_alternative(id, terms, data)) {
      true;
    } else {
      row_has_non_hole_alternative(candidate_id, rest, terms, data);
    }
  };

let candidate_allowed_by_holes =
    (candidate_id: Id.t, row: row_context, env: selection_env): bool =>
  /* Allow incomplete tiles even though they appear as MultiHoles,
   * but deprioritize incomplete binding forms (like `let` missing `in`)
   * similar to how we handle lets with hole bodies. */
  if (is_incomplete_tile(candidate_id, env.data)) {
    if (is_incomplete_binding_form(candidate_id, env.data)) {
      !
        row_has_non_hole_alternative(
          candidate_id,
          row.ids,
          env.terms,
          env.data,
        );
        /* For incomplete binding forms, only allow if no better alternatives */
    } else {
      true;
    };
  } else {
    switch (get_term(candidate_id, env.terms)) {
    | Some(term) when term_is_hole(term) => row.hole_only
    | _ => true
    };
  };

let candidate_allowed_by_function_types =
    (candidate_id: Id.t, env: selection_env): bool =>
  !has_function_type(candidate_id, env.info_map);

let candidate_allowed_by_container =
    (candidate_id: Id.t, env: selection_env): bool =>
  switch (get_term(candidate_id, env.terms)) {
  | Some(term) =>
    if (term_is_parens(term)) {
      false;
    } else if (term_is_tuple(term) || term_is_list_literal(term)) {
      !term_spans_multiple_rows(candidate_id, env.data, env.measured);
    } else {
      true;
    }
  | None => true
  };

let candidate_allowed_by_let_hole =
    (candidate_id: Id.t, row: row_context, env: selection_env): bool =>
  switch (get_term(candidate_id, env.terms)) {
  | Some(term) when term_is_let(term) && let_body_is_hole(term) =>
    !row_has_non_hole_alternative(candidate_id, row.ids, env.terms, env.data)
  | _ => true
  };

let is_redundant_variable_reference =
    (
      candidate_id: Id.t,
      info_map: Language.Statics.Map.t,
      seen_patterns: list(Id.t),
    )
    : bool =>
  switch (Language.Statics.Map.lookup(candidate_id, info_map)) {
  | Some(info) =>
    switch (Language.Info.get_binding_site(info)) {
    | Some(binding_id) => List.mem(binding_id, seen_patterns)
    | None => false
    }
  | None => false
  };

let candidate_allowed_by_variables =
    (candidate_id: Id.t, env: selection_env, state: selection_state): bool =>
  switch (get_term(candidate_id, env.terms)) {
  | Some(term) when term_is_variable(term) =>
    !
      is_redundant_variable_reference(
        candidate_id,
        env.info_map,
        state.seen_patterns,
      )
  | _ => true
  };

/* Filter out terms that should never have probes. */
let candidate_allowed_by_term_sort =
    (candidate_id: Id.t, env: selection_env): bool =>
  Language.Info.is_typable_term(
    Language.Statics.Map.lookup(candidate_id, env.info_map),
  );

let candidate_allowed_by_mod_declaration =
    (candidate_id: Id.t, env: selection_env): bool =>
  switch (get_term(candidate_id, env.terms)) {
  | Some(term) => !term_is_mod_declaration(term)
  | None => true
  };

let candidate_is_allowed =
    (
      candidate_id: Id.t,
      row: row_context,
      env: selection_env,
      _state: selection_state,
    )
    : bool =>
  candidate_allowed_by_term_sort(candidate_id, env)
  && candidate_allowed_by_mod_declaration(candidate_id, env)
  && candidate_allowed_by_holes(candidate_id, row, env)
  && candidate_allowed_by_function_types(candidate_id, env)
  && candidate_allowed_by_container(candidate_id, env)
  && candidate_allowed_by_let_hole(candidate_id, row, env);
//&& candidate_allowed_by_variables(candidate_id, env, state);

let rec extract_candidate_with_end =
        (
          ids: list(Id.t),
          env: selection_env,
          target_row: int,
          target_col: int,
          acc: list(Id.t),
        )
        : (option(Id.t), list(Id.t)) =>
  switch (ids) {
  | [] => (None, List.rev(acc))
  | [id, ...rest] =>
    let matches =
      switch (term_end_position(id, env)) {
      | Some((row_value, col_value)) =>
        row_value == target_row && col_value == target_col
      | None => false
      };
    if (matches) {
      (Some(id), List.rev_append(acc, rest));
    } else {
      extract_candidate_with_end(
        rest,
        env,
        target_row,
        target_col,
        [id, ...acc],
      );
    };
  };

let adjust_candidates_for_if =
    (row: row_context, env: selection_env): list(Id.t) =>
  switch (row.ids) {
  | [] => []
  | [first_id, ...rest] =>
    let is_if_candidate =
      switch (get_term(first_id, env.terms)) {
      | Some(term) => term_is_if(term)
      | None => false
      };
    if (!is_if_candidate) {
      row.ids;
    } else {
      switch (term_end_position(first_id, env)) {
      | Some((target_row, target_col)) =>
        let (maybe_else, remaining) =
          extract_candidate_with_end(rest, env, target_row, target_col, []);
        switch (maybe_else) {
        | Some(else_id) => [else_id, first_id, ...remaining]
        | None => row.ids
        };
      | None => row.ids
      };
    };
  };

let rec collect_pattern_binding_ids =
        (bindings: list(Language.Binding.t), acc: list(Id.t)): list(Id.t) =>
  switch (bindings) {
  | [] => List.rev(acc)
  | [binding, ...rest] =>
    collect_pattern_binding_ids(rest, [binding.id, ...acc])
  };

let update_state_with_candidate =
    (candidate_id: Id.t, env: selection_env, state: selection_state)
    : selection_state =>
  switch (Id.Map.find_opt(candidate_id, env.terms)) {
  | Some(Pat(pat)) =>
    let binding_ids =
      collect_pattern_binding_ids(Language.Pat.bindings(pat), []);
    {seen_patterns: binding_ids @ state.seen_patterns};
  | _ => state
  };

let rec choose_candidate =
        (
          candidates: list(Id.t),
          row: row_context,
          env: selection_env,
          state: selection_state,
        )
        : (option(Id.t), selection_state) =>
  switch (candidates) {
  | [] => (None, state)
  | [candidate_id, ...rest] =>
    if (candidate_is_allowed(candidate_id, row, env, state)) {
      let next_state = update_state_with_candidate(candidate_id, env, state);
      (Some(candidate_id), next_state);
    } else {
      choose_candidate(rest, row, env, state);
    }
  };

let select_in_row =
    (row: row_context, env: selection_env, state: selection_state)
    : (option(Id.t), selection_state) => {
  let adjusted_ids = adjust_candidates_for_if(row, env);
  choose_candidate(adjusted_ids, row, env, state);
};

let rec select_rows =
        (rows: list(row_context), env: selection_env, state: selection_state)
        : (list(option(Id.t)), selection_state) =>
  switch (rows) {
  | [] => ([], state)
  | [row, ...rest] =>
    let (selected, next_state) = select_in_row(row, env, state);
    let (tail, final_state) = select_rows(rest, env, next_state);
    ([selected, ...tail], final_state);
  };

/* Normalize an ID to its term's rep_id.
 * Multi-tile forms (like case expressions) have multiple IDs in their annotation,
 * but the evaluator stores samples keyed by the rep_id. If we probe a non-rep ID,
 * the sample lookup will fail. This ensures we always use the rep_id. */
let normalize_to_rep_id = (id: Id.t, terms: TermMap.t): Id.t =>
  switch (Id.Map.find_opt(id, terms)) {
  | Some(term) => Language.Any.rep_id(term)
  | None => id
  };

let ids_to_autoprobe =
    (
      id: Id.t,
      data: TermData.t,
      terms: TermMap.t,
      measured: Measured.t,
      info_map: Language.Statics.Map.t,
    )
    : option(list(option(Id.t))) => {
  let+ row_term_ids = get_row_term_ids_prioritized(id, data, measured);
  let row_contexts = build_row_contexts(row_term_ids, terms, 0, []);
  let env = {
    terms,
    data,
    measured,
    info_map,
  };
  let (selections, _) = select_rows(row_contexts, env, empty_state);
  /* Normalize each selected ID to its rep_id to ensure sample lookup works */
  List.map(
    fun
    | Some(selected_id) => Some(normalize_to_rep_id(selected_id, terms))
    | None => None,
    selections,
  );
};

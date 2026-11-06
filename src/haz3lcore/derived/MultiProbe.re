open Util.OptUtil.Syntax;
open Poly;
/* MULTI PROBE PLACEMENT
 * This module determines which term on each line should receive a multi probe.
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
  * RULE: TupLabel probes are redundant (label is visible in source):
  *   let r = (
  *     brush = "a",     // probe on '"a"', NOT on 'brush = "a"'
  *     palette = [...]  // probe on '[...]', NOT on 'palette = [...]'
  *   ) in
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
 * Behavior is aligned with the scenarios documented in `test/Test_MultiProbe.re`.
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
  Id.Map.find(terms, id);

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

let term_is_tuplabel = (term: Language.Any.t): bool =>
  switch (term) {
  | Exp({term: TupLabel(_, _), _})
  | Pat({term: TupLabel(_, _), _}) => true
  | _ => false
  };

let term_is_list_literal = (term: Language.Any.t): bool =>
  switch (term) {
  | Exp({term: ListLit(_), _})
  | Pat({term: ListLit(_), _}) => true
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
  Language.Statics.Map.ty_of(id, info_map)
  |> Option.exists(~f=Language.Typ.is_arrow);

let term_spans_multiple_rows =
    (id: Id.t, data: TermData.t, measured: Measured.t): bool =>
  TermData.extreme_measures(id, data, measured)
  |> Option.exists(
       ~f=((start_loc: Measured.Point.t, end_loc: Measured.Point.t)) =>
       start_loc.row < end_loc.row
     );

let term_end_position =
    (candidate_id: Id.t, env: selection_env): option((int, int)) =>
  TermData.extreme_measures(candidate_id, env.data, env.measured)
  |> Option.map(~f=((_, end_loc: Measured.Point.t)) =>
       (end_loc.row, end_loc.col)
     );

let row_is_hole_only = (ids: list(Id.t), terms: TermMap.t): bool =>
  switch (List.filter_map(ids, ~f=id => get_term(id, terms))) {
  | [] => false
  | collected_terms => List.for_all(~f=term_is_hole, collected_terms)
  };

let collect_terms_for_row =
    (
      pieces: list(Piece.t),
      current_row: int,
      data: TermData.t,
      measured: Measured.t,
    )
    : list((Id.t, int)) =>
  List.filter_map(pieces, ~f=piece =>
    switch (TermData.extreme_measures(Piece.id(piece), data, measured)) {
    | Some((_, end_loc)) when end_loc.row == current_row =>
      Some((Piece.id(piece), end_loc.col))
    | _ => None
    }
  );

let compare_by_col_desc =
    ((_, col1): (Id.t, int), (_, col2): (Id.t, int)): int =>
  Int.compare(col2, col1);

let get_row_term_ids_prioritized =
    (id: Id.t, data: TermData.t, measured: Measured.t)
    : option(list(list(Id.t))) => {
  let+ (start_row_idx, term_rows) =
    TermData.get_term_rows(id, data, measured);
  List.mapi(term_rows, ~f=(index, row) =>
    collect_terms_for_row(row, start_row_idx + index, data, measured)
    |> List.sort(~compare=compare_by_col_desc)
    |> List.map(~f=fst)
  );
};

let build_row_contexts =
    (rows: list(list(Id.t)), terms: TermMap.t): list(row_context) =>
  List.mapi(rows, ~f=(row_index, ids) =>
    {
      row_index,
      ids,
      hole_only: row_is_hole_only(ids, terms),
    }
  );

/* Check if a candidate corresponds to an incomplete tile.
 * Incomplete tiles become MultiHoles in the term structure,
 * but we don't want to filter them out like regular holes. */
let is_incomplete_tile = (candidate_id: Id.t, data: TermData.t): bool =>
  TermData.root_tile(candidate_id, data)
  |> Option.exists(~f=t => !Tile.is_complete(t));

/* Check if a candidate is a Rule tile (| pat => body).
 * Rule tiles never have values — they're structural, not evaluable.
 * When incomplete (just |), they appear as MultiHoles in the term map,
 * so the term-level candidate_allowed_by_rule won't catch them. */
let is_rule_tile = (candidate_id: Id.t, data: TermData.t): bool =>
  TermData.root_tile(candidate_id, data)
  |> Option.exists(~f=Tile.is_case_rule);

/* Check if an incomplete tile is missing a body-determining shard.
 * E.g., `let a = expr` (missing "in") - the body determines the value.
 * Such forms should be deprioritized in favor of their subexpressions. */
let is_incomplete_binding_form = (candidate_id: Id.t, data: TermData.t): bool =>
  switch (TermData.root_tile(candidate_id, data)) {
  | Some(t) when !Tile.is_complete(t) =>
    List.exists(~f=Tile.is_body_introducing_shard, Tile.missing_shards(t))
  | _ => false
  };

/* Check if a candidate is an InfixDelimiterPrefix tile -- a partial
 * keyword like `th` (prefix of `then`). These are transient artifacts
 * of typing and should never be probed or count as alternatives. */
let is_delimiter_prefix = (candidate_id: Id.t, data: TermData.t): bool =>
  TermData.root_tile(candidate_id, data)
  |> Option.exists(~f=t => Piece.is_infix_delimiter_op_prefix(Tile(t)));

/* Check if an id represents a "meaningful" alternative (not a hole,
 * not a delimiter prefix, or an incomplete tile that isn't a binding form). */
let is_meaningful_alternative =
    (id: Id.t, terms: TermMap.t, data: TermData.t): bool =>
  if (is_delimiter_prefix(id, data)) {
    false;
  } else if (is_rule_tile(id, data)) {
    false;
  } else if (is_incomplete_tile(id, data)
             && !is_incomplete_binding_form(id, data)) {
    true;
  } else {
    get_term(id, terms) |> Option.exists(~f=term => !term_is_hole(term));
  };

let row_has_non_hole_alternative =
    (candidate_id: Id.t, ids: list(Id.t), terms: TermMap.t, data: TermData.t)
    : bool =>
  List.exists(ids, ~f=id =>
    id != candidate_id && is_meaningful_alternative(id, terms, data)
  );

let candidate_allowed_by_holes =
    (candidate_id: Id.t, row: row_context, env: selection_env): bool =>
  /* Allow incomplete tiles even though they appear as MultiHoles,
   * but deprioritize incomplete binding forms (like `let` missing `in`)
   * similar to how we handle lets with hole bodies. */
  if (is_incomplete_tile(candidate_id, env.data)) {
    if (is_rule_tile(candidate_id, env.data)) {
      false;
           /* Rule tiles (| => ) never have values, even when incomplete */
    } else if (is_incomplete_binding_form(candidate_id, env.data)) {
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

/* Soft reject: allow function-typed terms only if no better alternative */
let candidate_allowed_by_function_types =
    (candidate_id: Id.t, row: row_context, env: selection_env): bool =>
  if (has_function_type(candidate_id, env.info_map)) {
    !row_has_non_hole_alternative(candidate_id, row.ids, env.terms, env.data);
  } else {
    true;
  };

let candidate_allowed_by_container =
    (candidate_id: Id.t, env: selection_env): bool =>
  switch (get_term(candidate_id, env.terms)) {
  | Some(term) =>
    if (term_is_parens(term) || term_is_tuplabel(term)) {
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

/* Filter out terms that should never have probes. When the info_map
 * is empty (e.g. during the initial post-reload calculate, before
 * statics have been computed), short-circuit to true so the candidate
 * set isn't wiped. The downstream calculate pass will re-run this
 * filter once statics are available. */
let candidate_allowed_by_term_sort =
    (candidate_id: Id.t, env: selection_env): bool =>
  Id.Map.is_empty(env.info_map)
  || Language.Info.is_typable_term(
       Language.Statics.Map.lookup(candidate_id, env.info_map),
     );

/* Never probe InfixDelimiterPrefix tiles (partial keywords like `th`
 * for `then`). These are transient typing artifacts. */
let candidate_allowed_by_delimiter_prefix =
    (candidate_id: Id.t, env: selection_env): bool =>
  !is_delimiter_prefix(candidate_id, env.data);

let candidate_allowed_by_mod_declaration =
    (candidate_id: Id.t, env: selection_env): bool =>
  get_term(candidate_id, env.terms)
  |> Option.for_all(~f=term => !term_is_mod_declaration(term));

/* Rule tiles (case arms: | pat => body) never have values and
 * should never be probed. Their children (pat, body) are valid. */
let candidate_allowed_by_rule = (candidate_id: Id.t, env: selection_env): bool =>
  switch (get_term(candidate_id, env.terms)) {
  | Some(Rul(_)) => false
  | _ => true
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
  && candidate_allowed_by_delimiter_prefix(candidate_id, env)
  && candidate_allowed_by_mod_declaration(candidate_id, env)
  && candidate_allowed_by_rule(candidate_id, env)
  && candidate_allowed_by_holes(candidate_id, row, env)
  && candidate_allowed_by_function_types(candidate_id, row, env)
  && candidate_allowed_by_container(candidate_id, env)
  && candidate_allowed_by_let_hole(candidate_id, row, env);
//&& candidate_allowed_by_variables(candidate_id, env, state);

let extract_candidate_with_end =
    (
      ids: list(Id.t),
      env: selection_env,
      (target_row, target_col): (int, int),
    )
    : option((Id.t, list(Id.t))) =>
  Util.ListUtil.find_with_rest(
    id =>
      switch (term_end_position(id, env)) {
      | Some((row_value, col_value))
          when row_value == target_row && col_value == target_col =>
        Some(id)
      | _ => None
      },
    ids,
  );

/* When the first candidate on a row is an if expression, promote its
 * else branch (which ends at the same position) to be tried first,
 * and drop the if from the candidates entirely. The branch is always
 * more specific than the enclosing if -- and critically, this prevents
 * hole avoidance from rejecting a hole else branch in favor of the
 * whole if expression. For branching forms, even a hole branch is
 * worth probing: the sample count shows branch frequency.
 *
 * NOTE: case expressions with `end` on the same line as the last
 * branch have an analogous issue -- a hole branch would be rejected
 * in favor of the whole case. A similar adjust_candidates_for_case
 * could handle this if it comes up in practice. */
let adjust_candidates_for_if =
    (row: row_context, env: selection_env): list(Id.t) =>
  switch (row.ids) {
  | [] => []
  | [first_id, ...rest] =>
    let is_if_candidate =
      get_term(first_id, env.terms) |> Option.exists(~f=term_is_if);
    if (!is_if_candidate) {
      row.ids;
    } else {
      switch (
        term_end_position(first_id, env)
        |> Option.bind(~f=extract_candidate_with_end(rest, env))
      ) {
      | Some((else_id, remaining)) => [else_id, ...remaining]
      | None => row.ids
      };
    };
  };

/* When the first candidate is an incomplete binding form (missing a
 * body-introducing keyword like `else`, `in`, or `end`), promote its
 * trailing sibling and drop the incomplete form. Same rationale as
 * adjust_candidates_for_if: the trailing expression/hole is more
 * specific, and for incomplete branching forms (e.g. `if cond then ?`
 * missing `else`), the hole's sample count shows branch frequency. */
let adjust_candidates_for_incomplete_binding =
    (ids: list(Id.t), env: selection_env): list(Id.t) =>
  switch (ids) {
  | [] => []
  | [first_id, ...rest] =>
    if (!is_incomplete_binding_form(first_id, env.data)) {
      ids;
    } else {
      switch (
        term_end_position(first_id, env)
        |> Option.bind(~f=extract_candidate_with_end(rest, env))
      ) {
      | Some((trailing_id, remaining)) => [trailing_id, ...remaining]
      | None => ids
      };
    }
  };

let update_state_with_candidate =
    (candidate_id: Id.t, env: selection_env, state: selection_state)
    : selection_state =>
  switch (Id.Map.find(env.terms, candidate_id)) {
  | Some(Pat(pat)) =>
    let binding_ids =
      List.map(Language.Pat.bindings(pat), ~f=(binding: Language.Binding.t) =>
        binding.id
      );
    {seen_patterns: binding_ids @ state.seen_patterns};
  | _ => state
  };

let choose_candidate =
    (
      candidates: list(Id.t),
      row: row_context,
      env: selection_env,
      state: selection_state,
    )
    : (option(Id.t), selection_state) =>
  switch (
    List.find(candidates, ~f=candidate_id =>
      candidate_is_allowed(candidate_id, row, env, state)
    )
  ) {
  | Some(candidate_id) => (
      Some(candidate_id),
      update_state_with_candidate(candidate_id, env, state),
    )
  | None => (None, state)
  };

let select_in_row =
    (row: row_context, env: selection_env, state: selection_state)
    : (option(Id.t), selection_state) => {
  let adjusted_ids = adjust_candidates_for_if(row, env);
  let adjusted_ids =
    adjust_candidates_for_incomplete_binding(adjusted_ids, env);
  /* Recompute hole_only after adjustments -- dropping forms from the
   * candidates may leave only holes, allowing them through. */
  let adjusted_row = {
    ...row,
    ids: adjusted_ids,
    hole_only: row_is_hole_only(adjusted_ids, env.terms),
  };
  choose_candidate(adjusted_ids, adjusted_row, env, state);
};

/* Normalize an ID to its term's rep_id.
 * Multi-tile forms (like case expressions) have multiple IDs in their annotation,
 * but the evaluator stores samples keyed by the rep_id. If we probe a non-rep ID,
 * the sample lookup will fail. This ensures we always use the rep_id. */
let normalize_to_rep_id = (id: Id.t, terms: TermMap.t): Id.t =>
  Id.Map.find(terms, id)
  |> Option.value_map(~default=id, ~f=Language.Any.rep_id);

let ids_to_multiprobe =
    (
      id: Id.t,
      data: TermData.t,
      terms: TermMap.t,
      measured: Measured.t,
      info_map: Language.Statics.Map.t,
    )
    : option(list(option(Id.t))) => {
  let+ row_term_ids = get_row_term_ids_prioritized(id, data, measured);
  let row_contexts = build_row_contexts(row_term_ids, terms);
  let env = {
    terms,
    data,
    measured,
    info_map,
  };
  let (_, selections) =
    List.fold_map(
      row_contexts,
      ~init=empty_state,
      ~f=(state, row) => {
        let (selected, next_state) = select_in_row(row, env, state);
        (next_state, selected);
      },
    );
  /* Normalize each selected ID to its rep_id to ensure sample lookup works */
  List.map(
    selections,
    ~f=Option.map(~f=selected_id => normalize_to_rep_id(selected_id, terms)),
  );
};

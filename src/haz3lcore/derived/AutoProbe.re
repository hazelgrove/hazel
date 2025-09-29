open Util.OptUtil.Syntax;

/*
 * AUTOMATIC PROBE PLACEMENT FOR REPL MODE
 *
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
  * The implementation processes each line through:
  * 1. Special case handlers (if expressions, containers)
  * 2. Predicate-based filtering (holes, functions, redundant containers, variables)
  * 3. Priority-based selection (largest term first, then others by ending position)
  */

/* Term analysis utilities */
let get_term_by_id: (Id.t, TermMap.t) => option(Language.Any.t) =
  (id: Id.t, terms: TermMap.t) => Id.Map.find_opt(id, terms);

let is_hole_term: Language.Any.t => bool = {
  fun
  | Exp({term: EmptyHole | MultiHole(_), _}) => true
  | Pat({term: EmptyHole | MultiHole(_), _}) => true
  | Typ({term: Unknown(Hole(EmptyHole | MultiHole(_))), _}) => true
  | TPat({term: EmptyHole | MultiHole(_), _}) => true
  | _ => false;
};

let is_parens_term: Language.Any.t => bool =
  fun
  | Exp({term: Parens(_), _}) => true
  | Pat({term: Parens(_), _}) => true
  | Typ({term: Parens(_), _}) => true
  | _ => false;

let is_tuple_term: Language.Any.t => bool =
  fun
  | Exp({term: Tuple(_), _}) => true
  | Pat({term: Tuple(_), _}) => true
  | _ => false;

let is_list_literal_term: Language.Any.t => bool =
  fun
  | Exp({term: ListLit(_), _}) => true
  | Pat({term: ListLit(_), _}) => true
  | _ => false;

let is_variable_reference: Language.Any.t => bool =
  fun
  | Exp({term: Var(_), _}) => true
  | Pat({term: Var(_), _}) => true
  | _ => false;

/* ===== REFACTORED PROBE PLACEMENT INFRASTRUCTURE ===== */

[@deriving show({with_path: false})]
type probe_candidate = {
  id: Id.t,
  row: int,
  col: int,
  is_largest_on_line: bool,
};

/* Extract term IDs on each row, ordered by priority:
 * 1. Largest term ending on that line (rightmost ending position)
 * 2. Other terms ending at the same position (ordered left to right)
 * 3. Other terms ending earlier on the line (ordered by ending position, then left to right)
 */
let get_row_term_ids_prioritized =
    (id: Id.t, data: TermData.t, measured: Measured.t)
    : option(list(list(Id.t))) => {
  let+ (start_row_idx, term_rows) =
    TermData.get_term_rows(id, data, measured);

  let get_final_col = (current_row: int, piece: Piece.t): option(int) =>
    switch (TermData.extreme_measures(Piece.id(piece), data, measured)) {
    | Some((_, final)) when final.row == current_row => Some(final.col)
    | _ => None
    };

  term_rows
  |> List.mapi((row_index: int, row: Segment.t) => {
       let current_row = start_row_idx + row_index;
       /* Build list of (id, col) pairs for terms ending on this row */
       let terms_with_cols =
         row
         |> List.filter_map(piece => {
              let id = Piece.id(piece);
              let+ col = get_final_col(current_row, piece);
              (id, col);
            });
       /* Sort by column (rightmost first), then by original order (leftmost first) */
       terms_with_cols
       |> List.sort(((_, col1), (_, col2)) => Int.compare(col2, col1))
       |> List.map(fst); /* Extract just the IDs */
     });
};

/* ===== PHASE 2: HOLE-AWARE PROBE PLACEMENT ===== */

let only_hole: (list(Id.t), TermMap.t) => bool =
  (term_ids: list(Id.t), terms: TermMap.t) => {
    let (hole_count, non_hole_count) =
      term_ids
      |> List.fold_left(
           ((hole_acc, non_hole_acc), id) => {
             switch (get_term_by_id(id, terms)) {
             | Some(term) when is_hole_term(term) => (
                 hole_acc + 1,
                 non_hole_acc,
               )
             | Some(_) => (hole_acc, non_hole_acc + 1)
             | None => (hole_acc, non_hole_acc) /* Term not found, skip */
             }
           },
           (0, 0),
         );
    hole_count > 0 && non_hole_count == 0;
  };

/* Enhanced probe context for sophisticated predicates */
type probe_context = {
  candidate: probe_candidate,
  term: option(Language.Any.t),
  hole_only: bool,
  terms: TermMap.t,
};

/* Predicate: Don't probe holes unless there are only holes on the line */
let should_not_probe_holes_unless_only_holes: probe_context => bool =
  context => {
    let is_candidate_hole =
      context.term
      |> Option.map(is_hole_term)
      |> Option.value(~default=false);

    /* If candidate is a hole, only allow it if all terms on line are holes */
    if (is_candidate_hole) {
      context.hole_only;
    } else {
      true; /* Non-holes are always ok */
    };
  };

/* ===== PHASE 3: FUNCTION TYPE DETECTION (Enhanced with Statics) ===== */

/* Check if a type represents a function type using statics information
 * This is more precise than syntactic detection since it catches:
 * - Function literals: fun x -> ...
 * - Variables bound to functions: let f = fun x -> ... in f
 * - Function applications that return functions: get_fn()(x)
 * - Any other expressions that evaluate to function values
 */
let has_function_type = (id: Id.t, info_map: Language.Statics.Map.t): bool => {
  switch (Language.Statics.Map.lookup(id, info_map)) {
  | Some(InfoExp({ty, _})) => Language.Typ.is_arrow(ty)
  | Some(InfoPat({ty, _})) => Language.Typ.is_arrow(ty) /* Pattern variables can have function types too */
  | _ => false /* If we can't determine the type, assume it's not a function */
  };
};

/* Predicate: Don't probe expressions with function types (we don't show function values) */
let should_not_probe_function_types =
    (context: probe_context, info_map: Language.Statics.Map.t): bool => {
  !has_function_type(context.candidate.id, info_map);
};

/* Combine multiple predicates with AND logic */
let combine_predicates: (list(probe_context => bool), probe_context) => bool =
  (predicates, context) => List.for_all(pred => pred(context), predicates);

/* ===== PHASE 4: TUPLE/LIST SPECIAL CASE LOGIC ===== */

/* Handle tuple/list special case: prefer elements over literals in multi-line cases
 * Examples:
 *   Single line: (a, b, c)     // probe the whole tuple (no special case)
 *   Multi-line:  (a,    // probe 'a'
 *                 b,    // probe 'b'
 *                 c)    // probe 'c', NOT the tuple
 *   Mixed:       (a, b, // probe 'b' (rightmost on this line)
 *                 c)    // probe 'c', NOT the tuple
 *   List:        [x,    // probe 'x'
 *                 y]    // probe 'y', NOT the list
 */
let handle_tuple_list_special_case =
    (
      candidates: list(Id.t),
      terms: TermMap.t,
      data: TermData.t,
      measured: Measured.t,
    )
    : option(Id.t) => {
  /* Strategy: Look for tuple/list literals and see if we should prefer their elements instead */
  candidates
  |> List.find_map(literal_id => {
       switch (get_term_by_id(literal_id, terms)) {
       | Some(literal_term)
           when
             is_tuple_term(literal_term)
             || is_list_literal_term(literal_term) =>
         /* Only apply special case logic for multi-line containers */
         switch (TermData.extreme_measures(literal_id, data, measured)) {
         | Some((literal_start, literal_end))
             when literal_start.row < literal_end.row =>
           /* Multi-line container: prefer elements over the container */
           candidates
           |> List.find_map(candidate_id =>
                if (candidate_id == literal_id) {
                  None; /* Skip the literal itself */
                } else {
                  /* Check if this candidate would be a better choice than the literal */
                  switch (
                    TermData.extreme_measures(candidate_id, data, measured)
                  ) {
                  | Some((_, candidate_end)) =>
                    /* Prefer the candidate if it ends on an earlier row than the literal */
                    if (candidate_end.row < literal_end.row) {
                      Some(candidate_id);
                    } else {
                      None;
                    }
                  | _ => None
                  };
                }
              )
         | _ => None /* Single-line container: no special case, use normal logic */
         }
       | _ => None /* Not a tuple/list literal */
       }
     });
};

/* Handle container closing elements: avoid redundant probes on multi-line containers
 * Examples:
 *   Tuple:  (a,
 *            b)   // probe on 'b', NOT on closing parens
 *   List:   [a,
 *            b]   // probe on 'b', NOT on the list literal [a,b]
 */
let should_not_probe_redundant_container =
    (context: probe_context, data: TermData.t, measured: Measured.t): bool => {
  let is_candidate_parens =
    context.term
    |> Option.map(is_parens_term)
    |> Option.value(~default=false);

  let is_multiline_container_on_final_line =
    context.term
    |> Option.map(term =>
         if (is_tuple_term(term) || is_list_literal_term(term)) {
           /* Check if this container spans multiple lines by looking at its start vs end */
           switch (
             TermData.extreme_measures(context.candidate.id, data, measured)
           ) {
           | Some((start, end_)) when start.row < end_.row =>
             /* Multi-line container: avoid probing on final line since elements were already probed */
             true
           | _ => false /* Single line container: normal probe logic applies */
           };
         } else {
           false;
         }
       )
    |> Option.value(~default=false);

  /* Avoid both: 1) parens terms, 2) multi-line containers on their final line */
  !is_candidate_parens && !is_multiline_container_on_final_line;
};

/* ===== PHASE 5: IF EXPRESSION SPECIAL CASE LOGIC ===== */

let is_if_expression: Language.Any.t => bool =
  fun
  | Exp({term: If(_, _, _), _}) => true
  | _ => false;

let is_let_expression: Language.Any.t => bool =
  fun
  | Exp({term: Let(_, _, _), _}) => true
  | _ => false;

/* Check if a let expression has a hole as its body */
let let_body_is_hole: Language.Any.t => bool =
  fun
  | Exp({term: Let(_, _, body), _}) => is_hole_term(Exp(body))
  | _ => false;

/* Handle if expression special case: prefer trailing else branch over whole if
 * Example:
 *   if cond
 *   then branch1
 *   else branch2    // probe on 'branch2', NOT on the whole if expression
 *
 * Only applies when the if is multi-line and else branch is the trailing term
 */
let handle_if_expression_special_case =
    (
      candidates: list(Id.t),
      terms: TermMap.t,
      data: TermData.t,
      measured: Measured.t,
    )
    : option(Id.t) => {
  /* Look for pattern where first candidate is if expression and there are other candidates */
  switch (candidates) {
  | [if_id, ...rest] when rest != [] =>
    switch (get_term_by_id(if_id, terms)) {
    | Some(if_term) when is_if_expression(if_term) =>
      /* Check if the if expression is multi-line by seeing if any candidates end later */
      switch (TermData.extreme_measures(if_id, data, measured)) {
      | Some((_if_start, if_end)) =>
        /* Look for a candidate that might be the else branch ending at the same position as the if */
        rest
        |> List.find_opt(candidate_id => {
             switch (TermData.extreme_measures(candidate_id, data, measured)) {
             | Some((_, candidate_end)) =>
               candidate_end.row == if_end.row
               && candidate_end.col == if_end.col
             | None => false
             }
           })
      | None => None
      }
    | _ => None
    }
  | _ => None
  };
};

/* Filter out let expressions with hole bodies to enable better selection
 * Returns filtered candidate list, or None if no filtering needed
 */
let filter_let_with_hole_body =
    (candidates: list(Id.t), terms: TermMap.t): option(list(Id.t)) => {
  /* Look for let expressions with hole bodies */
  let problematic_lets =
    candidates
    |> List.filter(candidate_id => {
         switch (get_term_by_id(candidate_id, terms)) {
         | Some(let_term)
             when is_let_expression(let_term) && let_body_is_hole(let_term) =>
           true
         | _ => false
         }
       });

  if (problematic_lets != []) {
    /* Remove problematic lets and return filtered list */
    let remaining_candidates =
      candidates |> List.filter(id => !List.mem(id, problematic_lets));

    switch (remaining_candidates) {
    | [] => None /* No alternatives, don't filter */
    | remaining => Some(remaining) /* Return filtered candidates */
    };
  } else {
    None; /* No problematic lets found, no filtering needed */
  };
};

/* ===== UNIFIED SELECTION CORE ===== */

/* Core selection function with configurable predicate.
 * This eliminates duplication between the two public selection functions.
 *
 * The predicate receives a probe_context and returns whether to probe that candidate.
 * Special case handlers always take precedence over the predicate.
 */
let select_probe_candidates_core =
    (
      row_term_ids: list(list(Id.t)),
      terms: TermMap.t,
      data: TermData.t,
      measured: Measured.t,
      predicate: probe_context => bool,
    ) => {
  row_term_ids
  |> List.mapi((row_index: int, candidates: list(Id.t)) => {
       /* 1. Apply let filtering first */
       let filtered_candidates =
         switch (filter_let_with_hole_body(candidates, terms)) {
         | Some(filtered) => filtered
         | None => candidates
         };

       /* 2. Try special case handlers on filtered candidates */
       let special_result =
         switch (
           handle_if_expression_special_case(
             filtered_candidates,
             terms,
             data,
             measured,
           )
         ) {
         | Some(id) => Some(id)
         | None =>
           handle_tuple_list_special_case(
             filtered_candidates,
             terms,
             data,
             measured,
           )
         };

       switch (special_result) {
       | Some(id) => Some(id)
       | None =>
         filtered_candidates
         |> List.mapi((candidate_index, id) => {
              let candidate = {
                id,
                row: row_index,
                col: 0,
                is_largest_on_line: candidate_index == 0,
              };
              let term = get_term_by_id(id, terms);
              let context = {
                candidate,
                term,
                hole_only: only_hole(filtered_candidates, terms),
                terms,
              };

              predicate(context) ? Some(id) : None;
            })
         |> List.find_map(Fun.id)
       };
     });
};

/* ===== PHASE 6: VARIABLE REFERENCE DETECTION WITH STATICS ===== */

/* Check if a variable ID corresponds to a pattern variable that we've already seen
 * Uses statics to look up binding sites and track which patterns have been probed
 */
let is_redundant_variable_reference =
    (
      var_id: Id.t,
      info_map: Language.Statics.Map.t,
      seen_pattern_ids: list(Id.t),
    )
    : bool => {
  /* Look up the variable in statics to find its binding site */
  switch (Language.Statics.Map.lookup(var_id, info_map)) {
  | Some(info) =>
    /* Try to get the binding site pattern ID from the variable info */
    switch (Language.Info.get_binding_site(info)) {
    | Some(binding_id) => List.mem(binding_id, seen_pattern_ids)
    | None => false
    }
  | None => false /* If we can't look it up, don't consider it redundant */
  };
};

/* Predicate: Don't probe variable references if we've already seen their pattern */
let should_not_probe_redundant_variables =
    (
      context: probe_context,
      info_map: Language.Statics.Map.t,
      seen_pattern_ids: list(Id.t),
    )
    : bool => {
  let is_candidate_variable =
    context.term
    |> Option.map(is_variable_reference)
    |> Option.value(~default=false);

  if (is_candidate_variable) {
    !
      is_redundant_variable_reference(
        context.candidate.id,
        info_map,
        seen_pattern_ids,
      );
      /* If it's a variable, check if it's redundant */
  } else {
    true; /* Non-variables are always ok */
  };
};

/* Helper: Get all pattern variable IDs from a pattern (including from tuples, etc) */
let get_pattern_variable_ids = (id: Id.t, terms: TermMap.t): list(Id.t) => {
  switch (Id.Map.find_opt(id, terms)) {
  | Some(Pat(pat)) =>
    /* Use the built-in bindings function to recursively collect all variable IDs */
    Language.Pat.bindings(pat) |> List.map((b: Language.Binding.t) => b.id)
  | _ => []
  };
};

/* Enhanced selection function with variable redundancy checking */
let select_probe_candidates_with_variable_awareness =
    (
      row_term_ids: list(list(Id.t)),
      terms: TermMap.t,
      data: TermData.t,
      measured: Measured.t,
      info_map: Language.Statics.Map.t,
    )
    : list(option(Id.t)) => {
  /* Track pattern IDs we've seen so far to detect redundant variables */
  let seen_pattern_ids = ref([]);

  /* Process rows sequentially so we can update seen_pattern_ids between rows */
  row_term_ids
  |> List.mapi((row_index: int, candidates: list(Id.t)) => {
       /* Variable-aware predicate: includes basic checks + variable redundancy */
       let variable_aware_predicate = context => {
         combine_predicates(
           [should_not_probe_holes_unless_only_holes],
           context,
         )
         && should_not_probe_function_types(context, info_map)
         && should_not_probe_redundant_container(context, data, measured)
         && should_not_probe_redundant_variables(
              context,
              info_map,
              seen_pattern_ids^,
            );
       };

       /* 1. Apply let filtering first */
       let filtered_candidates =
         switch (filter_let_with_hole_body(candidates, terms)) {
         | Some(filtered) => filtered
         | None => candidates
         };

       /* 2. Try special case handlers on filtered candidates */
       let special_result =
         switch (
           handle_if_expression_special_case(
             filtered_candidates,
             terms,
             data,
             measured,
           )
         ) {
         | Some(id) => Some(id)
         | None =>
           handle_tuple_list_special_case(
             filtered_candidates,
             terms,
             data,
             measured,
           )
         };

       let selected_probe =
         switch (special_result) {
         | Some(id) => Some(id)
         | None =>
           /* 3. Fall back to predicate-based selection on filtered candidates */
           filtered_candidates
           |> List.mapi((candidate_index, id) => {
                let candidate = {
                  id,
                  row: row_index,
                  col: 0,
                  is_largest_on_line: candidate_index == 0,
                };
                let context = {
                  candidate,
                  hole_only: only_hole(filtered_candidates, terms),
                  term: Id.Map.find_opt(id, terms),
                  terms,
                };
                variable_aware_predicate(context) ? Some(id) : None;
              })
           |> List.find_opt(Option.is_some)
           |> Option.join
         };

       /* 4. Update seen_pattern_ids if we selected a pattern with variables */
       switch (selected_probe) {
       | Some(id) =>
         let pattern_var_ids = get_pattern_variable_ids(id, terms);
         seen_pattern_ids := pattern_var_ids @ seen_pattern_ids^;
       | None => ()
       };

       selected_probe;
     });
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
  select_probe_candidates_with_variable_awareness(
    row_term_ids,
    terms,
    data,
    measured,
    info_map,
  );
};

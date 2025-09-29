open Util;
open OptUtil.Syntax;

[@deriving (show({with_path: false}), sexp, yojson)]
type data = {
  skel: Skel.t,
  sort: Sort.t,
  base_seg: Segment.t,
  root_piece: Piece.t,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = Id.Map.t(data);

let empty: t = Id.Map.empty;

let mk = (p: Piece.t, sort: Sort.t, skel: Skel.t, base_seg: Segment.t): data => {
  skel,
  sort,
  base_seg,
  root_piece: p,
};

let root_tile = (id: Id.t, data: t): option(Tile.t) =>
  switch (Id.Map.find_opt(id, data)) {
  | Some({root_piece: Tile(t), _}) => Some(t)
  | _ => None
  };

let sort = (id: Id.t, data: t): Sort.t =>
  switch (Id.Map.find_opt(id, data)) {
  | Some({sort, _}) => sort
  | None => Any
  };

let extremes_opt = (id: Id.t, data: t) =>
  /* This currently fails for singleton labelled tuples due
     to their maketerm hack, otherwise the extreme functions
     could be failwiths instead of options */
  switch (Id.Map.find_opt(id, data)) {
  | Some({skel, base_seg, _}) =>
    let (l, r) = Skel.range(skel);
    switch (List.nth(base_seg, l), List.nth(base_seg, r)) {
    | exception _ => None
    | (l, r) => Some((l, r))
    };
  | None => None
  };

let extremes_shards = (id: Id.t, data: t): option((Piece.t, Piece.t)) =>
  switch (extremes_opt(id, data)) {
  | Some((l, r)) => Some((Piece.l_shard_of(l), Piece.r_shard_of(r)))
  | None => None
  };

let root_shards = (id: Id.t, data: t): option((Piece.t, Piece.t)) =>
  switch (Id.Map.find_opt(id, data)) {
  | Some({root_piece, _}) =>
    Some((Piece.l_shard_of(root_piece), Piece.r_shard_of(root_piece)))
  | _ => None
  };

let extreme_ids = (id: Id.t, data: t): option((Id.t, Id.t)) =>
  switch (extremes_opt(id, data)) {
  | Some((l, r)) => Some((Piece.id(l), Piece.id(r)))
  | None => None
  };

let extreme_measures = (id: Id.t, data: t, measured: Measured.t) =>
  switch (extremes_opt(id, data)) {
  | Some((l, r)) =>
    switch (
      Measured.find_p(l, measured).origin,
      Measured.find_p(r, measured).last,
    ) {
    | exception _ => None
    | (l, r) => Some((l, r))
    }
  | None => None
  };

/* The segment corresponding to the `id` term */
let segment = (id: Id.t, data: t): option(Segment.t) => {
  let+ {base_seg, skel, _} = Id.Map.find_opt(id, data);
  let (l, r) = Skel.range(skel);
  ListUtil.sublist((l, r + 1), base_seg);
};

let get_term_rows =
    (id: Id.t, data: t, measured: Measured.t)
    : option((int, list(Segment.t))) => {
  let+ (start, final) = extreme_measures(id, data, measured);
  let term_rows =
    measured.piece_rows
    |> List.rev
    |> Util.ListUtil.sublist((start.row, final.row + 1))
    |> List.map(List.rev);
  (start.row, term_rows);
};

/*
 TODO: handle cases where the first term found is actually a subterm
 of another term on that line

 TODO: tuples special case? (try a 3-tuple of args, each on its own line)
 (it would be nice if last one was last arg instead of whole tuple)
 (prob want to so the sam with list literals)
 */

/* ===== REFACTORED PROBE PLACEMENT INFRASTRUCTURE ===== */

[@deriving (show({with_path: false}), sexp, yojson)]
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
let get_row_term_ids_prioritized:
  (Id.t, t, Measured.t) => option(list(list(Id.t))) =
  (id: Id.t, data: t, measured: Measured.t) => {
    let+ (start_row_idx, term_rows) = get_term_rows(id, data, measured);

    let get_final_col = (current_row: int, piece: Piece.t): option(int) =>
      switch (extreme_measures(Piece.id(piece), data, measured)) {
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

/* Simple predicate that matches original behavior: select first (largest) term */
let should_probe_largest_only: probe_candidate => bool =
  candidate => candidate.is_largest_on_line;

/* Fold-based selection function */
let select_probe_candidates:
  (list(list(Id.t)), t, Measured.t, probe_candidate => bool) =>
  list(option(Id.t)) =
  (
    row_term_ids: list(list(Id.t)),
    data: t,
    measured: Measured.t,
    should_probe: probe_candidate => bool,
  ) => {
    row_term_ids
    |> List.mapi((row_index: int, candidates: list(Id.t)) => {
         candidates
         |> List.mapi((candidate_index, id) => {
              let candidate = {
                id,
                row: row_index,
                col: 0, /* We'll populate this if needed later */
                is_largest_on_line: candidate_index == 0,
              };
              should_probe(candidate) ? Some(id) : None;
            })
         |> List.find_map(Fun.id) /* Return first Some, or None if all None */
       });
  };

/* Refactored version using new infrastructure */
let get_largest_terminal_term_ids_refactored:
  (Id.t, t, Measured.t) => option(list(option(Id.t))) =
  (id: Id.t, data: t, measured: Measured.t) => {
    let+ row_term_ids = get_row_term_ids_prioritized(id, data, measured);
    select_probe_candidates(
      row_term_ids,
      data,
      measured,
      should_probe_largest_only,
    );
  };

/* ===== PHASE 2: HOLE-AWARE PROBE PLACEMENT ===== */

/* Term analysis utilities */
let get_term_by_id: (Id.t, TermMap.t) => option(Language.Any.t) =
  (id: Id.t, terms: TermMap.t) => Id.Map.find_opt(id, terms);

let is_hole_term: Language.Any.t => bool = {
  Language.(
    fun
    | Exp({term: EmptyHole | MultiHole(_), _}) => true
    | Pat({term: EmptyHole | MultiHole(_), _}) => true
    | Typ({term: Unknown(Hole(EmptyHole | MultiHole(_))), _}) => true
    | TPat({term: EmptyHole | MultiHole(_), _}) => true
    | _ => false
  );
};

type row_analysis = {
  all_holes: bool,
  has_non_holes: bool,
  hole_count: int,
  non_hole_count: int,
};

let analyze_row_terms: (list(Id.t), TermMap.t) => row_analysis =
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

    {
      all_holes: hole_count > 0 && non_hole_count == 0,
      has_non_holes: non_hole_count > 0,
      hole_count,
      non_hole_count,
    };
  };

/* Enhanced probe context for sophisticated predicates */
type probe_context = {
  candidate: probe_candidate,
  term: option(Language.Any.t),
  row_analysis,
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
      context.row_analysis.all_holes;
    } else {
      true; /* Non-holes are always ok */
    };
  };

/* Enhanced selection function with hole awareness */
let select_probe_candidates_with_holes:
  (list(list(Id.t)), TermMap.t, t, Measured.t) => list(option(Id.t)) =
  (
    row_term_ids: list(list(Id.t)),
    terms: TermMap.t,
    _data: t,
    _measured: Measured.t,
  ) => {
    row_term_ids
    |> List.mapi((row_index: int, candidates: list(Id.t)) => {
         let row_analysis = analyze_row_terms(candidates, terms);

         candidates
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
                row_analysis,
                terms,
              };

              should_not_probe_holes_unless_only_holes(context)
                ? Some(id) : None;
            })
         |> List.find_map(Fun.id);
       });
  };

/* ===== PHASE 3: FUNCTION TYPE DETECTION (Enhanced with Statics) ===== */

/* Check if a type represents a function type using statics information
 * This is more precise than syntactic detection since it catches:
 * - Function literals: fun x -> ...
 * - Variables bound to functions: let f = fun x -> ... in f
 * - Function applications that return functions: get_fn()(x)
 * - Any other expressions that evaluate to function values
 */
let has_function_type: (Id.t, Language.Statics.Map.t) => bool =
  (id: Id.t, info_map: Language.Statics.Map.t) => {
    switch (Language.Statics.Map.lookup(id, info_map)) {
    | Some(InfoExp({ty, _})) => Language.Typ.is_arrow(ty)
    | Some(InfoPat({ty, _})) => Language.Typ.is_arrow(ty) /* Pattern variables can have function types too */
    | _ => false /* If we can't determine the type, assume it's not a function */
    };
  };

/* Predicate: Don't probe expressions with function types (we don't show function values) */
let should_not_probe_function_types:
  (probe_context, Language.Statics.Map.t) => bool =
  (context: probe_context, info_map: Language.Statics.Map.t) => {
    !has_function_type(context.candidate.id, info_map);
  };

/* Combine multiple predicates with AND logic */
let combine_predicates: (list(probe_context => bool), probe_context) => bool =
  (predicates, context) => List.for_all(pred => pred(context), predicates);

/* Enhanced selection function with holes + function types awareness */
let select_probe_candidates_enhanced:
  (list(list(Id.t)), TermMap.t, t, Measured.t, Language.Statics.Map.t) =>
  list(option(Id.t)) =
  (
    row_term_ids: list(list(Id.t)),
    terms: TermMap.t,
    _data: t,
    _measured: Measured.t,
    info_map: Language.Statics.Map.t,
  ) => {
    row_term_ids
    |> List.mapi((row_index: int, candidates: list(Id.t)) => {
         let row_analysis = analyze_row_terms(candidates, terms);

         /* Combined predicate: avoid holes AND avoid function types */
         let enhanced_predicate = context => {
           combine_predicates(
             [should_not_probe_holes_unless_only_holes],
             context,
           )
           && should_not_probe_function_types(context, info_map);
         };

         candidates
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
                row_analysis,
                terms,
              };

              enhanced_predicate(context) ? Some(id) : None;
            })
         |> List.find_map(Fun.id);
       });
  };

/* ===== PHASE 4: TUPLE/LIST SPECIAL CASE LOGIC ===== */

let is_parens_term: Language.Any.t => bool =
  Language.(
    fun
    | Exp({term: Parens(_), _}) => true
    | Pat({term: Parens(_), _}) => true
    | Typ({term: Parens(_), _}) => true
    | _ => false
  );

let is_tuple_term: Language.Any.t => bool =
  Language.(
    fun
    | Exp({term: Tuple(_), _}) => true
    | Pat({term: Tuple(_), _}) => true
    | _ => false
  );

let is_list_literal_term: Language.Any.t => bool =
  Language.(
    fun
    | Exp({term: ListLit(_), _}) => true
    | Pat({term: ListLit(_), _}) => true
    | _ => false
  );

/* Handle tuple/list special case: prefer last element over the literal when they end on same line
 * Example:
 *   (a,    // probe on 'a'
 *    b)    // probe on 'b', NOT on the tuple '(a,b)'
 */
let handle_tuple_list_special_case:
  (list(Id.t), TermMap.t, t, Measured.t) => option(Id.t) =
  (candidates: list(Id.t), terms: TermMap.t, data: t, measured: Measured.t) => {
    /* Look for pattern: [tuple_or_list_literal, ..., last_element] where both end on same line */
    switch (candidates) {
    | [literal_id, ...rest] when rest != [] =>
      let last_element_id = List.hd(List.rev(rest));

      /* Check if literal is tuple/list and last element ends at same position */
      switch (
        get_term_by_id(literal_id, terms),
        get_term_by_id(last_element_id, terms),
      ) {
      | (Some(literal_term), Some(_last_element_term)) =>
        if (is_tuple_term(literal_term) || is_list_literal_term(literal_term)) {
          /* Check if they end at the same position */
          switch (
            extreme_measures(literal_id, data, measured),
            extreme_measures(last_element_id, data, measured),
          ) {
          | (Some((_, literal_end)), Some((_, element_end)))
              when
                literal_end.row == element_end.row
                && literal_end.col == element_end.col =>
            Some(last_element_id)
          | _ => None
          };
        } else {
          None;
        }
      | _ => None
      };
    | _ => None
    };
  };

/* Handle parens special case: avoid probing closing parens after the last element
 * Example:
 *   (a,
 *    b)   // probe on 'b', NOT on the closing parens
 */
let should_not_probe_parens_after_last_element: probe_context => bool =
  context => {
    let is_candidate_parens =
      context.term
      |> Option.map(is_parens_term)
      |> Option.value(~default=false);

    /* For now, just avoid all parens - we can make this more sophisticated later */
    !is_candidate_parens;
  };

/* Enhanced selection function with special case handling */
let select_probe_candidates_with_special_cases:
  (list(list(Id.t)), TermMap.t, t, Measured.t, Language.Statics.Map.t) =>
  list(option(Id.t)) =
  (
    row_term_ids: list(list(Id.t)),
    terms: TermMap.t,
    data: t,
    measured: Measured.t,
    info_map: Language.Statics.Map.t,
  ) => {
    row_term_ids
    |> List.mapi((row_index: int, candidates: list(Id.t)) => {
         /* 1. Try special case handlers first */
         let special_result =
           handle_tuple_list_special_case(candidates, terms, data, measured);

         switch (special_result) {
         | Some(id) => Some(id)
         | None =>
           /* 2. Fall back to predicate-based selection */
           let row_analysis = analyze_row_terms(candidates, terms);

           /* Enhanced predicate: avoid holes, function types, AND parens */
           let enhanced_predicate = context => {
             combine_predicates(
               [
                 should_not_probe_holes_unless_only_holes,
                 should_not_probe_parens_after_last_element,
               ],
               context,
             )
             && should_not_probe_function_types(context, info_map);
           };

           candidates
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
                  row_analysis,
                  terms,
                };

                enhanced_predicate(context) ? Some(id) : None;
              })
           |> List.find_map(Fun.id);
         };
       });
  };

/* ===== PHASE 5: IF EXPRESSION SPECIAL CASE LOGIC ===== */

let is_if_expression: Language.Any.t => bool =
  Language.(
    fun
    | Exp({term: If(_, _, _), _}) => true
    | _ => false
  );

/* Extract the else branch from an if expression */
let get_if_else_branch: Language.Any.t => option(Language.Exp.t) =
  Language.(
    fun
    | Exp({term: If(_, _, else_branch), _}) => Some(else_branch)
    | _ => None
  );

/* Handle if expression special case: prefer trailing else branch over whole if
 * Example:
 *   if cond
 *   then branch1
 *   else branch2    // probe on 'branch2', NOT on the whole if expression
 *
 * Only applies when the if is multi-line and else branch is the trailing term
 */
let handle_if_expression_special_case:
  (list(Id.t), TermMap.t, t, Measured.t) => option(Id.t) =
  (candidates: list(Id.t), terms: TermMap.t, data: t, measured: Measured.t) => {
    /* Look for pattern where first candidate is if expression and there are other candidates */
    switch (candidates) {
    | [if_id, ...rest] when rest != [] =>
      switch (get_term_by_id(if_id, terms)) {
      | Some(if_term) when is_if_expression(if_term) =>
        /* Check if the if expression is multi-line by seeing if any candidates end later */
        switch (extreme_measures(if_id, data, measured)) {
        | Some((if_start, if_end)) =>
          /* Look for a candidate that might be the else branch ending at the same position as the if */
          rest
          |> List.find_opt(candidate_id => {
               switch (extreme_measures(candidate_id, data, measured)) {
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

/* Enhanced selection function with if expression handling */
let select_probe_candidates_with_all_special_cases:
  (list(list(Id.t)), TermMap.t, t, Measured.t, Language.Statics.Map.t) =>
  list(option(Id.t)) =
  (
    row_term_ids: list(list(Id.t)),
    terms: TermMap.t,
    data: t,
    measured: Measured.t,
    info_map: Language.Statics.Map.t,
  ) => {
    row_term_ids
    |> List.mapi((row_index: int, candidates: list(Id.t)) => {
         /* 1. Try special case handlers first (in priority order) */
         let special_result =
           switch (
             handle_if_expression_special_case(
               candidates,
               terms,
               data,
               measured,
             )
           ) {
           | Some(id) => Some(id)
           | None =>
             handle_tuple_list_special_case(candidates, terms, data, measured)
           };

         switch (special_result) {
         | Some(id) => Some(id)
         | None =>
           /* 2. Fall back to predicate-based selection */
           let row_analysis = analyze_row_terms(candidates, terms);

           /* Enhanced predicate: avoid holes, function types, AND parens */
           let enhanced_predicate = context => {
             combine_predicates(
               [
                 should_not_probe_holes_unless_only_holes,
                 should_not_probe_parens_after_last_element,
               ],
               context,
             )
             && should_not_probe_function_types(context, info_map);
           };

           candidates
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
                  row_analysis,
                  terms,
                };

                enhanced_predicate(context) ? Some(id) : None;
              })
           |> List.find_map(Fun.id);
         };
       });
  };

/* ===== PHASE 6: VARIABLE REFERENCE DETECTION WITH STATICS ===== */

let is_variable_reference: Language.Any.t => bool =
  Language.(
    fun
    | Exp({term: Var(_), _}) => true
    | Pat({term: Var(_), _}) => true
    | _ => false
  );

/* Extract variable name from a variable reference */
let get_variable_name: Language.Any.t => option(string) =
  Language.(
    fun
    | Exp({term: Var(name), _}) => Some(name)
    | Pat({term: Var(name), _}) => Some(name)
    | _ => None
  );

/* Check if a variable ID corresponds to a pattern variable that we've already seen
 * Uses statics to look up binding sites and track which patterns have been probed
 */
let is_redundant_variable_reference:
  (Id.t, Language.Statics.Map.t, list(Id.t)) => bool =
  (
    var_id: Id.t,
    info_map: Language.Statics.Map.t,
    seen_pattern_ids: list(Id.t),
  ) => {
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
let should_not_probe_redundant_variables:
  (probe_context, Language.Statics.Map.t, list(Id.t)) => bool =
  (
    context: probe_context,
    info_map: Language.Statics.Map.t,
    seen_pattern_ids: list(Id.t),
  ) => {
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

/* Enhanced selection function with variable redundancy checking */
let select_probe_candidates_with_variable_awareness:
  (list(list(Id.t)), TermMap.t, t, Measured.t, Language.Statics.Map.t) =>
  list(option(Id.t)) =
  (
    row_term_ids: list(list(Id.t)),
    terms: TermMap.t,
    data: t,
    measured: Measured.t,
    info_map: Language.Statics.Map.t,
  ) => {
    /* Track pattern IDs we've seen so far to detect redundant variables */
    let seen_pattern_ids = ref([]);

    row_term_ids
    |> List.mapi((row_index: int, candidates: list(Id.t)) => {
         /* 1. Try special case handlers first (in priority order) */
         let special_result =
           switch (
             handle_if_expression_special_case(
               candidates,
               terms,
               data,
               measured,
             )
           ) {
           | Some(id) => Some(id)
           | None =>
             handle_tuple_list_special_case(candidates, terms, data, measured)
           };

         switch (special_result) {
         | Some(id) =>
           /* If we selected something via special case, track it (patterns handled separately) */
           Some(id)
         | None =>
           /* 2. Fall back to predicate-based selection with variable awareness */
           let row_analysis = analyze_row_terms(candidates, terms);

           /* Enhanced predicate: avoid holes, function types, parens, AND redundant variables */
           let enhanced_predicate = context => {
             combine_predicates(
               [
                 should_not_probe_holes_unless_only_holes,
                 should_not_probe_parens_after_last_element,
               ],
               context,
             )
             && should_not_probe_function_types(context, info_map)
             && should_not_probe_redundant_variables(
                  context,
                  info_map,
                  seen_pattern_ids^,
                );
           };

           let selected =
             candidates
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
                    row_analysis,
                    terms,
                  };

                  enhanced_predicate(context) ? Some(id) : None;
                })
             |> List.find_map(Fun.id);

           /* Return the selected result (pattern tracking simplified for now) */
           selected;
         };
       });
  };

/* Public API with full variable awareness (requires statics) */
let get_sophisticated_probe_term_ids_with_statics:
  (Id.t, t, TermMap.t, Measured.t, Language.Statics.Map.t) =>
  option(list(option(Id.t))) =
  (
    id: Id.t,
    data: t,
    terms: TermMap.t,
    measured: Measured.t,
    info_map: Language.Statics.Map.t,
  ) => {
    let+ row_term_ids = get_row_term_ids_prioritized(id, data, measured);
    select_probe_candidates_with_variable_awareness(
      row_term_ids,
      terms,
      data,
      measured,
      info_map,
    );
  };

/* Public API fallback without statics (Phase 5 functionality) */
let get_sophisticated_probe_term_ids:
  (Id.t, t, TermMap.t, Measured.t) => option(list(option(Id.t))) =
  (id: Id.t, data: t, terms: TermMap.t, measured: Measured.t) => {
    let+ row_term_ids = get_row_term_ids_prioritized(id, data, measured);
    select_probe_candidates_with_all_special_cases(
      row_term_ids,
      terms,
      data,
      measured,
      Language.Statics.Map.empty /* No statics available, function type detection disabled */
    );
  };

/* ===== ORIGINAL FUNCTION (kept for compatibility) ===== */

let get_largest_terminal_term_ids = (id: Id.t, data: t, measured: Measured.t) => {
  let+ (start_row_idx, term_rows) = get_term_rows(id, data, measured);

  let get_final_col = (current_row: int, piece: Piece.t): option(int) =>
    /* Find the rightmost piece that is part of a term finishing on this line.
     * We definitely want a term sharing the final position of this term, but
     * not necessarily this term itself, if this term is a subterm of a term
     * with the same final position */
    switch (extreme_measures(Piece.id(piece), data, measured)) {
    | Some((_, final)) when final.row == current_row => Some(final.col)
    | _ => None
    };

  term_rows
  |> List.mapi((row_index: int, row: Segment.t) => {
       let current_row = start_row_idx + row_index;
       let* target_col =
         row |> List.rev |> List.find_map(get_final_col(current_row));
       /* Search from beginning of row to find largest terms first */
       List.find_map(
         piece =>
           switch (get_final_col(current_row, piece)) {
           | Some(col) when col == target_col => Some(Piece.id(piece))
           | _ => None
           },
         row,
       );
     });
};

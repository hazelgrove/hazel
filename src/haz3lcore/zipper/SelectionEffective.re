open Util;

type virtual_selection = {
  segment: Segment.t,
  exp: Language.Exp.t,
  container_id: Id.t,
};

type associative_candidate = {
  segment: Segment.t,
  container_id: Id.t,
  semantic_exp: option(Language.Exp.t),
};

type target =
  | Existing(Id.t)
  | Virtual(virtual_selection);

type t = {
  segment: Segment.t,
  target: option(target),
};

let current_level_segment = (z: Zipper.t): Segment.t => {
  let (left_sibs, right_sibs) = z.relatives.siblings;
  left_sibs @ z.selection.content @ right_sibs;
};

let piece_contains_any_id = (~ids: list(Id.t), piece: Piece.t): bool =>
  Segment.IDs.all_piece(piece)
  |> List.exists(piece_id => List.mem(piece_id, ids));

let contiguous_range = (~ids: list(Id.t), segment: Segment.t): Segment.t => {
  let indices =
    segment
    |> List.mapi((i, piece) =>
         piece_contains_any_id(~ids, piece) ? Some(i) : None
       )
    |> List.filter_map(Fun.id);
  switch (indices) {
  | [] => []
  | [first, ...rest] =>
    let (min_i, max_i) =
      List.fold_left(
        ((lo, hi), i) => (min(lo, i), max(hi, i)),
        (first, first),
        rest,
      );
    ListUtil.sublist((min_i, max_i + 1), segment);
  };
};

let segment_contains_all_ids = (~ids: list(Id.t), segment: Segment.t): bool => {
  let segment_ids = Segment.ids(segment);
  ids |> List.for_all(id => List.mem(id, segment_ids));
};

/* Whitespace is layout around the selected syntax, not a constraint on the
 * semantic range that an associative selection must contain. Discard it at
 * every depth while retaining comments, holes, and syntax IDs: those pieces
 * must still prevent a candidate from dropping selected source content. */
let rec selection_ids_without_whitespace = (selection: Segment.t): list(Id.t) =>
  selection
  |> List.concat_map(piece =>
       switch (piece) {
       | Piece.Secondary(w) =>
         Secondary.is_comment(w) ? [Piece.id(piece)] : []
       | Piece.Tile(t) => [
           Piece.id(piece),
           ...List.concat_map(selection_ids_without_whitespace, t.children),
         ]
       | Piece.Grout(_)
       | Piece.Projector(_) => [Piece.id(piece)]
       }
     )
  |> ListUtil.dedup;

let assoc_candidate_in_roots =
    (
      ~selected_ids: list(Id.t),
      ~snapped_ids: list(Id.t),
      ~info_map: Language.Statics.Map.t,
      ~term_data: TermData.t,
    )
    : option(associative_candidate) =>
  Language.AssocSelection.find_assoc_roots_for_ids(selected_ids, info_map)
  |> List.find_map(root_id =>
       switch (TermData.segment(root_id, term_data)) {
       | Some(root_segment) =>
         let root_range = contiguous_range(~ids=snapped_ids, root_segment);
         segment_contains_all_ids(~ids=snapped_ids, root_range)
         && segment_contains_all_ids(~ids=selected_ids, root_range)
           ? Some({
               segment: root_range,
               container_id: root_id,
               semantic_exp: None,
             })
           : None;
       | None => None
       }
     );

let exact_segment_root_id =
    (~segment: Segment.t, ~term_data: TermData.t, id: Id.t): bool =>
  switch (TermData.segment(id, term_data)) {
  | Some(root_segment) => root_segment == segment
  | None => false
  };

let candidate_with_container =
    (
      ~selected_ids: list(Id.t),
      ~segment: Segment.t,
      ~info_map: Language.Statics.Map.t,
      ~term_data: TermData.t,
    )
    : option(associative_candidate) => {
  let segment_ids = Segment.ids(segment);
  Language.AssocSelection.find_assoc_roots_for_ids(selected_ids, info_map)
  |> List.find_map(root_id =>
       switch (TermData.segment(root_id, term_data)) {
       | Some(root_segment) =>
         segment_contains_all_ids(~ids=segment_ids, root_segment)
           ? Some({
               segment,
               container_id: root_id,
               semantic_exp: None,
             })
           : None
       | None => None
       }
     );
};

let has_exact_root =
    (
      ~segment: Segment.t,
      ~info_map: Language.Statics.Map.t,
      ~measured: Measured.t,
      ~term_data: TermData.t,
    )
    : bool => {
  let ids = Segment.ids(segment) |> ListUtil.dedup;
  let assoc_root =
    Language.AssocSelection.find_assoc_roots_for_ids(ids, info_map)
    |> List.exists(exact_segment_root_id(~segment, ~term_data));
  assoc_root
  || (
    switch (TermData.get_root_id_using_ranges(segment, term_data, measured)) {
    | Some(id) => exact_segment_root_id(~segment, ~term_data, id)
    | None => false
    }
  );
};

let exp_of_segment = (segment: Segment.t): option(Language.Exp.t) =>
  switch (MakeTerm.for_projection(segment)) {
  | Some(Language.Grammar.Exp(exp)) => Some(exp)
  | Some(_)
  | None => None
  };

/* Reparenthesize can make any associative range into a subterm. For the
 * special source range that starts at an infix subtraction, accept only the
 * result whose leftmost operand is its explicitly preserved unary negative.
 * Ordinary additive selections retain the established range-based path. */
let rec begins_with_signed_additive_operand = (exp: Language.Exp.t): bool =>
  switch (exp.term) {
  | UnOp(_, _) => true
  | BinOp(_, left, _)
  | Parens(left) => begins_with_signed_additive_operand(left)
  | _ => false
  };

/* Dev's range-based selection remains authoritative unless an associative
 * operator identifies a contiguous expression slice that has no AST node of
 * its own (for example [3 + 4] in [1 + 2 + 3 + 4]). */
let virtual_selection =
    (
      ~info_map: Language.Statics.Map.t,
      ~measured: Measured.t,
      ~term_data: TermData.t,
      z: Zipper.t,
    )
    : option(virtual_selection) => {
  open OptUtil.Syntax;
  let selection = z.selection.content;
  let raw_selected_ids = selection_ids_without_whitespace(selection);
  let selected_ids =
    Language.AssocSelection.find_assoc_roots_for_ids(
      raw_selected_ids,
      info_map,
    )
    |> List.find_map(root_id =>
         switch (Language.Statics.Map.lookup(root_id, info_map)) {
         | Some(InfoExp({user_term, _})) =>
           Language.Reparenthesize.complete_signed_additive_selection_ids(
             raw_selected_ids,
             user_term,
           )
         | Some(_)
         | None => None
         }
       )
    |> Option.value(~default=raw_selected_ids);
  let snapped_ids =
    Language.AssocSelection.find_assoc_for_ids(selected_ids, info_map);
  let current_level = current_level_segment(z);
  let current_segment = contiguous_range(~ids=snapped_ids, current_level);
  let fallback_candidate =
    if (current_segment != []
        && segment_contains_all_ids(~ids=snapped_ids, current_segment)
        && segment_contains_all_ids(~ids=selected_ids, current_segment)) {
      candidate_with_container(
        ~selected_ids,
        ~segment=current_segment,
        ~info_map,
        ~term_data,
      );
    } else {
      assoc_candidate_in_roots(
        ~selected_ids,
        ~snapped_ids,
        ~info_map,
        ~term_data,
      );
    };
  /* Prefer a complete associative operation the user selected directly.
   * Ancestor-derived candidates remain the fallback for partial drags. */
  let direct_candidate =
    selected_ids
    |> List.filter_map(id =>
         Language.AssocSelection.find_assoc_root_for_id(id, info_map)
       )
    |> ListUtil.dedup
    |> List.find_map(root_id =>
         switch (TermData.segment(root_id, term_data)) {
         | Some(root_segment) =>
           let direct_snapped_ids =
             Language.AssocSelection.find_assoc_for_id(root_id, info_map);
           let root_range =
             contiguous_range(~ids=direct_snapped_ids, root_segment);
           segment_contains_all_ids(~ids=selected_ids, root_range)
             ? Some({
                 segment: root_range,
                 container_id: root_id,
                 semantic_exp: None,
               })
             : None;
         | None => None
         }
       );
  /* A suffix beginning with subtraction carries its leading sign in the
   * source selection itself.  The containing [plus] node is merely the
   * replacement container, so do not widen the selected range back across
   * the unselected prefix. */
  let direct_signed_candidate =
    Language.AssocSelection.find_assoc_roots_for_ids(selected_ids, info_map)
    |> List.find_map(root_id => {
         let signed_selected_ids =
           selected_ids @ snapped_ids |> ListUtil.dedup;
         switch (TermData.segment(root_id, term_data)) {
         | Some(root_segment)
             when
               selected_ids
               |> List.exists(id =>
                    Language.AssocSelection.is_signed_additive_suffix_for_id(
                      id,
                      info_map,
                    )
                  )
               && segment_contains_all_ids(
                    ~ids=signed_selected_ids,
                    root_segment,
                  ) =>
           switch (Language.Statics.Map.lookup(root_id, info_map)) {
           | Some(InfoExp({user_term, _})) =>
             /* Reparenthesize owns the signed-chain algebra. It accepts only a
              * complete suffix beginning at subtraction, preserves every sign,
              * and rejects whole-chain selections. Reusing it here keeps the
              * visual and semantic selection policies identical. */
             switch (
               Language.Reparenthesize.reparenthesize_selection(
                 ~selected_ids=signed_selected_ids,
                 user_term,
               )
             ) {
             | Some(result) =>
               switch (Language.Reparenthesize.selected_exp(result)) {
               | Some(semantic_exp)
                   when begins_with_signed_additive_operand(semantic_exp) =>
                 Some({
                   segment:
                     contiguous_range(~ids=signed_selected_ids, root_segment),
                   container_id: root_id,
                   semantic_exp: Some(semantic_exp),
                 })
               | Some(_) => None
               | None => None
               }
             | None => None
             }
           | Some(_) => None
           | None => None
           }
         | Some(_) => None
         | None => None
         };
       });
  let is_direct_signed_selection = direct_signed_candidate != None;
  let candidate =
    switch (direct_signed_candidate) {
    | Some(_) as candidate => candidate
    | None =>
      switch (direct_candidate) {
      | Some(_) as candidate => candidate
      | None => fallback_candidate
      }
    };
  let* {segment, container_id, semantic_exp} = candidate;
  if (!is_direct_signed_selection
      && has_exact_root(~segment, ~info_map, ~measured, ~term_data)) {
    None;
  } else {
    let+ exp =
      switch (semantic_exp) {
      | Some(exp) => Some(exp)
      | None => exp_of_segment(segment)
      };
    {
      segment,
      exp,
      container_id,
    };
  };
};

/* Keep the normal range-based behavior in one place. Associative selection
 * may refine this result, but every failed refinement returns this value
 * unchanged. */
let standard_selection =
    (~measured: Measured.t, ~term_data: TermData.t, z: Zipper.t): t => {
  let root_id =
    TermData.get_root_id_using_ranges(
      z.selection.content,
      term_data,
      measured,
    );
  {
    segment:
      root_id
      |> Option.bind(_, id => TermData.segment(id, term_data))
      |> Option.value(~default=z.selection.content),
    target: root_id |> Option.map(id => Existing(id)),
  };
};

let effective_selection =
    (
      ~info_map: Language.Statics.Map.t,
      ~measured: Measured.t,
      ~term_data: TermData.t,
      z: Zipper.t,
    )
    : t => {
  let standard = standard_selection(~measured, ~term_data, z);
  switch (virtual_selection(~info_map, ~measured, ~term_data, z)) {
  | Some(virtual_) => {
      segment: virtual_.segment,
      target: Some(Virtual(virtual_)),
    }
  | None => standard
  };
};

let expanded_segment =
    (~measured: Measured.t, ~term_data: TermData.t, z: Zipper.t): Segment.t =>
  standard_selection(~measured, ~term_data, z).segment;

let expanded_segment_with_associativity =
    (
      ~info_map: Language.Statics.Map.t,
      ~measured: Measured.t,
      ~term_data: TermData.t,
      z: Zipper.t,
    )
    : Segment.t =>
  effective_selection(~info_map, ~measured, ~term_data, z).segment;

let is_virtual = (selection: t): bool =>
  switch (selection.target) {
  | Some(Virtual(_)) => true
  | Some(Existing(_))
  | None => false
  };

let virtual_target = (selection: t): option(virtual_selection) =>
  switch (selection.target) {
  | Some(Virtual(virtual_)) => Some(virtual_)
  | Some(Existing(_))
  | None => None
  };

let reparenthesize_virtual =
    (~virtual_: virtual_selection, ~full_exp: Language.Exp.t) =>
  Language.Reparenthesize.reparenthesize_selection(
    ~selected_ids=Segment.ids(virtual_.segment),
    full_exp,
  );

let root_id = (selection: t): option(Id.t) =>
  switch (selection.target) {
  | Some(Existing(id)) => Some(id)
  | Some(Virtual({container_id, _})) => Some(container_id)
  | None => None
  };

let selected_exp =
    (~full_exp: Language.Exp.t, selection: t): option(Language.Exp.t) =>
  switch (selection.target) {
  | Some(Existing(id)) => Language.ProofHacks.find_exp_id(id, full_exp)
  | Some(Virtual({exp, _})) => Some(exp)
  | None => None
  };

type replacement_result = {
  at_exp: Language.Exp.t,
  with_exp: Language.Exp.t,
};

let replace_range =
    (~selected: Segment.t, ~replacement: Segment.t, container: Segment.t)
    : option(Segment.t) => {
  let selected_ids = Segment.ids(selected);
  let indices =
    container
    |> List.mapi((i, piece) =>
         piece_contains_any_id(~ids=selected_ids, piece) ? Some(i) : None
       )
    |> List.filter_map(Fun.id);
  switch (indices) {
  | [] => None
  | [first, ...rest] =>
    let (min_i, max_i) =
      List.fold_left(
        ((lo, hi), i) => (min(lo, i), max(hi, i)),
        (first, first),
        rest,
      );
    let matched = ListUtil.sublist((min_i, max_i + 1), container);
    matched == selected
      ? Some(
          ListUtil.sublist((0, min_i), container)
          @ replacement
          @ ListUtil.sublist((max_i + 1, List.length(container)), container),
        )
      : None;
  };
};

let replacement_for_virtual =
    (
      ~virtual_: virtual_selection,
      ~with_exp: Language.Exp.t,
      ~full_exp: Language.Exp.t,
      ~term_data: TermData.t,
    )
    : option(replacement_result) => {
  OptUtil.Syntax.(
    /* The selected source may begin at an infix sign, which is not itself a
     * parseable expression range. Prefer the structural reparenthesization that
     * created the virtual selection; it is independent of rendering layout and
     * applies the replacement to an actual subterm. Retain range replacement
     * only for older virtual selections that cannot be reparenthesized. */

    let* at_exp =
      Language.ProofHacks.find_exp_id(virtual_.container_id, full_exp);
    let range_replacement = () => {
      let* container_segment =
        TermData.segment(virtual_.container_id, term_data);
      let with_segment =
        ExpToSegment.exp_to_segment(
          ~settings=ExpToSegment.Settings.editable(~inline=true),
          with_exp,
        );
      let replacement =
        switch (with_exp.term) {
        | Atom(_)
        | Var(_)
        | BuiltinFun(_)
        | EmptyHole
        | MultiHole(_)
        | Parens(_) => with_segment
        | _ => [Segment.parenthesize(with_segment)]
        };
      let* replaced_segment =
        replace_range(
          ~selected=virtual_.segment,
          ~replacement,
          container_segment,
        );
      let+ with_exp = exp_of_segment(replaced_segment);
      {
        at_exp,
        with_exp,
      };
    };
    switch (
      Language.Reparenthesize.reparenthesize_selection(
        ~selected_ids=Segment.ids(virtual_.segment),
        at_exp,
      )
    ) {
    | Some(result)
        when
          switch (Language.Reparenthesize.selected_exp(result)) {
          | Some(selected) => Language.Exp.fast_equal(virtual_.exp, selected)
          | None => false
          } =>
      Some({
        at_exp,
        with_exp: Language.Reparenthesize.replace_selected(result, with_exp),
      })
    | Some(_)
    | None => range_replacement()
    };
  );
};

let replacement =
    (
      ~selection: t,
      ~with_exp: Language.Exp.t,
      ~full_exp: Language.Exp.t,
      ~term_data: TermData.t,
    )
    : option(replacement_result) =>
  switch (selection.target) {
  | None => None
  | Some(Existing(id)) =>
    Language.ProofHacks.find_exp_id(id, full_exp)
    |> Option.map(at_exp =>
         {
           at_exp,
           with_exp,
         }
       )
  | Some(Virtual(virtual_)) =>
    replacement_for_virtual(~virtual_, ~with_exp, ~full_exp, ~term_data)
  };

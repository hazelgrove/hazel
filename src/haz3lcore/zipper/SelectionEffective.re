open Util;

type associative_override = {
  segment: Segment.t,
  exp: Language.Exp.t,
  container_id: Id.t,
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

let exact_segment_root_id =
    (~segment: Segment.t, ~term_data: TermData.t, id: Id.t): bool =>
  switch (TermData.segment(id, term_data)) {
  | Some(root_segment) => root_segment == segment
  | None => false
  };

let assoc_root_containing_segment =
    (
      ~selected_ids: list(Id.t),
      ~segment: Segment.t,
      ~info_map: Language.Statics.Map.t,
      ~term_data: TermData.t,
    )
    : option(Id.t) => {
  let segment_ids = Segment.ids(segment);
  Language.AssocSelection.find_assoc_roots_for_ids(selected_ids, info_map)
  |> List.find_opt(root_id =>
       switch (TermData.segment(root_id, term_data)) {
       | Some(root_segment) =>
         segment_contains_all_ids(~ids=segment_ids, root_segment)
       | None => false
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

/* Dev's range-based selection remains authoritative unless an associative
 * operator identifies a contiguous expression slice that has no AST node of
 * its own (for example [3 + 4] in [1 + 2 + 3 + 4]). */
let associative_override =
    (
      ~info_map: Language.Statics.Map.t,
      ~measured: Measured.t,
      ~term_data: TermData.t,
      z: Zipper.t,
    )
    : option(associative_override) => {
  open OptUtil.Syntax;
  let selection = z.selection.content;
  let selected_ids = Segment.ids(selection) |> ListUtil.dedup;
  let snapped_ids =
    Language.AssocSelection.find_assoc_for_ids(selected_ids, info_map);
  let current_level = current_level_segment(z);
  let segment = contiguous_range(~ids=snapped_ids, current_level);
  let segment =
    if (segment != []
        && segment_contains_all_ids(~ids=snapped_ids, segment)
        && segment_contains_all_ids(~ids=selected_ids, segment)) {
      segment;
    } else {
      switch (
        Language.AssocSelection.find_assoc_root_for_ids(
          selected_ids,
          info_map,
        )
      ) {
      | Some(root_id) =>
        switch (TermData.segment(root_id, term_data)) {
        | Some(root_segment) =>
          let root_range = contiguous_range(~ids=snapped_ids, root_segment);
          segment_contains_all_ids(~ids=snapped_ids, root_range)
          && segment_contains_all_ids(~ids=selected_ids, root_range)
            ? root_range : [];
        | None => []
        }
      | None => []
      };
    };
  if (segment == []
      || has_exact_root(~segment, ~info_map, ~measured, ~term_data)) {
    None;
  } else {
    let* exp = exp_of_segment(segment);
    let+ container_id =
      assoc_root_containing_segment(
        ~selected_ids,
        ~segment,
        ~info_map,
        ~term_data,
      );
    {
      segment,
      exp,
      container_id,
    };
  };
};

let expanded_segment =
    (~measured: Measured.t, ~term_data: TermData.t, z: Zipper.t): Segment.t =>
  switch (
    TermData.get_root_id_using_ranges(
      z.selection.content,
      term_data,
      measured,
    )
  ) {
  | None => z.selection.content
  | Some(id) =>
    TermData.segment(id, term_data)
    |> Option.value(~default=z.selection.content)
  };

let expanded_segment_with_associativity =
    (
      ~info_map: Language.Statics.Map.t,
      ~measured: Measured.t,
      ~term_data: TermData.t,
      z: Zipper.t,
    )
    : Segment.t =>
  switch (associative_override(~info_map, ~measured, ~term_data, z)) {
  | Some(override) => override.segment
  | None => expanded_segment(~measured, ~term_data, z)
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
    Some(
      ListUtil.sublist((0, min_i), container)
      @ replacement
      @ ListUtil.sublist((max_i + 1, List.length(container)), container),
    );
  };
};

let replacement_for_override =
    (
      ~override: associative_override,
      ~with_exp: Language.Exp.t,
      ~full_exp: Language.Exp.t,
      ~term_data: TermData.t,
    )
    : option(replacement_result) => {
  open OptUtil.Syntax;
  let* at_exp =
    Language.ProofHacks.find_exp_id(override.container_id, full_exp);
  let* container_segment = TermData.segment(override.container_id, term_data);
  let with_segment =
    ExpToSegment.exp_to_segment(
      ~settings=ExpToSegment.Settings.editable(~inline=true),
      with_exp,
    );
  let* replaced_segment =
    replace_range(
      ~selected=override.segment,
      ~replacement=[Segment.parenthesize(with_segment)],
      container_segment,
    );
  let+ with_exp = exp_of_segment(replaced_segment);
  {
    at_exp,
    with_exp,
  };
};

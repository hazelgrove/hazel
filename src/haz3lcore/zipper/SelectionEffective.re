open Util;

type mode =
  | Raw
  | Associative
  | Expanded;

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

let ids_spanned_at_current_level =
    (~selected_ids: list(Id.t), ~current_level: Segment.t): list(Id.t) => {
  let indices =
    current_level
    |> List.mapi((i, piece) =>
         piece_contains_any_id(~ids=selected_ids, piece) ? Some(i) : None
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
    ListUtil.sublist((min_i, max_i + 1), current_level) |> Segment.ids;
  };
};

let segment_contains_all_ids = (~ids: list(Id.t), segment: Segment.t): bool => {
  let segment_ids = Segment.ids(segment);
  ids |> List.for_all(id => List.mem(id, segment_ids));
};

let piece_has_label = (label: list(string), piece: Piece.t): bool =>
  switch (piece) {
  | Tile(t) => t.label == label
  | Grout(_)
  | Secondary(_)
  | Projector(_) => false
  };
let segment_has_label = (label: list(string), segment: Segment.t): bool =>
  segment |> List.exists(piece_has_label(label));

let starts_with_label = (label: list(string), segment: Segment.t): bool =>
  switch (
    segment
    |> List.find_opt((piece: Piece.t) =>
         switch (piece) {
         | Tile(_) => true
         | _ => false
         }
       )
  ) {
  | Some(piece) => piece_has_label(label, piece)
  | None => false
  };

let segment_label_ids =
    (label: list(string), segment: Segment.t): list(Id.t) =>
  segment |> List.filter(piece_has_label(label)) |> List.map(Piece.id);

let intersects = (left: list(Id.t), right: list(Id.t)): bool =>
  left |> List.exists(id => List.mem(id, right));

let current_level_for_selected_comma =
    (~selection: Segment.t, ~current_level: Segment.t): option(Segment.t) => {
  let selected_comma_ids = segment_label_ids([","], selection);
  selected_comma_ids != []
  && intersects(selected_comma_ids, segment_label_ids([","], current_level))
    ? Some(current_level) : None;
};

let is_multi_shard_tile = (piece: Piece.t): bool =>
  switch (piece) {
  | Tile({label, _}) => List.length(label) > 1
  | _ => false
  };

let is_partial_multi_shard_tile = (piece: Piece.t): bool =>
  switch (piece) {
  | Tile({label, shards, children, _}) =>
    is_multi_shard_tile(piece)
    && (List.length(shards) < List.length(label) || children == [])
  | _ => false
  };

let is_case_rule_tile = (piece: Piece.t): bool =>
  switch (piece) {
  | Tile({label: ["|", "=>"], _}) => true
  | _ => false
  };

let rec find_piece_by_id = (id: Id.t, segment: Segment.t): option(Piece.t) =>
  segment
  |> List.find_map(piece =>
       if (Id.equal(Piece.id(piece), id)) {
         Some(piece);
       } else {
         switch (piece) {
         | Tile({children, _}) =>
           children |> List.find_map(find_piece_by_id(id))
         | _ => None
         };
       }
     );

let reassembled_tile_segment_for_id =
    (~id: Id.t, z: Zipper.t): option(Segment.t) => {
  switch (Zipper.unselect_and_zip(z) |> find_piece_by_id(id)) {
  | Some(piece) => Some([piece])
  | None => None
  };
};

let reassembled_larger_multi_shard_segment =
    (~piece: Piece.t, z: Zipper.t): option(Segment.t) =>
  switch (reassembled_tile_segment_for_id(~id=Piece.id(piece), z)) {
  | Some([reassembled_piece])
      when
        is_multi_shard_tile(reassembled_piece) && reassembled_piece != piece =>
    Some([reassembled_piece])
  | _ => None
  };

let term_segment_for_selection =
    (
      ~selection: Segment.t,
      ~info_map: Language.Statics.Map.t,
      ~measured: Measured.t,
      ~term_data: TermData.t,
      z: Zipper.t,
    )
    : option(Segment.t) => {
  let standard_selection = z =>
    Select.select_enclosing_term(term_data, measured, info_map, z);
  let standard_segment = z =>
    standard_selection(z)
    |> Option.map((z': Zipper.t) => z'.selection.content);
  let case_rule_standard_segment = z =>
    switch (standard_selection(z)) {
    | Some(z')
        when
          List.exists(is_case_rule_tile, z'.selection.content)
          && !segment_has_label(["case", "end"], z'.selection.content) =>
      switch (standard_segment(z')) {
      | Some(_) as segment => segment
      | None => Some(z'.selection.content)
      }
    | Some(z') => Some(z'.selection.content)
    | None => None
    };
  let term_data_segment = selection => {
    let selected_ids = Segment.ids(selection);
    selected_ids
    |> List.find_map(id =>
         switch (TermData.segment(id, term_data)) {
         | Some(segment)
             when segment_contains_all_ids(~ids=selected_ids, segment) =>
           Some(segment)
         | _ => None
         }
       );
  };
  switch (selection) {
  | [piece] =>
    let normalized_segment =
      reassembled_larger_multi_shard_segment(~piece, z)
      |> Option.value(~default=selection);
    let normalized_z = Zipper.replace_selection(Right, normalized_segment, z);
    switch (normalized_segment) {
    | [piece] when is_case_rule_tile(piece) => case_rule_standard_segment(z)
    | _ =>
      switch (term_data_segment(normalized_segment)) {
      | Some(_) as segment => segment
      | None => standard_segment(normalized_z)
      }
    };
  | _ => None
  };
};

type associative_result = {
  segment: Segment.t,
  root_id: option(Id.t),
};

let associative_result =
    (
      ~info_map: Language.Statics.Map.t,
      ~measured: Measured.t,
      ~term_data: TermData.t,
      z: Zipper.t,
    )
    : associative_result => {
  switch (z.selection.content) {
  | [] => {
      segment: [],
      root_id: None,
    }
  | selection =>
    let current_level = current_level_segment(z);
    switch (current_level_for_selected_comma(~selection, ~current_level)) {
    | Some(segment) => {
        segment,
        root_id: None,
      }
    | None =>
      let selected_ids = Segment.ids(selection) |> ListUtil.dedup;
      let snapped_ids =
        Language.AssocSelection.find_assoc_for_ids(selected_ids, info_map);
      switch (snapped_ids) {
      | [] => {
          segment:
            term_segment_for_selection(
              ~selection,
              ~info_map,
              ~measured,
              ~term_data,
              z,
            )
            |> Option.value(~default=selection),
          root_id: None,
        }
      | ids =>
        switch (contiguous_range(~ids, current_level)) {
        | [] => {
            segment: selection,
            root_id: None,
          }
        | segment =>
          if (segment_contains_all_ids(~ids=Segment.ids(selection), segment)) {
            {
              segment,
              root_id:
                Language.AssocSelection.find_assoc_root_for_ids(
                  selected_ids,
                  info_map,
                ),
            };
          } else {
            {
              segment: selection,
              root_id: None,
            };
          }
        }
      };
    };
  };
};

let associative_segment =
    (
      ~info_map: Language.Statics.Map.t,
      ~measured: Measured.t,
      ~term_data: TermData.t,
      z: Zipper.t,
    )
    : Segment.t =>
  associative_result(~info_map, ~measured, ~term_data, z).segment;

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
    switch (TermData.segment(id, term_data)) {
    | None => z.selection.content
    | Some(segment) => segment
    }
  };

let segment =
    (
      ~mode: mode,
      ~info_map: Language.Statics.Map.t,
      ~measured: Measured.t,
      ~term_data: TermData.t,
      z: Zipper.t,
    )
    : Segment.t =>
  switch (mode) {
  | Raw => z.selection.content
  | Associative => associative_segment(~info_map, ~measured, ~term_data, z)
  | Expanded => expanded_segment(~measured, ~term_data, z)
  };

let root_id =
    (
      ~mode: mode,
      ~info_map: Language.Statics.Map.t,
      ~measured: Measured.t,
      ~term_data: TermData.t,
      z: Zipper.t,
    )
    : option(Id.t) =>
  switch (mode) {
  | Associative =>
    let result = associative_result(~info_map, ~measured, ~term_data, z);
    switch (result.root_id) {
    | Some(_) as root_id => root_id
    | None =>
      result.segment
      |> TermData.get_root_id_using_ranges(_, term_data, measured)
    };
  | Raw
  | Expanded =>
    segment(~mode, ~info_map, ~measured, ~term_data, z)
    |> TermData.get_root_id_using_ranges(_, term_data, measured)
  };

let ids =
    (
      ~mode: mode,
      ~info_map: Language.Statics.Map.t,
      ~measured: Measured.t,
      ~term_data: TermData.t,
      z: Zipper.t,
    )
    : list(Id.t) =>
  segment(~mode, ~info_map, ~measured, ~term_data, z) |> Segment.ids;

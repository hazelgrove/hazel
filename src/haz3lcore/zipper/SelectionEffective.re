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

let binop_id = (~info_map: Language.Statics.Map.t, ids: list(Id.t)) =>
  ids
  |> List.filter(id =>
       switch (Language.Statics.Map.lookup(id, info_map)) {
       | Some(InfoExp({user_term: {term: BinOp(_, _, _), _}, _})) => true
       | _ => false
       }
     )
  |> List.find_opt(id =>
       switch (Language.Statics.Map.lookup(id, info_map)) {
       | Some(info) =>
         let ancestors = Language.Info.ancestors_of(info);
         !
           List.exists(
             other_id => other_id != id && List.mem(other_id, ancestors),
             ids,
           );
       | None => false
       }
     );

let associative_segment =
    (~info_map: Language.Statics.Map.t, ~term_data: TermData.t, z: Zipper.t)
    : Segment.t => {
  ignore(term_data);
  switch (z.selection.content) {
  | [] => []
  | selection =>
    let snapped_ids =
      selection
      |> List.map(Piece.id)
      |> List.concat_map(id =>
           Language.AssocSelection.find_assoc_for_id(id, info_map)
         );
    switch (snapped_ids) {
    | [] => selection
    | ids =>
      let current_level = current_level_segment(z);
      switch (contiguous_range(~ids, current_level)) {
      | [] => selection
      | segment => segment
      };
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
  | Associative => associative_segment(~info_map, ~term_data, z)
  | Expanded => expanded_segment(~measured, ~term_data, z)
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

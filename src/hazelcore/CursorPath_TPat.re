let of_z = (ztp: ZTPat.t): CursorPath_common.t =>
  switch (ztp) {
  | CursorP(cursor, _) => ([], cursor)
  };

let follow =
    ((steps, cursor): CursorPath_common.t, tp: TPat.t): option(ZTPat.t) =>
  switch (steps) {
  | [] => ZTPat.place_cursor(cursor, tp)
  | [_, ..._] => None
  };

let holes =
    (
      tp: TPat.t,
      rev_steps: CursorPath_common.rev_steps,
      hs: CursorPath_common.hole_list,
    )
    : CursorPath_common.hole_list =>
  switch (tp) {
  | EmptyHole(_)
  | TyVar(InVarHole(_), _) => [
      {sort: TPatHole, steps: List.rev(rev_steps), is_empty: true},
      ...hs,
    ]
  | TyVar(NotInVarHole, _) => hs
  };

let holes_z =
    (ztp: ZTPat.t, rev_steps: CursorPath_common.rev_steps)
    : CursorPath_common.zhole_list =>
  switch (ztp) {
  | CursorP(_, EmptyHole(_) | TyVar(InVarHole(_), _)) =>
    CursorPath_common.mk_zholes(
      ~hole_selected=
        Some({sort: TPatHole, steps: List.rev(rev_steps), is_empty: true}),
      (),
    )
  | CursorP(_, TyVar(NotInVarHole, _)) => CursorPath_common.no_holes
  };

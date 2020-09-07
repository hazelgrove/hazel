let rec of_z = (ztp: ZTPat.t): CursorPath_common.t => of_zoperand(ztp)
and of_zoperand = (zoperand: ZTPat.zoperand): CursorPath_common.t =>
  switch (zoperand) {
  | CursorP(cursor, _) => ([], cursor)
  };

let rec follow = (path: CursorPath_common.t, tp: TPat.t): option(ZTPat.t) =>
  follow_operand(path, tp)
and follow_operand =
    ((steps, cursor): CursorPath_common.t, operand: TPat.operand)
    : option(ZTPat.zoperand) =>
  switch (steps) {
  | [] => ZTPat.place_cursor(cursor, operand)
  | [_, ..._] => None
  };

let rec of_steps =
        (steps: CursorPath_common.steps, ~side: Side.t=Before, tp: TPat.t)
        : option(CursorPath_common.t) =>
  of_steps_operand(steps, ~side, tp)
and of_steps_operand =
    (steps: CursorPath_common.steps, ~side: Side.t, operand: TPat.operand)
    : option(CursorPath_common.t) =>
  switch (steps) {
  | [] =>
    let place_cursor =
      switch (side) {
      | Before => ZTPat.place_before_operand
      | After => ZTPat.place_after_operand
      };
    Some(of_zoperand(place_cursor(operand)));
  | [_, ..._] => None
  };

let rec holes =
        (
          tp: TPat.t,
          rev_steps: CursorPath_common.rev_steps,
          hs: CursorPath_common.hole_list,
        )
        : CursorPath_common.hole_list =>
  holes_operand(tp, rev_steps, hs)
and holes_operand =
    (
      operand: TPat.operand,
      rev_steps: CursorPath_common.rev_steps,
      hs: CursorPath_common.hole_list,
    )
    : CursorPath_common.hole_list =>
  switch (operand) {
  | EmptyHole(_)
  | TyVar(InVarHole(_), _) => [
      {sort: TPatHole, steps: List.rev(rev_steps), is_empty: true},
      ...hs,
    ]
  | TyVar(NotInVarHole, _) => hs
  };

let rec holes_z =
        (ztp: ZTPat.t, rev_steps: CursorPath_common.rev_steps)
        : CursorPath_common.zhole_list =>
  holes_zoperand(ztp, rev_steps)
and holes_zoperand =
    (zoperand: ZTPat.zoperand, rev_steps: CursorPath_common.rev_steps)
    : CursorPath_common.zhole_list =>
  switch (zoperand) {
  | CursorP(_, EmptyHole(_) | TyVar(InVarHole(_), _)) =>
    CursorPath_common.mk_zholes(
      ~hole_selected=
        Some({sort: TPatHole, steps: List.rev(rev_steps), is_empty: true}),
      (),
    )
  | CursorP(_, TyVar(NotInVarHole, _)) => CursorPath_common.no_holes
  };

let mk_hole_sort = CursorPath.mk_hole_sort;
let of_z =
  fun
  | ZTPat.CursorP(cursor, _) => ([], cursor);

let follow = ((steps, cursor): CursorPath.t, tp: TPat.t): option(ZTPat.t) =>
  switch (steps) {
  | [] => ZTPat.place_cursor(cursor, tp)
  | [_, ..._] => None
  };

let of_steps =
    (steps: CursorPath.steps, ~side: Side.t=Before, tp: TPat.t)
    : option(CursorPath.t) =>
  switch (steps) {
  | [] =>
    let place_cursor =
      switch (side) {
      | Before => ZTPat.place_before
      | After => ZTPat.place_after
      };
    Some(of_z(place_cursor(tp)));
  | [_, ..._] => None
  };

let holes =
    (tp: TPat.t, rev_steps: CursorPath.rev_steps, hs: CursorPath.hole_list)
    : CursorPath.hole_list =>
  switch (tp) {
  | EmptyHole => [
      mk_hole_sort(TPatHole(Empty), List.rev(rev_steps)),
      ...hs,
    ]
  | TyVar(Some(_), _) => [
      mk_hole_sort(TPatHole(VarErr), List.rev(rev_steps)),
      ...hs,
    ]
  | TyVar(None, _) => hs
  };

let holes_z =
    (ztp: ZTPat.t, rev_steps: CursorPath.rev_steps): CursorPath.zhole_list =>
  switch (ztp) {
  | CursorP(_, EmptyHole) =>
    CursorPath_common.mk_zholes(
      ~hole_selected=
        Some(mk_hole_sort(TPatHole(Empty), List.rev(rev_steps))),
      (),
    )
  | CursorP(_, TyVar(Some(_), _)) =>
    CursorPath_common.mk_zholes(
      ~hole_selected=
        Some(mk_hole_sort(TPatHole(VarErr), List.rev(rev_steps))),
      (),
    )
  | CursorP(_, TyVar(None, _)) => CursorPath_common.no_holes
  };

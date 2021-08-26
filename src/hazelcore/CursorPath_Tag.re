let of_z = (ztag: ZTag.t): CursorPath.t =>
  switch (ztag) {
  | CursorTag(cursor, _) => ([], cursor)
  };

let of_steps =
    (steps: CursorPath.steps, ~side: Side.t=Before, tag: UHTag.t)
    : option(CursorPath.t) =>
  switch (steps) {
  | [] =>
    let place_cursor =
      switch (side) {
      | Before => ZTag.place_before
      | After => ZTag.place_after
      };
    Some(of_z(place_cursor(tag)));
  | _ => None
  };

let follow = (path: CursorPath.t, tag: UHTag.t): option(ZTag.t) =>
  switch (path) {
  | ([], cursor) => Some(CursorTag(cursor, tag))
  | _ => None
  };

let holes =
    (tag: UHTag.t, rev_steps: CursorPath.rev_steps, hs: CursorPath.hole_list)
    : CursorPath.hole_list =>
  switch (tag) {
  | Tag(_) => hs
  | TagHole(u) =>
    let steps = List.rev(rev_steps);
    [{sort: TagHole(u), steps, ap_steps: steps}, ...hs];
  };

let holes_z =
    (ztag: ZTag.t, rev_steps: CursorPath.rev_steps): CursorPath.zhole_list =>
  switch (ztag |> ZTag.erase) {
  | Tag(_) => CursorPath.empty_zhole_list
  | TagHole(u) =>
    let steps = List.rev(rev_steps);
    {
      ...CursorPath.empty_zhole_list,
      hole_selected: Some({sort: TagHole(u), steps, ap_steps: steps}),
    };
  };

/* If follow(_, tag) = Some(ztag) then ZTag.erase(ztag) = tag. */
/* If UHExp.follow(_, e) = Some(ze) then ZExp.erase(ze) = e. */

/*
   1. If of_z(ztag) = path then follow(path, ZTag.erase(ztag)) = Some(ztag)
   2. If follow(path, tag) = Some(ztag) then of_z(ztag) = path.
 */

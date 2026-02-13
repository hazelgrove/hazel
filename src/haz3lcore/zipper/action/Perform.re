open Util;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type state = {
  zipper: Zipper.t,
  col_target: option(int),
};

let return = (error: Action.Failure.t, z: option(Zipper.t)) =>
  Result.of_option(~error, z);

let go =
    (
      ~statics: CachedStatics.t,
      ~syntax: CachedSyntax.t,
      a: Action.t,
      {zipper: z, col_target}: state,
    )
    : Action.Result.t(Zipper.t) =>
  switch (a) {
  | Introduce =>
    Select.current_term(
      syntax.term_data,
      ~defs_exclude_bodies=false,
      ~case_rules=false,
      z,
    )
    |> OptUtil.and_then(
         Introduce.introduce(Indicated.ci_of(z, statics.info_map)),
       )
    |> return(CantIntroduce)
  | Paste(String(clipboard)) =>
    Parser.to_zipper(~zipper_init=z, clipboard) |> return(CantPaste)
  | Paste(Segment(segment)) =>
    z.caret == Outer
      ? Ok(Zipper.insert_segment(z, segment))
      : Parser.to_zipper(~zipper_init=z, Printer.of_segment(segment))
        |> return(CantPaste)
  | Cut =>
    /* System clipboard handling is done in Page.view handlers */
    Destruct.go(Left, z) |> return(Cant_destruct)
  | Copy =>
    /* System clipboard handling itself is done in Page.view handlers.
     * This doesn't change state but is included here for logging purposes */
    Ok(z)
  | Reparse =>
    /* This serializes the current editor to text, resets the current
       editor, and then deserializes. It is intended as a (tactical)
       nuclear option for weird backpack states */
    Parser.to_zipper(
      ~zipper_init=Zipper.init(),
      Printer.of_zipper(~holes="", ~indent="", z),
    )
    |> return(CantReparse)
  | Buffer(a) => Buffer.go(~ci=Indicated.ci_of(z, statics.info_map), a, z)
  | Project(a) =>
    let refractor_list =
      List.map(fst, z.refractors.manuals)
      @ List.map(fst, Id.Map.to_list(z.refractors.autos.ephemerals));
    ProjectorPerform.go(
      syntax.term_data,
      a,
      z,
      syntax.projector_list,
      refractor_list,
    );
  | Move(d) =>
    Move.go(
      ~statics=statics.info_map,
      ~col_target=Option.value(col_target, ~default=0),
      ~measured=syntax.measured,
      d,
      z,
    )
    |> return(Cant_move)
  | Unselect(Some(d)) => Ok(Zipper.directional_unselect(d, z))
  | Unselect(None) => Ok(Zipper.unselect(z))
  | Select(Resize(Local(d, _))) =>
    Select.local(d, z) |> return(Cant_select)
  | Select(Resize(Vertical(d))) =>
    Select.vertical(
      ~col_target=Option.value(col_target, ~default=0),
      ~measured=syntax.measured,
      d,
      z,
    )
    |> return(Cant_select)
  | Select(Resize(Start)) => Ok(Select.to_start(z))
  | Select(Resize(End)) => Ok(Select.to_end(z))
  | Select(Resize(Line(d))) =>
    Select.to_linebreak(d, z) |> return(Cant_select)
  | Select(Resize(Point(goal))) =>
    Select.to_point(~measured=syntax.measured, ~goal, z)
    |> return(Cant_select)
  | Select(Resize(Goal(_))) => failwith("Select not implemented for goals")
  | Select(All) => Ok(Select.all(z))
  | Select(PointToPoint((p1, p2))) =>
    z
    |> Move.to_point(~measured=syntax.measured, ~goal=p1)
    |> OptUtil.and_then(z =>
         Select.to_point(~measured=syntax.measured, ~goal=p2, z)
       )
    |> return(Cant_select)
  | Select(Term(Current)) =>
    Select.current_term(
      syntax.term_data,
      ~defs_exclude_bodies=true,
      ~case_rules=true,
      z,
    )
    |> return(Cant_select)
  | Select(Smart(n)) =>
    Select.smart(syntax.term_data, statics.info_map, n, z)
    |> return(Cant_select)
  | Select(Term(Id(id, d))) =>
    switch (
      Select.term(
        ~defs_exclude_bodies=false,
        ~case_rules=false,
        syntax.term_data,
        id,
        z,
      )
    ) {
    | Some(z) => Ok(d == Right ? z : Zipper.toggle_focus(z))
    | None => Error(Cant_select)
    }
  | Select(Tile(Current)) => Select.current_tile(z) |> return(Cant_select)
  | Select(Tile(Id(id, d))) =>
    switch (Select.tile(id, z)) {
    | Some(z) => Ok(d == Right ? z : Zipper.toggle_focus(z))
    | None => Error(Cant_select)
    }
  | Select(ToggleFocus) => Ok(Zipper.toggle_focus(z))
  | Select(SetFocus(d)) => Ok(Zipper.set_focus(z, d))
  | Destruct(d) => Destruct.go(d, z) |> return(Cant_destruct)
  | Insert(char) =>
    z
    |> Insert.go(char, ~ci=Indicated.ci_of(z, statics.info_map))
    |> return(Cant_insert)
  | SyncReplace(delta_doc) => Ok(SyncReplace.sync_replace(z, delta_doc))

  | UpdateRemoteCarets =>
    /* No-op on zipper state, just triggers re-render for remote cursors */
    Ok(z)
  | Put_down => Zipper.put_down(z) |> return(Cant_put_down)
  | Probe(a) => Ok(ProbePerform.go(~statics, ~syntax, a, z))
  | Dump => Ok(Dump.to_zipper(z))
  };

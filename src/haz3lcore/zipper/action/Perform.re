open Util;
open Zipper;
open Language;

module State = {
  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type t = {
    zipper: Zipper.t,
    col_target: option(int),
  };
};

let paste_segment = (z: Zipper.t, segment: Segment.t): Zipper.t => {
  let replace_selection = (z, focus, segment): Zipper.t =>
    {
      ...z,
      selection: Selection.mk(~focus, segment),
    }
    |> Zipper.unselect
    |> Zipper.remold_regrout(Right);
  replace_selection(z, z.selection.focus, segment);
};

let go_z =
    (
      ~settings as _: Language.CoreSettings.t,
      ~statics: CachedStatics.t,
      ~syntax: CachedSyntax.t,
      a: Action.t,
      {zipper: z, col_target}: State.t,
    )
    : Action.Result.t(Zipper.t) => {
  switch (a) {
  | Introduce =>
    Select.current_term(
      syntax.term_data,
      ~defs_exclude_bodies=false,
      ~case_rules=false,
      z,
    )
    |> Option.bind(_, Introduce.introduce(statics.info_map, _))
    |> Result.of_option(~error=Action.Failure.CantIntroduce)
  | Paste(String(clipboard)) =>
    Parser.to_zipper(~zipper_init=z, clipboard)
    |> Result.of_option(~error=Action.Failure.CantPaste)
  | Paste(Segment(segment)) => Ok(paste_segment(z, segment))
  | Cut =>
    /* System clipboard handling is done in Page.view handlers */
    Destruct.go(Left, z)
    |> Result.of_option(~error=Action.Failure.Cant_destruct)
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
    |> Result.of_option(~error=Action.Failure.CantReparse)
  | Buffer(a) => Buffer.go(~info_map=statics.info_map, a, z)
  | Project(a) =>
    ProjectorPerform.go(
      Move.jump_to_id_indicated,
      Move.jump_to_side_of_id,
      Select.current_term(
        syntax.term_data,
        ~defs_exclude_bodies=false,
        ~case_rules=false,
      ),
      a,
      z,
    )
  | Move(d) =>
    Move.go(
      ~col_target=col_target |> Option.value(~default=0),
      ~measured=syntax.measured,
      d,
      z,
    )
    |> Result.of_option(~error=Action.Failure.Cant_move)
  | Jump(jump_target) =>
    (
      switch (jump_target) {
      | BindingSiteOfIndicatedVar =>
        open OptUtil.Syntax;
        let* idx = Indicated.index(z);
        let* ci = Id.Map.find_opt(idx, statics.info_map);
        let* binding_id = Language.Info.get_binding_site(ci);
        Move.jump_to_id_indicated(z, binding_id);
      | TileId(id) => Move.jump_to_id_indicated(z, id)
      }
    )
    |> Result.of_option(~error=Action.Failure.Cant_move)
  | Unselect(Some(d)) => Ok(Zipper.directional_unselect(d, z))
  | Unselect(None) => Ok(Zipper.unselect(z))
  | Select(Resize(d)) =>
    switch (
      Select.resize(
        ~col_target=col_target |> Option.value(~default=0),
        ~measured=syntax.measured,
        d,
        z,
      )
    ) {
    | None => Ok(z)
    | Some(z) => Ok(z)
    }
  | Select(All) =>
    let z = Move.do_to_extreme(Move.primary(ByToken, Left), z);
    Ok(Move.do_to_extreme(Select.primary(Right), z));
  | Select(Term(Current)) =>
    Select.current_term(
      syntax.term_data,
      ~defs_exclude_bodies=true,
      ~case_rules=true,
      z,
    )
    |> Result.of_option(~error=Action.Failure.Cant_select)
  | Select(Smart(n)) =>
    Select.smart(syntax.term_data, statics.info_map, n, z)
    |> Result.of_option(~error=Action.Failure.Cant_select)
  | Select(Term(Id(id, d))) =>
    switch (Select.term(syntax.term_data, id, z)) {
    | Some(z) =>
      let z = d == Right ? z : Zipper.toggle_focus(z);
      Ok(z);
    | None => Error(Action.Failure.Cant_select)
    }
  | Select(Tile(Current)) =>
    Select.current_tile(z)
    |> Result.of_option(~error=Action.Failure.Cant_select)
  | Select(Tile(Id(id, d))) =>
    switch (Select.tile(id, z)) {
    | Some(z) =>
      let z = d == Right ? z : Zipper.toggle_focus(z);
      Ok(z);
    | None => Error(Action.Failure.Cant_select)
    }
  | Select(ToggleFocus) => Ok(Zipper.toggle_focus(z))
  | Select(SetFocus(d)) => Ok(Zipper.set_focus(z, d))
  | Destruct(d) =>
    z
    |> Destruct.go(d)
    |> Result.of_option(~error=Action.Failure.Cant_destruct)
  | Insert(char) =>
    let id =
      switch (Indicated.index(z)) {
      | Some(id) => id
      | None => Id.invalid
      };
    let ctx =
      switch (Id.Map.find_opt(id, statics.info_map)) {
      | Some(ci) => Info.ctx_of(ci)
      | None => Ctx.empty
      };
    z
    |> Insert.go(char, ~ctx)
    |> Result.of_option(~error=Action.Failure.Cant_insert);
  | Put_down =>
    (
      switch (z.caret) {
      | Inner(_) => None
      | Outer =>
        switch (Zipper.glom_prev(z)) {
        | Some(z) => Some(z)
        | None => Zipper.put_down_regrout_remold(Left, z)
        }
      }
    )
    |> Result.of_option(~error=Action.Failure.Cant_put_down)
  | Dump => Ok(Zipper.try_to_dump_backpack(z))
  };
};

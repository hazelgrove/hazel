open Util;
open Language;

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson, eq)]
  type state = {
    zipper: Zipper.t,
    col_target: option(int),
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    // Updated
    state,
    // Calculated
    [@opaque]
    syntax: CachedSyntax.t,
  };

  let mk = zipper => {
    state: {
      zipper,
      col_target: None,
    },
    syntax:
      CachedSyntax.init(
        zipper,
        ~info_map=Id.Map.empty,
        ~dyn_map=Id.Map.empty,
      ),
  };

  type persistent = PersistentZipper.t;
  let persist = (model: t) => model.state.zipper |> PersistentZipper.persist;
  let unpersist = p => p |> PersistentZipper.unpersist |> mk;

  let trailing_hole_ctx = (ed: t, info_map: Language.Statics.Map.t) => {
    let segment = Zipper.unselect_and_zip(ed.state.zipper);
    let convex_grout = Segment.convex_grout(segment);
    // print_endline(String.concat("; ", List.map(Grout.show, convex_grout)));
    let last = Util.ListUtil.last_opt(convex_grout);
    switch (last) {
    | None => None
    | Some(grout) =>
      let id = grout.id;
      let info = Language.Statics.Map.lookup(id, info_map);
      switch (info) {
      | Some(info) => Some(Language.Info.ctx_of(info))
      | _ => None
      };
    };
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

let return = (error: Action.Failure.t, z: option(Zipper.t)) =>
  Result.of_option(~error, z);

let perform =
    (
      ~settings as _: Language.CoreSettings.t,
      ~statics: CachedStatics.t,
      ~syntax: CachedSyntax.t,
      a: Action.t,
      {zipper: z, col_target}: Model.state,
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
    |> Option.bind(_, Introduce.introduce(statics.info_map, _))
    |> return(CantIntroduce)
  | Paste(String(clipboard)) =>
    Parser.to_zipper(~zipper_init=z, clipboard) |> return(CantPaste)
  | Paste(Segment(segment)) => Ok(paste_segment(z, segment))
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
  | Buffer(a) => Buffer.go(~info_map=statics.info_map, a, z)
  | Project(a) => ProjectorPerform.go(syntax.term_data, a, z)
  | Move(d) =>
    Move.go(
      ~info_map=statics.info_map,
      ~col_target=Option.value(col_target, ~default=0),
      ~measured=syntax.measured,
      d,
      z,
    )
    |> return(Cant_move)
  | Unselect(Some(d)) => Ok(Zipper.directional_unselect(d, z))
  | Unselect(None) => Ok(Zipper.unselect(z))
  | Select(Resize(Local(d, _))) =>
    Select.primary(d, z) |> return(Cant_select)
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
    switch (Select.term(syntax.term_data, id, z)) {
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
  | Put_down => Zipper.put_down_glom(z) |> return(Cant_put_down)
  | Dump => Ok(Zipper.try_to_dump_backpack(z))
  };

module Update = {
  type t = Action.t;

  let update =
      (
        ~settings: Language.CoreSettings.t,
        a: Action.t,
        old_statics: CachedStatics.t,
        {state, syntax}: Model.t,
      )
      : Action.Result.t(Model.t) => {
    open Result.Syntax;
    let old_zipper = state.zipper;
    /* 1. Clear the autocomplete buffer if relevant. We clear the TyDi
     * (unparsed) buffer on every action except accept; for the LLM
     * (parsed) buffer, we accept resize actions to permit incremental
     * accepteance token-by-token or line-by-line */
    let clear_condition =
      (settings.assist && settings.statics && a != Buffer(Accept))
      && !(
           Selection.non_empty_parsed_buffer(state.zipper.selection)
           && (
             switch (a) {
             | Select(Resize(Local(_))) => true
             | _ => false
             }
           )
         );

    let state =
      clear_condition
        ? {
          ...state,
          zipper: Buffer.buffer_clear(state.zipper),
        }
        : state;
    let syntax =
      if (clear_condition) {
        CachedSyntax.mark_old(syntax);
      } else {
        syntax;
      };
    /* TODO(andrew): Apologize to matt for below.
       If a buffer clear happens above then we must recalculate the
       syntax cache as otherwise the measured, in particular caret_point,
       will be looking for tiles inside the buffer, for example if we try
       to click or move down to dismiss a completion.*/
    let syntax =
      if (clear_condition
          && Selection.non_empty_parsed_buffer(old_zipper.selection)) {
        CachedSyntax.calculate(
          state.zipper,
          old_statics.info_map,
          Id.Map.empty, //TODO
          syntax,
        );
      } else {
        syntax;
      };

    // 3. Record target column if moving up/down
    let col_target =
      switch (a) {
      | Move(Vertical(Up | Down))
      | Select(Resize(Vertical(Up | Down))) =>
        switch (state.col_target) {
        | Some(col) => Some(col)
        | None => Some(Zipper.Caret.point(syntax.measured, state.zipper).col)
        }
      | _ => None
      };
    let state = {
      ...state,
      col_target,
    };

    // 4. Update the zipper
    let+ zipper = perform(~settings, ~statics=old_statics, ~syntax, a, state);

    // Recombine
    Model.{
      state: {
        zipper,
        col_target,
      },
      syntax,
    };
  };

  let calculate =
      (
        ~settings: Language.CoreSettings.t,
        ~is_edited,
        new_statics: CachedStatics.t,
        dyn_map,
        {syntax, state}: Model.t,
      ) => {
    // 1. Recalculate the autocomplete buffer if necessary
    let zipper =
      if (settings.assist && settings.statics && is_edited) {
        Buffer.set_tydi_buffer(new_statics.info_map, state.zipper);
      } else {
        state.zipper;
      };
    // 2. Recalculate syntax cache
    let syntax = is_edited ? CachedSyntax.mark_old(syntax) : syntax;

    let syntax =
      CachedSyntax.calculate(zipper, new_statics.info_map, dyn_map, syntax);

    // Recombine
    Model.{
      state: {
        ...state,
        zipper,
      },
      syntax,
    };
  };
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = Model.t;

open Util;

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type mode =
    | Scratch
    | Documentation
    | Exercises
    | Config;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Scratch(ScratchMode.Model.t)
    | Documentation(ScratchMode.Model.t)
    | Exercises(ExercisesMode.Model.t)
    | Config(ScratchMode.Model.t);

  let mode_string: t => string =
    fun
    | Scratch(_) => "Scratch"
    | Documentation(_) => "Documentation"
    | Exercises(_) => "Exercises"
    | Config(_) => "Configuration";
};

module StoreMode =
  Store.F({
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = Model.mode;
    let key = Store.Mode;
    let default = (): Model.mode => Documentation;
  });

module Store = {
  let load = (~settings, ~instructor_mode) => {
    // Check if both name and share URL parameters are present
    let has_share_params =
      JsUtil.QueryParams.get_param("name") != None
      && JsUtil.QueryParams.get_param("share") != None;

    // If share parameters exist, force Scratch mode regardless of stored mode
    if (has_share_params) {
      Model.Scratch(
        ScratchMode.Store.load()
        |> ScratchMode.Store.integrate_share
        |> ScratchMode.Model.unpersist(~settings, "scratch"),
      );
    } else {
      // Otherwise, proceed with normal mode loading
      let mode = StoreMode.load();
      switch (mode) {
      | Scratch =>
        Model.Scratch(
          ScratchMode.Store.load()
          |> ScratchMode.Store.integrate_share
          |> ScratchMode.Model.unpersist(~settings, "scratch"),
        )
      | Documentation =>
        Model.Documentation(
          ScratchMode.StoreDocumentation.load()
          |> ScratchMode.Model.unpersist(~settings, "documentation"),
        )
      | Exercises =>
        Model.Exercises(
          ExercisesMode.Store.load(~settings, ~instructor_mode)
          |> ExercisesMode.Model.unpersist(~instructor_mode),
        )
      | Config =>
        Model.Config(
          ScratchMode.StoreConfig.load()
          |> ScratchMode.Model.unpersist(~settings, "configuration"),
        )
      };
    };
  };

  let save = (~instructor_mode, model: Model.t) => {
    switch (model) {
    | Model.Scratch(m) =>
      StoreMode.save(Scratch);
      ScratchMode.Store.save(ScratchMode.Model.persist(m));
    | Model.Documentation(m) =>
      StoreMode.save(Documentation);
      ScratchMode.StoreDocumentation.save(ScratchMode.Model.persist(m));
    | Model.Exercises(m) =>
      StoreMode.save(Exercises);
      ExercisesMode.Store.save(~instructor_mode, m);
    | Model.Config(m) =>
      StoreMode.save(Config);
      ScratchMode.StoreConfig.save(ScratchMode.Model.persist(m));
    };
  };
};

module Update = {
  open Updated;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | SwitchMode(Model.mode)
    // Scratch & Documentation
    | Scratch(ScratchMode.Update.t)
    // Exercises
    | Exercises(ExercisesMode.Update.t);

  let can_undo = (action: t) => {
    switch (action) {
    | SwitchMode(_) => true
    | Scratch(action) => ScratchMode.Update.can_undo(action)
    | Exercises(action) => ExercisesMode.Update.can_undo(action)
    };
  };

  let update =
      (
        ~globals: Globals.t,
        ~schedule_action: t => unit,
        ~send_assistant_insertion_info: CodeEditable.Model.t => unit,
        action: t,
        model: Model.t,
      ) => {
    switch (action, model) {
    | (Scratch(action), Scratch(m)) =>
      let* scratch =
        ScratchMode.Update.update(
          ~schedule_action=a => schedule_action(Scratch(a)),
          ~send_assistant_insertion_info,
          ~is_documentation=false,
          ~settings=globals.settings,
          action,
          m,
        );
      Model.Scratch(scratch);
    | (Scratch(action), Config(m)) =>
      let* scratch =
        ScratchMode.Update.update(
          ~settings=globals.settings,
          ~send_assistant_insertion_info,
          ~schedule_action=a => schedule_action(Scratch(a)),
          ~is_documentation=true,
          action,
          m,
        );
      Model.Config(scratch);
    | (Scratch(action), Documentation(m)) =>
      let* scratch =
        ScratchMode.Update.update(
          ~settings=globals.settings,
          ~schedule_action=a => schedule_action(Scratch(a)),
          ~send_assistant_insertion_info,
          ~is_documentation=true,
          action,
          m,
        );
      Model.Documentation(scratch);
    | (Exercises(action), Exercises(m)) =>
      let* exercises =
        ExercisesMode.Update.update(
          ~globals,
          ~schedule_action=a => schedule_action(Exercises(a)),
          action,
          m,
        );
      Model.Exercises(exercises);
    | (Scratch(_), Exercises(_))
    | (Exercises(_), Scratch(_))
    | (Exercises(_), Config(_))
    | (Exercises(_), Documentation(_)) => model |> return_quiet
    | (SwitchMode(Scratch), Scratch(_))
    | (SwitchMode(Documentation), Documentation(_))
    | (SwitchMode(Config), Config(_))
    | (SwitchMode(Exercises), Exercises(_)) => model |> return_quiet
    | (SwitchMode(Scratch), _) =>
      Model.Scratch(
        ScratchMode.Store.load()
        |> ScratchMode.Model.unpersist(
             ~settings=globals.settings.core,
             "scratch",
           ),
      )
      |> return
    | (SwitchMode(Documentation), _) =>
      Model.Documentation(
        ScratchMode.StoreDocumentation.load()
        |> ScratchMode.Model.unpersist(
             ~settings=globals.settings.core,
             "documentation",
           ),
      )
      |> return
    | (SwitchMode(Config), _) =>
      Model.Config(
        ScratchMode.StoreConfig.load()
        |> ScratchMode.Model.unpersist(
             ~settings=globals.settings.core,
             "configuration",
           ),
      )
      |> return
    | (SwitchMode(Exercises), _) =>
      Model.Exercises(
        ExercisesMode.Store.load(
          ~settings=globals.settings.core,
          ~instructor_mode=globals.settings.instructor_mode,
        )
        |> ExercisesMode.Model.unpersist(
             ~instructor_mode=globals.settings.instructor_mode,
           ),
      )
      |> return
    };
  };

  let calculate = (~settings, ~is_edited, ~schedule_action, model) => {
    switch (model) {
    | Model.Scratch(m) =>
      Model.Scratch(
        ScratchMode.Update.calculate(
          ~schedule_action=a => schedule_action(Scratch(a)),
          ~settings,
          ~is_edited,
          m,
        ),
      )
    | Model.Documentation(m) =>
      Model.Documentation(
        ScratchMode.Update.calculate(
          ~schedule_action=a => schedule_action(Scratch(a)),
          ~settings,
          ~is_edited,
          m,
        ),
      )
    | Model.Config(m) =>
      Model.Config(
        ScratchMode.Update.calculate(
          ~schedule_action=a => schedule_action(Scratch(a)),
          ~settings,
          ~is_edited,
          m,
        ),
      )
    | Model.Exercises(m) =>
      Model.Exercises(
        ExercisesMode.Update.calculate(
          ~schedule_action=a => schedule_action(Exercises(a)),
          ~settings,
          ~is_edited,
          m,
        ),
      )
    };
  };
};

module Selection = {
  open Cursor;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Scratch(ScratchMode.Selection.t)
    | Exercises(ExerciseMode.Selection.t);

  let get_cursor_info = (~selection: t, editors: Model.t): cursor(Update.t) => {
    switch (selection, editors) {
    | (Scratch(selection), Scratch(m)) =>
      let+ ci = ScratchMode.Selection.get_cursor_info(~selection, m);
      Update.Scratch(ci);
    | (Scratch(selection), Documentation(m)) =>
      let+ ci = ScratchMode.Selection.get_cursor_info(~selection, m);
      Update.Scratch(ci);
    | (Scratch(selection), Config(m)) =>
      let+ ci = ScratchMode.Selection.get_cursor_info(~selection, m);
      Update.Scratch(ci);

    | (Exercises(selection), Exercises(m)) =>
      let+ ci = ExercisesMode.Selection.get_cursor_info(~selection, m);
      Update.Exercises(ci);
    | (Scratch(_), Exercises(_))
    | (Exercises(_), Scratch(_))
    | (Exercises(_), Documentation(_))
    | (Exercises(_), Config(_)) => empty
    };
  };

  let handle_key_event =
      (~selection: option(t), ~event, editors: Model.t): option(Update.t) => {
    switch (selection, editors) {
    | (Some(Scratch(selection)), Scratch(m)) =>
      ScratchMode.Selection.handle_key_event(~selection, ~event, m)
      |> Option.map(x => Update.Scratch(x))
    | (Some(Scratch(selection)), Documentation(m)) =>
      ScratchMode.Selection.handle_key_event(~selection, ~event, m)
      |> Option.map(x => Update.Scratch(x))
    | (Some(Scratch(selection)), Config(m)) =>
      ScratchMode.Selection.handle_key_event(~selection, ~event, m)
      |> Option.map(x => Update.Scratch(x))

    | (Some(Exercises(selection)), Exercises(m)) =>
      ExercisesMode.Selection.handle_key_event(~selection, ~event, m)
      |> Option.map(x => Update.Exercises(x))
    | (Some(Scratch(_)), Exercises(_))
    | (Some(Exercises(_)), Scratch(_))
    | (Some(Exercises(_)), Documentation(_))
    | (Some(Exercises(_)), Config(_))
    | (None, _) => None
    };
  };

  let jump_to_tile =
      (~settings, tile, model: Model.t): option((Update.t, t)) =>
    switch (model) {
    | Scratch(m) =>
      ScratchMode.Selection.jump_to_tile(tile, m)
      |> Option.map(((x, y)) => (Update.Scratch(x), Scratch(y)))
    | Config(m)
    | Documentation(m) =>
      ScratchMode.Selection.jump_to_tile(tile, m)
      |> Option.map(((x, y)) => (Update.Scratch(x), Scratch(y)))
    | Exercises(m) =>
      ExercisesMode.Selection.jump_to_tile(~settings, tile, m)
      |> Option.map(((x, y)) => (Update.Exercises(x), Exercises(y)))
    };

  let default_selection =
    fun
    | Model.Scratch(_) => Scratch(Cell(MainEditor))
    | Model.Documentation(_) => Scratch(Cell(MainEditor))
    | Model.Config(_) => Scratch(Cell(MainEditor))
    | Model.Exercises(_) => Exercises(Cell(Exercise.Prelude, MainEditor));
};

module View = {
  open Virtual_dom.Vdom;
  open Node;

  type signal =
    | MakeActive(Selection.t);

  let view =
      (
        ~globals,
        ~selection: option(Selection.t),
        ~signal,
        ~inject,
        ~inject_explainthis: ExplainThisUpdate.update => 'b,
        editors: Model.t,
      ) =>
    switch (editors) {
    | Scratch(m) =>
      ScratchMode.View.view(
        ~signal=
          fun
          | MakeActive(s) => signal(MakeActive(Scratch(s))),
        ~globals,
        ~selected=
          switch (selection) {
          | Some(Scratch(s)) => Some(s)
          | _ => None
          },
        ~inject=a => Update.Scratch(a) |> inject,
        m,
      )
    | Documentation(m) =>
      ScratchMode.View.view(
        ~signal=
          fun
          | MakeActive(s) => signal(MakeActive(Scratch(s))),
        ~globals,
        ~selected=
          switch (selection) {
          | Some(Scratch(s)) => Some(s)
          | _ => None
          },
        ~inject=a => Update.Scratch(a) |> inject,
        m,
      )
    | Config(m) =>
      ScratchMode.View.view(
        ~signal=
          fun
          | MakeActive(s) => signal(MakeActive(Scratch(s))),
        ~globals,
        ~selected=
          switch (selection) {
          | Some(Scratch(s)) => Some(s)
          | _ => None
          },
        ~inject=a => Update.Scratch(a) |> inject,
        m,
      )
    | Exercises(m) =>
      ExercisesMode.View.view(
        ~signal=
          fun
          | MakeActive(s) => signal(MakeActive(Exercises(s))),
        ~globals,
        ~selection=
          switch (selection) {
          | Some(Exercises(s)) => Some(s)
          | _ => None
          },
        ~inject=a => Update.Exercises(a) |> inject,
        ~inject_explainthis: ExplainThisUpdate.update => 'b,
        m,
      )
    };

  let file_menu = (~globals, ~inject, editors: Model.t) =>
    switch (editors) {
    | Scratch(s)
    | Config(s)
    | Documentation(s) =>
      ScratchMode.View.file_menu(
        ~globals,
        ~inject=x => inject(Update.Scratch(x)),
        s,
      )
    | Exercises(e) =>
      ExercisesMode.View.file_menu(
        ~globals,
        ~inject=x => inject(Update.Exercises(x)),
        e,
      )
    };

  let top_bar =
      (~globals: Globals.t, ~inject: Update.t => 'a, ~editors: Model.t) => {
    let mode_menu = {
      div(
        ~attrs=[Attr.class_("mode-name"), Attr.title("Toggle Mode")],
        [
          select(
            ~attrs=[
              Attr.on_change(_ =>
                fun
                | "Scratch" => inject(Update.SwitchMode(Scratch))
                | "Documentation" => inject(Update.SwitchMode(Documentation))
                | "Exercises" => inject(Update.SwitchMode(Exercises))
                | "Configuration" => inject(Update.SwitchMode(Config))
                | _ => failwith("Invalid mode")
              ),
            ],
            List.map(
              EditorModeView.option_view(
                switch (editors) {
                | Scratch(_) => "Scratch"
                | Documentation(_) => "Documentation"
                | Exercises(_) => "Exercises"
                | Config(_) => "Configuration"
                },
              ),
              ["Scratch", "Documentation", "Configuration", "Exercises"],
            ),
          ),
        ],
      );
    };
    let contents =
      switch (editors) {
      | Scratch(m) =>
        ScratchMode.View.top_bar(
          ~globals,
          ~inject=a => Update.Scratch(a) |> inject,
          m,
        )
      | Documentation(m) =>
        ScratchMode.View.top_bar(
          ~globals,
          ~inject=a => Update.Scratch(a) |> inject,
          m,
        )
      | Config(m) =>
        ScratchMode.View.top_bar(
          ~globals,
          ~inject=a => Update.Scratch(a) |> inject,
          m,
        )
      | Exercises(m) =>
        ExercisesMode.View.top_bar(
          ~globals,
          ~inject=a => Update.Exercises(a) |> inject,
          m,
        )
      };
    div(
      ~attrs=[Attr.id("editor-mode")],
      [text("/"), mode_menu, text("/")] @ contents,
    );
  };
};

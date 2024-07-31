module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type mode =
    | Scratch
    | Documentation
    | Exercises;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Scratch(ScratchMode.Model.t)
    | Documentation(ScratchMode.Model.t)
    | Exercises(ExercisesMode.Model.t);

  let mode_string: t => string =
    fun
    | Scratch(_) => "Scratch"
    | Documentation(_) => "Documentation"
    | Exercises(_) => "Exercises";
};

module StoreMode =
  Store.F({
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = Model.mode;
    let key = Store.Mode;
    let default = (): Model.mode => Documentation;
  });

module Store = {
  let load = (~instructor_mode) => {
    let mode = StoreMode.load();
    switch (mode) {
    | Scratch =>
      Model.Scratch(ScratchMode.Store.load() |> ScratchMode.Model.unpersist)
    | Documentation =>
      Model.Documentation(
        ScratchMode.StoreDocumentation.load()
        |> ScratchMode.Model.unpersist_documentation,
      )
    | Exercises =>
      Model.Exercises(
        ExercisesMode.Store.load(~instructor_mode)
        |> ExercisesMode.Model.unpersist(~instructor_mode),
      )
    };
  };

  let save = (~instructor_mode, model: Model.t) => {
    switch (model) {
    | Model.Scratch(m) =>
      StoreMode.save(Scratch);
      ScratchMode.Store.save(ScratchMode.Model.persist(m));
    | Model.Documentation(m) =>
      StoreMode.save(Documentation);
      ScratchMode.StoreDocumentation.save(
        ScratchMode.Model.persist_documentation(m),
      );
    | Model.Exercises(m) =>
      StoreMode.save(Exercises);
      ExercisesMode.Store.save(~instructor_mode, m);
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

  let update = (~schedule_action, ~settings, action, model: Model.t) => {
    switch (action, model) {
    | (Scratch(action), Scratch(m)) =>
      let* scratch =
        ScratchMode.Update.update(
          ~schedule_action=a => schedule_action(Scratch(a)),
          ~is_documentation=false,
          ~settings,
          action,
          m,
        );
      Model.Scratch(scratch);
    | (Scratch(action), Documentation(m)) =>
      let* scratch =
        ScratchMode.Update.update(
          ~schedule_action=a => schedule_action(Scratch(a)),
          ~is_documentation=true,
          ~settings,
          action,
          m,
        );
      Model.Documentation(scratch);
    | (Exercises(action), Exercises(m)) =>
      let* exercises =
        ExercisesMode.Update.update(
          ~schedule_action=a => schedule_action(Exercises(a)),
          ~settings,
          action,
          m,
        );
      Model.Exercises(exercises);
    | (Scratch(_), Exercises(_))
    | (Exercises(_), Scratch(_))
    | (Exercises(_), Documentation(_)) => model |> return_quiet
    | (SwitchMode(Scratch), Scratch(_))
    | (SwitchMode(Documentation), Documentation(_))
    | (SwitchMode(Exercises), Exercises(_)) => model |> return_quiet
    | (SwitchMode(Scratch), _) =>
      Model.Scratch(ScratchMode.Store.load() |> ScratchMode.Model.unpersist)
      |> return
    | (SwitchMode(Documentation), _) =>
      Model.Documentation(
        ScratchMode.StoreDocumentation.load()
        |> ScratchMode.Model.unpersist_documentation,
      )
      |> return
    | (SwitchMode(Exercises), _) =>
      Model.Exercises(
        ExercisesMode.Store.load(~instructor_mode=settings.instructor_mode)
        |> ExercisesMode.Model.unpersist(
             ~instructor_mode=settings.instructor_mode,
           ),
      )
      |> return
    };
  };

  let calculate = (~settings, ~schedule_action, model) => {
    switch (model) {
    | Model.Scratch(m) =>
      Model.Scratch(
        ScratchMode.Update.calculate(
          ~schedule_action=a => schedule_action(Scratch(a)),
          ~settings,
          m,
        ),
      )
    | Model.Documentation(m) =>
      Model.Documentation(
        ScratchMode.Update.calculate(
          ~schedule_action=a => schedule_action(Scratch(a)),
          ~settings,
          m,
        ),
      )
    | Model.Exercises(m) =>
      Model.Exercises(
        ExercisesMode.Update.calculate(
          ~schedule_action=a => schedule_action(Exercises(a)),
          ~settings,
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
    | (Exercises(selection), Exercises(m)) =>
      let+ ci = ExercisesMode.Selection.get_cursor_info(~selection, m);
      Update.Exercises(ci);
    | (Scratch(_), Exercises(_))
    | (Exercises(_), Scratch(_))
    | (Exercises(_), Documentation(_)) => empty
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
    | (Some(Exercises(selection)), Exercises(m)) =>
      ExercisesMode.Selection.handle_key_event(~selection, ~event, m)
      |> Option.map(x => Update.Exercises(x))
    | (Some(Scratch(_)), Exercises(_))
    | (Some(Exercises(_)), Scratch(_))
    | (Some(Exercises(_)), Documentation(_))
    | (None, _) => None
    };
  };

  let jump_to_tile =
      (~settings, tile, model: Model.t): option((Update.t, t)) =>
    switch (model) {
    | Scratch(m) =>
      ScratchMode.Selection.jump_to_tile(tile, m)
      |> Option.map(((x, y)) => (Update.Scratch(x), Scratch(y)))
    | Documentation(m) =>
      ScratchMode.Selection.jump_to_tile(tile, m)
      |> Option.map(((x, y)) => (Update.Scratch(x), Scratch(y)))
    | Exercises(m) =>
      ExercisesMode.Selection.jump_to_tile(~settings, tile, m)
      |> Option.map(((x, y)) => (Update.Exercises(x), Exercises(y)))
    };

  let default_selection =
    fun
    | Model.Scratch(_) => Scratch(MainEditor)
    | Model.Documentation(_) => Scratch(MainEditor)
    | Model.Exercises(_) => Exercises((Exercise.Prelude, MainEditor));
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
        m,
      )
    };

  let export_menu = (~globals, editors: Model.t) =>
    switch (editors) {
    | Scratch(s) => ScratchMode.View.export_menu(s)
    | Documentation(s) => ScratchMode.View.export_menu(s)
    | Exercises(e) => ExercisesMode.View.export_menu(~globals, e)
    };

  let import_menu = (~globals, ~inject, editors: Model.t) =>
    switch (editors) {
    | Scratch(_) =>
      ScratchMode.View.import_menu(~inject=a => Update.Scratch(a) |> inject)
    | Documentation(_) =>
      ScratchMode.View.import_menu(~inject=a => Update.Scratch(a) |> inject)
    | Exercises(_) =>
      ExercisesMode.View.import_menu(~globals, ~inject=a =>
        Update.Exercises(a) |> inject
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
                | _ => failwith("Invalid mode")
              ),
            ],
            List.map(
              SlideSelect.option_view(
                switch (editors) {
                | Scratch(_) => "Scratch"
                | Documentation(_) => "Documentation"
                | Exercises(_) => "Exercises"
                },
              ),
              ["Scratch", "Documentation", "Exercises"],
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
          ~named_slides=false,
          ~inject=a => Update.Scratch(a) |> inject,
          m,
        )
      | Documentation(m) =>
        ScratchMode.View.top_bar(
          ~globals,
          ~named_slides=true,
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
    div(~attrs=[Attr.id("editor-mode")], [mode_menu] @ contents);
  };
};

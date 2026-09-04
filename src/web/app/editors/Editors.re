open Util_web;

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type mode =
    | Scratch
    | Documentation
    | Tutorial
    | Exercises;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Scratch(ScratchMode.Model.t)
    | Documentation(ScratchMode.Model.t)
    | Tutorial(TutorialsMode.Model.t)
    | Exercises(ExercisesMode.Model.t);

  let mode_string: t => string =
    fun
    | Scratch(_) => "Scratch"
    | Documentation(_) => "Documentation"
    | Tutorial(_) => "Tutorial"
    | Exercises(_) => "Exercises";

  /* Auxiliary classes on the main div, so CSS can target derivation-kind
     scratchpads inside the unified Scratch/Documentation modes. */
  let extra_main_classes = (model: t): list(string) => {
    let scratchpad_kind_class = (m: ScratchMode.Model.t) => {
      let current = List.nth(m.scratchpads, m.current);
      switch (current.kind) {
      | Code(_) => []
      | Drv(_) => ["Derivations"]
      };
    };
    switch (model) {
    | Scratch(m)
    | Documentation(m) => scratchpad_kind_class(m)
    | Tutorial(_)
    | Exercises(_) => []
    };
  };
};

/* Legacy-friendly wrapper for the Store.Mode key. Old persisted values
   may read as "Derivations"; we accept that during deserialization and
   coerce to Scratch. */
module StoreMode = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Model.mode;
  let key_string = Store.key_to_string(Store.Mode);
  let default = (): Model.mode => Scratch;

  let serialize = (data: t) => data |> sexp_of_t |> Sexplib.Sexp.to_string;

  let deserialize = (data: string, default: t) =>
    switch (Sexplib.Sexp.of_string(data)) {
    | sexp =>
      switch (t_of_sexp(sexp)) {
      | m => m
      | exception _ =>
        /* Legacy: was "Derivations" or otherwise unparseable; fall back. */
        switch (sexp) {
        | Sexplib.Sexp.Atom("Derivations") => Model.Scratch
        | _ =>
          print_endline("Could not deserialize " ++ key_string ++ ".");
          default;
        }
      }
    | exception _ =>
      print_endline("Could not deserialize " ++ key_string ++ ".");
      default;
    };

  let save = (data: t): unit =>
    HazelDB.kv_save(key_string, serialize(data));

  let load = (): t =>
    switch (HazelDB.kv_get(key_string)) {
    | Some(data) => deserialize(data, default())
    | None =>
      switch (Store.legacy_get(key_string)) {
      | None => default()
      | Some(data) => deserialize(data, default())
      }
    };
};

module Store = {
  let scratch_defaults = () => {
    let (current, slides) = Lazy.force(Init.startup).scratch;
    (current, List.map(fst, slides));
  };

  let doc_defaults = () => {
    let (current, slides) = Lazy.force(Init.startup).documentation;
    (current, List.map(fst, slides) @ Init.documentation_drv_slide_names());
  };

  let load_scratch = (~settings) => {
    let (default_current, default_names) = scratch_defaults();
    ScratchMode.Persist.load_all(
      "scratch",
      ~settings,
      ~default_names,
      ~default_current,
    )
    |> ScratchMode.integrate_share(~settings);
  };

  let load_documentation = (~settings) => {
    let (default_current, default_names) = doc_defaults();
    ScratchMode.Persist.load_all(
      "doc",
      ~settings,
      ~default_names,
      ~default_current,
    );
  };

  let load = (~settings, ~instructor_mode) => {
    let has_share_params =
      JsUtil.QueryParams.get_param("name") != None
      && JsUtil.QueryParams.get_param("share") != None;

    if (has_share_params) {
      Model.Scratch(load_scratch(~settings));
    } else {
      let mode = StoreMode.load();
      switch (mode) {
      | Scratch => Model.Scratch(load_scratch(~settings))
      | Documentation => Model.Documentation(load_documentation(~settings))
      | Tutorial =>
        Model.Tutorial(
          TutorialsMode.Store.load(~settings, ~instructor_mode)
          |> TutorialsMode.Model.unpersist(~settings, ~instructor_mode),
        )
      | Exercises =>
        Model.Exercises(
          ExercisesMode.Store.load(~settings, ~instructor_mode)
          |> ExercisesMode.Model.unpersist(~settings, ~instructor_mode),
        )
      };
    };
  };

  let save = (~instructor_mode, model: Model.t) => {
    switch (model) {
    | Model.Scratch(m) =>
      StoreMode.save(Scratch);
      ScratchMode.Persist.save_current("scratch", m);
    | Model.Documentation(m) =>
      StoreMode.save(Documentation);
      ScratchMode.Persist.save_current("doc", m);
    | Model.Tutorial(m) =>
      StoreMode.save(Tutorial);
      TutorialsMode.Store.save(~instructor_mode, m);
    | Model.Exercises(m) =>
      StoreMode.save(Exercises);
      ExercisesMode.Store.save(~instructor_mode, m);
    };
  };

  let reset = (~settings, ~instructor_mode) => {
    StoreMode.save(Tutorial);
    HazelDB.kv_clear();
    let _ = TutorialsMode.Store.reset(~settings, ~instructor_mode);
    let _ = ExercisesMode.Store.reset(~settings, ~instructor_mode);
    load(~settings, ~instructor_mode);
  };
};

module Update = {
  open Updated;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | SwitchMode(Model.mode)
    // Scratch & Documentation
    | Scratch(ScratchMode.Update.t)
    | Tutorial(TutorialsMode.Update.t)
    // Exercises
    | Exercises(ExercisesMode.Update.t);

  let update =
      (
        ~globals: Globals.t,
        ~schedule_action: t => unit,
        action: t,
        model: Model.t,
      ) => {
    switch (action, model) {
    | (Scratch(action), Scratch(m)) =>
      let* scratch =
        ScratchMode.Update.update(
          ~schedule_action=a => schedule_action(Scratch(a)),
          ~is_documentation=false,
          ~settings=globals.settings,
          action,
          m,
        );
      Model.Scratch(scratch);
    | (Scratch(action), Documentation(m)) =>
      let* scratch =
        ScratchMode.Update.update(
          ~settings=globals.settings,
          ~schedule_action=a => schedule_action(Scratch(a)),
          ~is_documentation=true,
          action,
          m,
        );
      Model.Documentation(scratch);
    | (Tutorial(action), Tutorial(m)) =>
      let* exercises =
        TutorialsMode.Update.update(
          ~globals,
          ~schedule_action=a => schedule_action(Tutorial(a)),
          action,
          m,
        );
      Model.Tutorial(exercises);
    | (Exercises(action), Exercises(m)) =>
      let* m' =
        ExercisesMode.Update.update(
          ~globals,
          ~schedule_action=a => schedule_action(Exercises(a)),
          action,
          m,
        );
      Model.Exercises(m');
    | (Tutorial(_), Exercises(_))
    | (Tutorial(_), Scratch(_))
    | (Tutorial(_), Documentation(_))
    | (Scratch(_), Exercises(_))
    | (Scratch(_), Tutorial(_))
    | (Exercises(_), Scratch(_))
    | (Exercises(_), Tutorial(_))
    | (Exercises(_), Documentation(_)) => model |> raise_invalid_action
    | (SwitchMode(Scratch), Scratch(_))
    | (SwitchMode(Documentation), Documentation(_))
    | (SwitchMode(Exercises), Exercises(_)) => model |> return_quiet
    | (SwitchMode(Scratch), _) =>
      Model.Scratch(Store.load_scratch(~settings=globals.settings.core))
      |> return
    | (SwitchMode(Documentation), _) =>
      Model.Documentation(
        Store.load_documentation(~settings=globals.settings.core),
      )
      |> return
    | (SwitchMode(Tutorial), Tutorial(_)) => model |> raise_invalid_action
    | (SwitchMode(Tutorial), _) =>
      Model.Tutorial(
        TutorialsMode.Store.load(
          ~settings=globals.settings.core,
          ~instructor_mode=globals.settings.instructor_mode,
        )
        |> TutorialsMode.Model.unpersist(
             ~settings=globals.settings.core,
             ~instructor_mode=globals.settings.instructor_mode,
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
             ~settings=globals.settings,
             ~instructor_mode=globals.settings.instructor_mode,
           ),
      )
      |> return
    };
  };

  let calculate =
      (~settings, ~autoprobe_mode, ~is_edited, ~schedule_action, model) => {
    switch (model) {
    | Model.Scratch(m) =>
      Model.Scratch(
        ScratchMode.Update.calculate(
          ~schedule_action=a => schedule_action(Scratch(a)),
          ~settings,
          ~autoprobe_mode,
          ~is_edited,
          m,
        ),
      )
    | Model.Documentation(m) =>
      Model.Documentation(
        ScratchMode.Update.calculate(
          ~schedule_action=a => schedule_action(Scratch(a)),
          ~settings,
          ~autoprobe_mode,
          ~is_edited,
          m,
        ),
      )
    | Model.Tutorial(m) =>
      Model.Tutorial(
        TutorialsMode.Update.calculate(
          ~schedule_action=a => schedule_action(Tutorial(a)),
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
    | Exercises(ExercisesMode.Selection.t)
    | Tutorial(TutorialMode.Selection.t)
    | Assistant;
  /* Assistant = user has focus in the sidebar (e.g. agent panel text box) */

  let get_cursor_info =
      (
        ~inject: Update.t => Ui_effect.t(unit),
        ~selection: t,
        editors: Model.t,
      )
      : cursor(Update.t) => {
    switch (selection, editors) {
    | (Scratch(selection), Scratch(m)) =>
      let ci =
        ScratchMode.Selection.get_cursor_info(
          ~inject=a => inject(Scratch(a)),
          ~selection,
          m,
        );
      let+ a = ci;
      Update.Scratch(a);
    | (Scratch(selection), Documentation(m)) =>
      let ci =
        ScratchMode.Selection.get_cursor_info(
          ~inject=a => inject(Scratch(a)),
          ~selection,
          m,
        );
      let+ a = ci;
      Update.Scratch(a);
    | (Assistant, _) => empty
    | (Tutorial(selection), Tutorial(m)) =>
      let ci =
        TutorialsMode.Selection.get_cursor_info(
          ~inject=a => inject(Tutorial(a)),
          ~selection,
          m,
        );
      let+ a = ci;
      Update.Tutorial(a);
    | (Exercises(selection), Exercises(m)) =>
      let ci =
        ExercisesMode.Selection.get_cursor_info(
          ~inject=a => inject(Exercises(a)),
          ~selection,
          m,
        );
      let+ a = ci;
      Update.Exercises(a);
    | (Scratch(_), Tutorial(_))
    | (Scratch(_), Exercises(_))
    | (Exercises(_), Scratch(_))
    | (Exercises(_), Documentation(_))
    | (Exercises(_), Tutorial(_))
    | (Tutorial(_), Scratch(_))
    | (Tutorial(_), Exercises(_))
    | (Tutorial(_), Documentation(_)) => empty
    };
  };

  let jump_to_tile =
      (~settings, tile, model: Model.t): option((Update.t, t)) =>
    switch (model) {
    | Scratch(m) =>
      ScratchMode.Selection.jump_to_tile(~settings, tile, m)
      |> Option.map(((x, y)) => (Update.Scratch(x), Scratch(y)))
    | Documentation(m) =>
      ScratchMode.Selection.jump_to_tile(~settings, tile, m)
      |> Option.map(((x, y)) => (Update.Scratch(x), Scratch(y)))
    | Tutorial(m) =>
      TutorialsMode.Selection.jump_to_tile(~settings, tile, m)
      |> Option.map(((x, y)) => (Update.Tutorial(x), Tutorial(y)))
    | Exercises(m) =>
      ExercisesMode.Selection.jump_to_tile(~settings, tile, m)
      |> Option.map(((x, y)) => (Update.Exercises(x), Exercises(y)))
    };

  let default_selection =
    fun
    | Model.Scratch(_) => Scratch(Cell(MainEditor))
    | Model.Documentation(_) => Scratch(Cell(MainEditor))
    | Model.Tutorial(_) => Tutorial(Cell(Tutorial.YourImpl, MainEditor))
    | Model.Exercises(_) =>
      Exercises(Code(Cell(CodeExercise.Prelude, MainEditor)));

  /* Selection-aware variant of Model.get_derivation_info: reports the
     derivation context only when the user's current focus is inside a
     derivation tree cell. Callers driving cursor-dependent UI (highlight
     maps, sidebar) should prefer this over the Model version, which reads
     the stale `model.pos`. */
  let get_derivation_info = (~selection: t, editors: Model.t) =>
    switch (selection, editors) {
    | (Scratch(sel), Scratch(m))
    | (Scratch(sel), Documentation(m)) =>
      ScratchMode.Selection.get_derivation_info(~selection=sel, m)
    | (Exercises(sel), Exercises(m)) =>
      ExercisesMode.Selection.get_derivation_info(~selection=sel, m)
    | _ => None
    };
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
    // Add in the line numbering for Scratch editor
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
        ~inject_explainthis,
        m,
      )
    // Add in the line numbering for Documentation editor
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
        ~inject_explainthis,
        m,
      )
    | Tutorial(m) =>
      TutorialsMode.View.view(
        ~signal=
          fun
          | MakeActive(s) => signal(MakeActive(Tutorial(s))),
        ~globals,
        ~selection=
          switch (selection) {
          | Some(Tutorial(s)) => Some(s)
          | _ => None
          },
        ~inject=a => Update.Tutorial(a) |> inject,
        ~inject_explainthis: ExplainThisUpdate.update => 'b,
        m,
      )
    | Exercises(m) =>
      ExercisesMode.View.view(
        ~take_focus=
          fun
          | s => signal(MakeActive(Exercises(s))),
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
    | Documentation(s) =>
      ScratchMode.View.file_menu(
        ~globals,
        ~inject=x => inject(Update.Scratch(x)),
        s,
      )
    | Tutorial(e) =>
      TutorialsMode.View.file_menu(
        ~globals,
        ~inject=x => inject(Update.Tutorial(x)),
        e,
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
                | "Tutorial" => inject(Update.SwitchMode(Tutorial))
                | "Exercises" => inject(Update.SwitchMode(Exercises))
                | _ => failwith("Invalid mode")
              ),
            ],
            List.map(
              s =>
                EditorModeView.option_view(
                  (
                    switch (editors) {
                    | Scratch(_) => "Scratch"
                    | Documentation(_) => "Documentation"
                    | Tutorial(_) => "Tutorial"
                    | Exercises(_) => "Exercises"
                    }
                  )
                  == s,
                  s,
                ),
              ["Scratch", "Documentation", "Tutorial", "Exercises"],
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
          ~is_documentation=false,
          ~inject=a => Update.Scratch(a) |> inject,
          m,
        )
      | Documentation(m) =>
        ScratchMode.View.top_bar(
          ~globals,
          ~is_documentation=true,
          ~inject=a => Update.Scratch(a) |> inject,
          m,
        )
      | Tutorial(m) =>
        TutorialsMode.View.top_bar(
          ~globals,
          ~inject=a => Update.Tutorial(a) |> inject,
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

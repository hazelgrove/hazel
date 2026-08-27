open Haz3lcore;
open Util;
open Language;

/* Dedicated ConfigurationMode module for handling different types of configuration
   with side effects. Currently supports ColorScheme configuration.

   This has a lot of overlap with ScratchMode as they're both full slide editors but configuration slides can not be added/deleted
   and each has a side effect after evaluation */

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson, enumerate)]
  type config_type =
    | ColorScheme
    | Shortcuts;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    current: int,
    configs: list((config_type, CellEditor.Model.t)),
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = (
    int,
    list((string, option(CellEditor.Model.persistent))),
  );

  let get_current_config = (model: t): (config_type, CellEditor.Model.t) => {
    List.nth(model.configs, model.current);
  };

  let get_current_config_type = (model: t): config_type => {
    get_current_config(model) |> fst;
  };

  let config_name_of_type = (config_type: config_type): string => {
    switch (config_type) {
    | ColorScheme => "Colors"
    | Shortcuts => "Shortcuts"
    };
  };

  let type_of_config_name = (name: string): option(config_type) => {
    List.find_opt(
      config_type => config_name_of_type(config_type) == name,
      all_of_config_type,
    );
  };

  /* The built-in source for each config slide, used as both the initial
     buffer and the baseline for "has the user changed this?". */
  let default_source = config_type => {
    switch (config_type) {
    | ColorScheme => ("Colors", ColorConfiguration.source)
    | Shortcuts => ("Shortcuts", ShortcutConfiguration.source)
    };
  };

  /* The type a config slide's editor is analyzed against, if any. Colors is
     a free-form list, so it stays synthetic. */
  let expected_type = config_type =>
    switch (config_type) {
    | ColorScheme => None
    | Shortcuts => Some(ShortcutConfiguration.expected_type)
    };

  /* Applied on every successful evaluation of a config slide.

     Colors is a direct DOM effect: CSS variables live on the document, so
     they outlive any re-render on their own. Shortcuts cannot do that — the
     command palette is rebuilt from scratch on every cursor change — so it
     records the override table in settings instead, where the palette build
     reads it and persistence carries it across reloads. */
  let perform_side_effect =
      (
        ~schedule_global: Globals.Update.t => unit,
        config_type: config_type,
        value: Language.Exp.t,
      )
      : unit => {
    switch (config_type) {
    | ColorScheme =>
      switch (value.term) {
      | ListLit(lits) =>
        let colors =
          List.concat_map(
            x => {
              switch (Unboxing.unbox(Tuple(2), x)) {
              | Matches([x, y]) =>
                switch (
                  Unboxing.unbox(Atom(String), x),
                  Unboxing.unbox(Atom(String), y),
                ) {
                | (Matches(name), Matches(color)) => [(name, color)]
                | _ => []
                }
              | _ => []
              }
            },
            lits,
          );
        List.iter(
          ((var, color)) => JsUtil.set_css_variable("--" ++ var, color),
          colors,
        );
      | _ => ()
      }
    | Shortcuts =>
      schedule_global(
        Set(
          SetShortcutOverrides(
            ShortcutConfiguration.overrides_of_value(value),
          ),
        ),
      )
    };
  };

  let persist = (model: t): persistent => (
    model.current,
    List.map(
      ((config_type: config_type, m: CellEditor.Model.t)) => {
        let name = config_name_of_type(config_type);
        let current_zipper = m.editor.editor.state.zipper;
        /* Built-in sources are text-backed and mint fresh ids on every
           parse, so id-sensitive segment equality can never match (same
           reasoning as ScratchMode.Scratchpad.persist). Compare the text
           projection instead, and store nothing for an untouched slide
           so a later change to the default is picked up. */
        let default_text =
          default_source(config_type)
          |> snd
          |> ((z: PersistentZipper.t) => z.backup_text)
          |> StringUtil.strip_final_newline;
        let unchanged =
          MarkerParse.seg_to_text(
            ~refractors=current_zipper.refractors.manuals,
            Zipper.zip(current_zipper),
          )
          == default_text;
        (name, unchanged ? None : Some(CellEditor.Model.persist(m)));
      },
      model.configs,
    ),
  );

  let unpersist = (~settings, (current, slides): persistent): t => {
    let get_persistent =
        ((s: string, m: option(CellEditor.Model.persistent))) => {
      let config_type =
        switch (type_of_config_name(s)) {
        | Some(ct) => ct
        | None =>
          // Fallback to first config type if name is not recognized
          List.hd(all_of_config_type)
        };
      (
        config_type,
        OptUtil.get(
          () =>
            default_source(config_type)
            |> snd
            |> CellEditor.Model.from_persistent_zipper(~root=Exp),
          m,
        )
        |> CellEditor.Model.unpersist(~settings),
      );
    };
    {
      current:
        List.find_index(
          config_type =>
            config_name_of_type(config_type)
            == (List.nth(slides, current) |> fst),
          all_of_config_type,
        )
        |> Option.value(~default=0),
      configs:
        List.map(
          (config_type: config_type) =>
            List.find_map(
              s =>
                s |> fst == config_name_of_type(config_type)
                  ? Some(get_persistent(s)) : None,
              slides,
            )
            |> OptUtil.get(() =>
                 (
                   config_type,
                   default_source(config_type)
                   |> snd
                   |> CellEditor.Model.from_persistent_zipper(~root=Exp)
                   |> CellEditor.Model.unpersist(~settings),
                 )
               ),
          all_of_config_type,
        ),
    };
  };
};

module StoreConfig =
  Store.F({
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = Model.persistent;
    let key = Store.Configuration;
    let default = () => (
      0,
      List.map(
        x =>
          Model.default_source(x)
          |> PairUtil.map_snd(
               CellEditor.Model.from_persistent_zipper(~root=Exp),
             )
          |> PairUtil.map_snd(Option.some),
        Model.all_of_config_type,
      ),
    );
  });

module Update = {
  open Updated;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | CellAction(CellEditor.Update.t)
    | SwitchConfig(int)
    | ResetCurrent
    | RefreshStatics;

  let update =
      (
        ~schedule_action as _,
        ~schedule_global: Globals.Update.t => unit,
        ~settings: Settings.t,
        action: t,
        model: Model.t,
      )
      : Updated.t(Model.t) => {
    switch (action) {
    | CellAction(a) =>
      switch (a) {
      | CellEditor.Update.ResultAction(UpdateResult(ResultOk({result, _}))) =>
        let (config_type, _) = Model.get_current_config(model);
        Model.perform_side_effect(~schedule_global, config_type, result);
      // Continue with normal cell update
      | _ => ()
      };

      let (_, ed) = Model.get_current_config(model);
      let* new_ed = CellEditor.Update.update(~settings, a, ed);
      {
        ...model,
        configs:
          ListUtil.put_nth(
            model.current,
            (Model.get_current_config_type(model), new_ed),
            model.configs,
          ),
      };
    | SwitchConfig(i) =>
      Updated.return({
        ...model,
        current: i,
      })
    | ResetCurrent =>
      let (config_type, _) = Model.get_current_config(model);
      let (_, source) = Model.default_source(config_type);
      Updated.return({
        ...model,
        configs:
          ListUtil.put_nth(
            model.current,
            (
              config_type,
              source
              |> CellEditor.Model.from_persistent_zipper(~root=Exp)
              |> CellEditor.Model.unpersist(~settings),
            ),
            model.configs,
          ),
      });
    | RefreshStatics =>
      CodeWithStatics.StaticsDebounce.force_on_next := true;
      model |> Updated.return_quiet(~recalculate=true);
    };
  };
  let calculate =
      (
        ~settings,
        ~autoprobe_mode,
        ~schedule_action,
        ~is_edited,
        model: Model.t,
      )
      : Model.t => {
    let statics_mode =
      CodeWithStatics.StaticsDebounce.consume(~is_edited, ~schedule_refresh=() =>
        schedule_action(RefreshStatics)
      );
    let (config_type, ed) = List.nth(model.configs, model.current);
    let worker_request = ref([]);
    let queue_worker =
      Some(
        (req_value: WorkerServer.Request.value) => {
          worker_request := worker_request^ @ [("", req_value)]
        },
      );
    let new_ed =
      CellEditor.Update.calculate(
        ~settings,
        ~autoprobe_mode,
        ~is_edited,
        ~statics_mode,
        /* the Shortcuts slide is checked against the known action set */
        ~ana=?Model.expected_type(config_type),
        ~queue_worker,
        ~stitch=x => x,
        ed,
      );
    let dispatch = (_key, action) =>
      schedule_action(CellAction(ResultAction(action)));
    EvalRequest.request(
      worker_request^,
      ~pos_of_key=key => key,
      ~dispatch,
      ~on_timeout=
        List.iter(((key, _)) =>
          dispatch(key, UpdateResult(ResultFail(Timeout)))
        ),
    );
    {
      ...model,
      configs:
        ListUtil.put_nth(
          model.current,
          (config_type, new_ed),
          model.configs,
        ),
    };
  };
};
module Selection = {
  open Cursor;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Cell(CellEditor.Selection.t)
    | TextBox;

  let get_cursor_info =
      (~inject: Update.t => Ui_effect.t(unit), ~selection, model: Model.t)
      : cursor(Update.t) => {
    switch (selection) {
    | Cell(selection) =>
      let+ a =
        CellEditor.Selection.get_cursor_info(
          ~inject=a => inject(CellAction(a)),
          ~selection,
          List.nth(model.configs, model.current) |> snd,
        );
      Update.CellAction(a);
    | TextBox => empty
    };
  };

  let jump_to_tile = (tile, model: Model.t): option((Update.t, t)) =>
    CellEditor.Selection.jump_to_tile(
      tile,
      List.nth(model.configs, model.current) |> snd,
    )
    |> Option.map(((x, y)) => (Update.CellAction(x), Cell(y)));
};
module View = {
  type event =
    | MakeActive(Selection.t);
  let view =
      (
        ~globals,
        ~signal: event => 'a,
        ~inject: Update.t => 'a,
        ~selected: option(Selection.t),
        model: Model.t,
      ) => {
    [
      CellEditor.View.view(
        ~globals,
        ~signal=
          fun
          | MakeActive(selection) => signal(MakeActive(Cell(selection))),
        ~inject=a => inject(CellAction(a)),
        ~selected=
          switch (selected) {
          | Some(Selection.Cell(s)) => Some(s)
          | _ => None
          },
        ~locked=false,
        List.nth(model.configs, model.current) |> snd,
      ),
    ];
  };

  let top_bar = (~inject: Update.t => 'a, model: Model.t) => {
    EditorModeView.view(
      ~edit_buttons=false,
      ~nav_buttons=false,
      ~signal=
        fun
        | Previous =>
          inject(
            SwitchConfig(
              (model.current + List.length(model.configs) - 1)
              mod List.length(model.configs),
            ),
          )
        | Next =>
          inject(
            SwitchConfig((model.current + 1) mod List.length(model.configs)),
          )
        | Add => Virtual_dom.Vdom.Effect.Ignore
        | Rename => Virtual_dom.Vdom.Effect.Ignore
        | Delete => Virtual_dom.Vdom.Effect.Ignore,
      ~indicator=
        EditorModeView.indicator_select(
          ~signal=i => inject(SwitchConfig(i)),
          model.current,
          List.map(
            ((config_type, _)) => Model.config_name_of_type(config_type),
            model.configs,
          ),
        ),
      (),
    );
  };

  let file_menu = (~globals: Globals.t, ~inject: Update.t => 'a, _: Model.t) => {
    let export_button_for_init =
      Widgets.button_named(
        Icons.export,
        _ => globals.inject_global(ExportForInit),
        ~tooltip="Export for Init",
      );

    let file_group_scratch =
      NutMenu.item_group(~inject, "File", [export_button_for_init]);

    let reset_button =
      Widgets.button_named(
        Icons.trash,
        _ => {
          let confirmed =
            JsUtil.confirm(
              "Are you SURE you want to reset this configuration? You will lose any existing settings.",
            );
          if (confirmed) {
            inject(ResetCurrent);
          } else {
            Virtual_dom.Vdom.Effect.Ignore;
          };
        },
        ~tooltip="Reset Editor",
      );

    let reparse =
      Widgets.button_named(
        Icons.backpack,
        _ => globals.inject_global(ActiveEditor(Reparse)),
        ~tooltip="Reparse Editor",
      );

    let reset_hazel =
      Widgets.button_named(
        Icons.bomb,
        _ => {
          let confirmed =
            JsUtil.confirm(
              "Are you SURE you want to reset Hazel to its initial state? You will lose any existing code that you have written, and course staff have no way to restore it!",
            );
          if (confirmed) {
            HazelDB.clear_all();
            Js_of_ocaml.Dom_html.window##.location##reload;
          };
          Virtual_dom.Vdom.Effect.Ignore;
        },
        ~tooltip="Reset Hazel (LOSE ALL DATA)",
      );

    let reset_group_scratch =
      NutMenu.item_group(
        ~inject,
        "Reset",
        [reset_button, reparse, reset_hazel],
      );

    [file_group_scratch, reset_group_scratch];
  };
};

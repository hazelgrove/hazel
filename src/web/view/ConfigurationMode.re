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

  let default_persisted_segment = config_type => {
    switch (config_type) {
    | ColorScheme => ("Colors", ColorConfiguration.segment)
    | Shortcuts => ("Shortcuts", ShortcutConfiguration.segment)
    };
  };

  let expected_type = config_type => {
    switch (config_type) {
    | ColorScheme => None
    | Shortcuts => Some(ShortcutConfiguration.expected_type)
    };
  };

  let perform_side_effect =
      (config_type: config_type, value: Language.Exp.t): unit => {
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
    | Shortcuts => ShortcutConfiguration.perform_shortcut_side_effect(value)
    };
  };

  let persist = (model: t): persistent => (
    model.current,
    List.map(
      ((s: config_type, m: CellEditor.Model.t)) => {
        let s = config_name_of_type(s);
        let current_segment = Zipper.zip(m.editor.editor.state.zipper);
        let original = Init.find_documentation_slide(s);
        let original_segment =
          original
          |> Option.map((pce: CellEditor.Model.persistent) =>
               PersistentZipper.unpersist(pce.editor)
             )
          |> Option.map(Zipper.zip);

        if (Option.equal(
              Base.equal_segment,
              original_segment,
              Some(current_segment),
            )) {
          (s, None);
        } else {
          (s, Some(CellEditor.Model.persist(m)));
        };
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
      print_endline("Unpersisting config: " ++ s);
      print_endline(
        "ana type: "
        ++ (
          switch (expected_type(config_type)) {
          | Some(t) => Language.Typ.show(t)
          | None => "None"
          }
        ),
      );
      (
        config_type,
        OptUtil.get(
          () =>
            default_persisted_segment(config_type)
            |> snd
            |> CellEditor.Model.from_persistent_segment,
          m,
        )
        |> CellEditor.Model.unpersist(
             ~ana=?expected_type(config_type),
             ~settings,
           ),
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
            |> OptUtil.get(() => {
                 let (_, seg) = default_persisted_segment(config_type);
                 (
                   config_type,
                   CellEditor.Model.mk(
                     ~ana=?expected_type(config_type),
                     Editor.Model.mk(
                       Zipper.unzip(
                         ~direction=Left,
                         PersistentSegment.unpersist(seg),
                       ),
                     ),
                   ),
                 );
               }),
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
          Model.default_persisted_segment(x)
          |> PairUtil.map_snd(CellEditor.Model.from_persistent_segment)
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
    | ResetCurrent;

  let update =
      (
        ~schedule_action as _,
        ~send_assistant_insertion_info: CodeEditable.Model.t => unit,
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
        Model.perform_side_effect(config_type, result);
      // Continue with normal cell update
      | _ => ()
      };

      let (_, ed) = Model.get_current_config(model);
      let* new_ed = CellEditor.Update.update(~settings, a, ed);
      let new_configs =
        ListUtil.put_nth(
          model.current,
          (Model.get_current_config_type(model), new_ed),
          model.configs,
        );
      let new_model = {
        ...model,
        configs: new_configs,
      };
      switch (a) {
      // Check for assistant hole completion triggers
      | MainEditor(Perform(Insert(_))) =>
        send_assistant_insertion_info(new_ed.editor)
      | _ => ()
      };
      new_model;
    | SwitchConfig(i) =>
      Updated.return({
        ...model,
        current: i,
      })
    | ResetCurrent =>
      let (config_type, _) = Model.get_current_config(model);
      let (_, source) = Model.default_persisted_segment(config_type);
      Updated.return({
        ...model,
        configs:
          ListUtil.put_nth(
            model.current,
            (
              config_type,
              source
              |> CellEditor.Model.from_persistent_segment
              |> CellEditor.Model.unpersist(~settings),
            ),
            model.configs,
          ),
      });
    };
  };
  let can_undo = (action: t) => {
    switch (action) {
    | CellAction(action) => CellEditor.Update.can_undo(action)
    | SwitchConfig(_) => false
    | ResetCurrent => true
    };
  };
  let calculate =
      (~settings, ~schedule_action, ~is_edited, model: Model.t): Model.t => {
    let (key, ed) = List.nth(model.configs, model.current);
    let worker_request = ref([]);
    let queue_worker =
      Some(expr => {worker_request := worker_request^ @ [("", expr)]});
    let new_ed =
      CellEditor.Update.calculate(
        ~settings,
        ~is_edited,
        ~queue_worker,
        ~stitch=x => x,
        ed,
      );
    switch (worker_request^) {
    | [] => ()
    | _ =>
      WorkerClient.request(
        worker_request^,
        ~handler=
          r =>
            schedule_action(
              CellAction(
                ResultAction(
                  UpdateResult(
                    switch (r |> List.hd |> snd) {
                    | Ok((r, s)) =>
                      Language.ProgramResult.ResultOk({
                        result: r,
                        state: s,
                      })
                    | Error(e) => Language.ProgramResult.ResultFail(e)
                    },
                  ),
                ),
              ),
            ),
        ~timeout=
          _ =>
            schedule_action(
              CellAction(ResultAction(UpdateResult(ResultFail(Timeout)))),
            ),
      )
    };
    let new_sp =
      ListUtil.put_nth(model.current, (key, new_ed), model.configs);
    {
      ...model,
      configs: new_sp,
    };
  };
};
module Selection = {
  open Cursor;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Cell(CellEditor.Selection.t)
    | TextBox;

  let get_cursor_info = (~selection, model: Model.t): cursor(Update.t) => {
    switch (selection) {
    | Cell(selection) =>
      let+ a =
        CellEditor.Selection.get_cursor_info(
          ~selection,
          List.nth(model.configs, model.current) |> snd,
        );
      Update.CellAction(a);
    | TextBox => empty
    };
  };

  let handle_key_event =
      (~selection, ~event: Key.t, model: Model.t): option(Update.t) =>
    switch (selection) {
    | Cell(selection) =>
      switch (event) {
      | _ =>
        CellEditor.Selection.handle_key_event(
          ~selection,
          ~event,
          List.nth(model.configs, model.current) |> snd,
        )
        |> Option.map(x => Update.CellAction(x))
      }
    | TextBox => None
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
            JsUtil.clear_localstore();
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

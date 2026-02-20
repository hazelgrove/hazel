open Haz3lcore;
open Util;

/* This file follows conventions in [docs/ui-architecture.md] */

module Scratchpad = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    name: string,
    editor: CellEditor.Model.t,
    agent: Agent.Agent.Model.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = {
    name: string,
    editor: option(CellEditor.Model.persistent),
    agent: Agent.Agent.Persistent.t,
  };

  let persist = (s: t): persistent => {
    let current_segment = Zipper.zip(s.editor.editor.editor.state.zipper);
    let original = Init.find_documentation_slide(s.name);
    let original_segment =
      original
      |> Option.map((pce: CellEditor.Model.persistent) =>
           PersistentZipper.unpersist(pce.editor)
         )
      |> Option.map(Zipper.zip);
    let editor =
      if (Option.equal(
            Base.equal_segment,
            original_segment,
            Some(current_segment),
          )) {
        None;
      } else {
        Some(CellEditor.Model.persist(s.editor));
      };
    {
      name: s.name,
      editor,
      agent: Agent.Agent.Persistent.persist(s.agent),
    };
  };

  let unpersist = (~settings, p: persistent): t => {
    name: p.name,
    editor:
      OptUtil.get(
        () => Init.default_documentation_slide_name(p.name),
        p.editor,
      )
      |> CellEditor.Model.unpersist(~settings),
    agent: Agent.Agent.Persistent.unpersist(p.agent),
  };

  let mk = (~name, ~editor, ()): t => {
    name,
    editor,
    agent: Agent.Agent.Utils.init(),
  };
};

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    current: int,
    scratchpads: list(Scratchpad.t),
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = (int, list(Scratchpad.persistent));

  let persist = (model: t): persistent => (
    model.current,
    List.map(Scratchpad.persist, model.scratchpads),
  );

  let unpersist = (~settings, (current, scratchpads): persistent): t => {
    current,
    scratchpads:
      List.map(sp => Scratchpad.unpersist(~settings, sp), scratchpads),
  };
};

let scratchpad_persistent_of_init =
    (name: string, editor: option(CellEditor.Model.persistent))
    : Scratchpad.persistent => {
  name,
  editor,
  agent: Agent.Agent.Utils.init() |> Agent.Agent.Persistent.persist,
};

module StoreDocumentation =
  Store.F({
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = Model.persistent;
    let key = Store.Documentation;
    let default = (): t =>
      Init.startup.documentation
      |> PairUtil.map_snd(list =>
           List.map(
             ((name, _)) => scratchpad_persistent_of_init(name, None),
             list,
           )
         );
  });

module Store = {
  include Store.F({
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = Model.persistent;
    let key = Store.Scratch;
    let default = (): t =>
      Init.startup.scratch
      |> PairUtil.map_snd(list =>
           List.map(
             ((name, editor)) =>
               scratchpad_persistent_of_init(name, Some(editor)),
             list,
           )
         );
  });

  let integrate_share = (model: t): t => {
    let share_name =
      switch (JsUtil.QueryParams.get_param("name")) {
      | None => "Unknown Share"
      | Some(name) => name
      };
    switch (JsUtil.QueryParams.get_param("share"), model) {
    | (None, _) => model
    | (Some(data), (_current, scratchpads)) =>
      let shared_text = data |> StringUtil.decompress;
      let shared: Haz3lcore.PersistentZipper.t = {
        zipper: "invalid",
        backup_text: shared_text,
      };
      let shared: CellEditor.Model.persistent = {
        editor: shared,
        result: EvalResult.Model.init |> EvalResult.Model.persist,
      };
      let new_scratchpad =
        scratchpad_persistent_of_init(share_name, Some(shared));

      (List.length(scratchpads), scratchpads @ [new_scratchpad]);
    };
  };
};

module Update = {
  open Updated;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | CellAction(CellEditor.Update.t)
    | AgentAction(Agent.Agent.Update.Action.t)
    | SwitchSlide(int)
    | ResetCurrent
    | InitImportScratchpad([@opaque] Js_of_ocaml.Js.t(Js_of_ocaml.File.file))
    | FinishImportScratchpad(option(string))
    | Export
    | Encode
    | AddSlide
    | RenameSlide
    | DeleteSlide;

  let can_undo = (action: t) => {
    switch (action) {
    | CellAction(action) => CellEditor.Update.can_undo(action)
    | AgentAction(_) => true
    | SwitchSlide(_) => false
    | ResetCurrent => true
    | InitImportScratchpad(_) => true
    | FinishImportScratchpad(_) => false
    | Export => false
    | Encode => false
    | AddSlide => true
    | DeleteSlide => true
    | RenameSlide => true
    };
  };

  let export_scratch_slide = (model: Model.t): unit => {
    Store.save(model |> Model.persist);
    let data = Store.export();
    let current_name = List.nth(model.scratchpads, model.current).name;
    let filename = current_name |> StringUtil.sanitize_filename;
    JsUtil.download_string_file(
      ~filename,
      ~content_type="text/plain",
      ~contents=data,
    );
  };

  let encode_scratch_slide = (model: Model.t): unit => {
    let scratchpad = List.nth(model.scratchpads, model.current);
    let c = scratchpad.editor |> CellEditor.Model.to_string;
    JsUtil.QueryParams.set_param("share", StringUtil.compress(c));
    JsUtil.QueryParams.set_param("name", scratchpad.name);
  };
  let rec prompt_slide_name =
          (
            ~error: option(string)=?,
            ~existing_scratchpads: Seq.t(string),
            default: string,
          )
          : Option.t(string) => {
    let new_name =
      JsUtil.prompt(
        (
          switch (error) {
          | Some(e) => e ++ "\n"
          | None => ""
          }
        )
        ++ "Enter new slide name:",
        default,
      );

    if (existing_scratchpads |> Seq.exists(name => Some(name) == new_name)) {
      prompt_slide_name(
        ~error="Slide name already exists. Please choose a different name.",
        ~existing_scratchpads,
        Option.value(~default, new_name),
      );
    } else {
      new_name;
    };
  };

  let add_new_slide = (model: Model.t, is_documentation: bool): Model.t => {
    let add_empty_slide = (name): Model.t => {
      current: List.length(model.scratchpads),
      scratchpads:
        model.scratchpads
        @ [
          Scratchpad.mk(
            ~name,
            ~editor=CellEditor.Model.mk(Editor.Model.mk(Zipper.init())),
            (),
          ),
        ],
    };
    switch (is_documentation) {
    | false =>
      let used_scratchpads =
        model.scratchpads
        |> List.filter_map((s: Scratchpad.t) => {
             switch (String.split_on_char(' ', s.name)) {
             | ["Scratchpad", num] => int_of_string_opt(num)
             | _ => None
             }
           });
      let unused_ids =
        Seq.filter(i => !List.mem(i, used_scratchpads), Seq.ints(1));
      let new_number =
        Seq.uncons(unused_ids)
        |> Option.get  // This is safe because unused_ids is infinite
        |> fst;

      add_empty_slide("Scratchpad " ++ string_of_int(new_number));
    | true =>
      let new_name =
        prompt_slide_name(
          ~existing_scratchpads=
            model.scratchpads
            |> List.to_seq
            |> Seq.map((s: Scratchpad.t) => s.name),
          "New Slide Name",
        );
      switch (new_name) {
      | None => model // Prompt cancelled so no new scratchpad created
      | Some(name) => add_empty_slide(name)
      };
    };
  };

  let update =
      (
        ~schedule_action,
        ~settings: Settings.t,
        ~is_documentation: bool,
        action,
        model: Model.t,
      ) => {
    switch (action) {
    | AgentAction(a) =>
      let scratchpad = List.nth(model.scratchpads, model.current);
      let schedule_agent = (a: Agent.Agent.Update.Action.t) =>
        schedule_action(AgentAction(a));
      let (new_agent, updated_editor) =
        Agent.Agent.Update.update(
          a,
          scratchpad.agent,
          scratchpad.editor,
          settings,
          schedule_agent,
        );
      let* new_ed = updated_editor;
      let new_sp =
        ListUtil.put_nth(
          model.current,
          {
            ...scratchpad,
            editor: new_ed,
            agent: new_agent,
          },
          model.scratchpads,
        );
      {
        ...model,
        scratchpads: new_sp,
      };
    | CellAction(a) =>
      let scratchpad = List.nth(model.scratchpads, model.current);
      let* new_ed = CellEditor.Update.update(~settings, a, scratchpad.editor);
      let new_sp =
        ListUtil.put_nth(
          model.current,
          {
            ...scratchpad,
            editor: new_ed,
          },
          model.scratchpads,
        );
      let new_model = {
        ...model,
        scratchpads: new_sp,
      };
      new_model;
    | SwitchSlide(i) =>
      let* current = i |> Updated.return;
      {
        ...model,
        current,
      };
    | AddSlide => Updated.return(add_new_slide(model, is_documentation))
    | RenameSlide =>
      let current = List.nth(model.scratchpads, model.current);
      let new_name =
        prompt_slide_name(
          ~existing_scratchpads=
            model.scratchpads
            |> List.to_seq
            |> Seq.zip(Seq.ints(0))
            |> Seq.filter(((idx, _)) => idx != model.current)
            |> Seq.map(snd)
            |> Seq.map((s: Scratchpad.t) => s.name),
          current.name,
        );

      switch (new_name) {
      | None => model |> return_quiet
      | Some(new_name) =>
        let new_sp =
          ListUtil.put_nth(
            model.current,
            {
              ...current,
              name: new_name,
            },
            model.scratchpads,
          );
        Updated.return({
          ...model,
          scratchpads: new_sp,
        });
      };
    | DeleteSlide =>
      let confirmed =
        JsUtil.confirm(
          "Are you SURE you want to delete this slide? You will lose any existing code that you have written, and course staff have no way to restore it!",
        );
      if (confirmed) {
        let new_sp =
          ListUtil.remove_nth(model.current, model.scratchpads)
          |> Option.value(~default=model.scratchpads);

        let m: Model.t =
          List.is_empty(new_sp)
            ? add_new_slide(
                {
                  ...model,
                  scratchpads: [],
                },
                is_documentation,
              )
            : {
              scratchpads: new_sp,
              current: max(model.current - 1, 0),
            };
        Updated.return(m);
      } else {
        model |> return_quiet;
      };

    | ResetCurrent =>
      let scratchpad = List.nth(model.scratchpads, model.current);
      let source =
        switch (is_documentation) {
        | false =>
          CellEditor.Model.mk(Editor.Model.mk(Zipper.init()))
          |> CellEditor.Model.persist
        | true => Init.default_documentation_slide_name(scratchpad.name)
        };
      let* data =
        source
        |> CellEditor.Model.unpersist(~settings=settings.core)
        |> Updated.return;
      {
        ...model,
        scratchpads:
          ListUtil.put_nth(
            model.current,
            {
              ...scratchpad,
              editor: data,
            },
            model.scratchpads,
          ),
      };
    | InitImportScratchpad(file) =>
      JsUtil.read_file(file, data =>
        schedule_action(FinishImportScratchpad(data))
      );
      model |> return_quiet;
    | FinishImportScratchpad(None) => model |> return_quiet
    | FinishImportScratchpad(Some(data)) =>
      let scratchpad = List.nth(model.scratchpads, model.current);
      let new_data =
        data
        |> Sexplib.Sexp.of_string
        |> CellEditor.Model.persistent_of_sexp
        |> CellEditor.Model.unpersist(~settings=settings.core);

      let scratchpads =
        ListUtil.put_nth(
          model.current,
          {
            ...scratchpad,
            editor: new_data,
          },
          model.scratchpads,
        );
      {
        ...model,
        scratchpads,
      }
      |> Updated.return;
    | Export =>
      export_scratch_slide(model);
      model |> Updated.return_quiet;
    | Encode =>
      encode_scratch_slide(model);
      model |> Updated.return_quiet;
    };
  };

  let calculate =
      (~settings, ~schedule_action, ~is_edited, model: Model.t): Model.t => {
    let scratchpad = List.nth(model.scratchpads, model.current);
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
        ~is_edited,
        ~queue_worker,
        ~stitch=x => x,
        scratchpad.editor,
      );
    switch (worker_request^) {
    | [] => ()
    | _ =>
      WorkerClient.request(
        worker_request^,
        ~handler=
          r => {
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
            )
          },
        ~timeout=
          _ =>
            schedule_action(
              CellAction(ResultAction(UpdateResult(ResultFail(Timeout)))),
            ),
      )
    };
    let new_sp =
      ListUtil.put_nth(
        model.current,
        {
          ...scratchpad,
          editor: new_ed,
        },
        model.scratchpads,
      );
    {
      ...model,
      scratchpads: new_sp,
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
          List.nth(model.scratchpads, model.current).editor,
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
          List.nth(model.scratchpads, model.current).editor,
        )
        |> Option.map(x => Update.CellAction(x))
      }
    | TextBox => None
    };

  let jump_to_tile = (tile, model: Model.t): option((Update.t, t)) =>
    CellEditor.Selection.jump_to_tile(
      tile,
      List.nth(model.scratchpads, model.current).editor,
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
    (
      SlideContent.get_content(
        List.nth(model.scratchpads, model.current).name,
      )
      |> Option.to_list
    )
    @ [
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
        ~lines=true,
        List.nth(model.scratchpads, model.current).editor,
      ),
    ];
  };

  let file_menu = (~globals: Globals.t, ~inject: Update.t => 'a, _: Model.t) => {
    let export_button =
      Widgets.button_named(
        Icons.export,
        _ => inject(Export),
        ~tooltip="Export Scratchpad",
      );

    let export_button_for_init =
      Widgets.button_named(
        Icons.export,
        _ => globals.inject_global(ExportForInit),
        ~tooltip="Export for Init",
      );

    let encode_button =
      Widgets.button_named(
        Icons.export,
        _ => inject(Encode),
        ~tooltip="Encode Scratchpad in URL",
      );

    let import_button =
      Widgets.file_select_button_named(
        "import-scratchpad",
        Icons.import,
        file => {
          switch (file) {
          | None => Virtual_dom.Vdom.Effect.Ignore
          | Some(file) => inject(InitImportScratchpad(file))
          }
        },
        ~tooltip="Import Scratchpad",
      );

    let file_group_scratch =
      NutMenu.item_group(
        ~inject,
        "File",
        [export_button, export_button_for_init, encode_button, import_button],
      );

    let reset_button =
      Widgets.button_named(
        Icons.trash,
        _ => {
          let confirmed =
            JsUtil.confirm(
              "Are you SURE you want to reset this scratchpad? You will lose any existing code.",
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

  let top_bar = (~globals as _, ~inject: Update.t => 'a, model: Model.t) => {
    EditorModeView.view(
      ~edit_buttons=true,
      ~nav_buttons=false,
      ~signal=
        fun
        | Previous =>
          inject(
            SwitchSlide(
              (model.current + List.length(model.scratchpads) - 1)
              mod List.length(model.scratchpads),
            ),
          )
        | Next =>
          inject(
            SwitchSlide(
              (model.current + 1) mod List.length(model.scratchpads),
            ),
          )
        | Add => inject(AddSlide)
        | Rename => inject(RenameSlide)
        | Delete => inject(DeleteSlide),
      ~indicator=
        EditorModeView.indicator_select(
          ~signal=i => inject(SwitchSlide(i)),
          model.current,
          List.map((s: Scratchpad.t) => s.name, model.scratchpads),
        ),
    );
  };
};

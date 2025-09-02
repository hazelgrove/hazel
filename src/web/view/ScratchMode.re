open Haz3lcore;
open Util;

/* This file follows conventions in [docs/ui-architecture.md] */

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    current: int,
    scratchpads: list((string, CellEditor.Model.t)),
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = (
    int,
    list((string, option(CellEditor.Model.persistent))),
  );

  let persist = (model: t): persistent => (
    model.current,
    List.map(
      ((s: string, m: CellEditor.Model.t)) => {
        (
          //TODO(andrew): reinstate
          // let current_segment = Zipper.zip(m.editor.editor.state.zipper);
          // let original = Init.find_documentation_slide(s);
          // let original_segment =
          //   original
          //   |> Option.map((pce: CellEditor.Model.persistent) =>
          //        PersistentZipper.unpersist(pce.editor)
          //      )
          //   |> Option.map(Zipper.zip);
          // if (Option.equal(
          //       Base.equal_segment,
          //       original_segment,
          //       Some(current_segment),
          //     )) {
          //   (s, None);
          // } else {
          s,
          Some(CellEditor.Model.persist(m)),
          // };
        )
      },
      model.scratchpads,
    ),
  );

  let unpersist = (~settings, (current, slides): persistent): t => {
    current,
    scratchpads:
      List.map(
        ((s: string, m: option(CellEditor.Model.persistent))) =>
          (
            s,
            OptUtil.get(() => Init.default_documentation_slide_name(s), m)
            |> CellEditor.Model.unpersist(~settings),
          ),
        slides,
      ),
  };
};

module StoreDocumentation =
  Store.F({
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = Model.persistent;
    let key = Store.Documentation;
    let default = (): t =>
      Init.startup.documentation
      |> PairUtil.map_snd(List.map(PairUtil.map_snd(_ => None)));
  });

module Store = {
  include Store.F({
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = Model.persistent;
    let key = Store.Scratch;
    let default = () =>
      Init.startup.scratch
      |> PairUtil.map_snd(List.map(PairUtil.map_snd(x => Some(x))));
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

      (
        List.length(scratchpads),
        scratchpads @ [(share_name, Some(shared))],
      );
    };
  };
};

module Update = {
  open Updated;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | CellAction(CellEditor.Update.t)
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
    let current_name = List.nth(model.scratchpads, model.current) |> fst;
    let filename = current_name |> StringUtil.sanitize_filename;
    JsUtil.download_string_file(
      ~filename,
      ~content_type="text/plain",
      ~contents=data,
    );
  };

  let encode_scratch_slide = (model: Model.t): unit => {
    let (name, ed) = List.nth(model.scratchpads, model.current);
    let c = ed |> CellEditor.Model.to_string;
    JsUtil.QueryParams.set_param("share", StringUtil.compress(c));
    JsUtil.QueryParams.set_param("name", name);
  };

  let mk_new_scratchpad =
      (model: Model.t, is_documentation: bool)
      : list((string, CellEditor.Model.t)) => {
    let new_key =
      switch (is_documentation) {
      | false =>
        let used_scratchpads =
          model.scratchpads
          |> List.filter_map(scratchpad => {
               switch (String.split_on_char(' ', fst(scratchpad))) {
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

        "Scratchpad " ++ string_of_int(new_number);
      | true =>
        JsUtil.prompt("Enter new buffer name:", "New Buffer Name")
        |> Option.get
      };
    model.scratchpads
    @ [(new_key, CellEditor.Model.mk(Editor.Model.mk(Zipper.init())))];
  };

  let update =
      (
        ~schedule_action,
        ~send_assistant_insertion_info: CodeEditable.Model.t => unit,
        ~settings: Settings.t,
        ~is_documentation: bool,
        action,
        model: Model.t,
      ) => {
    switch (action) {
    | CellAction(a) =>
      let (key, ed) = List.nth(model.scratchpads, model.current);
      let* new_ed = CellEditor.Update.update(~settings, a, ed);
      let new_sp =
        ListUtil.put_nth(model.current, (key, new_ed), model.scratchpads);
      let new_model = {
        ...model,
        scratchpads: new_sp,
      };
      switch (a) {
      // Check for assistant hole completion triggers
      | MainEditor(Perform(Insert(_))) =>
        send_assistant_insertion_info(new_ed.editor)
      | _ => ()
      };
      new_model;
    | SwitchSlide(i) =>
      let* current = i |> Updated.return;
      {
        ...model,
        current,
      };
    | AddSlide =>
      let new_sp = mk_new_scratchpad(model, is_documentation);
      Updated.return(
        {
          current: List.length(new_sp) - 1,
          scratchpads: new_sp,
        }: Model.t,
      );
    | RenameSlide =>
      let current = List.nth(model.scratchpads, model.current);
      let new_name = JsUtil.prompt("Enter new buffer name:", fst(current));
      switch (new_name) {
      | None => model |> return_quiet
      | Some(new_name) =>
        let new_sp =
          ListUtil.put_nth(
            model.current,
            (new_name, snd(current)),
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
          "Are you SURE you want to delete this buffer? You will lose any existing code that you have written, and course staff have no way to restore it!",
        );
      if (confirmed) {
        let new_sp =
          ListUtil.remove_nth(model.current, model.scratchpads)
          |> Option.value(~default=model.scratchpads);
        let safe_sp =
          new_sp |> List.length == 0
            ? mk_new_scratchpad(
                {
                  ...model,
                  scratchpads: new_sp,
                },
                is_documentation,
              )
            : new_sp;
        Updated.return(
          {
            current: max(model.current - 1, 0),
            scratchpads: safe_sp,
          }: Model.t,
        );
      } else {
        model |> return_quiet;
      };

    | ResetCurrent =>
      let (key, _) = List.nth(model.scratchpads, model.current);
      let source =
        switch (is_documentation) {
        | false =>
          CellEditor.Model.mk(Editor.Model.mk(Zipper.init()))
          |> CellEditor.Model.persist
        | true => Init.default_documentation_slide_name(key)
        };
      let* data =
        source
        |> CellEditor.Model.unpersist(~settings=settings.core)
        |> Updated.return;
      {
        ...model,
        scratchpads:
          ListUtil.put_nth(model.current, (key, data), model.scratchpads),
      };
    | InitImportScratchpad(file) =>
      JsUtil.read_file(file, data =>
        schedule_action(FinishImportScratchpad(data))
      );
      model |> return_quiet;
    | FinishImportScratchpad(None) => model |> return_quiet
    | FinishImportScratchpad(Some(data)) =>
      let key = List.nth(model.scratchpads, model.current) |> fst;
      let new_data =
        data
        |> Sexplib.Sexp.of_string
        |> CellEditor.Model.persistent_of_sexp
        |> CellEditor.Model.unpersist(~settings=settings.core);

      let scratchpads =
        ListUtil.put_nth(model.current, (key, new_data), model.scratchpads);
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
    let (key, ed) = List.nth(model.scratchpads, model.current);
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
      ListUtil.put_nth(model.current, (key, new_ed), model.scratchpads);
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
          List.nth(model.scratchpads, model.current) |> snd,
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
          List.nth(model.scratchpads, model.current) |> snd,
        )
        |> Option.map(x => Update.CellAction(x))
      }
    | TextBox => None
    };

  let jump_to_tile = (tile, model: Model.t): option((Update.t, t)) =>
    CellEditor.Selection.jump_to_tile(
      tile,
      List.nth(model.scratchpads, model.current) |> snd,
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
        List.nth(model.scratchpads, model.current) |> fst,
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
        List.nth(model.scratchpads, model.current) |> snd,
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
          List.map(((s, _)) => s, model.scratchpads),
        ),
    );
  };
};

open Haz3lcore;
open Util;

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    current: int,
    scratchpads: list((string, CellEditor.Model.t)),
  };

  let get_spliced_elabs = model => {
    let (key, ed) = List.nth(model.scratchpads, model.current);
    [(key, Elaborator.Elaboration.{d: ed.editor.statics.term})];
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type persistent = (int, list((string, CellEditor.Model.persistent)));

  let persist = model => (
    model.current,
    List.map(((_, m)) => CellEditor.Model.persist(m), model.scratchpads),
  );

  let unpersist = (~settings, (current, slides)) => {
    current,
    scratchpads:
      List.mapi(
        (i, m) =>
          (string_of_int(i), CellEditor.Model.unpersist(~settings, m)),
        slides,
      ),
  };

  let persist_documentation = model => (
    model.current,
    List.map(
      ((s, m)) => (s, CellEditor.Model.persist(m)),
      model.scratchpads,
    ),
  );

  let unpersist_documentation = (~settings, (current, slides)) => {
    current,
    scratchpads:
      List.map(
        ((s, m)) => (s, CellEditor.Model.unpersist(~settings, m)),
        slides,
      ),
  };
};

module StoreDocumentation =
  Store.F({
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = Model.persistent;
    let key = Store.Documentation;
    let default = () => Init.startup.documentation;
  });

module Store =
  Store.F({
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = (int, list(CellEditor.Model.persistent));
    let key = Store.Scratch;
    let default = () => Init.startup.scratch;
  });

module Update = {
  open Updated;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | CellAction(CellEditor.Update.t)
    | SwitchSlide(int)
    | ResetCurrent
    | InitImportScratchpad([@opaque] Js_of_ocaml.Js.t(Js_of_ocaml.File.file))
    | FinishImportScratchpad(option(string));

  let update =
      (
        ~schedule_action,
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
      {...model, scratchpads: new_sp};
    | SwitchSlide(i) =>
      let* current = i |> Updated.return;
      {...model, current};
    | ResetCurrent =>
      let (key, _) = List.nth(model.scratchpads, model.current);
      let source =
        switch (is_documentation) {
        | false => Init.startup.scratch |> snd
        | true => Init.startup.documentation |> snd |> List.map(snd)
        };
      let* data =
        List.nth(source, model.current)
        |> PersistentZipper.unpersist
        |> Editor.Model.mk
        |> CellEditor.Model.mk
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
      {...model, scratchpads} |> Updated.return;
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
                      Haz3lcore.ProgramResult.ResultOk({result: r, state: s})
                    | Error(e) => Haz3lcore.ProgramResult.ResultFail(e)
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
    {...model, scratchpads: new_sp};
  };
};

module Selection = {
  open Cursor;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = CellEditor.Selection.t;

  let get_cursor_info = (~selection, model: Model.t): cursor(Update.t) => {
    let+ ci =
      CellEditor.Selection.get_cursor_info(
        ~selection,
        List.nth(model.scratchpads, model.current) |> snd,
      );
    Update.CellAction(ci);
  };

  let handle_key_event =
      (~selection, ~event: Key.t, model: Model.t): option(Update.t) =>
    switch (event) {
    | {key: D(key), sys: Mac | PC, shift: Up, meta: Down, ctrl: Up, alt: Up}
        when Keyboard.is_digit(key) =>
      Some(Update.SwitchSlide(int_of_string(key)))
    | _ =>
      CellEditor.Selection.handle_key_event(
        ~selection,
        ~event,
        List.nth(model.scratchpads, model.current) |> snd,
      )
      |> Option.map(x => Update.CellAction(x))
    };

  let jump_to_tile = (tile, model: Model.t): option((Update.t, t)) =>
    CellEditor.Selection.jump_to_tile(
      tile,
      List.nth(model.scratchpads, model.current) |> snd,
    )
    |> Option.map(((x, y)) => (Update.CellAction(x), y));
};

module View = {
  type event =
    | MakeActive(CellEditor.Selection.t);

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
          | MakeActive(selection) => signal(MakeActive(selection)),
        ~inject=a => inject(CellAction(a)),
        ~selected,
        ~locked=false,
        List.nth(model.scratchpads, model.current) |> snd,
      ),
    ];
  };

  let export_button = (model: Model.t) =>
    Widgets.button_named(
      Icons.star,
      _ => {
        let json_data =
          List.nth(model.scratchpads, model.current)
          |> snd
          |> CellEditor.Model.persist
          |> CellEditor.Model.yojson_of_persistent;
        JsUtil.download_json("hazel-scratchpad", json_data);
        Virtual_dom.Vdom.Effect.Ignore;
      },
      ~tooltip="Export Scratchpad",
    );

  let export_menu = (model: Model.t) => [export_button(model)];

  let import_button = (inject: Update.t => 'a) =>
    Widgets.file_select_button_named(
      "import-scratchpad",
      Icons.star,
      file => {
        switch (file) {
        | None => Virtual_dom.Vdom.Effect.Ignore
        | Some(file) => inject(InitImportScratchpad(file))
        }
      },
      ~tooltip="Import Scratchpad",
    );

  let reset_button = (inject: Update.t => 'a) =>
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
      ~tooltip="Reset Scratchpad",
    );

  let import_menu = (~inject: Update.t => 'a) => [
    import_button(inject),
    reset_button(inject),
  ];

  let top_bar =
      (
        ~globals as _,
        ~named_slides: bool,
        ~inject: Update.t => 'a,
        model: Model.t,
      ) => {
    SlideSelect.view(
      ~signal=
        fun
        | Previous =>
          inject(
            SwitchSlide(
              (model.current - 1) mod List.length(model.scratchpads),
            ),
          )
        | Next =>
          inject(
            SwitchSlide(
              (model.current + 1) mod List.length(model.scratchpads),
            ),
          ),
      ~indicator=
        named_slides
          ? SlideSelect.indicator_select(
              ~signal=i => inject(SwitchSlide(i)),
              model.current,
              List.map(((s, _)) => s, model.scratchpads),
            )
          : SlideSelect.indicator_n(
              model.current,
              List.length(model.scratchpads),
            ),
    );
  };
};

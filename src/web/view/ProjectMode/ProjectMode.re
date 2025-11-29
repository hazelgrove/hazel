open Haz3lcore;
open Util;
open OptUtil.Syntax;

/* This file follows conventions in [docs/ui-architecture.md] */

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    file_system: FileSystem.Model.t,
    // agent: Agent.Model.t
    // ...
  };
};

module Persistent = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    file_tree: FileSystem.Persistent.t,
    // agent: Agent.Persistent.t
    // ...
  };

  let persist = (model: Model.t): t => {
    file_tree: FileSystem.Persistent.persist(model.file_system),
    // agent: Agent.Persistent.persist(model.agent),
    // ...
  };

  let unpersist = (~settings, p: t): Model.t => {
    file_system: FileSystem.Persistent.unpersist(~settings, p.file_tree),
    // agent: Agent.Persistent.unpersist(~settings, p.agent),
    // ...
  };

  let integrate_share = (model: t): t => {
    // TODO: I'm not really sure what this is supposed to do yet
    model;
  };
};

module Store = {
  include Store.F({
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = Persistent.t;
    let key = Store.Project;
    let default: unit => t =
      () => {
        let root = ""; // root name is the empty string
        let file_tree: FileSystem.Persistent.t = {
          file_tree: {
            let root_project_folder: FileSystem.Persistent.folder = {
              path: [root],
              name: root,
              children: [],
              expanded: true,
            };
            let file_tree =
              Maps.StringMap.singleton(
                root,
                FileSystem.Persistent.Folder(root_project_folder),
              );
            file_tree;
          },
          current: [root],
        };
        {file_tree: file_tree};
      };
  });

  let integrate_share = (model: t): t => {
    // TODO: I'm not really sure what this is supposed to do yet
    model;
  };
};

module Update = {
  open Updated;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | CellAction(CellEditor.Update.t)
    | FileSystemAction(FileSystem.Update.t);

  let can_undo = (_action: t) => {
    false;
  };

  let export_project = (model: Model.t): unit => {
    let persistent = Persistent.persist(model);
    Store.save(persistent);
    let data = Store.export();
    JsUtil.download_string_file(
      ~filename="project.hazel",
      ~content_type="text/plain",
      ~contents=data,
    );
  };

  let update =
      (~globals, ~schedule_action: t => unit, action: t, model: Model.t)
      : Updated.t(Model.t) => {
    switch (action) {
    | CellAction(cell_action) => Updated.return_quiet(model)
    | FileSystemAction(file_system_action) =>
      let* new_fs =
        FileSystem.Update.update(file_system_action, model.file_system);
      let new_model: Model.t = {file_system: new_fs};
      new_model;
    };
  };

  let calculate =
      (
        ~settings: Language.CoreSettings.t,
        ~schedule_action: t => unit,
        ~is_edited: bool,
        model: Model.t,
      )
      : Model.t => {
    switch (FileSystem.Utils.current_file(model.file_system)) {
    | None => model // No current file to calculate
    | Some(file) =>
      let worker_request = ref([]);
      let queue_worker =
        Some(expr => {worker_request := worker_request^ @ [("", expr)]});
      let new_ed =
        CellEditor.Update.calculate(
          ~settings,
          ~is_edited,
          ~queue_worker,
          ~stitch=x => x,
          file.editor,
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
                CellAction(
                  ResultAction(UpdateResult(ResultFail(Timeout))),
                ),
              ),
        )
      };
      let updated_file = {
        ...file,
        editor: new_ed,
      };
      let updated_file_system =
        Maps.StringMap.add(
          FileSystem.Utils.string_of_path(file.path),
          FileSystem.Model.File(updated_file),
          model.file_system.file_tree,
        );
      {
        file_system: {
          ...model.file_system,
          file_tree: updated_file_system,
        },
      };
    };
  };
};

module Selection = {
  open Cursor;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Cell(CellEditor.Selection.t)
    | TextBox;

  let get_cursor_info = (~selection: t, model: Model.t): cursor(Update.t) => {
    // Gets the cursor info from the currently set editor in the file system
    // If an editor is not set (eg. only a folder exists), we return the empty cursor
    switch (selection) {
    | Cell(selection) =>
      let file = FileSystem.Utils.current_file(model.file_system);
      switch (file) {
      | None => empty
      | Some(file) =>
        let+ a =
          CellEditor.Selection.get_cursor_info(~selection, file.editor);
        Update.CellAction(a);
      };
    | TextBox => empty
    };
  };

  let handle_key_event =
      (~selection, ~event: Key.t, model: Model.t): option(Update.t) =>
    switch (selection) {
    | Cell(selection) =>
      switch (event) {
      | _ =>
        let* file = FileSystem.Utils.current_file(model.file_system);
        CellEditor.Selection.handle_key_event(~selection, ~event, file.editor)
        |> Option.map(x => Update.CellAction(x));
      }
    | TextBox => None
    };

  let jump_to_tile = (tile: Id.t, model: Model.t): option((Update.t, t)) => {
    let* file = FileSystem.Utils.current_file(model.file_system);
    CellEditor.Selection.jump_to_tile(tile, file.editor)
    |> Option.map(((x, y)) => (Update.CellAction(x), Cell(y)));
  };
};

module View = {
  type event =
    | MakeActive(Selection.t);

  let project_sidebar = (~globals, ~inject, model: Model.t) => {
    []; // A left-hand side sidebar for project navigation
      // Displays the file system tree as clickable items
  };

  let view =
      (
        ~globals,
        ~signal: event => 'a,
        ~inject: Update.t => 'a,
        ~selected: option(Selection.t),
        model: Model.t,
      ) => {
    switch (FileSystem.Utils.current_file(model.file_system)) {
    | None => []
    | Some(file) => [
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
          file.editor,
        ),
      ]
    };
  };

  let file_menu = (~globals as _, ~inject as _, _: Model.t) => {
    [];
  };

  let top_bar = (~globals as _, ~inject as _, _model: Model.t) => {
    [Virtual_dom.Vdom.Node.div([])];
  };
};

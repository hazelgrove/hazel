open Haz3lcore;
open Util;
open OptUtil.Syntax;

/* This file follows conventions in [docs/ui-architecture.md] */

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type path = list(string);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type file = {
    path,
    name: string,
    editor: CellEditor.Model.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type folder = {
    path,
    name: string,
    children: list(path),
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type file_system_node =
    | File(file)
    | Folder(folder);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type file_system = Maps.StringMap.t(file_system_node);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    file_system, // root folder of the project. must be Folder(_)
    current: path // path to currently opened file
  };

  module Persistent = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type persistent_file = {
      path,
      name: string,
      editor: CellEditor.Model.persistent,
    };

    [@deriving (show({with_path: false}), sexp, yojson)]
    type persistent_folder = {
      path,
      name: string,
      children: list(path),
    };

    [@deriving (show({with_path: false}), sexp, yojson)]
    type persistent_file_system_node =
      | File(persistent_file)
      | Folder(persistent_folder);

    [@deriving (show({with_path: false}), sexp, yojson)]
    type persistent_file_system =
      Maps.StringMap.t(persistent_file_system_node);

    [@deriving (show({with_path: false}), sexp, yojson)]
    type persistent = {
      file_system: persistent_file_system,
      current: path,
    };
  };

  let rec persist_file_system =
          (fs: file_system): Persistent.persistent_file_system => {
    Maps.StringMap.map(
      fun
      | File(file: file) =>
        Persistent.File({
          path: file.path,
          name: file.name,
          editor: CellEditor.Model.persist(file.editor),
        })
      | Folder(folder: folder) => {
          Persistent.Folder({
            path: folder.path,
            name: folder.name,
            children: folder.children,
          });
        },
      fs,
    );
  };

  let persist = (model: t): Persistent.persistent => {
    file_system: persist_file_system(model.file_system),
    current: model.current,
  };

  let rec unpersist_file_system =
          (~settings, pfs: Persistent.persistent_file_system): file_system => {
    Maps.StringMap.map(
      fun
      | Persistent.File(p_file: Persistent.persistent_file) =>
        File({
          path: p_file.path,
          name: p_file.name,
          editor: CellEditor.Model.unpersist(~settings, p_file.editor),
        })
      | Persistent.Folder(p_folder: Persistent.persistent_folder) => {
          Folder({
            path: p_folder.path,
            name: p_folder.name,
            children: p_folder.children,
          });
        },
      pfs,
    );
  };

  let unpersist = (~settings, p: Persistent.persistent): t => {
    file_system: unpersist_file_system(~settings, p.file_system),
    current: [""] // TODO: Unpersist current file path, defaulting to root for now
  };

  let split_path = (path: string): list(string) => {
    String.split_on_char('/', path) |> List.filter(s => String.length(s) > 0);
  };

  module Utils = {
    let string_of_path = (path: path): string => {
      // Purpose: Namely for StringMap indexing
      // Eg. ["src"] -> "/src"
      // Eg. ["src", "Main.ml"] -> "/src/Main.ml"
      // Eg. [""] -> "/"
      "/" ++ String.concat("/", path);
    };

    let path_of_string = (path: string): path => {
      // Namely for
      // Eg. "/src" -> ["src"]
      // Eg. "/src/Main.ml" -> ["src", "Main.ml"]
      // Eg. "/" -> [""]
      let parts = String.split_on_char('/', path);
      if (List.length(parts) == 0) {
        [""];
      } else if (List.hd(parts) == "") {
        List.tl(parts);
      } else {
        parts;
      };
    };

    let current_file = (model: t): option(file) => {
      switch (
        Maps.StringMap.find_opt(
          string_of_path(model.current),
          model.file_system,
        )
      ) {
      | Some(File(file)) => Some(file)
      | _ => None
      };
    };

    let find_opt = (model: t, path: path): option(file_system_node) => {
      Maps.StringMap.find_opt(string_of_path(path), model.file_system);
    };
  };
};

module Store = {
  include Store.F({
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = Model.Persistent.persistent;
    let key = Store.Project;
    let default: unit => t =
      () => {
        let root = ""; // root name is the empty string
        {
          file_system: {
            let root_project_folder: Model.Persistent.persistent_folder = {
              path: [root],
              name: root,
              children: [],
            };
            let file_system =
              Maps.StringMap.singleton(
                root,
                Model.Persistent.Folder(root_project_folder),
              );
            file_system;
          },
          current: [root],
        };
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
    | SwitchFile(Model.path)
    | AddNewFile(Model.path, string) // parent folder path, new file name;
    | AddNewFolder(Model.path, string); // parent folder path, new folder name;

  let can_undo = (_action: t) => {
    false;
  };

  let export_project = (model: Model.t): unit => {
    let persistent = Model.persist(model);
    Store.save(persistent);
    let data = Store.export();
    JsUtil.download_string_file(
      ~filename="project.hazel",
      ~content_type="text/plain",
      ~contents=data,
    );
  };

  type action_result =
    | Success(Model.t)
    | Failure(string);

  let add_new_file_with_name =
      (model: Model.t, parent_folder_name: string, name: string)
      : action_result =>
    if (String.contains(name, '/')) {
      Failure("File name cannot contain '/' character.");
    } else {
      switch (Maps.StringMap.find_opt(parent_folder_name, model.file_system)) {
      | Some(Model.Folder(parent_folder)) =>
        let new_path = parent_folder.path @ [name];
        switch (Model.Utils.find_opt(model, new_path)) {
        | Some(_) =>
          Failure("A file or folder with that name already exists.")
        | None =>
          let new_file: Model.file = {
            path: new_path,
            name,
            editor: CellEditor.Model.mk(Editor.Model.mk(Zipper.init())),
          };
          let updated_parent_folder: Model.folder = {
            ...parent_folder,
            children: parent_folder.children @ [new_path],
          };
          let new_file_system =
            model.file_system
            |> Maps.StringMap.add(
                 Model.Utils.string_of_path(new_path),
                 Model.File(new_file),
               )
            |> Maps.StringMap.add(
                 parent_folder_name,
                 Model.Folder(updated_parent_folder),
               );
          Success({
            file_system: new_file_system,
            current: new_path,
          });
        };
      | Some(File(_)) =>
        Failure("Cannot add file to a file. Please select a folder.")
      | None => Failure("Parent folder does not exist.")
      };
    };

  let add_new_folder_with_name =
      (model: Model.t, parent_folder_name: string, name: string)
      : action_result =>
    if (String.contains(name, '/')) {
      Failure("Folder name cannot contain '/' character.");
    } else {
      switch (Maps.StringMap.find_opt(parent_folder_name, model.file_system)) {
      | Some(Model.Folder(parent_folder)) =>
        let new_path = parent_folder.path @ [name];
        switch (Model.Utils.find_opt(model, new_path)) {
        | Some(_) =>
          Failure("A file or folder with that name already exists.")
        | None =>
          let new_folder: Model.folder = {
            path: new_path,
            name,
            children: [],
          };
          let updated_parent_folder: Model.folder = {
            ...parent_folder,
            children: parent_folder.children @ [new_path],
          };
          let new_file_system =
            model.file_system
            |> Maps.StringMap.add(
                 Model.Utils.string_of_path(new_path),
                 Model.Folder(new_folder),
               )
            |> Maps.StringMap.add(
                 parent_folder_name,
                 Model.Folder(updated_parent_folder),
               );
          Success({
            file_system: new_file_system,
            current: new_path,
          });
        };
      | Some(File(_)) =>
        Failure("Cannot add folder to a file. Please select a folder.")
      | None => Failure("Parent folder does not exist.")
      };
    };

  let update =
      (~globals, ~schedule_action: t => unit, action: t, model: Model.t)
      : Updated.t(Model.t) => {
    switch (action) {
    | CellAction(cell_action) => Updated.return_quiet(model)
    | SwitchFile(path) =>
      Updated.return_quiet({
        ...model,
        current: path,
      })
    | AddNewFile(parent_folder_path, name) =>
      switch (
        add_new_file_with_name(
          model,
          Model.Utils.string_of_path(parent_folder_path),
          name,
        )
      ) {
      | Success(new_model) => Updated.return(new_model)
      | Failure(error_msg) => raise(Failure(error_msg))
      }
    | AddNewFolder(parent_folder_path, name) =>
      switch (
        add_new_folder_with_name(
          model,
          Model.Utils.string_of_path(parent_folder_path),
          name,
        )
      ) {
      | Success(new_model) => Updated.return(new_model)
      | Failure(error_msg) => raise(Failure(error_msg))
      }
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
    switch (Model.Utils.current_file(model)) {
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
          Model.Utils.string_of_path(file.path),
          Model.File(updated_file),
          model.file_system,
        );
      {
        ...model,
        file_system: updated_file_system,
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
      let file = Model.Utils.current_file(model);
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
        let* file = Model.Utils.current_file(model);
        CellEditor.Selection.handle_key_event(~selection, ~event, file.editor)
        |> Option.map(x => Update.CellAction(x));
      }
    | TextBox => None
    };

  let jump_to_tile = (tile: Id.t, model: Model.t): option((Update.t, t)) => {
    let* file = Model.Utils.current_file(model);
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
    switch (Model.Utils.current_file(model)) {
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

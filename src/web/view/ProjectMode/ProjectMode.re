open Haz3lcore;
open Util;
open OptUtil.Syntax;

/* This file follows conventions in [docs/ui-architecture.md] */

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type project = {
    id: Id.t,
    name: string,
    file_system: FileSystem.Model.t,
    // agent: Agent.Model.t
    // ...
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    projects: Id.Map.t(project),
    current: Id.t,
    // ...
  };
};

module Persistent = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type project = {
    id: Id.t,
    name: string,
    file_system: FileSystem.Persistent.t,
    // agent: Agent.Persistent.t
    // ...
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    projects: Id.Map.t(project),
    current: Id.t,
    // ...
  };

  let persist = (model: Model.t): t => {
    projects:
      Id.Map.map(
        (project: Model.project) =>
          {
            id: project.id,
            name: project.name,
            file_system: FileSystem.Persistent.persist(project.file_system),
          },
        model.projects,
      ),
    current: model.current,
  };

  let unpersist = (~settings, p: t): Model.t => {
    projects:
      Id.Map.map(
        (p: project): Model.project =>
          {
            id: p.id,
            name: p.name,
            file_system:
              FileSystem.Persistent.unpersist(~settings, p.file_system),
          },
        p.projects,
      ),
    current: p.current,
  };
};

module Utils = {
  let current_project = (model: Model.t): Model.project => {
    Id.Map.find_opt(model.current, model.projects)
    |> OptUtil.get_or_fail("Current project not found");
  };

  let add_project = (model: Model.t, project: Model.project): Model.t => {
    {
      // Adds new binding or overwrites the existing binding to the project map

      ...model,
      projects: Id.Map.add(project.id, project, model.projects),
    };
  };

  let mk_new_project = (name: string): Model.project => {
    let id = Id.mk();
    {
      id,
      name,
      file_system: FileSystem.Utils.init(),
    };
  };

  let sorted_projects = (model: Model.t): list(Model.project) => {
    // We sort projects by name
    Id.Map.bindings(model.projects)
    |> List.map(((_, project: Model.project)) => project)
    |> List.sort((a: Model.project, b: Model.project) =>
         String.compare(a.name, b.name)
       );
  };
};

module Store = {
  include Store.F({
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = Persistent.t;
    let key = Store.Project;
    let default: unit => t =
      () => {
        let root = "MyHazelProject";
        let file_system: FileSystem.Persistent.t =
          FileSystem.Persistent.persist(FileSystem.Utils.init());
        let id = Id.mk();
        let project: Persistent.project = {
          id,
          name: root,
          file_system,
        };
        {
          projects: Id.Map.singleton(id, project),
          current: id,
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
      (
        ~settings,
        ~globals,
        ~schedule_action: t => unit,
        action: t,
        model: Model.t,
      )
      : Updated.t(Model.t) => {
    let curr_project = Utils.current_project(model);
    switch (action) {
    | CellAction(a) =>
      switch (FileSystem.Utils.current_file(curr_project.file_system)) {
      | None => Updated.return_quiet(model)
      | Some(file) =>
        let* new_ed = CellEditor.Update.update(~settings, a, file.editor);
        let new_file = {
          ...file,
          editor: new_ed,
        };
        let updated_file_system =
          FileSystem.Utils.add_file_system(
            file.path,
            FileSystem.Model.File(new_file),
            curr_project.file_system,
          );
        Utils.add_project(
          model,
          {
            ...curr_project,
            file_system: updated_file_system,
          },
        );
      }
    | FileSystemAction(file_system_action) =>
      let* new_fs =
        FileSystem.Update.update(
          file_system_action,
          curr_project.file_system,
        );
      Utils.add_project(
        model,
        {
          ...curr_project,
          file_system: new_fs,
        },
      );
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
    let curr_project = Utils.current_project(model);
    switch (FileSystem.Utils.current_file(curr_project.file_system)) {
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
        FileSystem.Utils.add_file_system(
          file.path,
          FileSystem.Model.File(updated_file),
          curr_project.file_system,
        );
      Utils.add_project(
        model,
        {
          ...curr_project,
          file_system: updated_file_system,
        },
      );
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
      let file =
        FileSystem.Utils.current_file(
          Utils.current_project(model).file_system,
        );
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
        let* file =
          FileSystem.Utils.current_file(
            Utils.current_project(model).file_system,
          );
        CellEditor.Selection.handle_key_event(~selection, ~event, file.editor)
        |> Option.map(x => Update.CellAction(x));
      }
    | TextBox => None
    };

  let jump_to_tile = (tile: Id.t, model: Model.t): option((Update.t, t)) => {
    let* file =
      FileSystem.Utils.current_file(
        Utils.current_project(model).file_system,
      );
    CellEditor.Selection.jump_to_tile(tile, file.editor)
    |> Option.map(((x, y)) => (Update.CellAction(x), Cell(y)));
  };
};

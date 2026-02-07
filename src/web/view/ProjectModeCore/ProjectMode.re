open Util;
open OptUtil.Syntax;

/* This file follows conventions in [docs/ui-architecture.md] */

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    projects: Id.Map.t(Project.Model.t),
    current: Id.t,
    // ...
  };
};

module Persistent = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    projects: Id.Map.t(Project.Persistent.t),
    current: Id.t,
    // ...
  };

  let persist = (model: Model.t): t => {
    projects: Id.Map.map(Project.Persistent.persist, model.projects),
    current: model.current,
  };

  let unpersist = (~settings, p: t): Model.t => {
    projects:
      Id.Map.map(Project.Persistent.unpersist(~settings), p.projects),
    current: p.current,
  };
};

module Utils = {
  let find_project = (id: Id.t, model: Model.t): option(Project.Model.t) => {
    Id.Map.find_opt(id, model.projects);
  };

  let current_project = (model: Model.t): Project.Model.t => {
    find_project(model.current, model)
    |> OptUtil.get_or_fail("Current project not found");
  };

  let get_editor = (model: Model.t): CellEditor.Model.t => {
    current_project(model).file_system
    |> FileSystem.Utils.current_file
    |> Option.map((file: FileSystem.Model.file) => file.editor)
    |> OptUtil.get_or_fail("No current file");
  };

  let add_project = (model: Model.t, project: Project.Model.t): Model.t => {
    {
      // Adds new binding or overwrites the existing binding to the project map

      ...model,
      projects: Id.Map.add(project.id, project, model.projects),
    };
  };

  let sorted_projects = (model: Model.t): list(Project.Model.t) => {
    // We sort projects by name
    Id.Map.bindings(model.projects)
    |> List.map(((_, project: Project.Model.t)) => project)
    |> List.sort((a: Project.Model.t, b: Project.Model.t) =>
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
        let agent = Agent.Agent.Persistent.persist(Agent.Agent.Utils.init());
        let project: Project.Persistent.t = {
          id,
          name: root,
          file_system,
          agent,
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

  module ProjectMapUpdate = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t =
      | SwitchProject(Id.t)
      | AddNewProject(string)
      | DeleteProject(Id.t)
      | RenameProject(Id.t, string);

    let can_undo = (_action: t) => {
      true;
    };

    let switch_project = (id: Id.t, model: Model.t): Updated.t(Model.t) => {
      {
        ...model,
        current: id,
      }
      |> Updated.return;
    };

    let add_new_project = (name: string, model: Model.t): Updated.t(Model.t) => {
      let project = Project.Utils.mk_new_project(name);
      Utils.add_project(model, project) |> Updated.return;
    };

    let delete_project = (id: Id.t, model: Model.t): Updated.t(Model.t) =>
      if (Id.Map.cardinal(model.projects) <= 1) {
        failwith("Must have at least one project.");
      } else {
        let new_projects = Id.Map.remove(id, model.projects);
        let new_current = Id.Map.choose(new_projects) |> fst;
        let new_model: Model.t = {
          projects: new_projects,
          current: new_current,
        };
        new_model |> Updated.return;
      };

    let rename_project =
        (id: Id.t, name: string, model: Model.t): Updated.t(Model.t) => {
      let project =
        Utils.find_project(id, model)
        |> OptUtil.get_or_fail("Project not found");
      let renamed_project = {
        ...project,
        name,
      };
      Utils.add_project(model, renamed_project) |> Updated.return;
    };

    let update = (action: t, model: Model.t): Updated.t(Model.t) => {
      switch (action) {
      | SwitchProject(id) => switch_project(id, model)
      | AddNewProject(name) => add_new_project(name, model)
      | DeleteProject(id) => delete_project(id, model)
      | RenameProject(id, name) => rename_project(id, name, model)
      };
    };
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | FileSystemAction(FileSystem.Update.t)
    | ProjectMapAction(ProjectMapUpdate.t)
    | Project(Project.Update.action, option(Id.t));

  let can_undo = (_action: t) => {
    true;
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
        settings: Settings.t,
        action: t,
        model: Model.t,
        schedule_action: t => unit,
      )
      : Updated.t(Model.t) => {
    let curr_project = Utils.current_project(model);
    switch (action) {
    | Project(project_action, project_id) =>
      // By passing the project_id, we allow the schedule_action to be scoped to
      // asynchronously updating the specified project in particular,
      // independent of whether the user changes projects or not.
      let schedule_action = (a: Project.Update.action) =>
        schedule_action(Project(a, project_id));
      let project =
        switch (
          {
            open OptUtil.Syntax;
            let* project_id = project_id;
            Utils.find_project(project_id, model);
          }
        ) {
        | None => curr_project
        | Some(project) => project
        };
      let* project =
        Project.Update.update(
          project_action,
          project,
          settings,
          schedule_action,
        );
      Utils.add_project(model, project);
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
    | ProjectMapAction(project_map_action) =>
      ProjectMapUpdate.update(project_map_action, model)
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
                Project(
                  CellAction(
                    ResultAction(
                      UpdateResult(
                        switch (
                          r
                          |> ListUtil.hd_opt
                          |> OptUtil.get_or_fail(
                               "[ProjectMode.Update.calculate] Failed to get result",
                             )
                          |> snd
                        ) {
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
                  None,
                ),
              ),
          ~timeout=
            _ =>
              schedule_action(
                Project(
                  CellAction(
                    ResultAction(UpdateResult(ResultFail(Timeout))),
                  ),
                  None,
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
        Update.Project(CellAction(a), None);
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
        |> Option.map(x => Update.Project(CellAction(x), None));
      }
    | TextBox => None
    };

  let jump_to_tile = (tile: Id.t, model: Model.t): option((Update.t, t)) => {
    let* file =
      FileSystem.Utils.current_file(
        Utils.current_project(model).file_system,
      );
    CellEditor.Selection.jump_to_tile(tile, file.editor)
    |> Option.map(((x, y)) =>
         (Update.Project(CellAction(x), None), Cell(y))
       );
  };
};

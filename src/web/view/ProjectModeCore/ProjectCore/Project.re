open Util;

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    id: Id.t,
    name: string,
    file_system: FileSystem.Model.t,
    agent: Agent.Agent.Model.t,
    // agent: Agent.Model.t
    // ...
  };
};

module Persistent = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    id: Id.t,
    name: string,
    file_system: FileSystem.Persistent.t,
    agent: Agent.Agent.Persistent.t,
    // ...
  };

  let persist = (model: Model.t): t => {
    {
      id: model.id,
      name: model.name,
      file_system: FileSystem.Persistent.persist(model.file_system),
      agent: Agent.Agent.Persistent.persist(model.agent),
    };
  };

  let unpersist = (~settings, p: t): Model.t => {
    {
      id: p.id,
      name: p.name,
      file_system: FileSystem.Persistent.unpersist(~settings, p.file_system),
      agent: Agent.Agent.Persistent.unpersist(p.agent),
    };
  };
};

module Utils = {
  let mk_new_project = (name: string): Model.t => {
    let id = Id.mk();
    {
      id,
      name,
      file_system: FileSystem.Utils.init(),
      agent: Agent.Agent.Utils.init(),
    };
  };

  let get_file =
      (file_path: option(FileSystem.Model.path), model: Model.t)
      : FileSystem.Model.file => {
    let file =
      switch (
        {
          open OptUtil.Syntax;
          let* file_path = file_path;
          FileSystem.Utils.find_opt(file_path, model.file_system);
        }
      ) {
      | Some(file_tree_node) =>
        switch (file_tree_node) {
        | FileSystem.Model.File(file) => Some(file)
        | FileSystem.Model.Folder(_) =>
          FileSystem.Utils.current_file(model.file_system) // fallback
        }
      | None => FileSystem.Utils.current_file(model.file_system) // fallback
      };
    let file =
      file
      |> OptUtil.get_or_fail(
           "No active file is set. As of now, we only support agent actions when there exists an active file (as many agent actions need an editor.).",
         );
    file;
  };
};

module Update = {
  open Updated;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    | AgentAction(Agent.Agent.Update.Action.t, option(FileSystem.Model.path))
    | CellAction(CellEditor.Update.t);

  let update =
      (
        action: action,
        model: Model.t,
        settings: Settings.t,
        schedule_action: action => unit,
      )
      : Updated.t(Model.t) => {
    switch (action) {
    | CellAction(a) =>
      switch (FileSystem.Utils.current_file(model.file_system)) {
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
            model.file_system,
          );
        {
          ...model,
          file_system: updated_file_system,
        };
      }
    | AgentAction(a, file_path) =>
      // Try to find the file by the given path, otherwise fallback to the current file
      let file = Utils.get_file(file_path, model);
      // This allows the agent to continue work on the same file,
      // across asynchronous tool calls
      let schedule_action = (a: Agent.Agent.Update.Action.t) =>
        schedule_action(AgentAction(a, file_path));
      let (new_agent, updated_editor) =
        Agent.Agent.Update.update(
          a,
          model.agent,
          file.editor,
          settings,
          schedule_action,
        );
      let* new_ed = updated_editor;
      let new_file = {
        ...file,
        editor: new_ed,
      };
      let updated_file_system =
        FileSystem.Utils.add_file_system(
          file.path,
          FileSystem.Model.File(new_file),
          model.file_system,
        );
      {
        ...model,
        file_system: updated_file_system,
        agent: new_agent,
      };
    };
  };
};

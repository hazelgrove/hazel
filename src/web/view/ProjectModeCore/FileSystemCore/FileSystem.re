open Util;
open Haz3lcore;

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
    expanded: bool,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type file_tree_node =
    | File(file)
    | Folder(folder);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type file_tree = Maps.StringMap.t(file_tree_node);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    file_tree,
    current: path,
  };
};

module Persistent = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type path = Model.path;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type file = {
    path,
    name: string,
    editor: CellEditor.Model.persistent,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type folder = Model.folder;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type file_tree_node =
    | File(file)
    | Folder(folder);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type file_tree = Maps.StringMap.t(file_tree_node);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    file_tree,
    current: path,
  };

  let persist_file_tree = (ft: Model.file_tree): file_tree => {
    Maps.StringMap.map(
      fun
      | Model.File(file: Model.file) =>
        File({
          path: file.path,
          name: file.name,
          editor: CellEditor.Model.persist(file.editor),
        })
      | Model.Folder(folder: Model.folder) => {
          Folder({
            path: folder.path,
            name: folder.name,
            children: folder.children,
            expanded: folder.expanded,
          });
        },
      ft,
    );
  };

  let persist = (model: Model.t): t => {
    file_tree: persist_file_tree(model.file_tree),
    current: model.current,
  };

  let unpersist_file_tree = (~settings, pft: file_tree): Model.file_tree => {
    Maps.StringMap.map(
      fun
      | File(p_file: file) =>
        Model.File({
          path: p_file.path,
          name: p_file.name,
          editor: CellEditor.Model.unpersist(~settings, p_file.editor),
        })
      | Folder(p_folder: folder) => {
          Model.Folder({
            path: p_folder.path,
            name: p_folder.name,
            children: p_folder.children,
            expanded: p_folder.expanded,
          });
        },
      pft,
    );
  };

  let unpersist = (~settings, p: t): Model.t => {
    file_tree: unpersist_file_tree(~settings, p.file_tree),
    current: [""] /* TODO: Unpersist current file path, defaulting to root for now */,
  };
};

module Utils = {
  let string_of_path = (path: Model.path): string => {
    // Purpose: Namely for StringMap indexing
    // Eg. ["src"] -> "/src"
    // Eg. ["src", "Main.ml"] -> "/src/Main.ml"
    // Eg. [""] -> "/"
    "/" ++ String.concat("/", path);
  };

  let path_of_string = (path: string): Model.path => {
    // Namely for
    // Eg. "/src" -> ["src"]
    // Eg. "/src/Main.ml" -> ["src", "Main.ml"]
    // Eg. "/" -> [""]
    let parts = String.split_on_char('/', path);
    if (List.length(parts) == 0) {
      [""];
    } else if (ListUtil.hd_opt(parts)
               |> OptUtil.get_or_fail(
                    "[FileSystem.Utils.path_of_string] Failed to get first part of path",
                  )
               == "") {
      List.tl(parts);
    } else {
      parts;
    };
  };

  let init = (): Model.t => {
    let root = "";
    {
      file_tree: {
        let root_project_folder: Model.folder = {
          path: [root],
          name: root,
          children: [],
          expanded: true,
        };
        Maps.StringMap.singleton(
          string_of_path([root]),
          Model.Folder(root_project_folder),
        );
      },
      current: [root],
    };
  };

  let find_opt =
      (path: Model.path, model: Model.t): option(Model.file_tree_node) => {
    /* Primary lookup uses the canonical string_of_path key.
       For backwards compatibility, also try the empty-string key for the root. */
    let key = string_of_path(path);
    switch (Maps.StringMap.find_opt(key, model.file_tree)) {
    | Some(node) => Some(node)
    | None =>
      if (path == [""]) {
        Maps.StringMap.find_opt("", model.file_tree);
      } else {
        None;
      }
    };
  };

  let valid_name = (name: string): unit =>
    if (String.contains(name, '/')) {
      failwith("Name cannot contain '/' character.");
    } else if (String.contains(name, '.')) {
      failwith("Name cannot contain '.' character.");
    } else {
      ();
    };

  let current_file = (model: Model.t): option(Model.file) => {
    /* Use Utils.find_opt so we get the same backwards-compatible path lookup
       behaviour as the rest of the file system helpers (in particular, the
       special handling of the legacy root "" key). */
    switch (find_opt(model.current, model)) {
    | Some(File(file)) => Some(file)
    | _ => None
    };
  };

  let add_file_system =
      (path: Model.path, node: Model.file_tree_node, model: Model.t): Model.t => {
    let new_file_tree =
      Maps.StringMap.add(string_of_path(path), node, model.file_tree);
    {
      ...model,
      file_tree: new_file_tree,
    };
  };

  let remove = (path: Model.path, model: Model.t): Model.t => {
    let new_file_tree =
      Maps.StringMap.remove(string_of_path(path), model.file_tree);
    {
      ...model,
      file_tree: new_file_tree,
    };
  };

  let parent_folder = (path: Model.path): Model.path =>
    if (path == [""]) {
      failwith("Cannot get parent folder of the root folder.");
    } else {
      let parent_path = List.rev(List.tl(List.rev(path)));
      parent_path;
    };

  let remove_child_from_folder =
      (child_path: Model.path, folder_path: Model.path, model: Model.t)
      : Model.folder => {
    switch (find_opt(folder_path, model)) {
    | Some(Model.Folder(folder)) =>
      let new_children = List.filter((!=)(child_path), folder.children);
      let new_folder = {
        ...folder,
        children: new_children,
      };
      new_folder;
    | Some(Model.File(_)) =>
      failwith("Path is not a folder. Cannot remove child from a file.")
    | None => failwith("Folder does not exist.")
    };
  };
};

module Update = {
  open Model;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | AddNewFile(path, string)
    | AddNewFolder(path, string)
    | Delete(path) // Deletes the node and recursively deletes all descendants
    | Rename(path, string)
    | SetCurrentFile(path)
    | ToggleFolderExpansion(path);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type result =
    | Success(Model.t)
    | Failure(string);

  let can_undo = (_action: t) => {
    true;
  };

  let add_file =
      (model: Model.t, parent_folder_path: Model.path, name: string): result => {
    Utils.valid_name(name);
    let name = name ++ ".hz";
    switch (Utils.find_opt(parent_folder_path, model)) {
    | Some(Model.Folder(parent_folder)) =>
      let new_path = parent_folder.path @ [name];
      switch (Utils.find_opt(new_path, model)) {
      | Some(_) => Failure("A file or folder with that name already exists.")
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
        let model' =
          Utils.add_file_system(new_path, Model.File(new_file), model);
        let model'' =
          Utils.add_file_system(
            parent_folder_path,
            Model.Folder(updated_parent_folder),
            model',
          );
        Success({
          ...model'',
          current: new_path,
        });
      };
    | Some(File(_)) =>
      Failure("Cannot add file to a file. Please select a folder.")
    | None => Failure("Parent folder does not exist.")
    };
  };

  let add_folder =
      (model: Model.t, parent_folder_path: Model.path, name: string): result => {
    Utils.valid_name(name);
    switch (Utils.find_opt(parent_folder_path, model)) {
    | Some(Model.Folder(parent_folder)) =>
      let new_path = parent_folder_path @ [name];
      switch (Utils.find_opt(new_path, model)) {
      | Some(_) => Failure("A file or folder with that name already exists.")
      | None =>
        let new_folder: Model.folder = {
          path: new_path,
          name,
          children: [],
          expanded: true,
        };
        let updated_parent_folder: Model.folder = {
          ...parent_folder,
          children: parent_folder.children @ [new_path],
        };
        let model' =
          Utils.add_file_system(new_path, Model.Folder(new_folder), model);
        let model'' =
          Utils.add_file_system(
            parent_folder_path,
            Model.Folder(updated_parent_folder),
            model',
          );
        Success(model'');
      };
    | Some(File(_)) =>
      Failure("Cannot add folder to a file. Please select a folder.")
    | None => Failure("Parent folder does not exist.")
    };
  };

  let rec delete = (model: Model.t, path: Model.path): result =>
    if (path == [""]) {
      Failure("Cannot delete the root folder.");
    } else {
      let parent_folder_path = Utils.parent_folder(path);
      switch (Utils.find_opt(path, model)) {
      | Some(Model.File(_)) =>
        let model' = Utils.remove(path, model);
        let parent_folder =
          Utils.remove_child_from_folder(path, parent_folder_path, model');
        let model'' =
          Utils.add_file_system(
            parent_folder_path,
            Model.Folder(parent_folder),
            model',
          );
        Success(model'');
      | Some(Model.Folder(folder)) =>
        let children = folder.children;
        // We fold over the children first and delete them
        // If we ever hit a failure, we return that failure
        let result =
          List.fold_left(
            (res: result, child_path: path) => {
              switch (res) {
              | Success(model') => delete(model', child_path)
              | Failure(error_msg) => Failure(error_msg)
              }
            },
            Success(model),
            children,
          );
        switch (result) {
        | Success(model') =>
          let model'' = Utils.remove(path, model');
          let parent_folder =
            Utils.remove_child_from_folder(path, parent_folder_path, model'');
          let model''' =
            Utils.add_file_system(
              parent_folder_path,
              Model.Folder(parent_folder),
              model'',
            );
          Success(model''');
        | Failure(error_msg) => Failure(error_msg)
        };
      | None => Failure("File or folder does not exist.")
      };
    };

  let rec rename_paths =
          (model: Model.t, old_path: Model.path, new_path: Model.path): result => {
    switch (Utils.find_opt(old_path, model)) {
    | Some(Model.File(file)) =>
      // Rename the file
      let model' = Utils.remove(old_path, model);
      let new_file: Model.file = {
        ...file,
        path: new_path,
      };
      let model'' =
        Utils.add_file_system(new_path, Model.File(new_file), model');
      // Update the name of the path in the parent folder
      let parent_folder_path = Utils.parent_folder(old_path);
      let parent_folder =
        Utils.remove_child_from_folder(old_path, parent_folder_path, model'');
      let new_parent_folder: Model.folder = {
        ...parent_folder,
        children: parent_folder.children @ [new_path],
      };
      let model''' =
        Utils.add_file_system(
          parent_folder_path,
          Model.Folder(new_parent_folder),
          model'',
        );
      // Done
      Success(model''');
    | Some(Model.Folder(folder)) =>
      // Recursively rename the child paths to account for the changed name in the new path
      let result =
        List.fold_left(
          (res: result, child_path: path) => {
            let new_child_path = new_path @ [List.hd(List.rev(child_path))];
            switch (res) {
            | Success(model') =>
              rename_paths(model', child_path, new_child_path)
            | Failure(error_msg) => Failure(error_msg)
            };
          },
          Success(model),
          folder.children,
        );
      switch (result) {
      | Success(model') =>
        // Rename the folder
        let model'' = Utils.remove(old_path, model');
        let new_folder: Model.folder = {
          ...folder,
          path: new_path,
        };
        let model''' =
          Utils.add_file_system(new_path, Model.Folder(new_folder), model'');
        // Done
        // Update the name of the path in the parent folder
        let parent_folder_path = Utils.parent_folder(old_path);
        let parent_folder =
          Utils.remove_child_from_folder(
            old_path,
            parent_folder_path,
            model''',
          );
        let new_parent_folder: Model.folder = {
          ...parent_folder,
          children: parent_folder.children @ [new_path],
        };
        let model'''' =
          Utils.add_file_system(
            parent_folder_path,
            Model.Folder(new_parent_folder),
            model''',
          );
        // Done
        Success(model'''');
      | Failure(error_msg) => Failure(error_msg)
      };
    | None => Failure("File or folder does not exist.")
    };
  };

  let rename = (model: Model.t, path: Model.path, new_name: string): result =>
    if (path == [""]) {
      Failure("Cannot rename the root project folder.");
    } else {
      Utils.valid_name(new_name);
      switch (Utils.find_opt(path, model)) {
      | Some(Model.File(file)) =>
        let new_name = new_name ++ ".hz";
        let model' = Utils.remove(path, model);
        let new_file: Model.file = {
          ...file,
          name: new_name,
        };
        let model'' =
          Utils.add_file_system(path, Model.File(new_file), model');
        let new_path = List.rev(List.tl(List.rev(path))) @ [new_name];
        rename_paths(model'', path, new_path);
      | Some(Model.Folder(folder)) =>
        let model' = Utils.remove(path, model);
        let new_folder: Model.folder = {
          ...folder,
          name: new_name,
        };
        let model'' =
          Utils.add_file_system(path, Model.Folder(new_folder), model');
        let new_path = List.rev(List.tl(List.rev(path))) @ [new_name];
        rename_paths(model'', path, new_path);
      | None => Failure("File or folder does not exist.")
      };
    };

  let update = (action: t, model: Model.t): Updated.t(Model.t) =>
    switch (action) {
    | AddNewFile(path, name) =>
      switch (add_file(model, path, name)) {
      | Success(new_model) => new_model |> Updated.return
      | Failure(error_msg) => failwith(error_msg)
      }
    | AddNewFolder(path, name) =>
      switch (add_folder(model, path, name)) {
      | Success(new_model) => new_model |> Updated.return
      | Failure(error_msg) => failwith(error_msg)
      }
    | Delete(path) =>
      switch (delete(model, path)) {
      | Success(new_model) => new_model |> Updated.return
      | Failure(error_msg) => failwith(error_msg)
      }
    | Rename(path, new_name) =>
      switch (rename(model, path, new_name)) {
      | Success(new_model) => new_model |> Updated.return
      | Failure(error_msg) => failwith(error_msg)
      }
    | SetCurrentFile(path) =>
      {
        ...model,
        current: path,
      }
      |> Updated.return
    | ToggleFolderExpansion(path) =>
      switch (Utils.find_opt(path, model)) {
      | Some(Model.Folder(folder)) =>
        let updated_folder = {
          ...folder,
          expanded: !folder.expanded,
        };
        Utils.add_file_system(path, Model.Folder(updated_folder), model)
        |> Updated.return_quiet;
      | Some(Model.File(_)) =>
        failwith("Path is not a folder. Cannot toggle expansion on a file.")
      | None => failwith("Folder does not exist.")
      }
    };
};

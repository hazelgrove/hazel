open Util;
open Haz3lcore;
open AgentResult;
open AgentModel;

[@deriving (show({with_path: false}), sexp, yojson)]
type action = CompositionActions.action;

[@deriving (show({with_path: false}), sexp, yojson)]
type result =
  | Success(Model.t, Updated.t(CellEditor.Model.t))
  | Failure(string);

let update =
    (
      ~settings: Settings.t,
      action: action,
      agent: Model.t,
      editor: CodeWithStatics.Model.t,
      chat_id: Id.t,
    )
    : Result.t((Model.t, CodeWithStatics.Model.t)) => {
  switch (action) {
  | EditorAction(agent_editor_action) =>
    let action = Action.Structural(agent_editor_action);
    let updated_editor =
      Editor.Update.update(
        ~settings=settings.core,
        action,
        editor.statics,
        editor.dynamics,
        editor.editor,
      );
    switch (updated_editor) {
    | Ok(updated_editor) =>
      Ok((
        agent,
        CodeWithStatics.Model.{
          editor: updated_editor,
          statics: editor.statics,
          dynamics: editor.dynamics,
          context_menu: editor.context_menu,
        },
      ))
    | Error(err) =>
      switch (err) {
      | Action.Failure.Composition_action_failure(msg) =>
        Error(Failure.Info(msg))
      | _ =>
        Error(
          Failure.Info(
            Action.Failure.show(err)
            ++ " (structural editor tool could not be applied)",
          ),
        )
      }
    };
  | LanguageServerAction(_) =>
    Error(Failure.Info("LanguageServerAction is not implemented yet"))
  | InsertAtProgramBoundary(direction, code) =>
    /* No-path variant of insert_before/insert_after.
       - Before: move caret to program start, then paste code (prepend).
       - After: move caret to program end, then paste code (append).
       For an empty program (just `?`), either boundary effectively
       seeds the program with the provided code. */
    let z = editor.editor.state.zipper;
    let mk_statics = CompositionGo.Public.mk_statics;
    let initial_info_map = mk_statics(z);
    let return = (error: Action.Failure.t, z: option(Zipper.t)) =>
      Result.of_option(~error, z);
    let z_at_boundary =
      switch ((direction: Action.Structural.insert_target)) {
      | Before => Move.to_start(z)
      | After => Move.to_end(z)
      };
    switch (
      CompositionGo.Local.PerformUtils.introduce(
        z_at_boundary,
        "\n" ++ code ++ "\n",
        return,
      )
    ) {
    | Error(_) =>
      Error(Failure.Info("Failed to insert code at program boundary"))
    | Ok(new_z) =>
      let new_statics = mk_statics(new_z);
      let old_errors = ErrorPrint.all(initial_info_map);
      let new_errors = ErrorPrint.all(new_statics);
      if (List.length(new_errors) > List.length(old_errors)) {
        Error(
          Failure.Info(
            "Not applying the action you requested as it would introduce new static error(s): "
            ++ String.concat(", ", new_errors),
          ),
        );
      } else {
        let new_z = Dump.to_zipper(new_z);
        let new_editor_model = Editor.Model.mk(new_z);
        let new_code_with_statics =
          CodeWithStatics.Model.mk(new_editor_model);
        Ok((agent, new_code_with_statics));
      };
    };
  | WorkbenchAction(workbench_action) =>
    let action = AgentWorkbench.Update.Action.BackendAction(workbench_action);
    let chat_system =
      ChatSystem.Update.update(
        ChatSystem.Update.Action.ChatAction(
          Chat.Update.Action.WorkbenchAction(action),
          chat_id,
        ),
        agent.chat_system,
      );
    switch (chat_system) {
    | Ok(updated_chat_system) =>
      Ok((
        {
          ...agent,
          chat_system: updated_chat_system,
        },
        editor,
      ))
    | Error(error) => Error(error)
    };
  | AgentContextAction(agent_context_action) =>
    let action = agent_context_action;
    let chat_system =
      ChatSystem.Update.update(
        ChatSystem.Update.Action.ChatAction(
          Chat.Update.Action.AgentContextAction(action),
          chat_id,
        ),
        agent.chat_system,
      );
    switch (chat_system) {
    | Ok(updated_chat_system) =>
      Ok((
        {
          ...agent,
          chat_system: updated_chat_system,
        },
        editor,
      ))
    | Error(error) => Error(error)
    };
  | ProbeAction(probe_action) =>
    let z = editor.editor.state.zipper;
    let info_map = CompositionGo.Public.mk_statics(z);
    switch (HighLevelNodeMap.build(z, info_map)) {
    | None =>
      Error(
        Failure.Info(
          "No bindings in the program to probe. Add let/type bindings first.",
        ),
      )
    | Some(node_map) =>
      let syntax = CachedSyntax.init(z);
      let resolve_path = (path: string): option(Id.t) =>
        HighLevelNodeMap.Public.path_to_id_opt(node_map, path);

      let apply_probe_action =
          (z: Zipper.t, paths: list(string))
          : (Zipper.t, list(string), list(string)) => {
        List.fold_left(
          ((z, expanded, unresolved), path) =>
            switch (resolve_path(path)) {
            | Some(id) =>
              switch (probe_action) {
              | PlaceProbe(_) =>
                let z = ProbePerform.add_manual(~syntax, id, info_map, z);
                (z, [path, ...expanded], unresolved);
              | RemoveProbe(_) =>
                let target_ids =
                  ProbePerform.target_subterm_ids(id, info_map);
                let z = ProbePerform.rm_manual(target_ids, z);
                (z, expanded, unresolved);
              | ToggleProbe(_) =>
                let z = ProbePerform.toggle_manual(~syntax, id, ~info_map, z);
                let has_probe = ProbePerform.has_probe(id, z);
                let expanded = has_probe ? [path, ...expanded] : expanded;
                (z, expanded, unresolved);
              }
            | None => (z, expanded, [path, ...unresolved])
            },
          (z, [], []),
          paths,
        );
      };

      let paths =
        switch (probe_action) {
        | PlaceProbe(p)
        | RemoveProbe(p)
        | ToggleProbe(p) => p
        };
      let (new_z, paths_to_expand, unresolved_paths) =
        apply_probe_action(z, paths);
      if (List.length(paths) > 0
          && List.length(unresolved_paths) == List.length(paths)) {
        Error(
          Failure.Info(
            "Probe tool did not update the program: no path resolved to a binding. "
            ++ "Unresolved path(s): "
            ++ String.concat(", ", List.rev(unresolved_paths))
            ++ ". Paths must be **HighLevelNodeMap binding paths** (e.g. \"map\", \"filter\", or \"outer/inner\" for nested lets).",
          ),
        );
      } else {
        let new_z = Dump.to_zipper(new_z);
        let new_editor_model = Editor.Model.mk(new_z);
        let new_cws =
          CodeWithStatics.Model.mk(
            ~dynamics=editor.dynamics,
            new_editor_model,
          );

        /* Auto-expand probed definitions so results are visible */
        if (List.length(paths_to_expand) > 0) {
          let expand_action = AgentContext.Update.Expand(paths_to_expand);
          let chat_system =
            ChatSystem.Update.update(
              ChatSystem.Update.Action.ChatAction(
                Chat.Update.Action.AgentContextAction(expand_action),
                chat_id,
              ),
              agent.chat_system,
            );
          switch (chat_system) {
          | Ok(updated_chat_system) =>
            Ok((
              {
                ...agent,
                chat_system: updated_chat_system,
              },
              new_cws,
            ))
          | Error(_) => Ok((agent, new_cws))
          };
        } else {
          Ok((agent, new_cws));
        };
      };
    };
  | StaticsAction(statics_action) =>
    let z = editor.editor.state.zipper;
    let info_map = CompositionGo.Public.mk_statics(z);
    switch (HighLevelNodeMap.build(z, info_map)) {
    | None =>
      Error(
        Failure.Info(
          "No bindings in the program. Add let/type bindings first.",
        ),
      )
    | Some(node_map) =>
      let syntax = CachedSyntax.init(z);
      let resolve_path = (path: string): option(Id.t) =>
        HighLevelNodeMap.Public.path_to_id_opt(node_map, path);

      let apply_statics_action =
          (z: Zipper.t, paths: list(string))
          : (Zipper.t, list(string), list(string)) => {
        List.fold_left(
          ((z, expanded, unresolved), path) =>
            switch (resolve_path(path)) {
            | Some(id) =>
              switch (statics_action) {
              | PlaceStatics(_) =>
                let z =
                  ProbePerform.place_statics_at(~syntax, id, info_map, z);
                let expanded =
                  switch (
                    ProbePerform.probe_status(id, info_map, z.refractors)
                  ) {
                  | Statics(_) => [path, ...expanded]
                  | _ => expanded
                  };
                (z, expanded, unresolved);
              | RemoveStatics(_) =>
                let z =
                  ProbePerform.remove_statics_at(~syntax, id, info_map, z);
                (z, expanded, unresolved);
              | ToggleStatics(_) =>
                let z =
                  ProbePerform.toggle_statics_at(~syntax, id, info_map, z);
                let expanded =
                  switch (
                    ProbePerform.probe_status(id, info_map, z.refractors)
                  ) {
                  | Statics(_) => [path, ...expanded]
                  | _ => expanded
                  };
                (z, expanded, unresolved);
              }
            | None => (z, expanded, [path, ...unresolved])
            },
          (z, [], []),
          paths,
        );
      };

      let paths =
        switch (statics_action) {
        | PlaceStatics(p)
        | RemoveStatics(p)
        | ToggleStatics(p) => p
        };
      let (new_z, paths_to_expand, unresolved_paths) =
        apply_statics_action(z, paths);
      if (List.length(paths) > 0
          && List.length(unresolved_paths) == List.length(paths)) {
        Error(
          Failure.Info(
            "Statics tool did not update the program: no path resolved to a binding. "
            ++ "Unresolved path(s): "
            ++ String.concat(", ", List.rev(unresolved_paths))
            ++ ". Paths must be **HighLevelNodeMap binding paths** (e.g. \"map\", \"filter\", or \"outer/inner\" for nested lets).",
          ),
        );
      } else {
        let new_z = Dump.to_zipper(new_z);
        let new_editor_model = Editor.Model.mk(new_z);
        let new_cws =
          CodeWithStatics.Model.mk(
            ~dynamics=editor.dynamics,
            new_editor_model,
          );

        if (List.length(paths_to_expand) > 0) {
          let expand_action = AgentContext.Update.Expand(paths_to_expand);
          let chat_system =
            ChatSystem.Update.update(
              ChatSystem.Update.Action.ChatAction(
                Chat.Update.Action.AgentContextAction(expand_action),
                chat_id,
              ),
              agent.chat_system,
            );
          switch (chat_system) {
          | Ok(updated_chat_system) =>
            Ok((
              {
                ...agent,
                chat_system: updated_chat_system,
              },
              new_cws,
            ))
          | Error(_) => Ok((agent, new_cws))
          };
        } else {
          Ok((agent, new_cws));
        };
      };
    };
  | SyntaxProjectorAction(syntax_projector_action) =>
    let z = editor.editor.state.zipper;
    let info_map = CompositionGo.Public.mk_statics(z);
    switch (HighLevelNodeMap.build(z, info_map)) {
    | None =>
      Error(
        Failure.Info(
          "No bindings in the program. Add let/type bindings first.",
        ),
      )
    | Some(node_map) =>
      let syntax = CachedSyntax.init(z);
      let resolve_path = (path: string): option(Id.t) =>
        HighLevelNodeMap.Public.path_to_syntax_projector_target_id_opt(
          node_map,
          path,
        );

      let apply_syntax_projector_action =
          (z: Zipper.t, paths: list(string)): (Zipper.t, list(string), int) => {
        List.fold_left(
          ((z, expanded, n_placed), path) =>
            switch (resolve_path(path)) {
            | Some(id) =>
              let z_opt =
                switch (syntax_projector_action) {
                | PlaceSyntaxProjector(kind, _) =>
                  ProjectorPerform.try_place_syntax_projector(
                    ~term_data=syntax.term_data,
                    id,
                    kind,
                    z,
                  )
                | ToggleSyntaxProjector(kind, _) =>
                  ProjectorPerform.try_toggle_syntax_projector(
                    ~term_data=syntax.term_data,
                    id,
                    kind,
                    z,
                  )
                | RemoveSyntaxProjector(_) =>
                  ProjectorPerform.try_remove_syntax_projector(
                    ~term_data=syntax.term_data,
                    id,
                    z,
                  )
                };
              switch (z_opt) {
              | Some(z2) => (z2, [path, ...expanded], n_placed + 1)
              | None => (z, expanded, n_placed)
              };
            | None => (z, expanded, n_placed)
            },
          (z, [], 0),
          paths,
        );
      };

      let paths =
        switch (syntax_projector_action) {
        | PlaceSyntaxProjector(_, p)
        | ToggleSyntaxProjector(_, p)
        | RemoveSyntaxProjector(p) => p
        };
      let (new_z, paths_to_expand, n_placed) =
        apply_syntax_projector_action(z, paths);
      if (List.length(paths) > 0 && n_placed == 0) {
        Error(
          Failure.Info(
            "Syntax projector tool did not update the program: no path produced a change. "
            ++ "Paths must be **HighLevelNodeMap binding paths** (e.g. \"map\", \"filter\", or \"outer/inner\" for nested lets), "
            ++ "not pretty-printed expressions or type-applied text from statics overlays. "
            ++ "If a path matched but placement failed, the term at that binding may not support this projector kind.",
          ),
        );
      } else {
        let new_z = Dump.to_zipper(new_z);
        let new_editor_model = Editor.Model.mk(new_z);
        let new_cws =
          CodeWithStatics.Model.mk(
            ~dynamics=editor.dynamics,
            new_editor_model,
          );

        if (List.length(paths_to_expand) > 0) {
          let expand_action = AgentContext.Update.Expand(paths_to_expand);
          let chat_system =
            ChatSystem.Update.update(
              ChatSystem.Update.Action.ChatAction(
                Chat.Update.Action.AgentContextAction(expand_action),
                chat_id,
              ),
              agent.chat_system,
            );
          switch (chat_system) {
          | Ok(updated_chat_system) =>
            Ok((
              {
                ...agent,
                chat_system: updated_chat_system,
              },
              new_cws,
            ))
          | Error(_) => Ok((agent, new_cws))
          };
        } else {
          Ok((agent, new_cws));
        };
      };
    };
  };
};

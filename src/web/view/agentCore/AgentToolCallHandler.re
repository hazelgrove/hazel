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

/** The elaborated form cached alongside [syntax], for projectors that key their
    init off elaboration ([[ProjectorInit]]'s [elaborate_syntax] kinds, e.g.
    table). Falls back to the empty elaboration, in which case such projectors
    simply decline to attach. */
let elaborated_of_syntax = (syntax: CachedSyntax.t): Language.Exp.t =>
  switch (syntax.shape_elaborated) {
  | Some(e) => e
  | None => CachedStatics.empty.elaborated
  };

/** Shared dispatch for path-indexed overlay tools (probes, statics, syntax
    projectors). Each such tool takes a list of HighLevelNodeMap paths and
    runs a per-path operation; afterwards any paths that were actually
    placed are auto-expanded via an AgentContext.Expand on the chat system.

    [resolve_path] maps a path string to an Id using the node map. Probes and
    statics use [HighLevelNodeMap.Public.path_to_id_opt]; syntax projectors
    use [path_to_syntax_projector_target_id_opt].

    [perform] applies the action to one resolved id. [Some((z', should_expand))]
    means the operation changed state; [None] means resolved-but-noop (e.g.
    the term does not support this projector kind).

    If every input path either failed to resolve or was a resolved-noop, we
    return an Error so the agent gets a concrete failure signal instead of a
    silent-success tool message. */
let apply_overlay_action =
    (
      ~tool_label: string,
      ~resolve_path: (HighLevelNodeMap.t, string) => option(Id.t),
      ~perform:
         (~info_map: _, ~syntax: CachedSyntax.t, Zipper.t, Id.t) =>
         option((Zipper.t, bool)),
      ~paths: list(string),
      ~agent: Model.t,
      ~editor: CodeWithStatics.Model.t,
      ~chat_id: Id.t,
    )
    : Result.t((Model.t, CodeWithStatics.Model.t)) => {
  let z = editor.editor.state.zipper;
  let root = editor.editor.root;
  let info_map = CompositionGo.Public.mk_statics(~root, z);
  switch (CompositionGo.Public.node_map_of(~root, z)) {
  | None =>
    Error(
      Failure.Info(
        "No bindings in the program. Add let/type bindings first.",
      ),
    )
  | Some(node_map) =>
    let syntax = CachedSyntax.init(~root, z);
    let (new_z, paths_to_expand, n_changed, unresolved) =
      List.fold_left(
        ((z, expanded, n_changed, unresolved), path) =>
          switch (resolve_path(node_map, path)) {
          | Some(id) =>
            switch (perform(~info_map, ~syntax, z, id)) {
            | Some((z', should_expand)) =>
              let expanded = should_expand ? [path, ...expanded] : expanded;
              (z', expanded, n_changed + 1, unresolved);
            | None => (z, expanded, n_changed, unresolved)
            }
          | None => (z, expanded, n_changed, [path, ...unresolved])
          },
        (z, [], 0, []),
        paths,
      );
    if (List.length(paths) > 0 && n_changed == 0) {
      let unresolved_sfx =
        switch (unresolved) {
        | [] => ""
        | ps =>
          " Unresolved path(s): " ++ String.concat(", ", List.rev(ps)) ++ "."
        };
      Error(
        Failure.Info(
          tool_label
          ++ " tool did not update the program: no path produced a change."
          ++ unresolved_sfx
          ++ " Paths must be **HighLevelNodeMap binding paths** (e.g. \"map\", \"outer/inner\" for nested lets, \"M/helper\" or \"^graph/update\" for module/livelit members, \"name#k\" for duplicates).",
        ),
      );
    } else {
      let new_z = Materialize.all(new_z, ~root);
      let new_editor_model = Editor.Model.mk(new_z, ~root);
      let new_cws =
        CodeWithStatics.Model.mk(~dynamics=editor.dynamics, new_editor_model);

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

let update =
    (
      ~settings: Settings.t,
      action: action,
      agent: Model.t,
      editor: CodeWithStatics.Model.t,
      chat_id: Id.t,
    )
    : Result.t((Model.t, CodeWithStatics.Model.t)) => {
  /* stamp agent activity so the canvas paces the resulting updates
     into distinct beats (CanvasBuffer) */
  CanvasBuffer.note_agent_action();
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
      CodeWithStatics.StaticsDebounce.force_on_next := true;
      Ok((
        agent,
        CodeWithStatics.Model.{
          editor: updated_editor,
          /* the old statics describe the program BEFORE this edit: kept,
             they made the tool's canvas snapshot blank (its beat showed
             nothing) and the content landed later as an anonymous state
             change. Empty = the snapshot computes this program's (a
             DefStatics memo hit) and the editor's calculate refreshes. */
          statics: CachedStatics.empty,
          dynamics: editor.dynamics,
          context_menu: editor.context_menu,
        },
      ));
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
    /* No-path variant of insert_before/insert_after: the code at the
       program's start (Before) or end (After), at a Mod root as members
       (CompositionGo.Public.insert_at_boundary). For an empty program
       (just `?`), either boundary effectively seeds the program with the
       provided code. */
    let z = editor.editor.state.zipper;
    let root = editor.editor.root;
    /* the editor's statics for this program when it has them; the new
       program's computed the editor's way, per item (the editor's
       recompute is a DefStatics memo hit) */
    let eff_settings =
      Language.CoreSettings.{
        ...settings.core,
        probe_all: settings.core.probe_all && !Util.AgentPulse.in_burst(),
      };
    let initial_info_map =
      editor.statics.info_map != Id.Map.empty
        ? editor.statics.info_map
        : CompositionGo.Public.statics_for(~settings=eff_settings, ~root, z);
    /* inserted code arrives indentation-stripped; re-indent its new
       lines like user Paste */
    let before_pieces =
      LocalReformat.snapshot_pieces(~enabled=settings.core.auto_reindent, z);
    switch (
      CompositionGo.Public.insert_at_boundary(~root, direction, z, code)
    ) {
    | Error(Action.Failure.Composition_action_failure(msg)) =>
      Error(Failure.Info(msg))
    | Error(_) =>
      Error(Failure.Info("Failed to insert code at program boundary"))
    | Ok(new_z) =>
      let new_statics =
        CompositionGo.Public.statics_for(
          ~settings=eff_settings,
          ~root,
          new_z,
        );
      let old_errors = ErrorPrint.all(initial_info_map);
      let new_errors = ErrorPrint.all(new_statics);
      if (List.length(new_errors) > List.length(old_errors)) {
        Error(
          Failure.Info(
            "Not applying the action you requested as it would introduce new static error(s): "
            ++ String.concat(", ", new_errors)
            ++ CompositionGo.Local.PerformUtils.reserved_word_note(code),
          ),
        );
      } else {
        let final_z =
          CompositionGo.Local.PerformUtils.normalize_top_level(
            ~root,
            ~before=z,
            CompositionGo.Local.mentions_trigger(code)
              ? Materialize.all(new_z, ~root) : new_z,
          )
          |> LocalReformat.go_region(~before_pieces);
        let new_editor_model = Editor.Model.mk(final_z, ~root);
        let new_code_with_statics =
          CodeWithStatics.Model.mk(new_editor_model);
        CodeWithStatics.StaticsDebounce.force_on_next := true;
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
    let paths =
      switch (probe_action) {
      | PlaceProbe(p)
      | RemoveProbe(p)
      | ToggleProbe(p) => p
      };
    apply_overlay_action(
      ~tool_label="Probe",
      ~resolve_path=HighLevelNodeMap.Public.path_to_id_opt,
      ~perform=
        (~info_map, ~syntax, z, id) =>
          switch (probe_action) {
          | PlaceProbe(_) =>
            let z = ProbePerform.add_manual(~syntax, id, info_map, z);
            Some((z, true));
          | RemoveProbe(_) =>
            let target_ids = ProbeTargets.target_subterm_ids(id, info_map);
            let z = ProbePerform.rm_manual(target_ids, z);
            Some((z, false));
          | ToggleProbe(_) =>
            let z = ProbePerform.toggle_manual(~syntax, id, ~info_map, z);
            let has_probe = ProbePerform.has_probe(id, z);
            Some((z, has_probe));
          },
      ~paths,
      ~agent,
      ~editor,
      ~chat_id,
    );
  | StaticsAction(statics_action) =>
    let paths =
      switch (statics_action) {
      | PlaceStatics(p)
      | RemoveStatics(p)
      | ToggleStatics(p) => p
      };
    apply_overlay_action(
      ~tool_label="Statics",
      ~resolve_path=HighLevelNodeMap.Public.path_to_id_opt,
      ~perform=
        (~info_map, ~syntax, z, id) => {
          let (z, should_expand) =
            switch (statics_action) {
            | PlaceStatics(_) =>
              let z = ProbePerform.place_statics_at(~syntax, id, info_map, z);
              let expand =
                switch (ProbeTargets.probe_status(id, info_map, z.refractors)) {
                | Statics(_) => true
                | _ => false
                };
              (z, expand);
            | RemoveStatics(_) =>
              let z = ProbePerform.remove_statics_at(id, info_map, z);
              (z, false);
            | ToggleStatics(_) =>
              let z = ProbePerform.toggle_statics(~syntax, id, info_map, z);
              let expand =
                switch (ProbeTargets.probe_status(id, info_map, z.refractors)) {
                | Statics(_) => true
                | _ => false
                };
              (z, expand);
            };
          Some((z, should_expand));
        },
      ~paths,
      ~agent,
      ~editor,
      ~chat_id,
    );
  | SyntaxProjectorAction(syntax_projector_action) =>
    let paths =
      switch (syntax_projector_action) {
      | PlaceSyntaxProjector(_, p)
      | ToggleSyntaxProjector(_, p)
      | RemoveSyntaxProjector(p) => p
      };
    apply_overlay_action(
      ~tool_label="Syntax projector",
      ~resolve_path=HighLevelNodeMap.Public.path_to_syntax_projector_target_id_opt,
      ~perform=
        (~info_map as _, ~syntax, z, id) => {
          let z_opt =
            switch (syntax_projector_action) {
            | PlaceSyntaxProjector(kind, _) =>
              ProjectorPerform.try_place_syntax_projector(
                ~term_data=syntax.term_data,
                ~elaborated=elaborated_of_syntax(syntax),
                id,
                kind,
                z,
              )
            | ToggleSyntaxProjector(kind, _) =>
              ProjectorPerform.try_toggle_syntax_projector(
                ~term_data=syntax.term_data,
                ~elaborated=elaborated_of_syntax(syntax),
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
          z_opt |> Option.map(z' => (z', true));
        },
      ~paths,
      ~agent,
      ~editor,
      ~chat_id,
    );
  };
};

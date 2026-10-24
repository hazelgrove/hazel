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
    projectors). Each such tool takes a list of paths (EntryPath) and runs a
    per-path operation; afterwards any paths that were actually placed are
    auto-expanded via an AgentContext.Expand on the chat system.

    [resolve_path] maps a path to an id: probes and statics take the
    entry's, syntax projectors its definition's.

    [perform] applies the action to one resolved id. [Some((z', should_expand))]
    means the operation changed state; [None] means resolved-but-noop (e.g.
    the term does not support this projector kind).

    If every input path either failed to resolve or was a resolved-noop, we
    return an Error so the agent gets a concrete failure signal instead of a
    silent-success tool message. */
/* the entry [path] names: its anchor, the binding's term */
let entry_id = (~root, seg, path: string): option(Id.t) =>
  switch (EntryPath.resolve_string(~root, seg, path)) {
  | Ok(f) => Some(f.id)
  | Error(_) => None
  };

/* its definition's, where a syntax projector goes */
let definition_id = (~root, seg, path: string): option(Id.t) =>
  Option.bind(entry_id(~root, seg, path), head =>
    EntryEdit.part(~root, head, Definition, seg)
  );

let apply_overlay_action =
    (
      ~settings: Settings.t,
      ~tool_label: string,
      ~resolve_path: (~root: Sort.t, Segment.t, string) => option(Id.t),
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
  let seg = Zipper.unselect_and_zip(~erase_buffer=true, z);
  /* the editor's statics when they describe this program, else per item
     (DefStatics), as the edit check computes them */
  let info_map =
    switch (
      CachedStatics.for_zipper(~settings=settings.core, z, editor.statics)
    ) {
    | Some(st) when st.info_map != Id.Map.empty => st.info_map
    | _ =>
      CompositionGo.statics_of(
        ~settings=settings.core,
        ~root,
        ~probe_ids=CachedStatics.probe_ids_of_zipper(z),
        ~shared=editor.editor.syntax.t_cache,
        seg,
      ).
        info_map
    };

  /* the editor's syntax when it describes [z], else made with its
     incremental caches */
  let syntax = {
    let s = editor.editor.syntax;
    let (relatives, selection, _) = s.zipped;
    !s.old && relatives === z.relatives && selection === z.selection
      ? s
      : Util.PerfTimer.time("syntax", () =>
          CachedSyntax.init(~root, ~m_cache=s.m_cache, ~t_cache=s.t_cache, z)
        );
  };
  let (new_z, paths_to_expand, n_changed, unresolved) =
    List.fold_left(
      ((z, expanded, n_changed, unresolved), path) =>
        switch (resolve_path(~root, seg, path)) {
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
        ++ " Paths name entries (e.g. \"map\", \"outer/inner\" for nested lets, \"M/helper\" for module members, \"name#k\" for duplicates).",
      ),
    );
  } else {
    let new_z = Materialize.all(new_z, ~root);
    /* the editor's syntax, marked for its recalculation, keeps its
       caches */
    let new_editor_model = {
      ...editor.editor,
      state: {
        zipper: new_z,
        col_target: None,
      },
      syntax: CachedSyntax.mark_old(editor.editor.syntax),
    };
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
      CodeWithStatics.StaticsDebounce.force_on_next := true;
      Ok((
        agent,
        CodeWithStatics.Model.{
          editor: updated_editor,
          statics: editor.statics,
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
      ~settings,
      ~tool_label="Probe",
      ~resolve_path=entry_id,
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
      ~settings,
      ~tool_label="Statics",
      ~resolve_path=entry_id,
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
      ~settings,
      ~tool_label="Syntax projector",
      ~resolve_path=definition_id,
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

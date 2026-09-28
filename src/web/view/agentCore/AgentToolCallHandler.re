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
          ++ " Paths must be **HighLevelNodeMap binding paths** (e.g. \"map\", \"filter\", or \"outer/inner\" for nested lets).",
        ),
      );
    } else {
      let new_z = Dump.to_zipper(new_z, ~root=Exp);
      let new_editor_model = Editor.Model.mk(new_z, ~root=Exp);
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

/** The program's final expression: through binding bodies and the tail of
    `;` sequences, so new tests follow every binding and any existing tests. */
let rec final_expression = (e: Language.Exp.t): Language.Exp.t =>
  switch (Language.Exp.term_of(e)) {
  | Let(_, _, body)
  | TyAlias(_, _, body)
  | ModuleExp(_, _, body)
  | Seq(_, body) => final_expression(body)
  | _ => e
  };

/** Insert one `test e end;` line per expression just before the final
    expression. Refused if it adds static errors, like every insert. */
let insert_tests =
    (z: Zipper.t, tests: list(string)): Stdlib.result(Zipper.t, string) => {
  let code =
    tests |> List.map(e => "test " ++ e ++ " end;") |> String.concat("\n");
  let final_id =
    Language.Exp.rep_id(
      final_expression(MakeTerm.from_zip_for_sem(z, ~root=Exp).term),
    );
  switch (
    CompositionGo.Local.PerformUtils.insert_term(
      z,
      final_id,
      "\n" ++ code ++ "\n",
      Direction.Left,
      CachedSyntax.init(z),
    )
  ) {
  | Error(Action.Failure.Composition_action_failure(msg)) => Error(msg)
  | Error(_) =>
    Error("Could not place the tests before the final expression.")
  | Ok(new_z) =>
    let errors = z => ErrorPrint.all(CompositionGo.Public.mk_statics(z));
    let new_errors = errors(new_z);
    List.length(new_errors) > List.length(errors(z))
      ? Error(
          "Not adding the tests: they would introduce static error(s): "
          ++ String.concat(", ", new_errors),
        )
      : Ok(
          CompositionGo.Local.PerformUtils.normalize_top_level(
            Dump.to_zipper(new_z, ~root=Exp),
          ),
        );
  };
};

/* [resolved] holds Jev's answers for this reply's Jev-backed calls
   (modify_view, jev_edit), fetched over HTTP before the synchronous tool
   fold runs ([[AgentResponse.resolve_jev_then_handle]]). */
let rec update =
        (
          ~settings: Settings.t,
          ~resolved: AgentJev.resolved=AgentJev.unresolved,
          action: action,
          agent: Model.t,
          editor: CodeWithStatics.Model.t,
          chat_id: Id.t,
        )
        : Result.t((Model.t, CodeWithStatics.Model.t)) => {
  switch (action) {
  /* Same blindness as expand/collapse in the view arm: in the edit arm the
     planner writes code only through jev_edit. */
  | EditorAction(_)
  | InsertAtProgramBoundary(_) when settings.agent_globals.jev_edit_tool =>
    Error(
      Failure.Info(
        "Direct edit tools are not available here. Call jev_edit with a sketch.",
      ),
    )
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
    /* No-path insert_before / insert_after; see
       [[CompositionGo.Public.insert_at_boundary]]. */
    switch (
      CompositionGo.Public.insert_at_boundary(
        editor.editor.state.zipper,
        direction,
        code,
      )
    ) {
    | Error(msg) => Error(Failure.Info(msg))
    | Ok(new_z) =>
      Ok((
        agent,
        CodeWithStatics.Model.mk(Editor.Model.mk(new_z, ~root=Exp)),
      ))
    }
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
  /* Hidden from the tool list in the Jev arm, but the prompt still teaches
     them, so a model may call them from memory; refuse rather than let the
     arm quietly fall back to manual navigation. */
  | AgentContextAction(Expand(_) | Collapse(_))
      when settings.agent_globals.jev_view_tool =>
    Error(
      Failure.Info(
        "expand/collapse are not available here. Call modify_view with what you need to see.",
      ),
    )
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
  | ModifyView(intent, replace) =>
    switch (List.assoc_opt(intent, resolved.views)) {
    | _ when !settings.agent_globals.jev_view_tool =>
      Error(Failure.Info("modify_view is not enabled in this session."))
    | None =>
      Error(
        Failure.Info(
          "modify_view could not be resolved; the view is unchanged. Retry with a more concrete intent.",
        ),
      )
    | Some(selection) when selection.metrics.failed =>
      Error(
        Failure.Info(
          "The view selector failed; the view is unchanged. Retry modify_view.",
        ),
      )
    | Some(selection) =>
      update(
        ~settings,
        AgentContextAction(
          replace
            ? SetSuggested(selection.open_paths)
            : AddSuggested(selection.open_paths),
        ),
        agent,
        editor,
        chat_id,
      )
    }
  | AddTests(_) when !settings.agent_globals.jev_edit_tool =>
    Error(Failure.Info("add_tests is not enabled in this session."))
  | AddTests(tests) =>
    switch (insert_tests(editor.editor.state.zipper, tests)) {
    | Error(msg) => Error(Failure.Info(msg))
    | Ok(new_z) =>
      Ok((
        agent,
        CodeWithStatics.Model.mk(Editor.Model.mk(new_z, ~root=Exp)),
      ))
    }
  | JevEdit(request) =>
    switch (
      AgentJev.find_edit(~globals=settings.agent_globals, resolved, request)
    ) {
    | _ when !settings.agent_globals.jev_edit_tool =>
      Error(Failure.Info("jev_edit is not enabled in this session."))
    | None =>
      Error(
        Failure.Info(
          "jev_edit could not be resolved; the program is unchanged.",
        ),
      )
    | Some({metrics: {failed: true, error, _}, _}) =>
      Error(
        Failure.Info(
          "jev_edit failed; the program is unchanged: "
          ++ Option.value(~default="no reason given", error),
        ),
      )
    | Some(result)
        when
          AgentJev.program_text(editor.editor.state.zipper)
          != result.base_text =>
      Error(
        Failure.Info(
          "The program changed earlier in this reply; call jev_edit again.",
        ),
      )
    | Some(result) =>
      /* Rebuilt like a boundary insert: fresh statics and dynamics for the
         new program. */
      let z = Dump.to_zipper(result.zipper, ~root=Exp);
      Ok((agent, CodeWithStatics.Model.mk(Editor.Model.mk(z, ~root=Exp))));
    }
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

open Util;
open Haz3lcore;
open AgentResult;
open AgentModel;

/* an exception escaping a tool is a bug in OUR code: with the js-error
   build its JS stack names the OCaml function, so print it */
let report_tool_exn = (tool: string, exn: exn): unit =>
  Js_of_ocaml.(
    switch (Js_error.of_exn(exn)) {
    | Some(e) =>
      Firebug.console##error_2(
        Js.string("[tool " ++ tool ++ "] " ++ Printexc.to_string(exn)),
        Js.string(Option.value(~default="", Js_error.stack(e))),
      )
    | None =>
      Firebug.console##error(
        Js.string("[tool " ++ tool ++ "] " ++ Printexc.to_string(exn)),
      )
    }
  );

module Utils = AgentUtils;
module ToolCallHandler = AgentToolCallHandler;

let add_to_active_subtask =
    (
      ~tool_result: AgentToolResult.tool_result,
      ~model: Model.t,
      ~chat_id: Id.t,
    )
    : Model.t => {
  ...model,
  chat_system:
    ChatSystem.Update.update(
      ChatSystem.Update.Action.ChatAction(
        Chat.Update.Action.WorkbenchAction(
          AgentWorkbench.Update.Action.UIAction(
            AgentWorkbench.Update.Action.UIAction.AddToolResultToActiveSubtask(
              tool_result,
            ),
          ),
        ),
        chat_id,
      ),
      model.chat_system,
    )
    |> ChatSystem.Update.get,
};

let add_tool_result_to_active_subtask =
    (
      ~tool_result: AgentToolResult.tool_result,
      ~action: CompositionActions.action,
      ~model: Model.t,
      ~chat_id: Id.t,
    )
    : Model.t => {
  switch (action) {
  // Only add editor and context tools to subtask tool results for now
  // i.e. don't include workbench tool calls in the workbench view lol
  | EditorAction(_)
  | AgentContextAction(_) =>
    add_to_active_subtask(~tool_result, ~model, ~chat_id)
  | _ => model
  };
};

/* an overlay tool's change: the whole program before and after */
let mk_diff =
    (
      ~old_editor: Editor.t,
      ~new_editor: Editor.t,
      action: CompositionActions.action,
    )
    : option(AgentToolResult.diff) => {
  switch (action) {
  | SyntaxProjectorAction(_)
  | ProbeAction(_)
  | StaticsAction(_) =>
    let old_segment =
      Zipper.unselect_and_zip(~erase_buffer=true, old_editor.state.zipper);
    let new_segment =
      Zipper.unselect_and_zip(~erase_buffer=true, new_editor.state.zipper);
    let old_s = CompositionView.Public.print_segment(old_segment);
    let new_s = CompositionView.Public.print_segment(new_segment);
    if (old_s == new_s) {
      None;
    } else {
      Some(
        AgentToolResult.{
          old_text: old_s,
          new_text: Some(new_s),
        },
      );
    };
  | _ => None
  };
};

let mk_segment_snapshots =
    (
      ~old_editor: Editor.t,
      ~new_editor: Editor.t,
      action: CompositionActions.action,
    )
    : (option(string), option(string)) => {
  switch (action) {
  | EditorAction(_)
  | ProbeAction(_)
  | StaticsAction(_)
  | SyntaxProjectorAction(_) =>
    let text = (ed: Editor.t) =>
      PersistentZipper.to_string(ed.state.zipper) ++ "\n";
    (Some(text(old_editor)), Some(text(new_editor)));
  | _ => (None, None)
  };
};

/** Run one tool; returns chat message to append (caller batches append + one LLM request). */
let execute_one_tool_call =
    (
      ~tool_call: OpenRouter.Reply.Model.tool_call,
      ~model: Model.t,
      ~cell_editor: CellEditor.Model.t,
      ~settings: Settings.t,
      ~chat_id: Id.t,
    )
    : (Model.t, Updated.t(CellEditor.Model.t), Message.Model.t) => {
  switch (
    CompositionUtils.Public.action_of(
      ~tool_name=tool_call.name,
      ~args=tool_call.args,
    )
  ) {
  | Action(action) =>
    switch (
      try(
        Util.PerfTimer.time("tool/handler", () =>
          ToolCallHandler.update(
            ~settings,
            action,
            model,
            cell_editor.editor,
            chat_id,
          )
        )
      ) {
      | Failure(msg) as exn =>
        report_tool_exn(tool_call.name, exn);
        Error(Failure.Info(msg));
      | exn =>
        /* Catch all exceptions (e.g. Path not found) — report to agent, do not break state */
        report_tool_exn(tool_call.name, exn);
        Error(Failure.Info(Printexc.to_string(exn)));
      }
    ) {
    | Ok((model, editor)) =>
      /* the context the agent reads is rebuilt right before each send
         (AgentSend); rebuilding it after every tool as well was most of a
         tool call's cost (statics + fold + print of the whole program) and
         nothing read it in between */
      let success_message =
        "The "
        ++ tool_call.name
        ++ " tool call was successful and has been applied to the model.";
      let (before_text, after_text) =
        Util.PerfTimer.time("tool/snapshots", () =>
          mk_segment_snapshots(
            ~old_editor=cell_editor.editor.editor,
            ~new_editor=editor.editor,
            action,
          )
        );
      let diff_result =
        try(
          Ok(
            Util.PerfTimer.time("tool/diff", () =>
              mk_diff(
                ~old_editor=cell_editor.editor.editor,
                ~new_editor=editor.editor,
                action,
              )
            ),
          )
        ) {
        | exn => Error(exn)
        };
      switch (diff_result) {
      | Error(exn) =>
        /* mk_diff can raise (e.g. path_to_id); report to agent, keep state */
        let msg = Printexc.to_string(exn);
        let tool_result: AgentToolResult.tool_result = {
          tool_call,
          success: false,
          skipped: false,
          expanded: false,
          diff: None,
          before_text:
            Some(
              PersistentZipper.to_string(
                cell_editor.editor.editor.state.zipper,
              )
              ++ "\n",
            ),
          after_text: None,
          content: msg,
          content_is_payload: false,
          steps: [],
        };
        let model =
          add_tool_result_to_active_subtask(
            ~tool_result,
            ~action,
            ~model,
            ~chat_id,
          );
        (
          model,
          cell_editor |> Updated.return_quiet,
          Message.Utils.mk_tool_result_message(tool_result),
        );
      | Ok(diff) =>
        let tool_result: AgentToolResult.tool_result = {
          tool_call,
          success: true,
          skipped: false,
          expanded: false,
          diff,
          before_text,
          after_text,
          content: success_message,
          content_is_payload: false,
          steps: [],
        };
        let model =
          add_tool_result_to_active_subtask(
            ~tool_result,
            ~action,
            ~model,
            ~chat_id,
          );
        (
          model,
          {
            ...cell_editor,
            editor,
          }
          |> Updated.return,
          Message.Utils.mk_tool_result_message(tool_result),
        );
      };
    | Error(error) =>
      switch (error) {
      | Failure.Info(msg) =>
        let before_text =
          switch (action) {
          | EditorAction(_) =>
            Some(
              PersistentZipper.to_string(
                cell_editor.editor.editor.state.zipper,
              )
              ++ "\n",
            )
          | _ => None
          };
        let tool_result: AgentToolResult.tool_result = {
          tool_call,
          success: false,
          skipped: false,
          expanded: false,
          diff: None,
          before_text,
          after_text: None,
          content: msg,
          content_is_payload: false,
          steps: [],
        };
        let model =
          add_tool_result_to_active_subtask(
            ~tool_result,
            ~action,
            ~model,
            ~chat_id,
          );
        (
          model,
          cell_editor |> Updated.return_quiet,
          Message.Utils.mk_tool_result_message(tool_result),
        );
      }
    }
  | DocsRequest(topic) =>
    /* Answered from the DocPacks registry; no editor change */
    let (success, content) =
      switch (DocPacks.lookup(topic)) {
      | Some(pack) => (true, pack.body)
      | None => (
          false,
          "Unknown docs topic \""
          ++ topic
          ++ "\". Available topics:\n"
          ++ DocPacks.topic_lines,
        )
      };
    let tool_result: AgentToolResult.tool_result = {
      tool_call,
      success,
      skipped: false,
      expanded: false,
      diff: None,
      before_text: None,
      after_text: None,
      content,
      content_is_payload: true,
      steps: [],
    };
    (
      model,
      cell_editor |> Updated.return_quiet,
      Message.Utils.mk_tool_result_message(tool_result),
    );
  | Failure(msg) =>
    let tool_result: AgentToolResult.tool_result = {
      tool_call,
      success: false,
      skipped: false,
      expanded: false,
      diff: None,
      before_text: None,
      after_text: None,
      content: msg,
      content_is_payload: false,
      steps: [],
    };
    // Do not add unparseable tool calls to subtask tool results for now
    (
      model,
      cell_editor |> Updated.return_quiet,
      Message.Utils.mk_tool_result_message(tool_result),
    );
  };
};

/* What a reply's edits did to the tests: evaluation runs after the reply,
   so its last edit's result takes the line once the follow-up request
   finds the evaluation settled (AgentSend.handle_dispatch_send) */
module Tests = {
  type pending = {
    chat_id: Id.t,
    /* the reply's last edit's result message */
    message: Id.t,
    root: Sort.t,
    before_program: Segment.t,
    before: option(Language.TestResults.t),
    after_program: Segment.t,
  };
  let pending: ref(option(pending)) = ref(None);

  let status = (s: Language.TestStatus.t): string =>
    switch (s) {
    | Pass => "passing"
    | Fail => "failing"
    | Indet => "indeterminate"
    };

  /* each test by path, its status; [r]'s tests in [seg] */
  let by_path =
      (~root, seg: Segment.t, r: Language.TestResults.t)
      : list((Id.t, string, Language.TestStatus.t)) =>
    List.map2(
      ((id, _), s) =>
        (
          id,
          Option.value(
            CompositionGo.where_is(~root, seg, id),
            ~default="a test",
          ),
          s,
        ),
      r.test_map,
      r.statuses,
    );

  /* the tests whose status changed, by path; a test is matched by id,
     else by path */
  let changed =
      (
        ~root,
        ~before_program,
        ~after_program,
        before: option(Language.TestResults.t),
        after: Language.TestResults.t,
      )
      : string => {
    let was =
      Option.fold(~none=[], ~some=by_path(~root, before_program), before);
    let lines =
      List.filter_map(
        ((id, path, s)) => {
          let prev =
            switch (List.find_opt(((id', _, _)) => id' == id, was)) {
            | Some((_, _, s')) => Some(s')
            | None =>
              Option.map(
                ((_, _, s')) => s',
                List.find_opt(((_, p, _)) => p == path, was),
              )
            };
          switch (prev) {
          | Some(s') when s' == s => None
          | Some(s') =>
            Some(path ++ " " ++ status(s') ++ " → " ++ status(s))
          | None => Some(path ++ " new, " ++ status(s))
          };
        },
        by_path(~root, after_program, after),
      );
    switch (lines, after.total) {
    | (_, 0) => "Tests: none."
    | ([], n) =>
      "Tests: no change ("
      ++ string_of_int(after.passing)
      ++ " of "
      ++ string_of_int(n)
      ++ " passing)."
    | (lines, _) => "Tests changed: " ++ String.concat("; ", lines) ++ "."
    };
  };

  /* the pending reply's result, given the line, once its chat's program
     has settled results */
  let report =
      (
        ~chat_id: Id.t,
        results: option(Language.TestResults.t),
        chat_system: ChatSystem.Model.t,
      )
      : ChatSystem.Model.t =>
    switch (pending^, results) {
    | (Some(p), Some(after)) when p.chat_id == chat_id =>
      pending := None;
      let chat = ChatSystem.Utils.find_chat(chat_id, chat_system);
      switch (Chat.Utils.find_message_opt(p.message, chat)) {
      | Some({role: ToolResult(tr), _} as msg) =>
        let line =
          changed(
            ~root=p.root,
            ~before_program=p.before_program,
            ~after_program=p.after_program,
            p.before,
            after,
          );
        let made =
          Message.Utils.mk_tool_result_message({
            ...tr,
            content: tr.content ++ " " ++ line,
          });
        ChatSystem.Utils.update_chat(
          Chat.Utils.update_message(
            {
              ...made,
              id: msg.id,
              timestamp: msg.timestamp,
              children: msg.children,
              current_child: msg.current_child,
            },
            chat,
          ),
          chat_system,
        );
      | _ => chat_system
      };
    | _ => chat_system
    };
};

/* a tool whose change is the editor's: a reply's rollback undoes it */
let changes_editor = (tr: AgentToolResult.tool_result): bool =>
  switch (
    CompositionUtils.Public.action_of(
      ~tool_name=tr.tool_call.name,
      ~args=tr.tool_call.args,
    )
  ) {
  | Action(ProbeAction(_) | StaticsAction(_) | SyntaxProjectorAction(_)) =>
    true
  | _ => false
  };

/* A reply's tool calls. Its structural calls are one transaction
   (CompositionGo.Reply): each runs on the program as the calls before it
   left it, which the editor shows the reply's other tools, and their
   results wait for the reply's end, which checks the finished change. A
   failure anywhere in a reply with edits restores the editor the reply
   began with, and every result says which call failed */
module Reply = {
  let start =
      (~settings: Settings.t, ce: CellEditor.Model.t): CompositionGo.Reply.t => {
    let z = ce.editor.editor.state.zipper;
    let root = ce.editor.editor.root;
    let statics_of =
      CompositionGo.statics_of(
        ~settings=settings.core,
        ~root,
        ~probe_ids=CachedStatics.probe_ids_of_zipper(z),
        ~shared=ce.editor.editor.syntax.t_cache,
      );
    let seg = Zipper.unselect_and_zip(~erase_buffer=true, z);
    CompositionGo.Reply.start(
      ~checks={
        no_new_errors: settings.agent_globals.check_no_new_errors,
        dangling_uses: settings.agent_globals.check_dangling_uses,
      },
      ~root,
      ~statics=
        lazy(
          switch (
            CachedStatics.for_zipper(
              ~settings=settings.core,
              z,
              ce.editor.statics,
            )
          ) {
          | Some(st) when st.info_map != Id.Map.empty => st
          | _ => statics_of(seg)
          }
        ),
      ~statics_of,
      seg,
    );
  };

  /* [ce] showing [seg], its caret and probes kept */
  let showing = (ce: CellEditor.Model.t, seg: Segment.t): CellEditor.Model.t => {
    let ed = ce.editor.editor;
    {
      ...ce,
      editor: {
        ...ce.editor,
        editor: {
          ...ed,
          state: {
            zipper: CompositionGo.rezip(ed.state.zipper, seg),
            col_target: None,
          },
        },
      },
    };
  };

  let text_of = (seg: Segment.t): string =>
    PersistentZipper.to_string(Zipper.unzip(seg)) ++ "\n";

  let result =
      (
        ~tool_call: OpenRouter.Reply.Model.tool_call,
        ~root,
        ~report: string,
        m: CompositionGo.made,
      )
      : AgentToolResult.tool_result => {
    tool_call,
    success: true,
    skipped: false,
    expanded: false,
    diff:
      Option.map(
        ((old_text, new_text)) =>
          AgentToolResult.{
            old_text,
            new_text,
          },
        CompositionGo.diff(~root, m),
      ),
    before_text: Some(text_of(m.before)),
    after_text: Some(text_of(m.change.program)),
    content: tool_call.name ++ ": " ++ report,
    content_is_payload: true,
    steps: CompositionGo.step_records(~root, m),
  };

  let failed =
      (~tool_call: OpenRouter.Reply.Model.tool_call, ~before, content: string)
      : AgentToolResult.tool_result => {
    tool_call,
    success: false,
    skipped: false,
    expanded: false,
    diff: None,
    before_text: before,
    after_text: None,
    content,
    content_is_payload: false,
    steps: [],
  };

  type slot =
    /* a structural call, its result at the reply's end */
    | Edit(OpenRouter.Reply.Model.tool_call)
    | Done(Message.Model.t)
    | Skipped(OpenRouter.Reply.Model.tool_call);

  let merge =
      (a: Updated.t(CellEditor.Model.t), b: Updated.t(CellEditor.Model.t))
      : Updated.t(CellEditor.Model.t) => {
    model: b.model,
    is_edit: a.is_edit || b.is_edit,
    recalculate: a.recalculate || b.recalculate,
    scroll_active: a.scroll_active || b.scroll_active,
    logged: a.logged || b.logged,
    historic: a.historic || b.historic,
    tentative: a.tentative || b.tentative,
    retract: a.retract || b.retract,
  };

  /* [tool_calls] in order until one fails: the model, the editor (one
     update, so one undo step), and the results' messages in order */
  let run =
      (
        ~settings: Settings.t,
        ~chat_id: Id.t,
        ~model: Model.t,
        ~cell_editor: CellEditor.Model.t,
        tool_calls: list(OpenRouter.Reply.Model.tool_call),
      )
      : (Model.t, Updated.t(CellEditor.Model.t), list(Message.Model.t)) => {
    let calls = Array.of_list(tool_calls);
    let rec go = (i, m, ce: Updated.t(CellEditor.Model.t), edits, slots) =>
      if (i == Array.length(calls)) {
        (m, ce, edits, List.rev(slots), None);
      } else {
        let tc = calls[i];
        switch (
          CompositionUtils.Public.action_of(~tool_name=tc.name, ~args=tc.args)
        ) {
        | Action(EditorAction(call)) =>
          let r = Option.value(edits, ~default=start(~settings, ce.model));
          let seg =
            Zipper.unselect_and_zip(
              ~erase_buffer=true,
              ce.model.editor.editor.state.zipper,
            );
          switch (
            try(CompositionGo.Reply.add(r, ~index=i, ~seg, call)) {
            | exn =>
              report_tool_exn(tc.name, exn);
              Error(Printexc.to_string(exn));
            }
          ) {
          | Ok(r) =>
            let shown =
              showing(ce.model, CompositionGo.Reply.program(r))
              |> Updated.return;
            go(i + 1, m, merge(ce, shown), Some(r), [Edit(tc), ...slots]);
          | Error(message) => (
              m,
              ce,
              Some(r),
              List.rev([Edit(tc), ...slots]),
              Some((i, message)),
            )
          };
        | _ =>
          let (m, step, msg) =
            execute_one_tool_call(
              ~tool_call=tc,
              ~model=m,
              ~cell_editor=ce.model,
              ~settings,
              ~chat_id,
            );
          let ce = merge(ce, step);
          switch (msg.role) {
          | ToolResult(tr) when !tr.skipped && !tr.success => (
              m,
              ce,
              edits,
              List.rev([Done(msg), ...slots]),
              Some((i, "")),
            )
          | _ => go(i + 1, m, ce, edits, [Done(msg), ...slots])
          };
        };
      };
    let (model, ce, edits, slots, failure) =
      go(0, model, cell_editor |> Updated.return_quiet, None, []);
    let slots =
      slots
      @ List.map(
          tc => Skipped(tc),
          ListUtil.remove_first_n(List.length(slots), tool_calls),
        );
    /* the edits checked: their results, or the call that failed */
    let (ce, results, failure) =
      switch (edits, failure) {
      | (None, _) => (ce, [], failure)
      | (Some(_), Some(_)) => (
          cell_editor |> Updated.return_quiet,
          [],
          failure,
        )
      | (Some(r), None) =>
        switch (
          try(
            PerfTimer.time("tool/check", () => CompositionGo.Reply.finish(r))
          ) {
          | exn =>
            report_tool_exn("reply", exn);
            Error({
              index: 0,
              message: Printexc.to_string(exn),
            });
          }
        ) {
        | Ok(outcome) =>
          CachedStatics.offer(
            ~settings=settings.core,
            ce.model.editor.editor.state.zipper,
            outcome.statics,
          );
          CodeWithStatics.StaticsDebounce.force_on_next := true;
          let results =
            PerfTimer.time("tool/results", () =>
              List.map(
                (m: CompositionGo.made) =>
                  (
                    m.index,
                    result(
                      ~tool_call=calls[m.index],
                      ~root=r.root,
                      ~report=
                        Option.value(
                          List.assoc_opt(m.index, outcome.reports),
                          ~default="Applied.",
                        ),
                      m,
                    ),
                  ),
                CompositionGo.Reply.made(r),
              )
            );
          (ce, results, None);
        | Error(f) => (
            cell_editor |> Updated.return_quiet,
            [],
            Some((f.index, f.message)),
          )
        }
      };
    let undone = f =>
      " None of this reply's edits were applied: call "
      ++ string_of_int(f + 1)
      ++ " ("
      ++ calls[f].name
      ++ ") failed.";
    let rolled_back = (f, tr: AgentToolResult.tool_result) => {
      ...tr,
      success: false,
      content: "Rolled back." ++ undone(f),
      content_is_payload: false,
    };
    let messages =
      List.mapi(
        (i, slot) =>
          switch (slot, failure) {
          | (Skipped(tc), _) =>
            Message.Utils.mk_tool_result_message(
              AgentToolResult.mk_skipped(tc),
            )
          | (Edit(tc), Some((f, message))) =>
            Message.Utils.mk_tool_result_message(
              f == i
                ? failed(~tool_call=tc, ~before=None, message ++ undone(f))
                : rolled_back(f, failed(~tool_call=tc, ~before=None, "")),
            )
          | (Edit(tc), None) =>
            Message.Utils.mk_tool_result_message(
              Option.value(
                List.assoc_opt(i, results),
                ~default=failed(~tool_call=tc, ~before=None, "No result."),
              ),
            )
          /* an overlay's change goes with the reply's edits */
          | (Done({role: ToolResult(tr), _}), Some((f, _)))
              when edits != None && f != i && tr.success && changes_editor(tr) =>
            Message.Utils.mk_tool_result_message(rolled_back(f, tr))
          | (Done(msg), _) => msg
          },
        slots,
      );
    let model =
      List.fold_left(
        (model, i) =>
          switch (List.nth(messages, i).role) {
          | ToolResult(tool_result) =>
            add_to_active_subtask(~tool_result, ~model, ~chat_id)
          | _ => model
          },
        model,
        List.filter_map(
          ((i, slot)) =>
            switch (slot) {
            | Edit(_) => Some(i)
            | Done(_)
            | Skipped(_) => None
            },
          List.mapi((i, s) => (i, s), slots),
        ),
      );
    /* the edits' last result takes the tests' change once evaluation
       settles */
    switch (edits, failure, ListUtil.last_opt(results)) {
    | (Some(r), None, Some((i, _))) =>
      Tests.pending :=
        Some({
          chat_id,
          message: List.nth(messages, i).id,
          root: r.root,
          before_program: r.start,
          before: EvalResult.Model.test_results(cell_editor.result),
          after_program: CompositionGo.Reply.program(r),
        })
    | _ => ()
    };
    (model, ce, messages);
  };
};

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
    let chat_system =
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
      |> ChatSystem.Update.get;
    {
      ...model,
      chat_system,
    };
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
    let old_segment = Select.all(old_editor.state.zipper).selection.content;
    let new_segment = Select.all(new_editor.state.zipper).selection.content;
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

/* A reply's structural calls, as one transaction (CompositionGo.Reply):
   each runs on the program the calls before it left, which the editor
   shows to the reply's other tools; the reply's end checks the finished
   change, and a failure anywhere restores the editor the reply began
   with */
module Edits = {
  type t = {
    /* the editor before the reply */
    start: CellEditor.Model.t,
    reply: CompositionGo.Reply.t,
  };

  let editor_root = (ce: CellEditor.Model.t) => ce.editor.editor.root;

  let begin_ = (~settings: Settings.t, ce: CellEditor.Model.t): t => {
    let z = ce.editor.editor.state.zipper;
    let root = editor_root(ce);
    let statics_of =
      CompositionGo.statics_of(
        ~settings=settings.core,
        ~root,
        ~probe_ids=CachedStatics.probe_ids_of_zipper(z),
      );
    let seg = Zipper.unselect_and_zip(~erase_buffer=true, z);
    {
      start: ce,
      reply:
        CompositionGo.Reply.start(
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
        ),
    };
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

  let add = (t: t, ~index: int, call: CompositionGo.call): result(t, string) =>
    switch (CompositionGo.Reply.add(t.reply, ~index, call)) {
    | Ok(reply) =>
      Ok({
        ...t,
        reply,
      })
    | Error(msg) => Error(msg)
    };

  let editor = (t: t): CellEditor.Model.t =>
    switch (t.reply.made) {
    | [] => t.start
    | _ => showing(t.start, CompositionGo.Reply.program(t.reply))
    };

  let text_of = (seg: Segment.t): string =>
    PersistentZipper.to_string(Zipper.unzip(seg)) ++ "\n";

  /* the reply's tool results for its structural calls, by call index */
  let results =
      (
        ~tool_call: int => OpenRouter.Reply.Model.tool_call,
        t: t,
        outcome: CompositionGo.Reply.outcome,
      )
      : list((int, AgentToolResult.tool_result)) => {
    let root = t.reply.root;
    List.map(
      (m: CompositionGo.made) => {
        let tc = tool_call(m.index);
        let diff =
          Option.map(
            ((old_text, new_text)) =>
              AgentToolResult.{
                old_text,
                new_text,
              },
            CompositionGo.diff(~root, m),
          );
        (
          m.index,
          AgentToolResult.{
            tool_call: tc,
            success: true,
            skipped: false,
            expanded: false,
            diff,
            before_text: Some(text_of(m.before)),
            after_text: Some(text_of(m.change.program)),
            content:
              tc.name
              ++ ": "
              ++ Option.value(
                   List.assoc_opt(m.index, outcome.reports),
                   ~default="Applied.",
                 ),
            content_is_payload: true,
            steps: CompositionGo.step_records(~root, m),
          },
        );
      },
      CompositionGo.Reply.made(t.reply),
    );
  };

  /* the reply's end: its editor and its structural calls' results, or
     its failure */
  let finish =
      (
        ~settings: Settings.t,
        ~tool_call: int => OpenRouter.Reply.Model.tool_call,
        t: t,
      )
      : result(
          (CellEditor.Model.t, list((int, AgentToolResult.tool_result))),
          CompositionGo.Reply.failure,
        ) =>
    switch (
      PerfTimer.time("tool/check", () => CompositionGo.Reply.finish(t.reply))
    ) {
    | Error(f) => Error(f)
    | Ok(outcome) =>
      let ce = editor(t);
      CachedStatics.offer(
        ~settings=settings.core,
        ce.editor.editor.state.zipper,
        outcome.statics,
      );
      CodeWithStatics.StaticsDebounce.force_on_next := true;
      Ok((
        ce,
        PerfTimer.time("tool/results", () => results(~tool_call, t, outcome)),
      ));
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
};

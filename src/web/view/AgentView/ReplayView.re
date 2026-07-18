open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;

/* Replay bar: steps backward/forward through the agent's edit history via
   the app-wide undo/redo stack. Each successful agent edit tool call is
   recorded as an ordinary undo entry (see History.re), interleaved with the
   user's own edits, so replay is simply undo/redo: stepping back rewinds
   the program — including the cursor position (shown as a dimmed "ghost"
   caret while the editor is unfocused) and the highlight of the node the
   agent edited at that step (CodeWithStatics.Model.agent_highlight) — and
   the chat to the state before each recorded step. Also hosts the scripted
   selector-tool benchmark launcher, which runs each scripted tool call as a
   separate action so that every step is one undo entry. */
let view =
    (
      ~globals: Globals.t,
      ~agent_model: Agent.Agent.Model.t,
      ~agent_inject: Agent.Agent.Update.Action.t => Effect.t(unit),
      ~current_chat: Agent.Chat.Model.t,
    )
    : Node.t => {
  let n_edits =
    List.length(Agent.Replay.Utils.edit_tool_results(current_chat));
  let awaiting =
    switch (agent_model.awaiting_response) {
    | Some(id) => id == current_chat.id
    | None => false
    };

  let button =
      (
        ~enabled: bool,
        ~title: string,
        ~on_click: Effect.t(unit),
        ~extra_class=?,
        label: string,
      ) => {
    let classes =
      ["replay-button"]
      @ (enabled ? [] : ["disabled"])
      @ (
        switch (extra_class) {
        | Some(c) => [c]
        | None => []
        }
      );
    div(
      ~attrs=[
        clss(classes),
        Attr.title(title),
        Attr.on_click(_ =>
          if (enabled) {
            Effect.Many([on_click, Effect.Stop_propagation]);
          } else {
            Effect.Stop_propagation;
          }
        ),
      ],
      [text(label)],
    );
  };

  let status = {
    let (label, tooltip) =
      if (awaiting) {
        (
          "Agent is responding…",
          "Replay is unavailable while the agent is responding",
        );
      } else if (n_edits == 0) {
        (
          "No agent edits recorded",
          "Successful agent edit tool calls become undo entries",
        );
      } else {
        (
          string_of_int(n_edits)
          ++ " agent edit"
          ++ (n_edits == 1 ? "" : "s")
          ++ " on undo stack",
          "Undo/redo steps through agent edits and your own edits in order",
        );
      };
    div(
      ~attrs=[clss(["replay-status"]), Attr.title(tooltip)],
      [text(label)],
    );
  };

  let controls = [
    button(
      ~enabled=!awaiting && globals.undo_depth > 0,
      ~title="Step backward",
      ~on_click=globals.inject_global(Globals.Update.Undo),
      {|◀|},
    ),
    status,
    button(
      ~enabled=!awaiting && globals.redo_depth > 0,
      ~title="Step forward",
      ~on_click=globals.inject_global(Globals.Update.Redo),
      {|▶|},
    ),
  ];

  let benchmark_button =
    button(
      ~enabled=!awaiting,
      ~title=
        "Run the scripted selector-tool benchmark task in a new chat (no LLM required); each step is one undo entry",
      ~on_click=agent_inject(Agent.Agent.Update.Action.RunBenchmark),
      ~extra_class="replay-benchmark-button",
      "Benchmark",
    );

  div(
    ~attrs=[clss(["replay-bar"])],
    [
      div(~attrs=[clss(["replay-bar-title"])], [text("Replay")]),
      div(~attrs=[clss(["replay-bar-controls"])], controls),
      benchmark_button,
    ],
  );
};

/* Run Hazel's built-in AI agent headlessly, from the terminal.
 *
 * The agent core (the agentCore directory) is already browser-free: it is a
 * pure reducer [Agent.Update.update : (action, model, editor, settings,
 * schedule_action) => (model, editor)] whose only side effects are HTTP calls
 * that schedule further actions (AgentUpdate.re:17). The browser app supplies
 * three things this module has to replace:
 *
 *   1. an event pump, to drain [schedule_action] callbacks   -> [pump] below
 *   2. an XMLHttpRequest implementation                      -> src/CLI/xhrNode.js
 *   3. an API key and a model id (the web UI collects both
 *      through AgentMainMenuView.re and stores them in
 *      Settings.agent_globals)                               -> [api_key_env_var], [default_model_id]
 *
 * The output is (a) the edited program and (b) an editing trace in the format
 * `hazel bench-incr` consumes, because the agent's editor tools are exactly
 * [CompositionActions.EditorAction(Action.Structural.t)] (CompositionActions.re:31)
 * and [Action.Structural] is a constructor of [Haz3lcore.Action.t]
 * (Action.re:156) -- the same thing the bench/traces JSON files already contain. */

open Util;
open Haz3lcore;
open Web;

/* SEAM (key supply): the web UI takes the key from a text field
   (AgentMainMenuView.re:28) and stores it in Settings.agent_globals.api_key
   (AgentGlobals.re:22). Headlessly there is no field, so we read this
   environment variable. One constant, one place. Never logged, never written
   to a file. */
let api_key_env_var = "OPENROUTER_API_KEY";

/* SEAM (model choice): no default existed for headless use. This is the
   middle entry of the repo's own curated list (OpenRouter.re:1089,
   "Best balance of quality and cost"). Overridable with --model. */
let default_model_id = "google/gemini-3-flash-preview";

/* Set by [run] when the agent's work continues on the node event loop after
   [run] returns. Cli.re's toplevel checks this before calling [exit], which
   would otherwise tear down the in-flight request. */
let deferred = ref(false);

/* Safety rail so a looping agent cannot spend unbounded money. */
let default_max_tool_turns = 24;

let llm_info_of_id = (id: string): OpenRouter.AvailableLLMs.Model.llm_info => {
  id,
  name: id,
  pricing: {
    prompt: "0",
    completion: "0",
  },
  context_length: None,
  supports_reasoning: false,
};

let zipper_of_program = (program: string): Zipper.t =>
  switch (PersistentZipper.parse_text(~source="agent", ~root=Exp, program)) {
  | Some(z) => z
  | None => failwith("Could not parse starting program")
  };

let print_zipper = (z: Zipper.t): string =>
  CompositionView.Public.print_segment(Select.all(z).selection.content);

/* Is the agent mid-turn? Mirrors AgentSend.busy_for_send (AgentSend.re:325),
   inlined so we do not depend on it being re-exported. */
let busy = (m: Agent.Model.t): bool =>
  Option.is_some(m.compaction_in_progress)
  || Option.is_some(m.awaiting_response)
  || Option.is_some(m.pending_dispatch_send);

/* Every successful editor tool call the agent made, in order, as a
   Haz3lcore.Action.t. Non-editor tool calls (view/probe/workbench/context)
   have no Action.t form and are dropped -- they do not change the program, so
   dropping them keeps the trace replayable. */
let actions_of_chat = (chat: Chat.Model.t): list((string, Action.t)) =>
  Chat.Utils.linearize(chat)
  |> List.filter_map((m: Message.Model.t) =>
       switch (m.role) {
       | ToolResult(tr) when tr.success && !tr.skipped =>
         switch (
           CompositionUtils.Public.action_of(
             ~tool_name=tr.tool_call.name,
             ~args=tr.tool_call.args,
           )
         ) {
         | Action(EditorAction(s)) =>
           Some((tr.tool_call.name, Action.Structural(s)))
         | Action(_)
         | Failure(_) => None
         }
       | _ => None
       }
     );

let sexp_of_action = (a: Action.t): string =>
  Sexplib.Sexp.to_string(Action.sexp_of_t(a));

/* One step per editor action, so bench-incr times each edit separately. */
let trace_json =
    (~name: string, ~program: string, steps: list((string, Action.t)))
    : Yojson.Safe.t =>
  `Assoc([
    ("name", `String(name)),
    ("program", `String(program)),
    (
      "steps",
      `List(
        [`Assoc([("label", `String("cold")), ("actions", `List([]))])]
        @ List.map(
            ((label, a)) =>
              `Assoc([
                ("label", `String(label)),
                ("actions", `List([`String(sexp_of_action(a))])),
              ]),
            steps,
          ),
      ),
    ),
  ]);

let read_file = (path: string): string => {
  let ic = open_in_bin(path);
  let n = in_channel_length(ic);
  let s = really_input_string(ic, n);
  close_in(ic);
  s;
};

let write_file = (path: string, s: string): unit => {
  let oc = open_out_bin(path);
  output_string(oc, s);
  close_out(oc);
};

let run =
    (
      model_id: string,
      max_tool_turns: int,
      trace_out: option(string),
      trace_name: option(string),
      stub: bool,
      stub_path: string,
      stub_code: string,
      program_path: string,
      prompt: string,
    )
    : unit => {
  let api_key =
    switch (Sys.getenv_opt(api_key_env_var)) {
    | Some(k) when String.trim(k) != "" => Some(String.trim(k))
    | _ => None
    };
  if (!stub && api_key == None) {
    prerr_endline(
      "error: "
      ++ api_key_env_var
      ++ " is not set. Set it, or pass --stub to exercise the loop without "
      ++ "calling OpenRouter.",
    );
    exit(2);
  };

  let program = String.trim(read_file(program_path));
  let zipper = zipper_of_program(program);

  /* The browser defers phase 2 of a send by a 0ms timeout so it can paint
     (AgentAction.re:15). Headless there is nothing to paint, and running it
     inline keeps the pump below single-threaded. Same hook the existing tests
     use (Test_AgentControlFlow.re:22). */
  Agent.Update.defer_dispatch_send := (thunk => thunk());

  let settings = {
    ...Settings.Model.init,
    agent_globals: {
      ...Settings.Model.init.agent_globals,
      api_key,
      active_llm: Some(llm_info_of_id(model_id)),
      session_mode: Edit,
    },
  };

  let agent = ref(Agent.Utils.init());
  let editor = ref(CellEditor.Model.mk(Editor.Model.mk(zipper, ~root=Exp)));
  let chat_id = agent^.chat_system.current;

  let queue: ref(list(Agent.Update.Action.t)) = ref([]);
  let draining = ref(false);
  let turns = ref(0);
  let finished = ref(false);

  let report_and_exit = () =>
    if (! finished^) {
      finished := true;
      let final_z = editor^.editor.editor.state.zipper;
      let final_program = print_zipper(final_z);
      let chat = ChatSystem.Utils.find_chat(chat_id, agent^.chat_system);
      let steps = actions_of_chat(chat);
      print_endline("--- final program ---");
      print_endline(final_program);
      print_endline(
        "--- editor actions ("
        ++ string_of_int(List.length(steps))
        ++ ") ---",
      );
      List.iter(
        ((label, a)) =>
          print_endline("  " ++ label ++ ": " ++ sexp_of_action(a)),
        steps,
      );
      switch (trace_out) {
      | None => ()
      | Some(path) =>
        let name =
          switch (trace_name) {
          | Some(n) => n
          | None => Filename.remove_extension(Filename.basename(path))
          };
        write_file(
          path,
          Yojson.Safe.pretty_to_string(trace_json(~name, ~program, steps))
          ++ "\n",
        );
        print_endline("wrote trace: " ++ path);
      };
      exit(0);
    };

  let rec schedule_action = (a: Agent.Update.Action.t): unit => {
    queue := queue^ @ [a];
    if (! draining^) {
      draining := true;
      pump();
      draining := false;
      if (!busy(agent^) && queue^ == []) {
        report_and_exit();
      };
    };
  }
  and pump = () =>
    switch (queue^) {
    | [] => ()
    | [a, ...rest] =>
      queue := rest;
      switch (a) {
      | HandleLLMResponse(_) =>
        incr(turns);
        if (turns^ > max_tool_turns) {
          prerr_endline(
            "error: exceeded --max-turns ("
            ++ string_of_int(max_tool_turns)
            ++ "); stopping.",
          );
          report_and_exit();
        };
      | _ => ()
      };
      let (agent', editor') =
        Agent.Update.update(a, agent^, editor^, settings, schedule_action);
      agent := agent';
      editor := editor'.model;
      pump();
    };

  if (stub) {
    /* Prove the loop end-to-end with no network: hand the reducer a reply
       shaped exactly as the streaming path produces (AgentSend.re:160). The
       tool call is fixed rather than model-generated, so --stub exercises
       the pump, the tool executor, the editor threading and the trace
       emitter, but NOT payload construction or the HTTP transport. */
    let reply: OpenRouter.Reply.Model.t = {
      content: "Stubbed reply.",
      tool_calls: [
        {
          id: "stub-1",
          name: "update_definition",
          args:
            `Assoc([
              ("path", `String(stub_path)),
              ("code", `String(stub_code)),
            ]),
        },
      ],
      usage: None,
      reasoning: None,
    };
    schedule_action(HandleLLMResponse(reply, chat_id, 1, 0));
    report_and_exit();
  } else {
    schedule_action(
      SendMessage(Message.Utils.mk_user_message(prompt), chat_id),
    );
    /* Node keeps the process alive on the pending socket; the HTTP callbacks
       re-enter schedule_action, and report_and_exit fires when idle. Tell
       Cli.re not to exit underneath us in the meantime. */
    deferred := true;
  };
};

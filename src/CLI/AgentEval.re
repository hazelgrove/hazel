/* AgentEval.re: headless prompt-caching eval that drives the REAL agent.
 *
 * Simulates a user session on the website's coding agent: scripted user
 * inputs are fed through the production [Agent.Update] state machine — real
 * system prompt + dev notes (with the cache_anchor floor breakpoint), real
 * tool definitions, real payload serialization, real response/usage parsing,
 * real tool execution if the model calls tools — against live OpenRouter.
 * Network runs over the synchronous curl-backed XMLHttpRequest polyfill
 * (src/CLI/polyfill.js), so every handler completes before the CLI exits.
 *
 * Emits append-only JSONL to --out-dir (default agent-docs/caching-eval/
 * outputs): per-turn usage rows, per-model verdicts, a /credits ledger, and
 * the real [AgentSlashFormat.cost_payload] output — i.e. the same numbers
 * /cost shows a user, cross-checkable against raw provider usage.
 *
 * See agent-docs/caching-eval/pre-implementation-report.md. */
open Util;
open Haz3lcore;
open Web;

module Json = API.Json;

/* Same harness pattern as test/Test_AgentControlFlow.re: run the deferred
   phase-2 send synchronously and drain scheduled actions to quiescence. */
let install_sync_dispatch = (): unit =>
  Agent.Update.defer_dispatch_send := (thunk => thunk());

let cell_editor = () =>
  CellEditor.Model.mk(Editor.Model.mk(Zipper.init(), ~root=Exp));

let run_update =
    (
      ~settings: Settings.Model.t,
      action: Agent.Update.Action.t,
      agent: Agent.Model.t,
      scheduled: ref(list(Agent.Update.Action.t)),
    )
    : Agent.Model.t => {
  let editor = cell_editor();
  let (agent', _) =
    Agent.Update.update(action, agent, editor, settings, x =>
      scheduled := scheduled^ @ [x]
    );
  agent';
};

let rec drain =
        (
          ~settings: Settings.Model.t,
          ~max_rounds: int,
          agent: Agent.Model.t,
          scheduled: ref(list(Agent.Update.Action.t)),
        )
        : Agent.Model.t =>
  if (max_rounds <= 0) {
    Printf.eprintf("warning: drain exceeded round bound; continuing\n");
    agent;
  } else {
    switch (scheduled^) {
    | [] => agent
    | actions =>
      scheduled := [];
      let agent' =
        List.fold_left(
          (ag, act) => run_update(~settings, act, ag, scheduled),
          agent,
          actions,
        );
      drain(~settings, ~max_rounds=max_rounds - 1, agent', scheduled);
    };
  };

/* Validated list prices (see pre-implementation report), dollars per token,
   in OpenRouter's own string format. Used only to construct the active
   [llm_info]; billed truth comes from usage.cost. */
let known_models: list(OpenRouter.AvailableLLMs.Model.llm_info) =
  OpenRouter.AvailableLLMs.Model.[
    {
      id: "anthropic/claude-haiku-4.5",
      name: "Anthropic: Claude Haiku 4.5",
      pricing: {
        prompt: "0.000001",
        completion: "0.000005",
      },
      context_length: Some(200000),
      supports_reasoning: true,
    },
    {
      id: "anthropic/claude-sonnet-4.6",
      name: "Anthropic: Claude Sonnet 4.6",
      pricing: {
        prompt: "0.000003",
        completion: "0.000015",
      },
      context_length: Some(1000000),
      supports_reasoning: true,
    },
    {
      id: "google/gemini-3-flash-preview",
      name: "Google: Gemini 3 Flash Preview",
      pricing: {
        prompt: "0.0000005",
        completion: "0.000003",
      },
      context_length: Some(1000000),
      supports_reasoning: true,
    },
    {
      id: "qwen/qwen3-coder",
      name: "Qwen: Qwen3 Coder",
      pricing: {
        prompt: "0.00000022",
        completion: "0.0000018",
      },
      context_length: Some(262144),
      supports_reasoning: false,
    },
  ];

/* Scripted user session: a small coding ask, a follow-up (cache read against
   the grown history floor), and a terse control turn. */
let scripted_inputs: list(string) = [
  "Write a recursive function fib that computes the nth Fibonacci number.",
  "Now explain in one sentence why your implementation terminates.",
  "Reply with exactly: OK",
];

let jsonl_append = (path: string, json: Json.t): unit => {
  let oc = open_out_gen([Open_append, Open_creat, Open_text], 0o644, path);
  output_string(oc, Yojson.Safe.to_string(json) ++ "\n");
  close_out(oc);
};

let fetch_credits = (~key: string): option((float, float)) => {
  let result = ref(None);
  OpenRouter.Credits.Utils.get_credits(~key, ~handler=response =>
    switch (response) {
    | Some(json) =>
      switch (Json.dot("data", json)) {
      | Some(data) =>
        let num = f =>
          switch (Json.dot(f, data)) {
          | Some(`Float(x)) => Some(x)
          | Some(`Int(n)) => Some(float_of_int(n))
          | _ => None
          };
        switch (num("total_credits"), num("total_usage")) {
        | (Some(c), Some(u)) => result := Some((c, u))
        | _ => ()
        };
      | None => ()
      }
    | None => ()
    }
  );
  result^; /* sync XHR: handler has already run */
};

let usage_rows =
    (~run_id: string, ~model_id: string, chat: Chat.Model.t): list(Json.t) => {
  let messages = Chat.Utils.get(chat);
  let (_, rows) =
    List.fold_left(
      ((turn, acc), msg: Message.Model.t) =>
        switch (msg.role) {
        | Message.Model.Agent(Some(usage)) => (
            turn + 1,
            [
              `Assoc([
                ("run_id", `String(run_id)),
                ("model", `String(model_id)),
                ("turn", `Int(turn)),
                ("usage", OpenRouter.Reply.Model.yojson_of_usage(usage)),
              ]),
              ...acc,
            ],
          )
        | _ => (turn, acc)
        },
      (1, []),
      messages,
    );
  List.rev(rows);
};

let cache_verdict = (chat: Chat.Model.t): (string, string) => {
  let usages =
    Chat.Utils.get(chat)
    |> List.filter_map((msg: Message.Model.t) =>
         switch (msg.role) {
         | Message.Model.Agent(Some(u)) => Some(u)
         | _ => None
         }
       );
  let read = (u: OpenRouter.Reply.Model.usage) =>
    Option.value(~default=0, u.cache_read_input_tokens);
  let write = (u: OpenRouter.Reply.Model.usage) =>
    Option.value(~default=0, u.cache_write_tokens)
    + Option.value(~default=0, u.cache_creation_input_tokens);
  switch (usages) {
  | [] => ("INCONCLUSIVE", "no agent messages carried usage")
  | [_] => ("INCONCLUSIVE", "only one turn completed")
  | [first, ...rest] =>
    let max_read = List.fold_left((m, u) => max(m, read(u)), 0, rest);
    let baseline = max(write(first), 1);
    max_read * 2 >= baseline
      ? (
        "WORKING",
        Printf.sprintf(
          "cache_read %d vs turn-1 write %d",
          max_read,
          write(first),
        ),
      )
      : (
        "NOT_WORKING",
        Printf.sprintf(
          "max cache_read %d vs turn-1 write %d",
          max_read,
          write(first),
        ),
      );
  };
};

let run_model =
    (
      ~run_id: string,
      ~key: string,
      ~out_dir: string,
      ~turns: int,
      info: OpenRouter.AvailableLLMs.Model.llm_info,
    )
    : unit => {
  Printf.printf("=== %s ===\n%!", info.id);
  let settings = {
    ...Settings.Model.init,
    agent_globals: {
      ...AgentGlobals.init(),
      api_key: Some(key),
      active_llm: Some(info),
    },
  };
  let agent = ref(Agent.Utils.init());
  let scheduled: ref(list(Agent.Update.Action.t)) = ref([]);
  let chat_id = agent^.chat_system.current;
  let inputs = ListUtil.take(turns, scripted_inputs);
  List.iteri(
    (i, input) => {
      Printf.printf("  turn %d: %s\n%!", i + 1, input);
      let msg = Message.Utils.mk_user_message(input);
      agent :=
        run_update(
          ~settings,
          Agent.Update.Action.SendMessage(msg, chat_id),
          agent^,
          scheduled,
        );
      agent := drain(~settings, ~max_rounds=200, agent^, scheduled);
    },
    inputs,
  );
  let chat = ChatSystem.Utils.find_chat(chat_id, agent^.chat_system);
  List.iter(
    jsonl_append(Filename.concat(out_dir, "summary/results.jsonl")),
    usage_rows(~run_id, ~model_id=info.id, chat),
  );
  let cost = AgentSlashFormat.cost_payload(~chat, ~active_llm=Some(info));
  let (verdict, reason) = cache_verdict(chat);
  jsonl_append(
    Filename.concat(out_dir, "summary/verdicts.jsonl"),
    `Assoc([
      ("run_id", `String(run_id)),
      ("model", `String(info.id)),
      ("driver", `String("agent-eval (real Agent.Update loop)")),
      ("verdict", `String(verdict)),
      ("reason", `String(reason)),
      ("cost_payload", Message.Model.yojson_of_cost_output(cost)),
    ]),
  );
  Printf.printf("  verdict: %s (%s)\n%!", verdict, reason);
};

let run = (models_csv: option(string), turns: int, out_dir: string): unit => {
  install_sync_dispatch();
  let key =
    switch (Sys.getenv_opt("OPENROUTER_API_KEY")) {
    | Some(k) when String.trim(k) != "" => String.trim(k)
    | _ =>
      Printf.eprintf("OPENROUTER_API_KEY is not set; aborting.\n");
      exit(1);
    };
  let selected =
    switch (models_csv) {
    | None => known_models
    | Some(csv) =>
      let ids = String.split_on_char(',', csv) |> List.map(String.trim);
      List.filter(
        (m: OpenRouter.AvailableLLMs.Model.llm_info) => List.mem(m.id, ids),
        known_models,
      );
    };
  if (selected == []) {
    Printf.eprintf("no known models matched --models; aborting.\n");
    exit(1);
  };
  let run_id = Printf.sprintf("agent-eval-%.0f", JsUtil.timestamp());
  let ledger = Filename.concat(out_dir, "summary/ledger.jsonl");
  let note_credits = phase =>
    switch (fetch_credits(~key)) {
    | Some((total, used)) =>
      jsonl_append(
        ledger,
        `Assoc([
          ("run_id", `String(run_id)),
          ("phase", `String(phase)),
          ("total_credits", `Float(total)),
          ("total_usage", `Float(used)),
        ]),
      )
    | None => Printf.eprintf("warning: /credits fetch failed (%s)\n", phase)
    };
  note_credits("before");
  List.iter(run_model(~run_id, ~key, ~out_dir, ~turns), selected);
  note_credits("after");
  Printf.printf("done. run_id=%s outputs=%s\n", run_id, out_dir);
};

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

/* Has the agent declared itself finished?
 *
 * Without this the feedback loop keeps prompting until [--feedback] rounds are
 * exhausted, even after the agent has nothing left to do. A nagged agent does
 * not sit still: it re-applies an edit it has already made. Those repeats are
 * the worst possible thing to leave in a benchmark trace, because a step that
 * does not change the program is one every calculus can serve entirely from
 * cache -- so they inflate the apparent value of caching while measuring no
 * real work. Stopping when the agent says DONE is what keeps the recorded
 * trace a record of the agent's actual decisions.
 *
 * Matched on the last Agent message only, and as a whole word, so that an
 * agent narrating "I am not done yet" does not end the run. */
let said_done = (chat: Chat.Model.t): bool => {
  let is_word_char = c =>
    c >= 'a'
    && c <= 'z'
    || c >= 'A'
    && c <= 'Z'
    || c >= '0'
    && c <= '9'
    || c == '_';
  let is_done_token = (s: string) => {
    let n = String.length(s);
    let rec scan = i =>
      if (i + 4 > n) {
        false;
      } else if (String.sub(s, i, 4) == "DONE"
                 && (i == 0 || !is_word_char(s.[i - 1]))
                 && (i + 4 == n || !is_word_char(s.[i + 4]))) {
        true;
      } else {
        scan(i + 1);
      };
    scan(0);
  };
  let rec last_agent = (msgs: list(Message.Model.t)) =>
    switch (msgs) {
    | [] => None
    | [m, ...rest] =>
      switch (last_agent(rest)) {
      | Some(_) as found => found
      | None =>
        switch (m.role) {
        | Agent(_) => Some(m.content)
        | _ => None
        }
      }
    };
  switch (last_agent(Chat.Utils.linearize(chat))) {
  | Some(content) => is_done_token(content)
  | None => false
  };
};

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

/* Evaluate the current program and describe the outcome for the agent.
 *
 * This is what turns a one-shot "apply these edits" run into an iterate-until-
 * it-works loop: without it the agent only ever sees TYPE errors (which the
 * agent context already carries, via ErrorPrint over mk_statics in
 * AgentUtils.re:129-133) and never finds out that a type-correct program
 * computes the wrong answer.
 *
 * Caveat worth knowing: Hazel's evaluator has no fuel bound here, so a
 * non-terminating program the agent wrote will hang this process rather than
 * report an error. There is no timeout because js_of_ocaml evaluation cannot
 * be interrupted from the same thread. */
let static_errors = (z: Zipper.t): list(string) =>
  ErrorPrint.all(CompositionGo.Public.mk_statics(z));

let describe_evaluation = (z: Zipper.t): (option(string), string) => {
  let errors = static_errors(z) |> String.concat("\n");
  let term = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  let value =
    try(Some(Print.print(Run.evaluate(term)))) {
    | exn => Some("<evaluation raised: " ++ Printexc.to_string(exn) ++ ">")
    };
  (value, errors);
};

/* The follow-up message sent back into the same chat after a round of edits,
   so the next turn is informed by what the program actually did.

   Deliberately says nothing about [goal]. The goal is an ORACLE, not a hint:
   it decides when the run has succeeded and whether to stop, but telling the
   agent the expected value would hand it the answer to tasks whose whole
   point is that the answer has to be discovered by running the program. A
   trace recorded against a leaked answer measures an agent typing in a
   number, not an agent iterating. */
let feedback_message = (~value: option(string), ~errors: string): string => {
  let v =
    switch (value) {
    | Some(v) => "The program currently evaluates to:\n" ++ v
    | None => "The program could not be evaluated."
    };
  let e =
    errors == ""
      ? "There are no static errors." : "Static errors remain:\n" ++ errors;
  String.concat(
    "\n\n",
    [v, e, "If this is correct and complete, say DONE and stop."],
  );
};

/* The oracle: the program evaluates to [goal] with no static errors. */
let goal_met = (~goal: string, ~value: option(string), ~errors: string): bool =>
  switch (value) {
  | Some(v) => String.trim(v) == String.trim(goal) && errors == ""
  | None => false
  };

/* Who ran, and under which arm. Carried into the metrics row verbatim so a
   JSONL file from many runs can be grouped without parsing log filenames. */
type run_info = {
  task: option(string),
  arm: option(string),
  model_id: string,
  jev_prepass: bool,
  jev_view_tool: bool,
  jev_edit_tool: bool,
  jev_edit_builds: bool,
  jev_batch_max_tokens: int,
  nav_targets: option(list(string)),
};

/* One JSONL row per run (docs/notes/jev-nav/plan.md §6). Pure: everything it
   reports is read off the finished chat, the final program's outcome and the
   Jev selection log, so it can be tested without running an agent. */
module Metrics = {
  let json_of_opt = (f: 'a => Yojson.Safe.t, o: option('a)): Yojson.Safe.t =>
    switch (o) {
    | Some(x) => f(x)
    | None => `Null
    };

  let usages =
      (msgs: list(Message.Model.t)): list(OpenRouter.Reply.Model.usage) =>
    List.filter_map(
      (m: Message.Model.t) =>
        switch (m.role) {
        | Agent(Some(u)) => Some(u)
        | _ => None
        },
      msgs,
    );

  /* Billed cost is [usage.cost] (what OpenRouter charged), not a token-price
     estimate: cached tokens make the estimate wrong in both directions. None
     when no turn reported a cost, so "free" and "unknown" stay distinct. */
  let billed_cost = (us: list(OpenRouter.Reply.Model.usage)): option(float) =>
    List.fold_left(
      (acc, u: OpenRouter.Reply.Model.usage) =>
        switch (acc, u.cost) {
        | (None, c) => c
        | (Some(a), Some(c)) => Some(a +. c)
        | (Some(_), None) => acc
        },
      None,
      us,
    );

  /* One Agent message per main-model reply, with or without usage (the
     stub's reply carries none). */
  let agent_turns = (msgs: list(Message.Model.t)): int =>
    List.length(
      List.filter(
        (m: Message.Model.t) =>
          switch (m.role) {
          | Agent(_) => true
          | _ => false
          },
        msgs,
      ),
    );

  /* What the reducer reported as API failures (HTTP errors, empty replies),
     in order. Integration runs need these in the row: a run that "failed to
     meet the goal" because every request 401'd is a different finding from
     one where the model got it wrong. */
  let api_failures = (msgs: list(Message.Model.t)): list(string) =>
    List.filter_map(
      (m: Message.Model.t) =>
        switch (m.role) {
        | System(ApiFailure) => Some(m.content)
        | _ => None
        },
      msgs,
    );

  /* Counted from tool results, not the editor trace: the trace keeps only
     editor actions, but navigation calls are exactly what this study is
     about. Every call gets a result (failed and skipped ones too), so this
     counts calls the model made, not calls that succeeded. */
  let count_tool_results =
      (
        msgs: list(Message.Model.t),
        keep: AgentToolResult.tool_result => bool,
      )
      : int =>
    List.length(
      List.filter(
        (m: Message.Model.t) =>
          switch (m.role) {
          | ToolResult(tr) => keep(tr)
          | _ => false
          },
        msgs,
      ),
    );

  /* Refused = the handler rejected the call itself. Skipped calls also carry
     success=false, but only because an earlier call in the turn failed, so
     they say nothing about whether the tool was available. */
  let refused = (tr: AgentToolResult.tool_result): bool =>
    !tr.success && !tr.skipped;

  /* An editor tool is one whose call parses to an editor action: the same
     classification [actions_of_chat] uses, so no second list of tool names
     can drift from the real one. */
  let is_editor_call = (tr: AgentToolResult.tool_result): bool =>
    switch (
      CompositionUtils.Public.action_of(
        ~tool_name=tr.tool_call.name,
        ~args=tr.tool_call.args,
      )
    ) {
    | Action(EditorAction(_)) => true
    | Action(_)
    | Failure(_) => false
    };

  /* Paths a selection contributed across the run. A failed selection opens
     nothing in the real view, even though [yes] still holds the answers from
     its batches that did succeed. */
  let jev_paths =
      (f: JevNav.metrics => list(string), jev: list(JevNav.metrics))
      : list(string) =>
    List.concat_map((m: JevNav.metrics) => m.failed ? [] : f(m), jev)
    |> List.sort_uniq(String.compare);

  /* None when there is nothing to score: no targets for the task, or no
     selection was attempted (the control arm), where 0 recall would read as
     a failure rather than as "not applicable". A selection that was
     attempted but opened nothing (all "no", or failed) scores recall 0 with
     precision undefined.

     Recall is over what the agent saw (yes plus closure ancestors), so a
     target that is a parent of a yes still counts as found. Precision is
     over Jev's yes answers alone: closure ancestors are needed to reach a
     target, and counting them would dock precision for correct behaviour. */
  let selection_quality =
      (~nav_targets: option(list(string)), jev: list(JevNav.metrics))
      : option((float, option(float))) =>
    switch (nav_targets, jev) {
    | (None, _)
    | (Some([]), _)
    | (_, []) => None
    | (Some(targets), _) =>
      let yes = jev_paths(m => m.yes, jev);
      let opened = jev_paths(m => m.yes @ m.closure_added, jev);
      let count_in = paths =>
        List.length(List.filter(t => List.mem(t, paths), targets));
      Some((
        float_of_int(count_in(opened))
        /. float_of_int(List.length(targets)),
        yes == []
          ? None
          : Some(
              float_of_int(count_in(yes)) /. float_of_int(List.length(yes)),
            ),
      ));
    };

  let sum_jev = (f: JevNav.metrics => int, jev: list(JevNav.metrics)): int =>
    List.fold_left((acc, m) => acc + f(m), 0, jev);

  let sum_float = (f: 'a => float, xs: list('a)): float =>
    List.fold_left((acc, x) => acc +. f(x), 0., xs);

  /* V3 (Jev implements): totals over every jev_edit call in the run. Fill
     rate is filled / holes seen: the share of the planner's holes Jev
     closed without handing them back. */
  let jev_edit_json = (edits: list(JevEdit.metrics)): Yojson.Safe.t => {
    let sum = (f: JevEdit.metrics => int) =>
      List.fold_left((acc, m) => acc + f(m), 0, edits);
    let holes_seen = sum(m => m.holes_seen);
    let filled = sum(m => m.filled);
    `Assoc([
      ("edits", `List(List.map(JevEdit.yojson_of_metrics, edits))),
      ("count", `Int(List.length(edits))),
      (
        "failed",
        `Int(
          List.length(List.filter((m: JevEdit.metrics) => m.failed, edits)),
        ),
      ),
      ("rounds", `Int(sum(m => m.rounds))),
      ("holes_seen", `Int(holes_seen)),
      ("filled", `Int(filled)),
      ("escalated", `Int(sum(m => List.length(m.escalated)))),
      (
        "fill_rate",
        holes_seen == 0
          ? `Null : `Float(float_of_int(filled) /. float_of_int(holes_seen)),
      ),
      ("requests", `Int(sum(m => m.requests))),
      ("input_tokens", `Int(sum(m => m.input_tokens))),
      (
        "cost_usd",
        `Float(sum_float((m: JevEdit.metrics) => m.cost_usd, edits)),
      ),
      (
        "latency_ms",
        `Float(sum_float((m: JevEdit.metrics) => m.latency_ms, edits)),
      ),
    ]);
  };

  let row =
      (
        ~info: run_info,
        ~wall_ms: float,
        ~chat: Chat.Model.t,
        ~goal_met: option(bool),
        ~said_done: bool,
        ~feedback_rounds_used: int,
        ~static_errors: list(string),
        ~jev: list(JevNav.metrics),
        ~jev_edit: list(JevEdit.metrics),
        ~transcript: option(string),
      )
      : Yojson.Safe.t => {
    let msgs = Chat.Utils.linearize(chat);
    let us = usages(msgs);
    let sum_usage = f => List.fold_left((acc, u) => acc + f(u), 0, us);
    let count = keep => `Int(count_tool_results(msgs, keep));
    let calls = name => count(tr => name == "" || tr.tool_call.name == name);
    let str = s => `String(s);
    `Assoc([
      ("task", json_of_opt(str, info.task)),
      ("arm", json_of_opt(str, info.arm)),
      ("model", `String(info.model_id)),
      (
        "flags",
        `Assoc([
          ("jev_prepass", `Bool(info.jev_prepass)),
          ("jev_view_tool", `Bool(info.jev_view_tool)),
          ("jev_edit_tool", `Bool(info.jev_edit_tool)),
          ("jev_edit_builds", `Bool(info.jev_edit_builds)),
          ("jev_batch_max_tokens", `Int(info.jev_batch_max_tokens)),
        ]),
      ),
      ("wall_ms", `Float(wall_ms)),
      /* Links the row to its transcript file (basename; same folder), so
         reports never have to guess which transcript belongs to which row. */
      ("transcript", json_of_opt(str, transcript)),
      (
        "main",
        `Assoc([
          ("turns", `Int(agent_turns(msgs))),
          (
            "tool_calls",
            `Assoc([
              ("expand", calls("expand")),
              ("collapse", calls("collapse")),
              ("modify_view", calls("modify_view")),
              ("jev_edit", calls("jev_edit")),
              ("total", calls("")),
              /* Arms hide tools (expand/collapse under the view tool, editor
                 tools under the edit tool); a model that calls them anyway
                 pays a turn for nothing, which is part of the arm's cost.
                 Gated on the arm's flag: outside it, a failed call is an
                 ordinary failure (bad path, type guard), not a refusal. */
              (
                "refused_nav",
                count(tr =>
                  info.jev_view_tool
                  && refused(tr)
                  && (
                    tr.tool_call.name == "expand"
                    || tr.tool_call.name == "collapse"
                  )
                ),
              ),
              (
                "refused_edit",
                count(tr =>
                  info.jev_edit_tool && refused(tr) && is_editor_call(tr)
                ),
              ),
            ]),
          ),
          (
            "prompt_tokens",
            `Int(
              sum_usage((u: OpenRouter.Reply.Model.usage) => u.prompt_tokens),
            ),
          ),
          (
            "completion_tokens",
            `Int(
              sum_usage((u: OpenRouter.Reply.Model.usage) =>
                u.completion_tokens
              ),
            ),
          ),
          (
            "cached_tokens",
            `Int(
              sum_usage((u: OpenRouter.Reply.Model.usage) =>
                Option.value(~default=0, u.cache_read_input_tokens)
              ),
            ),
          ),
          ("cost_usd", json_of_opt(c => `Float(c), billed_cost(us))),
        ]),
      ),
      (
        "outcome",
        `Assoc([
          ("goal_met", json_of_opt(b => `Bool(b), goal_met)),
          ("said_done", `Bool(said_done)),
          ("feedback_rounds_used", `Int(feedback_rounds_used)),
          ("static_errors_final", `Int(List.length(static_errors))),
          (
            "api_failures",
            `List(List.map(e => `String(e), api_failures(msgs))),
          ),
        ]),
      ),
      (
        "jev",
        `Assoc([
          ("selections", `List(List.map(JevNav.yojson_of_metrics, jev))),
          (
            "requests",
            `Int(sum_jev((m: JevNav.metrics) => m.requests, jev)),
          ),
          (
            "questions",
            `Int(sum_jev((m: JevNav.metrics) => m.questions, jev)),
          ),
          (
            "input_tokens",
            `Int(sum_jev((m: JevNav.metrics) => m.input_tokens, jev)),
          ),
          (
            "cost_usd",
            `Float(
              List.fold_left(
                (acc, m: JevNav.metrics) => acc +. m.cost_usd,
                0.,
                jev,
              ),
            ),
          ),
        ]),
      ),
      ("jev_edit", jev_edit_json(jev_edit)),
      (
        "selection",
        json_of_opt(
          ((recall, precision)) =>
            `Assoc([
              ("recall", `Float(recall)),
              ("precision", json_of_opt(p => `Float(p), precision)),
            ]),
          selection_quality(~nav_targets=info.nav_targets, jev),
        ),
      ),
    ]);
  };
};

/* Stand-in for Jev under --stub, so the real JevEdit engine (TyDi
   candidates, rounds, splicing, metrics) runs end to end with no HTTP. It
   stays deliberately naive, but must be able to finish a build: a function
   hole takes a `fun` form (else a planner name, offered untyped, would
   escalate at once), any other hole prefers a complete term over a form
   that opens more holes. Escalates only when a hole has no candidate. */
let expected_type_of = (state: API.Json.t, hole_id: string): string =>
  API.Json.(
    dot("holes", state)
    |> Option.bind(_, list)
    |> Option.value(~default=[])
    |> List.find_map(h =>
         dot("hole", h) |> Option.bind(_, str) == Some(hole_id)
           ? dot("expected_type", h) |> Option.bind(_, str) : None
       )
    |> Option.value(~default="")
  );

let stub_choice = (~expected_type: string, options: list(string)): string => {
  let real = List.filter(o => o != JevEdit.escalate, options);
  let is_fun = o => String.starts_with(~prefix="fun ", o);
  let is_complete = o => !String.contains(o, '?');
  let rec has_arrow = i =>
    i
    + 1 < String.length(expected_type)
    && (
      expected_type.[i] == '-'
      && expected_type.[i + 1] == '>'
      || has_arrow(i + 1)
    );
  let wants_fun = has_arrow(0);
  let preferred =
    wants_fun
      ? List.find_opt(is_fun, real) : List.find_opt(is_complete, real);
  switch (preferred, real) {
  | (Some(o), _)
  | (None, [o, ..._]) => o
  | (None, []) => JevEdit.escalate
  };
};

let stub_decider: JevEdit.decide_choices =
  (~state, ~questions, ~handler) =>
    handler(
      OpenRouter.SystemOne.Chosen(
        List.map(
          (q: OpenRouter.SystemOne.choice_question) =>
            {
              OpenRouter.SystemOne.id: q.id,
              choice:
                stub_choice(
                  ~expected_type=expected_type_of(state, q.id),
                  q.options,
                ),
              confidence: 1.0,
            },
          questions,
        ),
        {
          input_tokens: 0,
          cost_usd: Some(0.),
        },
      ),
    );

/* A readable record of one run, for writing up what the agent did. Pure,
   like [Metrics.row]: built from the finished chat and the drained Jev logs.
   It never sees Settings, so it cannot carry the API key. Long texts are cut:
   the transcript is for reading, and the full program is kept separately. */
module Transcript = {
  let cut = (limit: int, s: string): string =>
    String.length(s) <= limit
      ? s
      : String.sub(s, 0, limit)
        ++ "… ["
        ++ string_of_int(String.length(s) - limit)
        ++ " more chars]";

  let json_of_usage = (u: OpenRouter.Reply.Model.usage): Yojson.Safe.t =>
    `Assoc([
      ("prompt", `Int(u.prompt_tokens)),
      ("completion", `Int(u.completion_tokens)),
      ("cached", `Int(Option.value(~default=0, u.cache_read_input_tokens))),
      ("cost", Metrics.json_of_opt(c => `Float(c), u.cost)),
    ]);

  let json_of_result = (tr: AgentToolResult.tool_result): Yojson.Safe.t =>
    `Assoc([
      ("success", `Bool(tr.success)),
      ("skipped", `Bool(tr.skipped)),
      ("content", `String(cut(400, tr.content))),
      (
        "diff",
        Metrics.json_of_opt(
          (d: AgentToolResult.diff) =>
            `Assoc([
              (
                "old",
                `String(CompositionView.Public.print_segment(d.old_segment)),
              ),
              (
                "new",
                Metrics.json_of_opt(
                  seg => `String(CompositionView.Public.print_segment(seg)),
                  d.new_segment,
                ),
              ),
            ]),
          tr.diff,
        ),
      ),
    ]);

  let json_of_call = (tr: AgentToolResult.tool_result): Yojson.Safe.t =>
    `Assoc([
      ("name", `String(tr.tool_call.name)),
      ("args", tr.tool_call.args),
      ("result", json_of_result(tr)),
    ]);

  /* Tool results follow the Agent message that made the calls, so each is
     folded into the latest agent event. Built newest-first, reversed once. */
  let events = (msgs: list(Message.Model.t)): list(Yojson.Safe.t) => {
    let agent_event = (~turn, ~text, ~usage, calls) =>
      `Assoc([
        ("kind", `String("agent")),
        ("turn", `Int(turn)),
        ("text", `String(cut(600, text))),
        ("usage", Metrics.json_of_opt(json_of_usage, usage)),
        ("tool_calls", `List(List.rev(calls))),
      ]);
    let text_event = (kind, text) =>
      `Assoc([("kind", `String(kind)), ("text", `String(text))]);
    /* The open agent event is kept unbuilt until its last tool result. */
    let close = ((done_rev, open_agent)) =>
      switch (open_agent) {
      | Some((turn, text, usage, calls)) => [
          agent_event(~turn, ~text, ~usage, calls),
          ...done_rev,
        ]
      | None => done_rev
      };
    let (done_rev, open_agent, _) =
      List.fold_left(
        ((done_rev, open_agent, turn), m: Message.Model.t) =>
          switch (m.role) {
          | Agent(usage) => (
              close((done_rev, open_agent)),
              Some((turn + 1, m.content, usage, [])),
              turn + 1,
            )
          | ToolResult(tr) =>
            switch (open_agent) {
            | Some((t, text, usage, calls)) => (
                done_rev,
                Some((t, text, usage, [json_of_call(tr), ...calls])),
                turn,
              )
            | None => (done_rev, open_agent, turn)
            }
          | User => (
              [
                text_event("user", m.content),
                ...close((done_rev, open_agent)),
              ],
              None,
              turn,
            )
          | System(ApiFailure) => (
              [
                text_event("api_failure", m.content),
                ...close((done_rev, open_agent)),
              ],
              None,
              turn,
            )
          | System(_) => (done_rev, open_agent, turn)
          },
        ([], None, 0),
        msgs,
      );
    List.rev(close((done_rev, open_agent)));
  };

  let of_run =
      (
        ~info: run_info,
        ~program: string,
        ~prompt: string,
        ~chat: Chat.Model.t,
        ~final_program: string,
        ~final_value: option(string),
        ~goal: option(string),
        ~goal_met: option(bool),
        ~said_done: bool,
        ~jev: list(JevNav.metrics),
        ~jev_edit: list(JevEdit.metrics),
      )
      : Yojson.Safe.t => {
    let str = s => `String(s);
    `Assoc([
      ("task", Metrics.json_of_opt(str, info.task)),
      ("arm", Metrics.json_of_opt(str, info.arm)),
      ("model", `String(info.model_id)),
      ("program", `String(program)),
      ("prompt", `String(prompt)),
      ("events", `List(events(Chat.Utils.linearize(chat)))),
      /* In call order; the logs carry latency but no wall-clock stamps. */
      ("jev_selections", `List(List.map(JevNav.yojson_of_metrics, jev))),
      ("jev_edits", `List(List.map(JevEdit.yojson_of_metrics, jev_edit))),
      ("final_program", `String(final_program)),
      ("final_value", Metrics.json_of_opt(str, final_value)),
      ("goal", Metrics.json_of_opt(str, goal)),
      ("goal_met", Metrics.json_of_opt(b => `Bool(b), goal_met)),
      ("said_done", `Bool(said_done)),
    ]);
  };
};

/* Append, never truncate: one file accumulates every run of a study. */
let append_line = (path: string, line: string): unit => {
  let oc = open_out_gen([Open_append, Open_creat], 0o644, path);
  output_string(oc, line ++ "\n");
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
      feedback_rounds: int,
      goal: option(string),
      jev_prepass: bool,
      jev_view_tool: bool,
      jev_edit_tool: bool,
      jev_edit_builds: bool,
      jev_batch_max_tokens: option(int),
      task: option(string),
      arm: option(string),
      nav_targets: option(list(string)),
      metrics_out: option(string),
      transcript_out: option(string),
      program_path: string,
      prompt: string,
    )
    : unit => {
  let started_ms = JsUtil.timestamp();
  /* Building is a mode of jev_edit: asking for it switches the tool on, so
     one flag names the arm and the two can never disagree. */
  let jev_edit_tool = jev_edit_tool || jev_edit_builds;
  /* --stub never reads the key: with one set, the reducer's follow-up turn
     after the canned reply would otherwise send a real request. */
  let api_key =
    switch (stub ? None : Sys.getenv_opt(api_key_env_var)) {
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
      jev_prepass,
      jev_view_tool,
      jev_edit_tool,
      jev_edit_builds,
      jev_batch_max_tokens:
        Option.value(
          ~default=Settings.Model.init.agent_globals.jev_batch_max_tokens,
          jev_batch_max_tokens,
        ),
    },
  };
  let info = {
    task,
    arm,
    model_id,
    jev_prepass,
    jev_view_tool,
    jev_edit_tool,
    jev_edit_builds,
    jev_batch_max_tokens: settings.agent_globals.jev_batch_max_tokens,
    nav_targets,
  };

  let agent = ref(Agent.Utils.init());
  let editor = ref(CellEditor.Model.mk(Editor.Model.mk(zipper, ~root=Exp)));
  let chat_id = agent^.chat_system.current;

  let queue: ref(list(Agent.Update.Action.t)) = ref([]);
  let draining = ref(false);
  let turns = ref(0);
  let finished = ref(false);
  let rounds_used = ref(0);

  let report_and_exit = () =>
    if (! finished^) {
      finished := true;
      let final_z = editor^.editor.editor.state.zipper;
      let final_program = print_zipper(final_z);
      let chat = ChatSystem.Utils.find_chat(chat_id, agent^.chat_system);
      let steps = actions_of_chat(chat);
      let (final_value, final_errors) = describe_evaluation(final_z);
      print_endline("--- final program ---");
      print_endline(final_program);
      print_endline("--- final value ---");
      print_endline(Option.value(~default="<none>", final_value));
      /* The oracle's verdict, reported only now that the run is over. Whether
         the agent got the right answer is not needed to time a trace, but it
         is needed to describe one honestly: "the agent iterated six times and
         converged on the wrong number" is a different trace from "the agent
         solved it in two edits", and both are worth having. */
      switch (goal) {
      | None => ()
      | Some(g) =>
        let ok = goal_met(~goal=g, ~value=final_value, ~errors=final_errors);
        print_endline(
          "--- goal: "
          ++ (ok ? "MET" : "NOT met")
          ++ " (expected "
          ++ String.trim(g)
          ++ ") ---",
        );
      };
      if (final_errors != "") {
        print_endline("--- static errors remain ---");
        print_endline(final_errors);
      };
      print_endline(
        "--- feedback rounds used: " ++ string_of_int(rounds_used^) ++ " ---",
      );
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
      /* Drained once: the metrics row and the transcript report the same
         Jev calls. */
      let jev = JevNav.Log.drain();
      let jev_edit = JevEdit.Log.drain();
      let goal_met_final =
        Option.map(
          g => goal_met(~goal=g, ~value=final_value, ~errors=final_errors),
          goal,
        );
      switch (transcript_out) {
      | None => ()
      | Some(path) =>
        write_file(
          path,
          Yojson.Safe.pretty_to_string(
            Transcript.of_run(
              ~info,
              ~program,
              ~prompt,
              ~chat,
              ~final_program,
              ~final_value,
              ~goal,
              ~goal_met=goal_met_final,
              ~said_done=said_done(chat),
              ~jev,
              ~jev_edit,
            ),
          )
          ++ "\n",
        );
        print_endline("wrote transcript: " ++ path);
      };
      switch (metrics_out) {
      | None => ()
      | Some(path) =>
        let row =
          Metrics.row(
            ~info,
            ~wall_ms=JsUtil.timestamp() -. started_ms,
            ~chat,
            ~goal_met=goal_met_final,
            ~said_done=said_done(chat),
            ~feedback_rounds_used=rounds_used^,
            ~static_errors=static_errors(final_z),
            ~jev,
            ~jev_edit,
            ~transcript=Option.map(Filename.basename, transcript_out),
          );
        append_line(path, Yojson.Safe.to_string(row));
        print_endline("appended metrics: " ++ path);
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
        settle();
      };
    };
  }
  /* The agent has gone idle. Either hand it the result of actually running
     what it wrote and let it iterate, or stop and emit the trace. */
  and settle = () =>
    if (rounds_used^ >= feedback_rounds) {
      report_and_exit();
    } else {
      incr(rounds_used);
      let z = editor^.editor.editor.state.zipper;
      let (value, errors) = describe_evaluation(z);
      let satisfied =
        switch (goal) {
        | Some(g) => goal_met(~goal=g, ~value, ~errors)
        | None => false
        };
      let done_ =
        said_done(ChatSystem.Utils.find_chat(chat_id, agent^.chat_system));
      if (satisfied || done_) {
        if (done_ && !satisfied) {
          prerr_endline("[agent said DONE; stopping]");
        };
        report_and_exit();
      } else {
        prerr_endline(
          "[feedback round "
          ++ string_of_int(rounds_used^)
          ++ "] value="
          ++ Option.value(~default="<none>", value),
        );
        schedule_action(
          SendMessage(
            Message.Utils.mk_user_message(feedback_message(~value, ~errors)),
            chat_id,
          ),
        );
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
       emitter, but NOT payload construction or the HTTP transport.

       In the edit arm the canned call is the arm's own edit tool, jev_edit,
       with --stub-code as the sketch; Jev is replaced through AgentJev's
       seam by [stub_decider], keeping the real JevEdit engine.
       In the builds arm the call has no sketch, as the builds schema has
       none, and --stub-code is the planner's comma-separated names. */
    AgentJev.edit_code :=
      (
        (~api_key as _, ~context, ~request, ~on_done, z) =>
          JevEdit.edit(
            ~decide_choices=stub_decider,
            ~context,
            ~request,
            ~on_done,
            z,
          )
      );
    let (tool_name, args) =
      jev_edit_tool
        ? (
          "jev_edit",
          `Assoc(
            [
              ("path", `String(stub_path)),
              ("intent", `String("stub edit")),
            ]
            @ (
              jev_edit_builds
                ? [
                  (
                    "names",
                    `List(
                      String.split_on_char(',', stub_code)
                      |> List.map(String.trim)
                      |> List.filter((n: string) => n != "")
                      |> List.map(n => `String(n)),
                    ),
                  ),
                ]
                : [("sketch", `String(stub_code))]
            ),
          ),
        )
        : (
          "update_definition",
          `Assoc([
            ("path", `String(stub_path)),
            ("code", `String(stub_code)),
          ]),
        );
    let reply: OpenRouter.Reply.Model.t = {
      content: "Stubbed reply.",
      tool_calls: [
        {
          id: "stub-1",
          name: tool_name,
          args,
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

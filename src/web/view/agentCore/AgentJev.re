open Util;
open Haz3lcore;
open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv;

/* Jev inside the agent loop (docs/notes/jev-nav/plan.md §5,
   v3-jev-implementor.md). View selection serves the pre-pass
   ([[AgentSend]]) and modify_view; [resolve] pre-answers every Jev-backed
   tool call in a reply (modify_view, jev_edit) so the synchronous tool fold
   in [[AgentResponse]] can run unchanged. */

type select_view =
  (
    ~api_key: option(string),
    ~max_tokens: int,
    ~intent: string,
    ~on_done: JevNav.selection => unit,
    Zipper.t
  ) =>
  unit;

/* Without a key Jev cannot answer; failing through [JevNav.select] keeps
   "failed ⇒ view unchanged" the only path callers have to handle. */
let decide_for = (api_key: option(string)): JevNav.decide =>
  switch (api_key) {
  | Some(key) => JevNav.jev_decide(~key)
  | None => (
      (~state as _, ~questions as _, ~handler) =>
        handler(OpenRouter.SystemOne.Failed(401, "No API key set."))
    )
  };

/* Seam like [AgentSend.defer_dispatch_send]: tests swap in a fake selector
   so no HTTP runs. */
let select_view: ref(select_view) =
  ref((~api_key, ~max_tokens, ~intent, ~on_done, z) =>
    JevNav.select(
      ~decide=decide_for(api_key),
      ~max_tokens,
      ~intent,
      ~on_done,
      z,
    )
  );

/** One selection per intent, run in parallel; [on_done] fires once with
    every result, in intent order. [intents] must be distinct and non-empty. */
let select_all =
    (
      ~api_key: option(string),
      ~max_tokens: int,
      ~intents: list(string),
      ~on_done: list((string, JevNav.selection)) => unit,
      z: Zipper.t,
    )
    : unit => {
  let finished = ref([]);
  List.iter(
    intent =>
      select_view^(
        ~api_key,
        ~max_tokens,
        ~intent,
        ~on_done=
          selection => {
            finished := [(intent, selection), ...finished^];
            if (List.length(finished^) == List.length(intents)) {
              on_done(
                List.map(i => (i, List.assoc(i, finished^)), intents),
              );
            };
          },
        z,
      ),
    intents,
  );
};

/* ---- jev_edit ---- */

type edit_code =
  (
    ~api_key: option(string),
    ~context: string,
    ~request: JevEdit.request,
    ~on_done: JevEdit.outcome => unit,
    Zipper.t
  ) =>
  unit;

let decide_choices_for = (api_key: option(string)): JevEdit.decide_choices =>
  switch (api_key) {
  | Some(key) => JevEdit.jev_decide_choices(~key)
  | None => (
      (~state as _, ~questions as _, ~handler) =>
        handler(OpenRouter.SystemOne.ChoiceFailed(401, "No API key set."))
    )
  };

/* Seam, as [select_view]: tests swap in a fake editor so no HTTP runs. */
let edit_code: ref(edit_code) =
  ref((~api_key, ~context, ~request, ~on_done, z) =>
    JevEdit.edit(
      ~decide_choices=decide_choices_for(api_key),
      ~context,
      ~request,
      ~on_done,
      z,
    )
  );

/** One jev_edit answered by Jev, applied later by the tool handler. */
[@deriving (show({with_path: false}), sexp, yojson)]
type edit_result = {
  request: JevEdit.request,
  /* Program text the edit was computed from. The handler applies [zipper]
     only over this exact program, so an edit can never silently drop a
     change made earlier in the same reply. */
  base_text: string,
  zipper: Zipper.t,
  metrics: JevEdit.metrics,
};

/** Every Jev answer one reply needs, keyed by its tool call's arguments. */
[@deriving (show({with_path: false}), sexp, yojson)]
type resolved = {
  views: list((string, JevNav.selection)),
  edits: list(edit_result),
};

let unresolved = {
  views: [],
  edits: [],
};

let program_text = (z: Zipper.t): string =>
  CompositionView.Public.print_zipper(z);

/** The request Jev actually runs. In the builds arm a sketch the model sends
    anyway (e.g. from the prompt's examples) is dropped, so Jev always builds
    from scratch there and the arm measures what it claims to. */
let effective_request =
    (globals: AgentGlobals.Model.t, request: JevEdit.request): JevEdit.request =>
  globals.jev_edit_builds
    ? {
      ...request,
      sketch: "",
    }
    : request;

let find_edit =
    (
      ~globals: AgentGlobals.Model.t,
      resolved: resolved,
      request: JevEdit.request,
    )
    : option(edit_result) => {
  let request = effective_request(globals, request);
  List.find_opt((r: edit_result) => r.request == request, resolved.edits);
};

/** One line for the planner: coverage, then the holes it must fill itself. */
let edit_summary = (m: JevEdit.metrics): string => {
  let filled =
    "filled "
    ++ string_of_int(m.filled)
    ++ "/"
    ++ string_of_int(m.holes_seen)
    ++ " holes";
  switch (m.escalated) {
  | [] => filled
  | holes =>
    filled
    ++ " · unfilled: "
    ++ String.concat(
         ", ",
         List.map(
           (h: JevEdit.hole) => h.hole_id ++ " : " ++ h.expected_type,
           holes,
         ),
       )
  };
};

/** Edits run in call order, each on the previous one's result, so applying
    them in the same order reproduces the chain. A failed edit leaves the
    program as it was for the next. */
let rec edit_chain =
        (
          ~api_key: option(string),
          ~context_of: Zipper.t => string,
          ~on_done: list(edit_result) => unit,
          requests: list(JevEdit.request),
          done_rev: list(edit_result),
          z: Zipper.t,
        )
        : unit =>
  switch (requests) {
  | [] => on_done(List.rev(done_rev))
  | [request, ...rest] =>
    edit_code^(
      ~api_key,
      ~context=context_of(z),
      ~request,
      ~on_done=
        (outcome: JevEdit.outcome) => {
          let result = {
            request,
            base_text: program_text(z),
            zipper: outcome.zipper,
            metrics: outcome.metrics,
          };
          edit_chain(
            ~api_key,
            ~context_of,
            ~on_done,
            rest,
            [result, ...done_rev],
            outcome.metrics.failed ? z : outcome.zipper,
          );
        },
      z,
    )
  };

/* ---- one pre-resolution step for a reply ---- */

/** The reply's Jev-backed calls whose arm is on, distinct, in call order.
    Unparseable calls are left to the normal tool path, which reports them. */
let jev_calls =
    (
      globals: AgentGlobals.Model.t,
      tool_calls: list(OpenRouter.Reply.Model.tool_call),
    )
    : (list(string), list(JevEdit.request)) => {
  let add = (x, xs) => List.mem(x, xs) ? xs : xs @ [x];
  List.fold_left(
    ((intents, requests), tool_call: OpenRouter.Reply.Model.tool_call) =>
      switch (
        CompositionUtils.Public.action_of(
          ~tool_name=tool_call.name,
          ~args=tool_call.args,
        )
      ) {
      | Action(ModifyView(intent, _)) when globals.jev_view_tool => (
          add(intent, intents),
          requests,
        )
      | Action(JevEdit(request)) when globals.jev_edit_tool => (
          intents,
          add(effective_request(globals, request), requests),
        )
      | _ => (intents, requests)
      },
    ([], []),
    tool_calls,
  );
};

/** Ask Jev everything [tool_calls] needs: views in parallel, edits as a
    chain. [on_done] fires once with all answers. Returns false (and never
    calls [on_done]) when there is nothing to ask. */
let resolve =
    (
      ~globals: AgentGlobals.Model.t,
      ~context_of: Zipper.t => string,
      ~on_done: resolved => unit,
      tool_calls: list(OpenRouter.Reply.Model.tool_call),
      z: Zipper.t,
    )
    : bool => {
  let (intents, requests) = jev_calls(globals, tool_calls);
  if (intents == [] && requests == []) {
    false;
  } else {
    let views = ref(intents == [] ? Some([]) : None);
    let edits = ref(None);
    let finish = () =>
      switch (views^, edits^) {
      | (Some(views), Some(edits)) =>
        on_done({
          views,
          edits,
        })
      | _ => ()
      };
    if (intents != []) {
      select_all(
        ~api_key=globals.api_key,
        ~max_tokens=globals.jev_batch_max_tokens,
        ~intents,
        ~on_done=
          selections => {
            views := Some(selections);
            finish();
          },
        z,
      );
    };
    edit_chain(
      ~api_key=globals.api_key,
      ~context_of,
      ~on_done=
        results => {
          edits := Some(results);
          finish();
        },
      requests,
      [],
      z,
    );
    true;
  };
};

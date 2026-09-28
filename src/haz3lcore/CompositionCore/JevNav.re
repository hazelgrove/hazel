/* Jev navigation: select which bindings the agent sees for an intent.

   Functional core: every function here is pure except [select], which only
   composes them around an injected [decide] (the one side effect). Design and
   rationale: docs/notes/jev-nav/plan.md §2–3, nav-encodings.md (E3). */
open Util;
open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv;

module SystemOne = OpenRouter.SystemOne;

/** One binding as Jev sees it: its own code with nested bindings folded. */
[@deriving (show({with_path: false}), sexp, yojson)]
type node = {
  path: string,
  name: string,
  typ: string,
  code: string,
  refs: list(string),
  used_by: list(string),
};

/** Per-selection record; one per [select] call, consumed by eval logging. */
[@deriving (show({with_path: false}), sexp, yojson)]
type metrics = {
  intent: string,
  requests: int,
  questions: int,
  input_tokens: int,
  cost_usd: float,
  latency_ms: float,
  yes: list(string),
  closure_added: list(string),
  failed: bool,
};

/** The one side effect, injected so tests never touch HTTP. */
type decide =
  (
    ~state: API.Json.t,
    ~questions: list(SystemOne.noul_question),
    ~handler: SystemOne.reply => unit
  ) =>
  unit;

[@deriving (show({with_path: false}), sexp, yojson)]
type selection = {
  open_paths: list(string),
  metrics,
};

/** Every selection made in this process, newest last. Eval logging drains it;
    module-level like [AgentSend.pending_main_stream_handle]. */
module Log = {
  let recorded: ref(list(metrics)) = ref([]);
  let record = (m: metrics): unit => recorded := recorded^ @ [m];
  let drain = (): list(metrics) => {
    let all = recorded^;
    recorded := [];
    all;
  };
};

/* ---- nodes: one per binding, from the existing node map ---- */

module Nodes = {
  open Language;
  open Language.Statics;

  /* Shadowed bindings share a name path; suffix the same `#k` the node map
     resolves ([[HighLevelNodeMap.path_to_id]]), so Jev's question ids stay
     unique and its answers address the binding the renderer will open. */
  let path_of = (node_map: HighLevelNodeMap.t, id: Id.t): string => {
    let names =
      HighLevelNodeMap.id_path_to_name_path(
        HighLevelNodeMap.find(node_map, id).path,
        node_map,
      );
    let base = String.concat("/", names);
    switch (HighLevelNodeMap.matches_for_path(node_map, names)) {
    | [_, _, ..._] as shadowed =>
      switch (List.find_index(Id.equal(id), shadowed)) {
      | Some(i) => base ++ "#" ++ string_of_int(i + 1)
      | None => base
      }
    | _ => base
    };
  };

  /* The node map is keyed by Id, so its own order is arbitrary; Jev and the
     logs should see bindings in reading order. */
  let ids_in_program_order = (node_map: HighLevelNodeMap.t): list(Id.t) => {
    let rec with_descendants = (id: Id.t): list(Id.t) => [
      id,
      ...List.concat_map(
           with_descendants,
           HighLevelNodeMap.find(node_map, id).children,
         ),
    ];
    HighLevelNodeMap.gather_top_level(node_map)
    |> HighLevelNodeMap.sort_ids_in_program_order(node_map)
    |> List.concat_map(with_descendants);
  };

  let rec strip_comments = (segment: Segment.t): Segment.t =>
    segment
    |> List.filter(piece => !PrettySegment.is_comment(piece))
    |> List.map((piece: Piece.t) =>
         switch (piece) {
         | Tile(t) =>
           Piece.Tile({
             ...t,
             children: List.map(strip_comments, t.children),
           })
         | _ => piece
         }
       );

  /* Own definition only: nested bindings fold to ⋱ (they are nodes of their
     own), and the body is excluded, so every line appears in one node. */
  let code_of = (z: Zipper.t, node_map: HighLevelNodeMap.t, id: Id.t): string => {
    let nested_defs =
      HighLevelNodeMap.find(node_map, id).children
      |> List.filter_map(
           HighLevelNodeMap.binding_id_to_syntax_projector_target_id(
             node_map,
           ),
         );
    let folded =
      CompositionView.Local.ViewUtils.collapse_terms(
        ~z,
        ~ids=nested_defs,
        ~root=Exp,
      );
    CompositionGo.Local.segment_of_term(
      folded,
      Some(id),
      CachedSyntax.init(folded),
    )
    |> Option.map(segment =>
         segment
         |> strip_comments
         |> CompositionView.Public.print_segment
         |> String.trim
       )
    |> Option.value(~default="");
  };

  let typ_of =
      (info_map: Id.Map.t(Info.t), node: HighLevelNodeMap.node): string =>
    switch (node.info) {
    | InfoExp({user_term, _}) =>
      switch (Exp.term_of(user_term)) {
      | Let(pat, _, _) =>
        switch (Id.Map.find_opt(Pat.rep_id(pat), info_map)) {
        | Some(InfoPat({ty, _})) => Typ.pretty_print(ty)
        | _ => ""
        }
      | _ => ""
      }
    | _ => ""
    };

  let body_id_of = (node: HighLevelNodeMap.node): option(Id.t) =>
    switch (node.info) {
    | InfoExp({user_term, _}) =>
      switch (Exp.term_of(user_term)) {
      | Let(_, _, body)
      | TyAlias(_, _, body)
      | ModuleExp(_, _, body) => Some(Exp.rep_id(body))
      | _ => None
      }
    | _ => None
    };

  /* Innermost binding whose definition (not body) contains [use_id]. A body
     is scope, not ownership: a use in [let x = .. in e] belongs to [e]'s own
     binding. Matching "not the body" rather than "the def" also covers
     fn-sugar, whose def is reached through a synthetic Fun wrapper. */
  let owner_of =
      (
        node_map: HighLevelNodeMap.t,
        info_map: Id.Map.t(Info.t),
        use_id: Id.t,
      )
      : option(Id.t) => {
    let rec first_owner = (child: Id.t, ancestors: list(Id.t)) =>
      switch (ancestors) {
      | [] => None
      | [parent, ...rest] =>
        switch (Id.Map.find_opt(parent, node_map)) {
        | Some(binding) when body_id_of(binding) != Some(child) =>
          Some(parent)
        | _ => first_owner(parent, rest)
        }
      };
    switch (Id.Map.find_opt(use_id, info_map)) {
    | Some(info) => first_owner(use_id, Info.ancestors_of(info))
    | None => None
    };
  };

  /* (user, used) pairs between bindings, from the statics' co-contexts. */
  let uses =
      (
        node_map: HighLevelNodeMap.t,
        info_map: Id.Map.t(Info.t),
        ids: list(Id.t),
      )
      : list((Id.t, Id.t)) =>
    ids
    |> List.concat_map((used: Id.t) => {
         let refs =
           try(
             GeneralTreeUtils.get_refs_to(
               HighLevelNodeMap.find(node_map, used).info,
               info_map,
             )
           ) {
           | _ => VarMap.empty
           };
         refs
         |> List.concat_map(((_, entries: list(CoCtx.entry))) => entries)
         |> List.filter_map((entry: CoCtx.entry) =>
              owner_of(node_map, info_map, entry.id)
            )
         |> List.filter(user => user != used)
         |> List.sort_uniq(Id.compare)
         |> List.map(user => (user, used));
       });

  let of_zipper = (z: Zipper.t): list(node) => {
    let info_map = CompositionGo.Public.mk_statics(z);
    switch (HighLevelNodeMap.build(z, info_map)) {
    | None => []
    | Some(node_map) =>
      let ids = ids_in_program_order(node_map);
      let edges = uses(node_map, info_map, ids);
      /* Filter over [ids] rather than [edges] so refs/used_by keep program order. */
      let related = (keep: Id.t => bool): list(string) =>
        ids |> List.filter(keep) |> List.map(path_of(node_map));
      List.map(
        (id: Id.t) => {
          let node = HighLevelNodeMap.find(node_map, id);
          {
            path: path_of(node_map, id),
            name: node.name,
            typ: typ_of(info_map, node),
            code: code_of(z, node_map, id),
            refs: related(other => List.mem((id, other), edges)),
            used_by: related(other => List.mem((other, id), edges)),
          };
        },
        ids,
      );
    };
  };
};

/* ---- pure core ---- */

let nodes_of = (z: Zipper.t): list(node) => Nodes.of_zipper(z);

/* Wording follows Jev's documented weak spots (plan.md §3): name the path,
   positive criteria on both sides, no counting. "Relevant" was too loose
   (Eval 002: most bindings looked related), so it asks for strict need. */
let question_of = (node: node): SystemOne.noul_question => {
  id: node.path,
  instructions:
    "Must the agent read or change binding `"
    ++ node.path
    ++ "` to carry out the intent?",
  criteria_true:
    "The intent cannot be carried out without reading or changing `"
    ++ node.path
    ++ "`.",
  criteria_false:
    "`"
    ++ node.path
    ++ "` can stay folded: the intent does not require its code.",
};

let strings = (xs: list(string)): API.Json.t =>
  `List(List.map(x => `String(x), xs));

/* Hand-built rather than [yojson_of_node]: Jev's schema says "type", which
   is a reserved word for the record field. The path is the key in the
   state, so it is not repeated here. */
let json_of_binding = (node: node): API.Json.t =>
  `Assoc([
    ("name", `String(node.name)),
    ("type", `String(node.typ)),
    ("code", `String(node.code)),
    ("refs", strings(node.refs)),
    ("used_by", strings(node.used_by)),
  ]);

/* Keyed by path, not an array: each question names a path, and a lookup by
   key is far less error-prone for a model than finding an array position. */
let state_of = (~intent: string, nodes: list(node)): API.Json.t =>
  `Assoc([
    ("intent", `String(intent)),
    (
      "bindings",
      `Assoc(List.map((n: node) => (n.path, json_of_binding(n)), nodes)),
    ),
  ]);

/* Identifier-like runs, keeping `/` and `.` so qualified paths stay whole;
   sentence punctuation at the edges is trimmed. */
let words_of = (text: string): list(string) => {
  let is_word_char = c =>
    switch (c) {
    | 'a' .. 'z'
    | 'A' .. 'Z'
    | '0' .. '9'
    | '_'
    | '\''
    | '.'
    | '/' => true
    | _ => false
    };
  let trim = w => {
    let rec strip = w =>
      w != "" && String.contains("./", w.[String.length(w) - 1])
        ? strip(String.sub(w, 0, String.length(w) - 1)) : w;
    strip(w);
  };
  String.to_seq(text)
  |> Seq.map(c => is_word_char(c) ? c : ' ')
  |> String.of_seq
  |> String.split_on_char(' ')
  |> List.map(trim)
  |> List.filter(w => w != "");
};

let last_segment = (path: string): string =>
  ListUtil.last_opt(String.split_on_char('/', path))
  |> Option.value(~default=path);

/** Bindings the intent names exactly, as a whole word: the full path, its
    dotted form (`Fuel.fuel_cost`), or its last segment when no other
    binding shares it. The planner already decided these; asking Jev would
    only add error. */
let mentioned_paths = (~intent: string, nodes: list(node)): list(string) => {
  let words = words_of(intent);
  let unique_last = (path: string) =>
    List.length(
      List.filter(
        (n: node) => last_segment(n.path) == last_segment(path),
        nodes,
      ),
    )
    == 1;
  nodes
  |> List.filter((n: node) =>
       List.exists(
         form => List.mem(form, words),
         [n.path, String.map(c => c == '/' ? '.' : c, n.path)]
         @ (unique_last(n.path) ? [last_segment(n.path)] : []),
       )
     )
  |> List.map((n: node) => n.path);
};

/* ~4 chars per token: crude, but batching only needs the right order of
   magnitude to stay far under Jev's 32K context. */
let tokens_of = (node: node): int =>
  (
    String.length(node.path)
    + String.length(API.Json.to_string(json_of_binding(node)))
  )
  / 4;

let first_line = (code: string): string =>
  switch (String.index_opt(code, '\n')) {
  | Some(i) => String.sub(code, 0, i)
  | None => code
  };

let batches = (~max_tokens: int, nodes: list(node)): list(list(node)) => {
  let close = (current, done_) =>
    current == [] ? done_ : [List.rev(current), ...done_];
  let (current, _, done_) =
    List.fold_left(
      ((current, used, done_), node) => {
        let tokens = tokens_of(node);
        if (tokens > max_tokens) {
          (
            [],
            0,
            [
              [
                {
                  ...node,
                  code: first_line(node.code),
                },
              ],
              ...close(current, done_),
            ],
          );
        } else if (used + tokens > max_tokens) {
          ([node], tokens, close(current, done_));
        } else {
          ([node, ...current], used + tokens, done_);
        };
      },
      ([], 0, []),
      nodes,
    );
  List.rev(close(current, done_));
};

/* Take Jev's own answer: "yes" means it rated yes more likely than no. No
   extra cutoff on top; if Jev opens too much, the question is what to fix. */
let jev_says_yes = (answers: list(SystemOne.answer)): list(string) =>
  answers
  |> List.filter((a: SystemOne.answer) => a.p_yes > 0.5)
  |> List.map((a: SystemOne.answer) => a.id);

let proper_prefixes = (path: string): list(string) => {
  let segments = String.split_on_char('/', path);
  List.init(List.length(segments) - 1, n =>
    String.concat("/", List.filteri((i, _) => i <= n, segments))
  );
};

/* A folded parent hides an open child, so a view must hold every ancestor
   of what it opens. */
let close_ancestors = (yes: list(string)): list(string) =>
  yes
  |> List.concat_map(path => proper_prefixes(path) @ [path])
  |> List.fold_left(
       (seen, path) => List.mem(path, seen) ? seen : seen @ [path],
       [],
     );

/* ---- composition ---- */

/** Pure fold of all batch replies into the selection. Any failed batch
    fails the whole selection: a partial view could hide what matters, so the
    caller keeps its current view instead. */
let selection_of_replies =
    (
      ~mentioned: list(string)=[],
      ~intent: string,
      ~questions: int,
      ~latency_ms: float,
      replies: list(SystemOne.reply),
    )
    : selection => {
  let answered =
    List.filter_map(
      (reply: SystemOne.reply) =>
        switch (reply) {
        | Answers(answers, usage) => Some((answers, usage))
        | Failed(_) => None
        },
      replies,
    );
  let failed = List.length(answered) != List.length(replies);
  let answers = List.concat_map(fst, answered);
  let usages = List.map(snd, answered);
  let asked_yes = jev_says_yes(answers);
  /* Mentioned paths count as "yes" so metrics and closure treat them like
     any other opened binding. */
  let yes = mentioned @ asked_yes;
  let opened = failed ? [] : close_ancestors(yes);
  {
    open_paths: opened,
    metrics: {
      intent,
      requests: List.length(replies),
      questions,
      input_tokens:
        List.fold_left(
          (sum, u: SystemOne.usage) => sum + u.input_tokens,
          0,
          usages,
        ),
      cost_usd:
        List.fold_left(
          (sum, u: SystemOne.usage) =>
            sum +. Option.value(~default=0.0, u.cost_usd),
          0.0,
          usages,
        ),
      latency_ms,
      yes,
      closure_added: List.filter(path => !List.mem(path, yes), opened),
      failed,
    },
  };
};

/** Sends every batch at once and finishes on the last reply, whether
    [decide] answers synchronously (tests) or later (HTTP). Replies are kept
    by batch index so the result is in program order regardless of arrival. */
let select =
    (
      ~decide: decide,
      ~max_tokens: int=8000,
      ~intent: string,
      ~on_done: selection => unit,
      z: Zipper.t,
    )
    : unit => {
  let nodes = nodes_of(z);
  let mentioned = mentioned_paths(~intent, nodes);
  let asked = List.filter((n: node) => !List.mem(n.path, mentioned), nodes);
  let batched = batches(~max_tokens, asked);
  let started_ms = JsUtil.timestamp();
  let finish = (replies: list(SystemOne.reply)) => {
    let selection =
      selection_of_replies(
        ~mentioned,
        ~intent,
        ~questions=List.length(asked),
        ~latency_ms=JsUtil.timestamp() -. started_ms,
        replies,
      );
    Log.record(selection.metrics);
    on_done(selection);
  };
  let replies = Array.make(List.length(batched), None);
  let pending = ref(List.length(batched));
  let on_reply = (index: int, reply: SystemOne.reply) => {
    replies[index] = Some(reply);
    pending := pending^ - 1;
    if (pending^ == 0) {
      finish(replies |> Array.to_list |> List.filter_map(Fun.id));
    };
  };
  batched == []
    ? finish([])
    : List.iteri(
        (index, batch) =>
          decide(
            ~state=state_of(~intent, batch),
            ~questions=List.map(question_of, batch),
            ~handler=on_reply(index),
          ),
        batched,
      );
};

/** Real Jev via OpenRouter, for callers that hold an API key. */
let jev_decide = (~key: string): decide =>
  (~state, ~questions, ~handler) =>
    SystemOne.decide(~key, ~state, ~questions, ~handler, ());

/* Jev as the implementor (V3): fill a planner's sketch by choosing, per typed
   hole, among the well-typed candidates Hazel already computes (TyDi).
   Functional core: pure except [edit], which composes around an injected
   [decide_choices]. Design: docs/notes/jev-nav/v3-jev-implementor.md. */
open Util;
open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv;

module SystemOne = OpenRouter.SystemOne;

/** What the planner asks for. [sketch] is Hazel code with `?` holes; Jev fills
    only the holes. [names]/[literals] are the planner's vocabulary: the only
    free-form text Jev may insert. */
[@deriving (show({with_path: false}), sexp, yojson)]
type request = {
  path: string,
  sketch: string,
  names: list(string),
  literals: list(string),
  intent: string,
  /* Build mode's typed spec, as Hazel type text ("Int -> Int"); "" = none.
     It gives the root hole a precise type, so the type system prunes Jev's
     options from the first round. */
  [@yojson.default ""] [@sexp.default ""]
  signature: string,
};

/** One open hole, as Jev sees it. */
[@deriving (show({with_path: false}), sexp, yojson)]
type hole = {
  hole_id: string,
  expected_type: string,
  candidates: list(string),
};

/** Candidate text meaning "none of these fit; hand back to the planner". */
let escalate = "<escalate>";

[@deriving (show({with_path: false}), sexp, yojson)]
type metrics = {
  intent: string,
  path: string,
  rounds: int,
  holes_seen: int,
  filled: int,
  escalated: list(hole),
  requests: int,
  input_tokens: int,
  cost_usd: float,
  latency_ms: float,
  failed: bool,
  error: option(string),
};

type outcome = {
  zipper: Zipper.t,
  metrics,
};

type decide_choices =
  (
    ~state: API.Json.t,
    ~questions: list(SystemOne.choice_question),
    ~handler: SystemOne.choice_reply => unit
  ) =>
  unit;

/** Every edit run in this process, newest last; eval logging drains it. */
module Log = {
  let recorded: ref(list(metrics)) = ref([]);
  let record = (m: metrics): unit => recorded := recorded^ @ [m];
  let drain = (): list(metrics) => {
    let all = recorded^;
    recorded := [];
    all;
  };
};

/* Choice allows 255 options; one is always [escalate]. */
let max_candidates = 254;

/* ---- candidate engine: TyDi, restricted to what a hole can take whole ---- */

module Candidates = {
  open Language;
  open Language.Statics;

  let builtins = lazy(Builtins.ctx_init(Some(Operators.default_mode)));

  /* Hundreds of builtins type-check at almost any hole and would drown the
     program's own bindings; the planner names any builtin it wants. */
  let program_only = (ci: Info.t): Info.t =>
    switch (ci) {
    | InfoExp(e) =>
      InfoExp({
        ...e,
        ctx: Ctx.added_bindings(e.ctx, Lazy.force(builtins)),
      })
    | _ => ci
    };

  let is_name_char = (c: char): bool =>
    switch (c) {
    | 'a' .. 'z'
    | 'A' .. 'Z'
    | '0' .. '9'
    | '_'
    | '\''
    | '.' => true
    | _ => false
    };

  /* Rejects lookahead forms like `x::` or `x, ` that open new syntax. */
  let is_reference = (token: string): bool =>
    token != ""
    && (
      switch (token.[0]) {
      | 'a' .. 'z'
      | 'A' .. 'Z' => true
      | _ => false
      }
    )
    && String.for_all(is_name_char, token);

  /* Keep only suggestions that fill a hole as one complete expression:
     references, and applications `f(?)` whose argument becomes a hole for
     the next round. TyDi's keyword and operator tokens are partial syntax;
     their complete forms come from [forms] below. */
  let of_suggestion = (s: TyDiSuggestion.t): option(string) =>
    switch (s.strategy) {
    | Exp(Common(FromCtx(_))) when is_reference(s.content) =>
      Some(s.content)
    | Exp(Common(FromCtxAp(_))) =>
      let callee = String.sub(s.content, 0, String.length(s.content) - 1);
      String.ends_with(~suffix="(", s.content) && is_reference(callee)
        ? Some(callee ++ "(?)") : None;
    | Exp(Common(NewForm(_)))
        when s.content == "true" || s.content == "false" =>
      Some(s.content)
    | _ => None
    };

  let dedupe = (xs: list(string)): list(string) =>
    List.fold_left(
      (seen, x) => List.mem(x, seen) ? seen : seen @ [x],
      [],
      xs,
    );

  let unknown = TyDiForms.Typ.unk;

  /* Tuples and sequencing have their own forms or none; pipe and the
     logic-only connectives would be offered at nearly every hole. */
  let excluded_infix = [",", ";", "|>", "!", "\\/"];

  let infix_forms: list((string, Typ.t)) =
    TyDiForms.Typ.of_infix_delim
    |> List.filter(((token, _)) => !List.mem(token, excluded_infix))
    |> List.map(((token, result)) =>
         ("? " ++ token ++ " ?", Typ.fresh(result))
       );

  /** Structure Jev can build: complete forms whose sub-terms are holes for
      later rounds. Binders need a name, so there is one per planner name.
      Types are the forms' result types, as TyDiForms records them. */
  let forms = (~names: list(string)): list((string, Typ.t)) =>
    [
      ("if ? then ? else ?", unknown),
      ("(?, ?)", Typ.fresh(Prod([unknown, unknown]))),
      ("[?]", Typ.fresh(List(unknown))),
      ("[]", Typ.fresh(List(unknown))),
    ]
    @ List.concat_map(
        name =>
          [
            ("fun " ++ name ++ " -> ?", Typ.fresh(Arrow(unknown, unknown))),
            ("let " ++ name ++ " = ? in ?", unknown),
          ],
        names,
      )
    @ infix_forms;

  /* Only a light type filter: a form whose result can never fit the hole is
     noise in Jev's options. Anything subtler is left to the fill guard. */
  let fitting_forms = (~names: list(string), ci: Info.t): list(string) =>
    switch (ci) {
    | InfoExp({ana, ctx, _}) =>
      forms(~names)
      |> List.filter(((_, ty)) => Typ.is_consistent(ctx, ana, ty))
      |> List.map(fst)
    | _ => []
    };

  /* Order matters to Jev's reading and to the cap: the program's own
     references, then the planner's vocabulary, then new structure. A
     pattern hole only binds, so it takes planner names and nothing else. */
  let for_hole = (~request: request, ~z: Zipper.t, ci: Info.t): list(string) =>
    (
      switch (ci) {
      | InfoPat(_) => request.names
      | _ =>
        (
          TyDi.suggest(program_only(ci), z) |> List.filter_map(of_suggestion)
        )
        @ request.names
        @ request.literals
        @ fitting_forms(~names=request.names, ci)
      }
    )
    |> dedupe
    |> List.filteri((i, _) => i < max_candidates);
};

/* ---- holes: located in the target binding's definition ---- */

/** Which holes an edit owns. Replacing a definition owns the holes in that
    definition; creating new code owns only the holes the new code brought, so
    a pre-existing `?` elsewhere (e.g. the empty program itself) is never asked. */
type scope =
  | Definition(string)
  | NewCode(list(Id.t));

module Holes = {
  open Language;
  open Language.Statics;
  open OptUtil.Syntax;

  /* Positional ids read naturally in Jev's state ("hole_2") and are stable
     for a round, which is as long as any id lives: every round re-reads the
     program. */
  let marker = (index: int): string => "hole_" ++ string_of_int(index + 1);

  let is_empty_hole = (info: Info.t): bool =>
    switch (info) {
    | InfoExp({user_term, _}) =>
      switch (Exp.term_of(user_term)) {
      | EmptyHole => true
      | _ => false
      }
    | InfoPat({user_term, _}) =>
      switch (Pat.term_of(user_term)) {
      | EmptyHole => true
      | _ => false
      }
    | _ => false
    };

  /* Inside the binding but not under its body: the body is later code that
     the edit does not own. */
  let in_definition =
      (binding: HighLevelNodeMap.node, (id: Id.t, info: Info.t)): bool => {
    let chain = [id, ...Info.ancestors_of(info)];
    List.mem(HighLevelNodeMap.id_of(binding), chain)
    && (
      switch (JevNav.Nodes.body_id_of(binding)) {
      | Some(body) => !List.mem(body, chain)
      | None => true
      }
    );
  };

  let in_program_order = (z: Zipper.t, ids: list(Id.t)): list(Id.t) => {
    let order = Segment.ids(Zipper.unselect_and_zip(z));
    let position = id =>
      ListUtil.findi_opt(Id.equal(id), order)
      |> Option.map(fst)
      |> Option.value(~default=max_int);
    List.sort((a, b) => compare(position(a), position(b)), ids);
  };

  /** Each hole with the term id it fills; [holes_in] drops the ids. */
  let located =
      (~request: request, ~scope: scope, z: Zipper.t): list((hole, Id.t)) => {
    let info_map = CompositionGo.Public.mk_statics(z);
    let owns: option(((Id.t, Info.t)) => bool) =
      switch (scope) {
      | NewCode(existing) => Some(((id, _)) => !List.mem(id, existing))
      | Definition(path) =>
        let* node_map = HighLevelNodeMap.build(z, info_map);
        let+ id = HighLevelNodeMap.path_to_id_opt(node_map, path);
        in_definition(HighLevelNodeMap.find(node_map, id));
      };
    switch (owns) {
    | None => []
    | Some(owns) =>
      Id.Map.bindings(info_map)
      |> List.filter(((_, info)) => is_empty_hole(info))
      |> List.filter(owns)
      |> List.map(fst)
      |> in_program_order(z)
      |> List.mapi((index, id) => {
           let ci = Id.Map.find(id, info_map);
           (
             {
               hole_id: marker(index),
               expected_type:
                 switch (ci) {
                 | InfoExp({ana, _})
                 | InfoPat({ana, _}) => ErrorPrint.Print.typ(ana)
                 | _ => ""
                 },
               candidates: Candidates.for_hole(~request, ~z, ci),
             },
             id,
           );
         })
    };
  };

  let static_errors = (z: Zipper.t): int =>
    List.length(ErrorPrint.all(CompositionGo.Public.mk_statics(z)));

  /* TyDi candidates are well-typed at their hole, but the planner's names and
     literals are not checked by anyone else. Refusing any fill that adds a
     static error keeps every Jev edit well-typed by construction; the hole
     escalates instead. */
  let overwrite = (z: Zipper.t, id: Id.t, code: string): option(Zipper.t) =>
    CompositionGo.Local.PerformUtils.overwrite_term(
      z,
      id,
      code,
      false,
      CachedSyntax.init(z),
    )
    |> Stdlib.Result.to_option;

  let fill = (z: Zipper.t, id: Id.t, code: string): option(Zipper.t) =>
    switch (overwrite(z, id, code)) {
    | Some(z') when static_errors(z') <= static_errors(z) => Some(z')
    | _ => None
    };

  /* The target's current definition with each hole written as its marker,
     so Jev can see where [hole_k] sits. Rendering only; never applied. */
  let marked_definition =
      (
        ~request: request,
        ~scope: scope,
        z: Zipper.t,
        located: list((hole, Id.t)),
      )
      : string => {
    let marked =
      List.fold_left(
        /* Markers are unbound names, so the type guard in [fill] would
           reject them; this rendering is never applied to the program. */
        (z, (h, id)) =>
          overwrite(z, id, h.hole_id) |> Option.value(~default=z),
        z,
        located,
      );
    let info_map = CompositionGo.Public.mk_statics(marked);
    switch (scope) {
    /* New code has no single binding to show; the program is the context. */
    | NewCode(_) => CompositionView.Public.print_zipper(marked)
    | Definition(path) =>
      {
        let* node_map = HighLevelNodeMap.build(marked, info_map);
        let* id = HighLevelNodeMap.path_to_id_opt(node_map, path);
        CompositionGo.Local.segment_of_term(
          marked,
          Some(id),
          CachedSyntax.init(marked),
        );
      }
      |> Option.map(CompositionView.Public.print_segment)
      |> Option.value(~default=request.sketch)
    };
  };

  /* A `let … in` needs a body after it, so new code goes after the last
     top-level binding (keeping everything above it in scope), or in front of
     the whole program when there are none (the old program becomes the body). */
  let create = (z: Zipper.t, sketch: string): result(Zipper.t, string) => {
    let last_top_level = {
      let* node_map =
        HighLevelNodeMap.build(z, CompositionGo.Public.mk_statics(z));
      HighLevelNodeMap.gather_top_level(node_map)
      |> List.map(id => HighLevelNodeMap.find(node_map, id))
      |> List.sort((a: HighLevelNodeMap.node, b: HighLevelNodeMap.node) =>
           compare(b.sibling_idx, a.sibling_idx)
         )
      |> ListUtil.hd_opt
      |> Option.map((node: HighLevelNodeMap.node) =>
           String.concat(
             "/",
             HighLevelNodeMap.id_path_to_name_path(node.path, node_map),
           )
         );
    };
    switch (last_top_level) {
    | None => CompositionGo.Public.insert_at_boundary(z, Before, sketch)
    | Some(path) =>
      CompositionGo.Public.go(
        ~syntax=CachedSyntax.init(z),
        ~z,
        ~a=Action.Structural.Insert(After, path, sketch),
      )
      |> Stdlib.Result.map_error(Action.Failure.show)
    };
  };

  let binding_exists = (z: Zipper.t, path: string): bool =>
    {
      let* node_map =
        HighLevelNodeMap.build(z, CompositionGo.Public.mk_statics(z));
      HighLevelNodeMap.path_to_id_opt(node_map, path);
    }
    |> Option.is_some;
};

/* ---- pure core ---- */

let holes_in = (~path: string, z: Zipper.t): list(hole) =>
  Holes.located(
    ~scope=Definition(path),
    ~request={
      path,
      sketch: "",
      names: [],
      literals: [],
      intent: "",
      signature: "",
    },
    z,
  )
  |> List.map(fst);

/* Wording per Jev's weak spots (plan.md §3): name the hole explicitly and
   make giving up an ordinary, positive option. */
let question_of = (h: hole): SystemOne.choice_question => {
  id: h.hole_id,
  instructions:
    "What should replace `"
    ++ h.hole_id
    ++ "` (expected type "
    ++ h.expected_type
    ++ ") to achieve the intent? Choose "
    ++ escalate
    ++ " when none of the options fits.",
  options:
    List.filteri((i, _) => i < max_candidates, h.candidates) @ [escalate],
};

let strings = (xs: list(string)): API.Json.t =>
  `List(List.map(x => `String(x), xs));

let json_of_hole = (h: hole): API.Json.t =>
  `Assoc([
    ("hole", `String(h.hole_id)),
    ("expected_type", `String(h.expected_type)),
  ]);

/** Like [state_of], with the target's current code (holes shown by their
    ids); after round 1 the original sketch no longer shows where holes are. */
let state_with_current =
    (
      ~request: request,
      ~context: string,
      ~current: string,
      holes: list(hole),
    )
    : API.Json.t =>
  `Assoc([
    ("intent", `String(request.intent)),
    ("view", `String(context)),
    ("target", `String(request.path)),
    ("signature", `String(request.signature)),
    ("current_definition", `String(current)),
    ("new_names", strings(request.names)),
    ("literals", strings(request.literals)),
    ("holes", `List(List.map(json_of_hole, holes))),
  ]);

let state_of =
    (~request: request, ~context: string, holes: list(hole)): API.Json.t =>
  state_with_current(~request, ~context, ~current=request.sketch, holes);

/* The parser is error-tolerant, so "Int ->" parses too, with a hole where
   the result type should be; printing holes as `?` exposes it. */
let complete_type = (text: string): bool =>
  switch (Parser.to_segment(text, ~root=Typ)) {
  | Some(segment) =>
    !String.contains(Printer.of_segment(~holes="?", segment), '?')
  | None => false
  };

let no_holes_error = "sketch has no ? holes — leave ? for Jev to fill, or omit the sketch (build mode) and give signature/intent/names/literals";

/* ---- composition ---- */

type round_result = {
  zipper: Zipper.t,
  filled: int,
  escalated: list((hole, Id.t)),
};

/** Pure: apply one round's answers. A pick below [min_confidence], the
    escape option, or a fill the editor rejects all escalate the hole, so a
    doubtful guess is never written into the program. */
let apply_answers =
    (
      ~min_confidence: float,
      z: Zipper.t,
      located: list((hole, Id.t)),
      answers: list(SystemOne.choice_answer),
    )
    : round_result =>
  List.fold_left(
    (acc: round_result, (h: hole, id: Id.t)) => {
      let pick =
        List.find_opt(
          (a: SystemOne.choice_answer) => a.id == h.hole_id,
          answers,
        )
        |> Option.to_list
        |> List.filter((a: SystemOne.choice_answer) =>
             a.choice != escalate && a.confidence >= min_confidence
           );
      switch (pick) {
      | [a] =>
        switch (Holes.fill(acc.zipper, id, a.choice)) {
        | Some(zipper) => {
            ...acc,
            zipper,
            filled: acc.filled + 1,
          }
        | None => {
            ...acc,
            escalated: acc.escalated @ [(h, id)],
          }
        }
      | _ => {
          ...acc,
          escalated: acc.escalated @ [(h, id)],
        }
      };
    },
    {
      zipper: z,
      filled: 0,
      escalated: [],
    },
    located,
  );

/** Apply [request.sketch] at [request.path], then fill holes round by round
    (one Jev request per round, one Choice per hole) until none remain, all
    remaining escalate, or [max_rounds]. [context] is the curated program view.

    The sketch goes through the agent's own Update Definition path, so the
    same static-error veto applies. On a failed Jev request the outcome is
    the program from before the sketch: the edit is all-or-nothing on
    transport failure, and the planner can retry the same call. Escalated
    holes stay as `?` in the program for the planner to fill. */
let edit =
    (
      ~decide_choices: decide_choices,
      ~max_rounds: int=4,
      ~max_rounds_build: int=10,
      ~max_holes: int=40,
      ~min_confidence: float=0.5,
      ~context: string,
      ~request: request,
      ~on_done: outcome => unit,
      z: Zipper.t,
    )
    : unit => {
  /* An empty sketch asks Jev to build the whole definition from one hole,
     which takes more rounds than filling a planner's sketch. */
  let building = String.trim(request.sketch) == "";
  let max_rounds = building ? max_rounds_build : max_rounds;
  let started_ms = JsUtil.timestamp();
  let base = {
    intent: request.intent,
    path: request.path,
    rounds: 0,
    holes_seen: 0,
    filled: 0,
    escalated: [],
    requests: 0,
    input_tokens: 0,
    cost_usd: 0.0,
    latency_ms: 0.0,
    failed: false,
    error: None,
  };
  let finish = (zipper: Zipper.t, m: metrics) => {
    let metrics = {
      ...m,
      latency_ms: JsUtil.timestamp() -. started_ms,
    };
    Log.record(metrics);
    on_done({
      zipper,
      metrics,
    });
  };
  let fail = (m: metrics, error: string) =>
    finish(
      z,
      {
        ...m,
        failed: true,
        error: Some(error),
      },
    );
  /* [skipped]: escalated holes stay in the program, and asking again would
     get the same answer, so they are not re-asked. Term ids survive fills
     elsewhere, unlike the positional hole ids. */
  let rec round =
          (~scope: scope, zipper: Zipper.t, skipped: list(Id.t), m: metrics) => {
    let open_holes =
      Holes.located(~request, ~scope, zipper)
      |> List.filter(((_, id)) => !List.mem(id, skipped));
    let over_budget = m.holes_seen + List.length(open_holes) > max_holes;
    if (open_holes == [] || m.rounds >= max_rounds || over_budget) {
      finish(
        CompositionGo.Local.PerformUtils.normalize_top_level(zipper),
        /* Forms beget holes; past the budget the expansion is running away,
           so what is left goes back to the planner. */
        over_budget
          ? {
            ...m,
            escalated: m.escalated @ List.map(fst, open_holes),
          }
          : m,
      );
    } else {
      let holes = List.map(fst, open_holes);
      decide_choices(
        ~state=
          state_with_current(
            ~request,
            ~context,
            ~current=
              Holes.marked_definition(~request, ~scope, zipper, open_holes),
            holes,
          ),
        ~questions=List.map(question_of, holes),
        ~handler=
          fun
          | SystemOne.ChoiceFailed(code, message) =>
            fail(
              {
                ...m,
                rounds: m.rounds + 1,
                requests: m.requests + 1,
              },
              Printf.sprintf("Jev Choice failed (%d): %s", code, message),
            )
          | Chosen(answers, usage) => {
              let result =
                apply_answers(~min_confidence, zipper, open_holes, answers);
              round(
                ~scope,
                result.zipper,
                skipped @ List.map(snd, result.escalated),
                {
                  ...m,
                  rounds: m.rounds + 1,
                  requests: m.requests + 1,
                  holes_seen: m.holes_seen + List.length(holes),
                  filled: m.filled + result.filled,
                  escalated: m.escalated @ List.map(fst, result.escalated),
                  input_tokens: m.input_tokens + usage.input_tokens,
                  cost_usd:
                    m.cost_usd +. Option.value(~default=0.0, usage.cost_usd),
                },
              );
            },
      );
    };
  };
  /* Only build mode reads the signature: a sketch already fixes its own
     shape, and an annotation there is the planner's to write. */
  let typed = building && String.trim(request.signature) != "";
  let name =
    ListUtil.last_opt(String.split_on_char('/', request.path))
    |> Option.value(~default=request.path);
  let annotated = "let " ++ name ++ " : " ++ request.signature ++ " = ? in";
  /* A signature is only useful if it type-checks and pins the root hole's
     type completely; otherwise Jev would build against a guess. Checked
     before any Jev call. */
  let signature_error = (sketched: Zipper.t): option(string) =>
    if (!typed) {
      None;
    } else if (Holes.static_errors(sketched) > Holes.static_errors(z)) {
      Some("Signature `" ++ request.signature ++ "` does not type-check.");
    } else if (!complete_type(request.signature)) {
      Some(
        "Signature `"
        ++ request.signature
        ++ "` is not a complete type (it leaves a `?` hole).",
      );
    } else {
      None;
    };
  /* Jev never re-types what the planner already wrote: a sketch with
     nothing left to fill is a plain edit, which belongs to the planner's own
     tools. Failing returns the original program, so nothing is applied. */
  let start = (~scope: scope, sketched: Zipper.t) =>
    switch (signature_error(sketched)) {
    | Some(error) => fail(base, error)
    | None when !building && Holes.located(~request, ~scope, sketched) == [] =>
      fail(base, no_holes_error)
    | None => round(~scope, sketched, [], base)
    };
  /* An existing path is revised in place; a new path (or empty program) is
     created by appending the sketch, as a no-path insert_after would. */
  if (Holes.binding_exists(z, request.path)) {
    /* With a signature the whole clause is rewritten, which sets the
       annotation as well as resetting the definition to one hole. */
    let action: Action.Structural.t =
      typed
        ? Update(BindingClause, request.path, annotated)
        : Update(Definition, request.path, building ? "?" : request.sketch);
    switch (
      CompositionGo.Public.go(~syntax=CachedSyntax.init(z), ~z, ~a=action)
    ) {
    | Ok(sketched) => start(~scope=Definition(request.path), sketched)
    | Error(e) => fail(base, "Sketch rejected: " ++ Action.Failure.show(e))
    };
  } else {
    let existing =
      Id.Map.bindings(CompositionGo.Public.mk_statics(z)) |> List.map(fst);
    let sketch =
      switch (building, typed) {
      | (true, true) => annotated
      | (true, false) => "let " ++ request.path ++ " = ? in"
      | (false, _) => request.sketch
      };
    switch (Holes.create(z, sketch)) {
    | Ok(created) => start(~scope=NewCode(existing), created)
    | Error(msg) => fail(base, "Sketch rejected: " ++ msg)
    };
  };
};

let jev_decide_choices = (~key: string): decide_choices =>
  (~state, ~questions, ~handler) =>
    SystemOne.decide_choices(~key, ~state, ~questions, ~handler, ());

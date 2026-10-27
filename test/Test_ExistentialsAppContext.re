open Alcotest;
open Util;
open Haz3lcore;
open Web;
open Language;

/* Reset-existentials-app-context: the ACTUAL app proof path for the
   existential witness, against native evaluation with real statics on the
   SAME source. Statics as the editor computes them
   (CachedStatics.init_from_term, is_dynamic_term=false); the real
   EvalResult.Update.update/calculate wiring with a capturing queue_worker;
   worker replies from the real WorkerServer.evaluate_sync on the captured
   request; the displayed map is EvalResult.Model.proof_map (both CellEditor
   consumers). Fixtures are gibr's Test_ResetExistentials programs.
   Map comparison reuses the root-contract comparator frozen in
   Test_ResetProofContextIntegration (22ace4de): per key and per entry field,
   strict identities, lexeme-aware terms, never whole-map show. Every result
   is printed as an OBS line; assertions are predictions, kept as written. */

let show_ids = l => "[" ++ String.concat("; ", l) ++ "]";
let ids_of = m =>
  Id.Map.bindings(m) |> List.map(((id, _)) => Id.to_string(id));
let obs = (label, line) => print_endline("OBS " ++ label ++ " " ++ line);

/* Map comparison, key by key and over every ProofMap.entry field (the
   whole-map pp is unusable: Id.Map.pp does not print non-empty maps).
   Root's comparator contract:
   - terms: "strict" (OCaml structural equality, generated ids included) or
     "semantic" (Exp.fast_equal_with_lexemes: same literals, operators,
     ascriptions and lexemes; only generated term ids/parens may differ);
     every semantic field is printed by key and field;
   - obligations: origin, discharge (constructor and Local/Remote id) and
     bindings must be STRICT; goal/display_goal compare as terms above;
   - marks: STRICT only;
   - anything else is "DIFF", printed with both raw values. */
let strict_status = (a, b) =>
  switch (a == b) {
  | true => "equal"
  | false => "unequal"
  | exception e => "== raised " ++ Printexc.to_string(e)
  };
let strict_eq = (a, b) => strict_status(a, b) == "equal";
let one_line = s =>
  String.map(c => c == '\n' ? ' ' : c, s)
  |> (s => String.length(s) > 400 ? String.sub(s, 0, 400) ++ "…" : s);
let raw = (f, x) =>
  switch (f(x)) {
  | s => one_line(s)
  | exception e => "<show raised " ++ Printexc.to_string(e) ++ ">"
  };
let diff = (f, a, b) =>
  "DIFF(" ++ strict_status(a, b) ++ ") a=" ++ raw(f, a) ++ " b=" ++ raw(f, b);
let raw_full = (f, x) =>
  switch (f(x)) {
  | s =>
    let s = String.map(c => c == '\n' ? ' ' : c, s);
    String.length(s) > 3000 ? String.sub(s, 0, 3000) ++ "…" : s;
  | exception e => "<show raised " ++ Printexc.to_string(e) ++ ">"
  };
let term_verdict = (a: Exp.t, b: Exp.t) =>
  strict_eq(a, b)
    ? "strict"
    : Exp.fast_equal_with_lexemes(a, b) ? "semantic" : diff(Exp.show, a, b);
let strict_verdict = (f, a, b) => strict_eq(a, b) ? "strict" : diff(f, a, b);
/* Worst of a list of verdicts: any DIFF, else semantic, else strict. */
let combine = vs =>
  switch (List.find_opt(v => String.length(v) >= 4 && String.sub(v, 0, 4) == "DIFF", vs)) {
  | Some(d) => d
  | None => List.mem("semantic", vs) ? "semantic" : "strict"
  };
let opt_term_verdict = (a, b) =>
  switch (a, b) {
  | (None, None) => "strict"
  | (Some(a), Some(b)) => term_verdict(a, b)
  | _ =>
    diff(o => Option.fold(~none="None", ~some=Exp.show, o), a, b)
  };
/* Ctx bindings: LivelitEntry holds closures, so OCaml == cannot compare a
   binding list. Compare each entry field by field instead: constructor,
   names, ids and custom statics STRICT; types strict or Typ.fast_equal
   ("semantic"); theorem props as terms; livelits by physical identity. */
let typ_verdict = (a: Typ.t, b: Typ.t) =>
  strict_eq(a, b)
    ? "strict" : Typ.fast_equal(a, b) ? "semantic" : diff(Typ.show, a, b);
let name_verdict = (a: string, b: string) =>
  a == b ? "strict" : diff(n => n, a, b);
let id_verdict = (a: Id.t, b: Id.t) =>
  Id.equal(a, b) ? "strict" : diff(Id.to_string, a, b);
let ctx_entry_verdict = (x: Ctx.entry, y: Ctx.entry): string =>
  switch (x, y) {
  | (VarEntry(a), VarEntry(b))
  | (ConstructorEntry(a), ConstructorEntry(b)) =>
    combine([
      name_verdict(a.name, b.name),
      id_verdict(a.id, b.id),
      typ_verdict(a.typ, b.typ),
      strict_verdict(
        o => Option.fold(~none="None", ~some=Ctx.show_custom_statics, o),
        a.custom_statics,
        b.custom_statics,
      ),
    ])
  | (TVarEntry(a), TVarEntry(b)) =>
    combine([
      name_verdict(a.name, b.name),
      id_verdict(a.id, b.id),
      switch (a.kind, b.kind) {
      | (Abstract, Abstract) => "strict"
      | (Singleton(t1), Singleton(t2)) => typ_verdict(t1, t2)
      | (k1, k2) => diff(Ctx.show_kind, k1, k2)
      },
    ])
  | (LivelitEntry(a), LivelitEntry(b)) =>
    a === b
      ? "strict"
      : "DIFF(livelit not physically identical) a="
        ++ a.name
        ++ "@"
        ++ Id.to_string(a.id)
        ++ " b="
        ++ b.name
        ++ "@"
        ++ Id.to_string(b.id)
  | (TheoremEntry(a), TheoremEntry(b)) =>
    combine([
      name_verdict(a.name, b.name),
      id_verdict(a.id, b.id),
      opt_term_verdict(a.prop, b.prop),
    ])
  | _ => diff(Ctx.show_entry, x, y)
  };
/* Any exception inside a comparator is reported as a DIFF, not an abort. */
let guarded = (f: unit => string): string =>
  switch (f()) {
  | v => v
  | exception e => "DIFF(comparator raised " ++ Printexc.to_string(e) ++ ")"
  };
let bindings_verdict = (a: list(Ctx.entry), b: list(Ctx.entry)) =>
  List.length(a) != List.length(b)
    ? "DIFF(" ++ string_of_int(List.length(a)) ++ " vs "
      ++ string_of_int(List.length(b)) ++ " bindings)"
    : {
      let vs =
        List.combine(a, b)
        |> List.mapi((i, (x, y)) =>
             (i, guarded(() => ctx_entry_verdict(x, y)))
           );
      switch (
        List.find_opt(
          ((_, v)) => String.length(v) >= 4 && String.sub(v, 0, 4) == "DIFF",
          vs,
        )
      ) {
      | Some((i, v)) =>
        "DIFF at bindings[" ++ string_of_int(i) ++ "]: " ++ v
      | None =>
        let semantic_at =
          vs |> List.filter(((_, v)) => v == "semantic") |> List.map(fst);
        if (semantic_at != []) {
          obs(
            "bindings-semantic",
            "indices="
            ++ show_ids(List.map(string_of_int, semantic_at))
            ++ " entries="
            ++ show_ids(
                 List.map(
                   i => raw(Ctx.show_entry, List.nth(a, i)),
                   semantic_at,
                 ),
               ),
          );
        };
        combine(List.map(snd, vs));
      };
    };

let pairs_verdict = (term_of_pair, str_of_pair, show_pair, a, b) =>
  List.length(a) != List.length(b)
    ? diff(l => string_of_int(List.length(l)) ++ " items", a, b)
    : combine(
        List.map2(
          (x, y) =>
            str_of_pair(x) == str_of_pair(y)
              ? term_verdict(term_of_pair(x), term_of_pair(y))
              : diff(show_pair, x, y),
          a,
          b,
        ),
      );

let obligation_fields =
    (a: list(Obligation.t), b: list(Obligation.t)): list((string, string)) =>
  List.length(a) != List.length(b)
    ? [("obligations", diff(l => string_of_int(List.length(l)) ++ " obligations", a, b))]
    : (
      List.mapi(
        (i, (x, y)) => {
          let f = n => "obligations[" ++ string_of_int(i) ++ "]." ++ n;
          [
            (
              f("origin"),
              Id.equal(x.Obligation.origin, y.Obligation.origin)
                ? "strict"
                : diff(Id.to_string, x.Obligation.origin, y.Obligation.origin),
            ),
            (
              f("discharge"),
              strict_verdict(Obligation.show_discharge, x.Obligation.discharge,
                y.Obligation.discharge),
            ),
            (
              f("bindings"),
              guarded(() =>
                bindings_verdict(x.Obligation.bindings, y.Obligation.bindings)
              ),
            ),
            (f("goal"), term_verdict(x.Obligation.goal, y.Obligation.goal)),
            (
              f("display_goal"),
              term_verdict(x.Obligation.display_goal, y.Obligation.display_goal),
            ),
          ];
        },
        List.combine(a, b),
      )
      |> List.flatten
    );

let entry_fields =
    (a: ProofMap.entry, b: ProofMap.entry): list((string, string)) =>
  [
    ("incoming", opt_term_verdict(a.incoming, b.incoming)),
    (
      "auto_incoming",
      pairs_verdict(snd, fst, ((s, e)) => s ++ ":" ++ Exp.show(e),
        a.auto_incoming, b.auto_incoming),
    ),
    (
      "auto_outgoing",
      pairs_verdict(fst, snd, ((e, s)) => Exp.show(e) ++ ":" ++ s,
        a.auto_outgoing, b.auto_outgoing),
    ),
    ("outgoing", opt_term_verdict(a.outgoing, b.outgoing)),
    (
      "substitutions",
      pairs_verdict(snd, fst, ((s, e)) => s ++ ":" ++ Exp.show(e),
        a.substitutions, b.substitutions),
    ),
    (
      "marks",
      strict_verdict(l => String.concat("; ", List.map(ProofMark.show, l)),
        a.marks, b.marks),
    ),
  ]
  @ obligation_fields(a.obligations, b.obligations);

/* (keys equal, [(id, field, verdict)]) for expected vs actual. */
let compare_maps = (expected: ProofMap.t, actual: ProofMap.t) => (
  ids_of(expected) == ids_of(actual),
  Id.Map.bindings(expected)
  |> List.map(((id, e)) =>
       switch (Id.Map.find_opt(id, actual)) {
       | Some(a) =>
         (
           switch (entry_fields(e, a)) {
           | fs => fs
           | exception ex =>
             [("<entry>", "DIFF(comparator raised " ++ Printexc.to_string(ex) ++ ")")]
           }
         )
         |> List.map(((f, v)) => (Id.to_string(id), f, v))
       | None => [(Id.to_string(id), "<entry>", "MISSING")]
       }
     )
  |> List.flatten,
);

let check_maps = (msg, expected, actual) => {
  let (keys_equal, fields) = compare_maps(expected, actual);
  let pick = p =>
    fields
    |> List.filter(((_, _, v)) => p(v))
    |> List.map(((id, f, v)) => id ++ "." ++ f ++ "=" ++ v);
  obs(
    "cmp " ++ msg,
    "keys_equal="
    ++ string_of_bool(keys_equal)
    ++ " fields_compared="
    ++ string_of_int(List.length(fields))
    ++ " strict="
    ++ string_of_int(List.length(pick(v => v == "strict")))
    ++ " semantic="
    ++ string_of_int(List.length(pick(v => v == "semantic")))
    ++ " non_strict="
    ++ show_ids(pick(v => v != "strict")),
  );
  check(bool, msg ++ ": same keys", true, keys_equal);
  check(list(string), msg ++ ": no field DIFF or MISSING", [],
    pick(v => v == "MISSING" || String.length(v) >= 4 && String.sub(v, 0, 4) == "DIFF"));
};

let highlight_of = pm => List.map(Id.to_string, ProofMap.error_ids(pm));

/* --- App wiring ---------------------------------------------------------- */

let settings = CoreSettings.on;
let env = Builtins.env_init;

let cached_statics = (term: Exp.t): CachedStatics.t =>
  CachedStatics.init_from_term(~settings, ~is_dynamic_term=false, term);

let eval_info_of = (cs: CachedStatics.t) =>
  EvalInfo.of_info_map(
    ~probe_all=settings.probe_all,
    ~targets=cs.targets,
    cs.info_map,
  );

/* Native reference: the same elaborated program evaluated WITH its statics. */
let native_map = (cs: CachedStatics.t): ProofMap.t =>
  Evaluator.evaluate(
    ~eval_info=eval_info_of(cs),
    ~statics=cs.info_map,
    ~env,
    cs.elaborated,
  )
  |> snd
  |> EvaluatorState.get_proof_map;

let static_errors = (cs: CachedStatics.t): list(string) =>
  cs.error_ids
  |> List.map(id =>
       switch (Statics.Map.lookup(id, cs.info_map)) {
       | Some(info) =>
         Id.to_string(id)
         ++ ":"
         ++ String.concat("|", List.map(Mark.show, Info.marks_of(info)))
       | None => Id.to_string(id) ++ ":?"
       }
     );

let theorem_of = (cs: CachedStatics.t): (Id.t, Proof.t) => {
  let found = ref([]);
  let _ =
    Exp.map_term(
      ~f_exp=
        (continue, e: Exp.t) => {
          switch (e.term) {
          | Theorem(_, _, proof, _) => found := [(Exp.rep_id(e), proof), ...found^]
          | _ => ()
          };
          continue(e);
        },
      cs.elaborated,
    );
  switch (found^) {
  | [t] => t
  | _ => Alcotest.fail("expected exactly one theorem")
  };
};

let queue: ref(list(WorkerServer.Request.value)) = ref([]);
let queue_worker = Some(r => queue := queue^ @ [r]);

let calc =
    (
      ~settings=settings,
      ~queue_worker=queue_worker,
      ~is_edited,
      cs: CachedStatics.t,
      m: EvalResult.Model.t,
    ) =>
  EvalResult.Update.calculate(~settings, ~queue_worker, ~is_edited, cs, m);

let upd = (a: EvalResult.Update.t, m: EvalResult.Model.t): EvalResult.Model.t =>
  EvalResult.Update.update(~settings=Settings.Model.init, a, m).model;

let worker_reply = (req: WorkerServer.Request.value) =>
  switch (WorkerServer.evaluate_sync(req)) {
  | Ok((exp, state)) =>
    ProgramResult.ResultOk(ProgramResult.{result: exp, state})
  | Error(e) => ProgramResult.ResultFail(e)
  };

let state_of_reply =
    (r: ProgramResult.t(ProgramResult.inner)): EvaluatorState.t =>
  switch (r) {
  | ProgramResult.ResultOk(inner) => inner.state
  | _ => Alcotest.fail("worker reply was not ResultOk")
  };

let last_request = () =>
  switch (List.rev(queue^)) {
  | [r, ..._] => r
  | [] => Alcotest.fail("no worker request queued")
  };

let app_status = (m: EvalResult.Model.t, thm: Id.t): string =>
  switch (Id.Map.find_opt(thm, m.theorems.thm_map)) {
  | Some(t) =>
    switch (Calc.get_saved_opt(t.full_status)) {
    | Some(s) => ProofMap.show_full_status(s)
    | None => "pending"
    }
  | None => "absent"
  };

/* SEAM: the callback constructors of the build under test. After the
   repair, worker callbacks carry the generation of the request they answer
   (taken from the captured request, as EvalRequest takes it from its batch).
   The batch-1 control used the base's untagged constructors here. */
module Wire = {
  let ack = (req: WorkerServer.Request.value, hold) =>
    EvalResult.Update.UpdateStreamingEval(hold, req.generation, IncrEval.empty_outbox);
  let stream = (req: WorkerServer.Request.value) =>
    EvalResult.Update.MergeStreamingEval(req.generation, IncrEval.empty_outbox);
  let result = (req: WorkerServer.Request.value, r) =>
    EvalResult.Update.UpdateResult(req.generation, r);
  let timeout = (req: WorkerServer.Request.value) =>
    EvalResult.Update.UpdateResult(
      req.generation,
      ProgramResult.ResultFail(ProgramResult.Timeout),
    );
};

/* One completed run through the actual wiring: enqueue, ack, result. */
let complete = (cs: CachedStatics.t, m: EvalResult.Model.t) => {
  let m = calc(~is_edited=true, cs, m);
  let req = last_request();
  let m = upd(Wire.ack(req, 1), m);
  let m = calc(~is_edited=false, cs, m);
  let reply = worker_reply(req);
  let m = upd(Wire.result(req, reply), m);
  (calc(~is_edited=false, cs, m), reply);
};

let rel_eq_by_node = (proof: Proof.t): option(Proof.t) =>
  Test_ResetExistentials.proof_nodes(proof)
  |> List.find_opt((n: Proof.t) =>
       switch (n.term) {
       | RelEqBy(_) => true
       | _ => false
       }
     );

/* --- Case 1: witness proofs, app vs native --------------------------------- */

type expect =
  | ProvenExp
  | AloneExp
  | BadExp;

let witness_case = ((label, src, expect)) =>
  test_case(
    label ++ ": actual app map/status = native real statics",
    `Quick,
    () => {
      queue := [];
      let cs = cached_statics(Test_Evaluator_Prelude.parse_exp(src));
      let (thm, proof) = theorem_of(cs);
      let native = native_map(cs);
      let (m, reply) = complete(cs, EvalResult.Model.init);
      let worker = EvaluatorState.get_proof_map(state_of_reply(reply));
      let accessor = EvalResult.Model.proof_map(m);
      let native_status =
        ProofMap.show_full_status(ProofMap.full_status_of_proof(native, proof));
      let status = app_status(m, thm);
      let witness_out = pm =>
        switch (rel_eq_by_node(proof)) {
        | Some(n) =>
          switch (ProofMap.lookup(Proof.rep_id(n), pm)) {
          | Some(e) =>
            e.outgoing
            |> Option.map(Test_ResetExistentials.print_exp)
            |> Option.value(~default="-")
          | None => "absent"
          }
        | None => "no-witness-node"
        };
      let marks_line = pm =>
        Id.Map.bindings(pm)
        |> List.filter_map(((id, _)) =>
             switch (ProofMap.marks_of(id, pm)) {
             | [] => None
             | ms =>
               Some(Id.to_string(id) ++ ":" ++ String.concat("|", List.map(ProofMark.show, ms)))
             }
           )
        |> show_ids;
      obs(
        label,
        "static_errors="
        ++ show_ids(static_errors(cs))
        ++ " theorem="
        ++ Id.to_string(thm)
        ++ " worker_map_ids="
        ++ show_ids(ids_of(worker))
        ++ " worker_marks="
        ++ marks_line(worker)
        ++ " native_ids="
        ++ show_ids(ids_of(native))
        ++ " native_marks="
        ++ marks_line(native)
        ++ " native_status="
        ++ native_status
        ++ " native_witness_out="
        ++ witness_out(native)
        ++ " accessor_ids="
        ++ show_ids(ids_of(accessor))
        ++ " accessor_marks="
        ++ marks_line(accessor)
        ++ " app_status="
        ++ status
        ++ " app_witness_out="
        ++ witness_out(accessor)
        ++ " highlight="
        ++ show_ids(highlight_of(accessor)),
      );
      check(list(string), label ++ ": static-valid (app statics)", [], static_errors(cs));
      check(bool, label ++ ": worker (no-statics) map is EMPTY", true,
        Id.Map.is_empty(worker));
      check_maps(label ++ ": app accessor vs native", native, accessor);
      check(string, label ++ ": app status = native status", native_status, status);
      switch (expect) {
      | ProvenExp =>
        check(string, label ++ ": Proven", "Proven", status)
      | AloneExp =>
        check(string, label ++ ": witness-only body is P[3]", "3 * 3 == 9",
          witness_out(accessor));
        check(string, label ++ ": Incomplete", "Incomplete", status)
      | BadExp =>
        check(bool, label ++ ": never Proven or Refuted", true,
          status != "Proven" && status != "Refuted")
      };
    },
  );


/* --- Shared helpers for cases 2 and 3 --------------------------------------- */

/* Reused verbatim from the frozen freshness oracle (f76300ec): one definition
   above a same-id theorem; the edit changes k's static type (Bool -> ?) and
   j's value (40 + 1 -> 41 + 1), so a current-statics/old-env map differs from
   both genuine maps. */
let v1_src = {|let (k, j) : (Bool, Int) = (true, 40 + 1) in theorem t = k || j == 41 proof
  induction k
  | true => eval true || j == 41 at 0 end
  | false => eval false || j == 41 at 0 end
  end
in 0|};

/* The `?` type term, taken from a parse rather than constructed. */
let unknown_typ_term = (): Typ.term => {
  let found = ref(None);
  let f_typ = (continue, t: Typ.t): Typ.t => {
    switch (t.term) {
    | Unknown(_) when found^ == None => found := Some(t.term)
    | _ => ()
    };
    continue(t);
  };
  let _ =
    TermBase.Exp.map_term(
      ~f_typ,
      Test_Evaluator_Prelude.parse_exp({|let x : ? = 1 in 0|}),
    );
  switch (found^) {
  | Some(t) => t
  | None => Alcotest.fail("no ? type term in the parsed annotation")
  };
};

/* v1 -> v2, every id kept: Bool annotation -> ?, literal 40 -> 41. */
let edit = (term: Exp.t): Exp.t => {
  let f_typ = (continue, t: Typ.t): Typ.t =>
    switch (t.term) {
    | Atom(Bool) => {...t, term: unknown_typ_term()}
    | _ => continue(t)
    };
  Test_Evaluator_Incremental.replace_int_lit(
    ~from=40,
    ~to_=41,
    TermBase.Exp.map_term(~f_typ, term),
  );
};


let dynamics_of_state = (state: EvaluatorState.t): Dynamics.t => {
  probe_map: EvaluatorState.get_probes(state),
  test_results: EvaluatorState.get_tests(state) |> TestResults.mk_results,
  theorems: EvaluatorState.get_theorems(state),
  proof_map: EvaluatorState.get_proof_map(state),
};

/* The deliberately MIXED reference: current statics paired with a previous
   evaluation, through the real Theorems.Update.calculate. */
let mixed_map = (cs: CachedStatics.t, state: EvaluatorState.t): ProofMap.t =>
  Theorems.Update.calculate(
    ~settings=Calc.NewValue(settings),
    ~statics=Calc.NewValue(cs),
    ~dynamics=Calc.NewValue(Some(dynamics_of_state(state))),
    ~checked=Calc.NewValue(Some((cs, dynamics_of_state(state)))),
    Theorems.Model.init,
  ).
    proof_map
  |> Calc.get_saved(ProofMap.empty);

let equiv = (a: ProofMap.t, b: ProofMap.t): bool => {
  let (keys_equal, fields) = compare_maps(a, b);
  keys_equal
  && !
       List.exists(
         ((_, _, v)) =>
           v == "MISSING"
           || String.length(v) >= 4
           && String.sub(v, 0, 4) == "DIFF",
         fields,
       );
};

let phase_of = (m: EvalResult.Model.t) =>
  switch (Calc.get_value(m.result)) {
  | ProgramResult.ResultPending(ProgramResult.AwaitingWorkerAck) => "AwaitingWorkerAck"
  | ProgramResult.ResultPending(ProgramResult.Evaluating) => "Evaluating"
  | ProgramResult.ResultOk(_) => "ResultOk"
  | ProgramResult.ResultFail(_) => "ResultFail"
  };

let snap = (~label, ~prev, ~cur, ~mixed, m: EvalResult.Model.t): ProofMap.t => {
  let pm = EvalResult.Model.proof_map(m);
  obs(
    "phase " ++ label,
    "result="
    ++ phase_of(m)
    ++ " request_gen="
    ++ string_of_int(EvalResult.Model.request_gen(m))
    ++ " hold="
    ++ (
      switch (m.stepper_hold) {
      | Some(h) => string_of_int(h)
      | None => "none"
      }
    )
    ++ " map_ids="
    ++ show_ids(ids_of(pm))
    ++ " highlight="
    ++ show_ids(highlight_of(pm))
    ++ " equiv_prev="
    ++ string_of_bool(equiv(prev, pm))
    ++ " equiv_current="
    ++ string_of_bool(equiv(cur, pm))
    ++ " equiv_mixed="
    ++ string_of_bool(equiv(mixed, pm)),
  );
  pm;
};

/* --- Case 2: edit above a stable theorem id ---------------------------------- */

let edit_case =
  test_case(
    "edit above stable theorem: pending/ack/stream/hold keep the genuine previous map; final = current",
    `Quick,
    () => {
      queue := [];
      let term1 = Test_Evaluator_Prelude.parse_exp(v1_src);
      let cs1 = cached_statics(term1);
      let cs2 = cached_statics(edit(term1));
      let (thm1, _) = theorem_of(cs1);
      let (thm2, _) = theorem_of(cs2);
      let native1 = native_map(cs1);
      let native2 = native_map(cs2);
      check(list(string), "v1 static-valid", [], static_errors(cs1));
      check(list(string), "v2 static-valid", [], static_errors(cs2));
      check(string, "theorem id kept across the edit", Id.to_string(thm1), Id.to_string(thm2));
      let (m, r1) = complete(cs1, EvalResult.Model.init);
      let mixed = mixed_map(cs2, state_of_reply(r1));
      let (_, fields_prev) = compare_maps(native1, mixed);
      let (_, fields_cur) = compare_maps(native2, mixed);
      let differing = fs =>
        fs
        |> List.filter(((_, _, v)) => v != "strict" && v != "semantic")
        |> List.map(((id, f, _)) => id ++ "." ++ f);
      obs("mixed", "vs_prev=" ++ show_ids(differing(fields_prev)) ++ " vs_current=" ++ show_ids(differing(fields_cur)));
      check(bool, "fixture discriminates (mixed differs from previous)", false, equiv(native1, mixed));
      check(bool, "fixture discriminates (mixed differs from current)", false, equiv(native2, mixed));
      let p0 = snap(~label="v1-completed", ~prev=native1, ~cur=native1, ~mixed, m);
      check(bool, "v1 completed = native v1", true, equiv(native1, p0));
      let m = calc(~is_edited=true, cs2, m);
      let req2 = last_request();
      let p1 = snap(~label="v2-pending", ~prev=native1, ~cur=native2, ~mixed, m);
      let m = upd(Wire.ack(req2, 7), m);
      let m = calc(~is_edited=false, cs2, m);
      let p2 = snap(~label="v2-ack-hold", ~prev=native1, ~cur=native2, ~mixed, m);
      let m = upd(Wire.stream(req2), m);
      let m = calc(~is_edited=false, cs2, m);
      let m = upd(EvalResult.Update.ReleaseStepperHold(7), m);
      let m = calc(~is_edited=false, cs2, m);
      let p3 = snap(~label="v2-stream-hold-released", ~prev=native1, ~cur=native2, ~mixed, m);
      let m = upd(Wire.result(req2, worker_reply(req2)), m);
      let m = calc(~is_edited=false, cs2, m);
      let p4 = snap(~label="v2-result", ~prev=native1, ~cur=native2, ~mixed, m);
      check(bool, "pending: genuine previous map (never mixed)", true, equiv(native1, p1));
      check(bool, "ack/hold: genuine previous map (never mixed)", true, equiv(native1, p2));
      check(bool, "stream + hold release: genuine previous map (never mixed)", true, equiv(native1, p3));
      check(bool, "final = current native", true, equiv(native2, p4));
    },
  );

/* --- Case 3: undo restore, dynamics off, stale replies, two cells ------------ */

let undo_case =
  test_case(
    "undo restore + dynamics-off re-mint cannot collide with an in-flight tag; stale events refused; cells mint distinct tags",
    `Quick,
    () => {
      queue := [];
      let term1 = Test_Evaluator_Prelude.parse_exp(v1_src);
      let cs1 = cached_statics(term1);
      let cs2 = cached_statics(edit(term1));
      let native1 = native_map(cs1);
      /* S0: v1 completed; its issued tag is g. */
      let (m_g, _) = complete(cs1, EvalResult.Model.init);
      let g = EvalResult.Model.request_gen(m_g);
      let queued = List.length(queue^);
      /* Unchanged input: no mint, no request. */
      let m_same = calc(~is_edited=false, cs1, m_g);
      check(bool, "unchanged input: same issued tag", true,
        EvalResult.Model.request_gen(m_same) == g);
      check(int, "unchanged input: no request", queued, List.length(queue^));
      /* Edit: R1 issued with tag g1. */
      let m1 = calc(~is_edited=true, cs2, m_g);
      let r1 = last_request();
      /* Undo: the whole S0 model value comes back (tag g), statics v1. */
      let m_restored = m_g;
      /* Dynamics off on the restored model: inputs changed, so a tag is minted. */
      let off = {...settings, dynamics: false};
      let m_off = calc(~settings=off, ~is_edited=false, cs1, m_restored);
      let g_off = EvalResult.Model.request_gen(m_off);
      obs(
        "undo",
        "g="
        ++ string_of_int(g)
        ++ " r1="
        ++ string_of_int(r1.generation)
        ++ " after_edit="
        ++ string_of_int(EvalResult.Model.request_gen(m1))
        ++ " restored="
        ++ string_of_int(EvalResult.Model.request_gen(m_restored))
        ++ " after_off="
        ++ string_of_int(g_off),
      );
      check(bool, "restored model carries the old tag", true,
        EvalResult.Model.request_gen(m_restored) == g);
      check(bool, "dynamics-off mint is a fresh tag (not the in-flight R1's, not the restored one)",
        true, g_off != r1.generation && g_off != g);
      /* R1's reply arrives: it must be refused (physically unchanged model). */
      let m_after = upd(Wire.result(r1, worker_reply(r1)), m_off);
      check(bool, "in-flight R1 reply refused after undo + re-mint", true, m_after === m_off);
      check(bool, "map still the genuine previous one", true,
        equiv(native1, EvalResult.Model.proof_map(m_after)));
      /* Stale R1 ack/stream/timeout on the post-edit model whose current
         request is newer: refused. Then the current request's events. */
      let m2 = calc(~is_edited=true, cs1, m1);
      let r2 = last_request();
      check(bool, "re-edit issues a fresh tag", true, r2.generation != r1.generation);
      List.iter(
        ((name, action)) =>
          check(bool, "stale " ++ name ++ " refused", true, upd(action, m2) === m2),
        [
          ("ack", Wire.ack(r1, 9)),
          ("stream", Wire.stream(r1)),
          ("final", Wire.result(r1, worker_reply(r1))),
          ("timeout", Wire.timeout(r1)),
        ],
      );
      let m3 = upd(Wire.ack(r2, 11), m2);
      check(bool, "current ack accepted", true, m3.stepper_hold == Some(11));
      let m4 = upd(Wire.stream(r2), m3);
      check(bool, "current stream accepted", true, m4 !== m3);
      let m5 = upd(Wire.timeout(r2), calc(~is_edited=false, cs1, m4));
      let m5 = calc(~is_edited=false, cs1, m5);
      check(string, "current timeout accepted", "ResultFail", phase_of(m5));
      /* Two independent cells mint distinct tags. */
      let (ca, _) = complete(cs1, EvalResult.Model.init);
      let (cb, _) = complete(cs2, EvalResult.Model.init);
      let tags = [g, r1.generation, g_off, r2.generation,
                  EvalResult.Model.request_gen(ca), EvalResult.Model.request_gen(cb)];
      obs("tags", show_ids(List.map(string_of_int, tags)));
      check(bool, "two cells: distinct tags", true,
        EvalResult.Model.request_gen(ca) != EvalResult.Model.request_gen(cb));
      check(int, "every issued tag in this case is unique",
        List.length(tags), List.length(List.sort_uniq(compare, tags)));
    },
  );

let tests = (
  "ExistentialsAppContext",
  List.map(
    witness_case,
    [
      ("witness3", Test_ResetExistentials.positive, ProvenExp),
      ("witness3-alone", Test_ResetExistentials.alone, AloneExp),
      ("bad-witness2", Test_ResetExistentials.bad, BadExp),
    ],
  )
  @ [edit_case, undo_case],
);

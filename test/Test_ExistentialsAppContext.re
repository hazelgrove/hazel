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

/* SEAM: the callback constructors of the build under test. */
module Wire = {
  let ack = (_req: WorkerServer.Request.value, hold) =>
    EvalResult.Update.UpdateStreamingEval(hold, IncrEval.empty_outbox);
  let result = (_req: WorkerServer.Request.value, r) =>
    EvalResult.Update.UpdateResult(r);
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

let tests = (
  "ExistentialsAppContext",
  List.map(
    witness_case,
    [
      ("witness3", Test_ResetExistentials.positive, ProvenExp),
      ("witness3-alone", Test_ResetExistentials.alone, AloneExp),
      ("bad-witness2", Test_ResetExistentials.bad, BadExp),
    ],
  ),
);

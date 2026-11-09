open Alcotest;
open Haz3lcore;
open Web;
open Language;

/* Evaluation runs off the main thread without statics, so EvalResult has
 * Theorems check proofs with the statics of the request each evaluation
 * answers, and ignores replies to requests it has since replaced. The
 * reference is the evaluator checking the same program with its statics. */

let settings = CoreSettings.on;

let cached_statics = (term: Exp.t): CachedStatics.t =>
  CachedStatics.init_from_term(~settings, ~is_dynamic_term=false, term);

let native_map = (cs: CachedStatics.t): ProofMap.t =>
  Evaluator.evaluate(
    ~eval_info=
      EvalInfo.of_info_map(
        ~probe_all=settings.probe_all,
        ~targets=cs.targets,
        cs.info_map,
      ),
    ~statics=cs.info_map,
    ~env=Builtins.env_init,
    cs.elaborated,
  )
  |> snd
  |> EvaluatorState.get_proof_map;

/* Each entry's marks and goals, as the user sees them. */
let summary = (pm: ProofMap.t): list(string) => {
  let exp = e => Option.fold(~none="-", ~some=Test_ProofMap.print_exp, e);
  Id.Map.bindings(pm)
  |> List.map(((id, entry: ProofMap.entry)) =>
       Id.to_string(id)
       ++ " ["
       ++ String.concat("; ", List.map(ProofMark.show, entry.marks))
       ++ "] "
       ++ exp(entry.incoming)
       ++ " => "
       ++ exp(entry.outgoing)
     );
};

let check_map = (msg, ~expected: ProofMap.t, m: EvalResult.Model.t) =>
  check(
    list(string),
    msg,
    summary(expected),
    summary(EvalResult.Model.proof_map(m)),
  );

let test_checked_with_statics = () => {
  /* Without the type of `b`, the checker can't tell `false` is missing. */
  let cs =
    cached_statics(
      Test_Evaluator_Prelude.parse_exp(
        {|theorem t = forall (b : Bool) -> b || true proof
  induction b
  | true => from true || true do eval end
  end
in 0|},
      ),
    );
  let m =
    EvalResult.Update.calculate(
      ~settings,
      ~queue_worker=None,
      ~is_edited=true,
      cs,
      EvalResult.Model.init,
    );
  check_map("checked with the statics", ~expected=native_map(cs), m);
};

/* The edit changes `k`'s type, which only the statics know, and `j`'s
 * value, which only the evaluation's env knows, so checking the edited
 * program's statics against the old env matches neither program. The edit
 * keeps every id, as edits in the editor do. */
let before_edit =
  Test_Evaluator_Prelude.parse_exp(
    {|let (k, j) : (Bool, Int) = (true, 40 + 1) in theorem t = k || j == 41 proof
  induction k
  | true => from true || j == 41 do eval end
  | false => from false || j == 41 do eval end
  end
in 0|},
  );

let after_edit = {
  let unknown_typ = ref(None);
  let _ =
    TermBase.Exp.map_term(
      ~f_typ=
        (continue, t: Typ.t) => {
          switch (t.term) {
          | Unknown(_) => unknown_typ := Some(t.term)
          | _ => ()
          };
          continue(t);
        },
      Test_Evaluator_Prelude.parse_exp("let x : ? = 1 in 0"),
    );
  let f_typ = (continue, t: Typ.t): Typ.t =>
    switch (t.term, unknown_typ^) {
    | (Atom(Bool), Some(unknown)) => {
        ...t,
        term: unknown,
      }
    | _ => continue(t)
    };
  Test_Evaluator_Incremental.replace_int_lit(
    ~from=40,
    ~to_=41,
    TermBase.Exp.map_term(~f_typ, before_edit),
  );
};

/* EvalResult with a worker whose requests are collected. */
let queued = ref([]);

let calculate = (~is_edited, cs, m) =>
  EvalResult.Update.calculate(
    ~settings,
    ~queue_worker=Some(r => queued := [r, ...queued^]),
    ~is_edited,
    cs,
    m,
  );

let update = (action, m) =>
  EvalResult.Update.update(~settings=Settings.Model.init, action, m).model;

let latest_request = (): WorkerServer.Request.value =>
  switch (queued^) {
  | [r, ..._] => r
  | [] => Alcotest.fail("no request queued")
  };

let reply = (req: WorkerServer.Request.value) =>
  switch (WorkerServer.evaluate_sync(req)) {
  | Ok((result, state)) =>
    ProgramResult.ResultOk(
      ProgramResult.{
        result,
        state,
      },
    )
  | Error(e) => ProgramResult.ResultFail(e)
  };

let ack = (req: WorkerServer.Request.value): EvalResult.Update.t =>
  UpdateStreamingEval(req.generation, IncrEval.empty_outbox);

/* Evaluates `cs` on the worker through to its result. */
let evaluate = (cs, m) => {
  let m = calculate(~is_edited=true, cs, m);
  let req = latest_request();
  let m = calculate(~is_edited=false, cs, update(ack(req), m));
  let m = update(UpdateResult(req.generation, reply(req)), m);
  calculate(~is_edited=false, cs, m);
};

let test_pending_keeps_previous_check = () => {
  queued := [];
  let before = cached_statics(before_edit);
  let after = cached_statics(after_edit);
  let native_before = native_map(before);
  let native_after = native_map(after);
  check(
    bool,
    "the edit changes the proof map",
    false,
    summary(native_before) == summary(native_after),
  );
  let m = evaluate(before, EvalResult.Model.init);
  check_map("before the edit", ~expected=native_before, m);
  let m = calculate(~is_edited=true, after, m);
  let req = latest_request();
  check_map("edit pending", ~expected=native_before, m);
  let m = calculate(~is_edited=false, after, update(ack(req), m));
  check_map("edit acknowledged", ~expected=native_before, m);
  let m = update(UpdateResult(req.generation, reply(req)), m);
  let m = calculate(~is_edited=false, after, m);
  check_map("edit evaluated", ~expected=native_after, m);
};

let test_replaced_request_ignored = () => {
  queued := [];
  let before = cached_statics(before_edit);
  let after = cached_statics(after_edit);
  let m = evaluate(before, EvalResult.Model.init);
  let m = calculate(~is_edited=true, after, m);
  let replaced = latest_request();
  /* Undoing the edit replaces its request. */
  let undone = cached_statics(before_edit);
  let m = calculate(~is_edited=true, undone, m);
  let current = latest_request();
  let m = update(ack(replaced), m);
  let m = update(UpdateResult(replaced.generation, reply(replaced)), m);
  let m =
    update(
      UpdateResult(replaced.generation, ResultFail(ProgramResult.Timeout)),
      m,
    );
  let m = calculate(~is_edited=false, undone, m);
  check(bool, "still pending", true, EvalResult.Model.eval_is_pending(m));
  check_map("its result is ignored", ~expected=native_map(before), m);
  let m = update(UpdateResult(current.generation, reply(current)), m);
  let m = calculate(~is_edited=false, undone, m);
  check_map("the current result", ~expected=native_map(undone), m);
};

let tests = (
  "ProofContext",
  [
    test_case(
      "proofs are checked with the cell's statics",
      `Quick,
      test_checked_with_statics,
    ),
    test_case(
      "while an edit is evaluated, proofs stay checked against the program before it",
      `Quick,
      test_pending_keeps_previous_check,
    ),
    test_case(
      "replies to a replaced request are ignored",
      `Quick,
      test_replaced_request_ignored,
    ),
  ],
);

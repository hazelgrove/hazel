open Alcotest;
open Haz3lcore;
open Language;

/* "Show steps" on a probe sample: the drawer's state, the capture that
   keeps function values for it, and the expression its stepper starts
   from */

let src = {|let f = fun x -> x + 1 in
let y = ^^probe(f(2)) in
y|};

let parsed = (): Zipper.t =>
  switch (Parser.to_zipper(~root=Exp, src)) {
  | Some(z) => z
  | None => Alcotest.fail("could not parse")
  };

let probe_id = (z: Zipper.t): Id.t =>
  switch (z.refractors.manuals) {
  | [(id, _)] => id
  | _ => Alcotest.fail("expected one probe")
  };

let span = (z: Zipper.t): Sample.span_ref => {
  probe_id: probe_id(z),
  stack: [],
  opened: None,
};

let stepping = (ed: Editor.Model.t) => ed.state.zipper.refractors.stepping;

let act = (a: Action.t, ed: Editor.Model.t): Editor.Model.t =>
  switch (
    Editor.Update.update(
      ~settings=CoreSettings.on,
      a,
      CachedStatics.empty,
      Dynamics.Map.empty,
      ed,
    )
  ) {
  | Ok(ed) => ed
  | Error(e) => Alcotest.fail(Action.Failure.show(e))
  };

/* opening and closing are view state: no edit, no undo step */
let show_hide = () => {
  let z = parsed();
  let a = Action.Probe(ShowSteps(span(z)));
  check(bool, "not an edit", false, Action.is_edit(a));
  check(bool, "not an undo step", false, Action.is_historic(a));
  let ed = Editor.Model.mk(z, ~root=Exp) |> act(a);
  check(bool, "open", true, stepping(ed) != None);
  let ed = act(Probe(HideSteps), ed);
  check(bool, "closed", true, stepping(ed) == None);
};

/* a caret move leaves it open; an edit to the program closes it, as does
   losing its probe */
let closes = () => {
  let z = parsed();
  let ed =
    Editor.Model.mk(z, ~root=Exp) |> act(Probe(ShowSteps(span(z))));
  let moved = act(Move(Local(Left, ByChar)), ed);
  check(bool, "open after a move", true, stepping(moved) != None);
  let edited = act(Insert(" "), ed);
  check(bool, "closed by an edit", true, stepping(edited) == None);
  let removed = act(Probe(RemoveAll), ed);
  check(bool, "closed with its probe", true, stepping(removed) == None);
};

let term = (z: Zipper.t): Exp.t =>
  MakeTerm.from_zip_for_sem(z, ~root=Exp).term;

/* with the probe ids, as the editor runs it: they are the reuse witness */
let statics = (z: Zipper.t) =>
  Statics.mk(
    ~probe_ids=CachedStatics.probe_ids_of_zipper(z),
    CoreSettings.on,
    Builtins.ctx_init(Some(Int)),
    term(z),
  );

let targets = (~full, z: Zipper.t): Sample.targets => {
  let (info_map, _) = statics(z);
  CachedStatics.compute_targets(
    ~settings=CoreSettings.on,
    ~info_map,
    ~probe_ids=CachedStatics.probe_ids_of_zipper(z),
    ~full_ids=full ? Id.Map.singleton(probe_id(z), ()) : Id.Map.empty,
    (),
  );
};

let eval = (~prev=IncrEval.empty, ~targets, z: Zipper.t) => {
  let (info_map, elab) = statics(z);
  let eval_info = EvalInfo.of_info_map(~probe_all=false, ~targets, info_map);
  let (_, state) =
    Evaluator.evaluate(~prev, ~eval_info, ~env=Builtins.env_init, elab);
  (info_map, state);
};

let samples = (state: EvaluatorState.t): list(Sample.t) =>
  EvaluatorState.get_probes(state) |> Id.Map.bindings |> List.concat_map(snd);

/* f is in the sample's environment as a value, not elided */
let keeps_f = (state: EvaluatorState.t): bool =>
  List.exists(
    (s: Sample.t) =>
      List.exists(
        (en: Sample.Env.entry) =>
          en.binding.name == "f"
          && (
            switch (en.value) {
            | Val(_) => true
            | Opaque => false
            }
          ),
        s.env,
      ),
    samples(state),
  );

let full_targets = () => {
  let z = parsed();
  let full = (_, spec: Sample.capture_spec) => spec.full;
  check(
    bool,
    "other probes elide",
    false,
    Id.Map.exists(full, targets(~full=false, z)),
  );
  check(
    bool,
    "the stepping probe keeps values",
    true,
    Id.Map.for_all(full, targets(~full=true, z)),
  );
};

/* an incremental run retakes the stepping probe's samples rather than
   replaying the elided ones; closing it elides them again */
let retakes = () => {
  let z = parsed();
  let plain = targets(~full=false, z);
  let full = targets(~full=true, z);
  let (_, s1) = eval(~targets=plain, z);
  check(bool, "elided", false, keeps_f(s1));
  let (_, s2) = eval(~prev=s1.incr_eval, ~targets=full, z);
  check(bool, "retaken with f", true, keeps_f(s2));
  let (_, s3) = eval(~prev=s2.incr_eval, ~targets=plain, z);
  check(bool, "elided again", false, keeps_f(s3));
};

/* the stepper starts from the probed expression closed over its sample's
   values, which evaluates to the sample's value; an elided sample has
   none yet */
let starts = () => {
  let z = parsed();
  let (info_map, s) = eval(~targets=targets(~full=true, z), z);
  let dynamics = EvaluatorState.get_probes(s);
  switch (
    Web.ProbeSteps.closed_exp(~info_map, ~dynamics, span(z)),
    samples(s),
  ) {
  | (Some(e), [sample]) =>
    let (v, _) = Evaluator.evaluate(~env=Builtins.env_init, e);
    check(bool, "the sample's value", true, Exp.fast_equal(v, sample.value));
  | (None, _) => Alcotest.fail("no closed expression")
  | (_, _) => Alcotest.fail("expected one sample")
  };
  switch (
    Web.ProbeSteps.calculate(
      ~settings=Util.Calc.NewValue(CoreSettings.on),
      ~info_map,
      ~dynamics,
      ~stepping=Some(span(z)),
      None,
    )
  ) {
  | Some(ps) =>
    check(
      int,
      "the title bar and the first step",
      2,
      Web.ProbeSteps.rows(~settings=CoreSettings.on, ps),
    )
  | None => Alcotest.fail("no stepper")
  };
  let (info_map, s) = eval(~targets=targets(~full=false, z), z);
  check(
    bool,
    "an elided sample waits",
    true,
    Web.ProbeSteps.closed_exp(
      ~info_map,
      ~dynamics=EvaluatorState.get_probes(s),
      span(z),
    )
    == None,
  );
};

/* the drawer's rows are its title bar and the steps it shows: with
   history off, just the current one */
let history_rows = () => {
  let z = parsed();
  let (info_map, s) = eval(~targets=targets(~full=true, z), z);
  let dynamics = EvaluatorState.get_probes(s);
  let calc = prev =>
    Web.ProbeSteps.calculate(
      ~settings=Util.Calc.NewValue(CoreSettings.on),
      ~info_map,
      ~dynamics,
      ~stepping=Some(span(z)),
      prev,
    )
    |> Option.get;
  let stepped =
    Web.ProbeSteps.update(
      ~settings=Web.Settings.Model.init,
      StepForward(0),
      calc(None),
    ).
      model;
  let ps = calc(Some(stepped));
  let history = (on: bool): CoreSettings.t => {
    ...CoreSettings.on,
    evaluation: {
      ...CoreSettings.on.evaluation,
      stepper_history: on,
    },
  };
  check(
    int,
    "history off",
    2,
    Web.ProbeSteps.rows(~settings=history(false), ps),
  );
  check(
    int,
    "history on",
    3,
    Web.ProbeSteps.rows(~settings=history(true), ps),
  );
};

/* the title bar's undo takes back the step: nothing to take back at the
   start */
let undo = () => {
  let z = parsed();
  let (info_map, s) = eval(~targets=targets(~full=true, z), z);
  let dynamics = EvaluatorState.get_probes(s);
  let calc = prev =>
    Web.ProbeSteps.calculate(
      ~settings=Util.Calc.NewValue(CoreSettings.on),
      ~info_map,
      ~dynamics,
      ~stepping=Some(span(z)),
      prev,
    )
    |> Option.get;
  let start = calc(None);
  check(
    bool,
    "nothing at the start",
    true,
    Web.StepperView.Update.undo(start.stepper) == None,
  );
  let step = (a, ps) =>
    calc(
      Some(
        Web.ProbeSteps.update(~settings=Web.Settings.Model.init, a, ps).model,
      ),
    );
  let stepped = step(StepForward(0), start);
  let back =
    switch (Web.StepperView.Update.undo(stepped.stepper)) {
    | Some(a) => step(a, stepped)
    | None => Alcotest.fail("no undo after a step")
    };
  let history: CoreSettings.t = {
    ...CoreSettings.on,
    evaluation: {
      ...CoreSettings.on.evaluation,
      stepper_history: true,
    },
  };
  check(
    int,
    "back to one step",
    Web.ProbeSteps.rows(~settings=history, start),
    Web.ProbeSteps.rows(~settings=history, back),
  );
};

/* a pass over the stepper (each click makes one) re-analyzes only what
   changed: the first step's statics survive a second step and a
   selection */
let incremental = () => {
  let z = parsed();
  let (info_map, s) = eval(~targets=targets(~full=true, z), z);
  let dynamics = EvaluatorState.get_probes(s);
  let calc = prev =>
    Web.ProbeSteps.calculate(
      ~settings=Util.Calc.OldValue(CoreSettings.on),
      ~info_map,
      ~dynamics,
      ~stepping=Some(span(z)),
      prev,
    )
    |> Option.get;
  let step = (a, ps) =>
    calc(
      Some(
        Web.ProbeSteps.update(~settings=Web.Settings.Model.init, a, ps).model,
      ),
    );
  /* the analysis itself: a pass rebuilds the record around it (targets) */
  let first_statics = (ps: Web.ProbeSteps.t) =>
    Util.Calc.get_saved_exc(~print="first step", ps.stepper.root.editor).
      statics.
      info_map;
  let one = step(StepForward(0), calc(None));
  let before = first_statics(one);
  let again =
    step(EditorAction(Select(All)), step(NextStep(StepForward(0)), one));
  check(
    bool,
    "the first step's statics kept",
    true,
    before === first_statics(again),
  );
};

/* the bar names the stepped expression on one line */
let one_line = () => {
  check(
    string,
    "lines and runs of space fold",
    "case xs | [] => 0 | _ => 1 end",
    Web.ProbeSteps.one_line("  case xs\n  | [] => 0\n  | _ =>   1\nend\n"),
  );
  let long = String.make(300, 'a');
  check(
    int,
    "capped, with an ellipsis",
    240 + String.length({js|…|js}),
    String.length(Web.ProbeSteps.one_line(long)),
  );
};

let tests = (
  "ProbeSteps",
  [
    test_case("show and hide", `Quick, show_hide),
    test_case("edits and probe loss close", `Quick, closes),
    test_case("the stepping probe keeps values", `Quick, full_targets),
    test_case("incremental runs retake", `Quick, retakes),
    test_case("the stepper's start", `Quick, starts),
    test_case("rows follow history", `Quick, history_rows),
    test_case("undo", `Quick, undo),
    test_case("one line", `Quick, one_line),
    test_case("passes are incremental", `Quick, incremental),
  ],
);

open Alcotest;
open Language;
open Haz3lcore;
open Test_Evaluator_Prelude;

/* Adversarial tests for the incremental-evaluation calculi (Calculus.t).
 *
 * The property under test is SOUNDNESS: for every program and every
 * id-preserving edit, each calculus must produce the value -- and, under aL,
 * the probe samples -- that a0 (plain evaluation, no cache) produces.
 *
 * As in Test_Evaluator_Incremental, an "edit" must never be simulated by
 * re-parsing: parse_exp mints fresh ids, so reuse_check never fires and any
 * such test passes vacuously. Every edit below is an in-place rewrite that
 * keeps the surrounding IdTagged annotations, which is what the Zipper does
 * for a character-level edit. The two tuple-shape edits are cross-checked
 * end to end against real editor actions via `hazel bench-incr`. */

let statics_and_elab = (exp: Exp.t): (Statics.Map.t, Exp.t) =>
  Statics.mk(
    CoreSettings.on,
    Builtins.ctx_init(Some(Operators.default_mode)),
    exp,
  );

let eval_info_of = (info_map, targets) =>
  EvalInfo.of_info_map(
    ~probe_all=CoreSettings.on.probe_all,
    ~targets,
    info_map,
  );

let eval_under =
    (
      ~calculus: Calculus.t,
      ~prev: EvaluatorState.incr_eval=IncrEval.empty,
      ~targets: Sample.targets=Id.Map.empty,
      exp: Exp.t,
    )
    : (Exp.t, EvaluatorState.t) => {
  let (info_map, elab) = statics_and_elab(exp);
  Evaluator.evaluate(
    ~calculus,
    ~prev,
    ~eval_info=eval_info_of(info_map, targets),
    ~env=Builtins.env_init,
    elab,
  );
};

let print_settings: ExpToSegment.Settings.t = {
  secondary: AutoFormat,
  parenthesization: Defensive,
  label_format: QuoteWhenNecessary,
  inline: true,
  fold_case_clauses: false,
  fold_fn_bodies: `NoFold,
  hide_fixpoints: false,
  show_ascriptions: true,
  show_filters: true,
  show_unknown_as_hole: true,
  hole_tiles: false,
  project_tables: false,
};

let show = (e: Exp.t): string =>
  Printer.of_segment(
    ~holes="?",
    ExpToSegment.exp_to_segment(~settings=print_settings, e),
  );

/* Run [before] then [after] under one calculus, threading the cache, and
 * return the printed value of the second run. */
let replay = (~calculus: Calculus.t, before: Exp.t, after: Exp.t): string => {
  let (_, state) = eval_under(~calculus, before);
  let (v, _) = eval_under(~calculus, ~prev=state.incr_eval, after);
  show(v);
};

/* a0 is the reference: no cache at all. */
let reference = (after: Exp.t): string => {
  let (v, _) = eval_under(~calculus=Calculus.A0, after);
  show(v);
};

/* Assert that every available calculus agrees with a0 on the value produced
 * for [after] after an id-preserving edit from [before]. */
let check_sound = (name, before: Exp.t, after: Exp.t) => {
  let want = reference(after);
  List.iter(
    (calculus: Calculus.t) =>
      check(
        string,
        name ++ ": " ++ Calculus.name(calculus) ++ " agrees with a0",
        want,
        replay(~calculus, before, after),
      ),
    Calculus.available,
  );
};

/* ---- id-preserving edits ------------------------------------------ */

/* Drop the last component of every n-ary Tuple with >2 components, keeping
 * the Tuple node's own annotation and the surviving components' annotations.
 * This is what deleting the trailing `, 3` from `(1, 2, 3)` does in the
 * editor: the tuple tile and the untouched literals keep their ids. */
let drop_last_tuple_component = (exp: Exp.t): Exp.t => {
  let f_exp = (continue, e: Exp.t): Exp.t =>
    switch (e.term) {
    | Tuple(es) when List.length(es) > 2 =>
      let keep = List.filteri((i, _) => i < List.length(es) - 1, es);
      {
        annotation: e.annotation,
        term: Tuple(keep),
      };
    | _ => continue(e)
    };
  TermBase.Exp.map_term(~f_exp, exp);
};

/* Reverse the components of every 2-tuple, keeping each component's own
 * annotation and the Tuple's. Models an edit that permutes tuple components
 * without changing their ids -- the shape of edit that
 * LabeledTupleHelpers.align_exp performs (it reorders a Tuple's components
 * to match an expected label order while deliberately preserving the Tuple's
 * id). */
let swap_tuple_components = (exp: Exp.t): Exp.t => {
  let f_exp = (continue, e: Exp.t): Exp.t =>
    switch (e.term) {
    | Tuple([a, b]) => {
        annotation: e.annotation,
        term: Tuple([continue(b), continue(a)]),
      }
    | _ => continue(e)
    };
  TermBase.Exp.map_term(~f_exp, exp);
};

/* Empty every non-empty Tuple, keeping the Tuple node's annotation: deleting
 * a tuple's contents without deleting its parentheses. `norm([])` is Clean,
 * so the emptied tuple is reported fully clean. */
let empty_the_tuple = (exp: Exp.t): Exp.t => {
  let f_exp = (continue, e: Exp.t): Exp.t =>
    switch (e.term) {
    | Tuple([_, ..._]) => {
        annotation: e.annotation,
        term: Tuple([]),
      }
    | _ => continue(e)
    };
  TermBase.Exp.map_term(~f_exp, exp);
};

/* Replace Atom(Int(from)) with Atom(Int(to_)) everywhere, preserving every
 * IdTagged annotation including the edited leaf's: a single-token text edit
 * as the Zipper would produce it. */
let replace_int_lit = (~from: int, ~to_: int, exp: Exp.t): Exp.t => {
  let f_exp = (continue, e: Exp.t): Exp.t =>
    switch (e.term) {
    | Atom(Int(n)) when Bigint.to_string(n) == string_of_int(from) => {
        annotation: e.annotation,
        term: Atom(Int(Bigint.of_int(to_))),
      }
    | _ => continue(e)
    };
  TermBase.Exp.map_term(~f_exp, exp);
};

/* ---- value-soundness tests ---------------------------------------- */

/* aM's Tuple rule (IncrEval.exp_flag) collapses an all-clean component list
 * to Clean via `norm`. Section 8's Pair rule is binary, so there "both
 * components clean" really does entail "the pair is the cached pair".
 * Hazel's tuples are n-ary and their arity can change under an edit, so a
 * tuple that LOSES a component still has all-clean surviving components and
 * is wrongly reported Clean; every binding projected from it is then re-used
 * against a value of the wrong shape, breaking the section 8 invariant that
 * "if any part of the flag is clean, that part of the value must be the same
 * as the value found in the cache for the given expression". */
let test_tuple_shrink = () => {
  let before = parse_exp("let z = (1, 2, 3) in z");
  let after = drop_last_tuple_component(before);
  check(
    string,
    "sanity: the edit really shrank the tuple",
    "let z = (1, 2) in z",
    show(after),
  );
  check_sound("tuple loses a component", before, after);
};

/* Same root cause, reached through a second binder. */
let test_tuple_shrink_through_binder = () => {
  let before = parse_exp("let z = (1, 2, 3) in let w = z in w");
  let after = drop_last_tuple_component(before);
  check_sound("shrunk tuple flows through a second binder", before, after);
};

/* `norm` says nothing about component ORDER either: permuting components
 * while keeping their ids leaves every component Clean, so the composite is
 * reported Clean even though its value changed. */
let test_tuple_swap = () => {
  let before = parse_exp("let z = (1, 2) in z");
  let after = swap_tuple_components(before);
  check(
    string,
    "sanity: the edit really swapped the components",
    "let z = (2, 1) in z",
    show(after),
  );
  check_sound("tuple components permuted", before, after);
};

/* Section 8 says nothing about a nullary tuple, and `norm([])` is Clean
 * because List.for_all over an empty list is vacuously true. So emptying a
 * tuple reports it clean and its whole cached value is re-used. */
let test_tuple_emptied = () => {
  let before = parse_exp("let z = (1, 2) in z");
  let after = empty_the_tuple(before);
  check(
    string,
    "sanity: the edit really emptied the tuple",
    "let z = (()) in z",
    show(after),
  );
  check_sound("tuple emptied", before, after);
};

/* The aM win itself: editing one component of a tuple must leave bindings
 * projected from the other components re-usable AND correct. */
let test_tuple_partial_edit_value = () => {
  let before = parse_exp("let z = (1, 2) in let (a, b) = z in a * 10 + b");
  let after = replace_int_lit(~from=2, ~to_=7, before);
  check_sound("one tuple component edited", before, after);
};

/* ---- flag primitives (section 8) ---------------------------------- */

let flag = testable(Fmt.using(IncrEval.show_flag, Fmt.string), (==));

/* Section 8: "A flag is normalized if it does not contain any clean,clean
 * pairs or any dirty,dirty pairs." */
let test_norm_normalizes = () => {
  check(
    flag,
    "all clean collapses",
    IncrEval.Clean,
    IncrEval.norm([Clean, Clean]),
  );
  check(
    flag,
    "all dirty collapses",
    IncrEval.Dirty,
    IncrEval.norm([Dirty, Dirty]),
  );
  check(
    flag,
    "mixed stays split",
    IncrEval.Parts([Clean, Dirty]),
    IncrEval.norm([Clean, Dirty]),
  );
  /* The zero-ary case: `norm([])` is Clean, which is what `()` gets. */
  check(flag, "norm of no components", IncrEval.Clean, IncrEval.norm([]));
};

/* `split` must agree with the paper's split_tuple: a bare flag applies to
 * every component alike, a Parts flag is taken componentwise. */
let test_split = () => {
  check(
    flag,
    "split clean",
    IncrEval.Clean,
    IncrEval.split(~arity=2, ~index=0, Clean),
  );
  check(
    flag,
    "split dirty",
    IncrEval.Dirty,
    IncrEval.split(~arity=2, ~index=1, Dirty),
  );
  check(
    flag,
    "split parts",
    IncrEval.Dirty,
    IncrEval.split(~arity=2, ~index=1, Parts([Clean, Dirty])),
  );
  check(
    flag,
    "arity mismatch degrades to dirty",
    IncrEval.Dirty,
    IncrEval.split(~arity=3, ~index=0, Parts([Clean, Dirty])),
  );
};

/* Section 8 insists the flag (WHICH parts are clean) and the projection path
 * (WHERE a binding sits) never mix. Every pattern form that is not a tuple
 * projection must therefore hand a component-shaped flag on as Dirty, or a
 * Parts flag reaches a binder whose value it does not describe. */
let parse_pat = (pat_src: string): Pat.t => {
  let e = parse_exp("let " ++ pat_src ++ " = 0 in 0");
  let rec find = (e: Exp.t): option(Pat.t) =>
    switch (e.term) {
    | Let(p, _, _) => Some(p)
    | Parens(e) => find(e)
    | _ => None
    };
  switch (find(e)) {
  | Some(p) => p
  | None => Alcotest.fail("could not parse pattern: " ++ pat_src)
  };
};

let prov_flags = (~flag: IncrEval.flag, pat_src: string) => {
  let pat = parse_pat(pat_src);
  IncrEval.pat_provenance(~source_id=Id.invalid, ~flag, pat)
  |> Util.Maps.StringMap.bindings
  |> List.map(((name, p: IncrEval.provenance)) =>
       (name, IncrEval.show_flag(p.flag))
     );
};

let flags_list = list(pair(string, string));

let test_pat_provenance_opaque = () => {
  let f = IncrEval.Parts([Clean, Dirty]);
  check(
    flags_list,
    "tuple pattern splits the flag",
    [("a", "Clean"), ("b", "Dirty")],
    prov_flags(~flag=f, "(a, b)"),
  );
  check(
    flags_list,
    "cons pattern is opaque to a component flag",
    [("hd", "Dirty"), ("tl", "Dirty")],
    prov_flags(~flag=f, "hd :: tl"),
  );
  check(
    flags_list,
    "list-literal pattern is opaque to a component flag",
    [("x", "Dirty"), ("y", "Dirty")],
    prov_flags(~flag=f, "[x, y]"),
  );
  check(
    flags_list,
    "constructor-arg pattern is opaque to a component flag",
    [("x", "Dirty")],
    prov_flags(~flag=f, "Some(x)"),
  );
  /* A bare variable legitimately takes the whole flag: it binds the whole
   * value, so a partly-clean flag describes it exactly. */
  check(
    flags_list,
    "variable pattern keeps the component flag",
    [("v", "(Parts [Clean; Dirty])")],
    prov_flags(~flag=f, "v"),
  );
  /* Labeled tuples: the Tuple splits first and TupLabel is a transparent
   * wrapper, so the labels get the components' flags, not the whole flag. */
  check(
    flags_list,
    "labeled tuple pattern splits the flag",
    [("x", "Clean"), ("y", "Dirty")],
    prov_flags(~flag=f, "(a = x, b = y)"),
  );
  /* A single labeled binder is the case where TupLabel could hand a
   * whole-tuple flag to a component binder, since there is no enclosing
   * Tuple to split it first. Pinned so a change to the parse shape or to
   * `pat_provenance` shows up here. */
  check(
    flags_list,
    "single labeled binder",
    [("x", "Dirty")],
    prov_flags(~flag=f, "(a = x)"),
  );
};

/* ---- probe (aL) machinery ----------------------------------------- */

/* Per-probe-target report: the ordered list of printed sample values for
 * each targeted id. This is the aL-visible output, which the benchmark's
 * value-only cross-check does not compare at all. */
let show_sample = (s: Sample.t): string =>
  Printf.sprintf(
    "%s @depth%d env=%s args=%s origin=%s",
    show(s.value),
    List.length(s.call_stack),
    Sample.Env.show(s.env),
    switch (s.args) {
    | None => "-"
    | Some(a) => CallStack.show_elided_value(a)
    },
    Sample.show_origin(s.origin),
  );

let probe_report = (targets: Sample.targets, state: EvaluatorState.t) =>
  Id.Map.fold(
    (id, _, acc) => {
      let samples =
        switch (Sample.Map.lookup(id, EvaluatorState.get_probes(state))) {
        | Some(l) => l
        | None => []
        };
      [(Id.to_string(id), List.map(show_sample, samples)), ...acc];
    },
    targets,
    [],
  )
  |> List.sort(compare);

let show_report = (r: list((string, list(string)))): string =>
  r
  |> List.map(((id, vs)) => id ++ "=[" ++ String.concat(";", vs) ++ "]")
  |> String.concat(" ");

/* A probe sample carries a monotonically increasing `seq` minted from a
 * global counter when the sample is taken. A sample that a cache entry
 * REPLAYS keeps the seq it was minted with on the earlier run. So any sample
 * whose seq predates the start of run 2 witnesses that the subtree carrying
 * that probe was re-used. That gives an exact black-box re-use detector at
 * probe granularity, which is what keeps the aL tests below from passing
 * vacuously when no re-use happened at all. */
type probe_run = {
  report: list((string, list(string))),
  replayed: list(string),
  fresh: list(string),
};

let run_probes =
    (
      ~calculus: Calculus.t,
      ~term: Exp.t,
      ~targets1: Sample.targets,
      ~after: Exp.t,
      ~targets2: Sample.targets,
      ~labels: Id.t => string,
    )
    : probe_run => {
  let (_, state1) = eval_under(~calculus, ~targets=targets1, term);
  let cutoff = Sample.seq_counter^;
  let (_, state2) =
    eval_under(~calculus, ~prev=state1.incr_eval, ~targets=targets2, after);
  let probes = EvaluatorState.get_probes(state2);
  let (replayed, fresh) =
    Id.Map.fold(
      (id, _, (r, f)) => {
        let samples =
          switch (Sample.Map.lookup(id, probes)) {
          | Some(l) => l
          | None => []
          };
        List.exists((s: Sample.t) => s.seq <= cutoff, samples)
          ? ([labels(id), ...r], f) : (r, [labels(id), ...f]);
      },
      targets2,
      ([], []),
    );
  {
    report: probe_report(targets2, state2),
    replayed: List.sort(compare, replayed),
    fresh: List.sort(compare, fresh),
  };
};

let with_probes = (~src: string, ~edit: Exp.t => Exp.t, k) =>
  switch (Parser.to_zipper(~root=Exp, src)) {
  | None => failwith("could not parse: " ++ src)
  | Some(z) =>
    let MakeTerm.{term, _} = MakeTerm.from_zip_for_sem(z, ~root=Exp);
    let (info_map1, _) = statics_and_elab(term);
    let targets1 = targets_of_zipper(z, info_map1);
    let after = edit(term);
    let (info_map2, _) = statics_and_elab(after);
    let targets2 = targets_of_zipper(z, info_map2);
    let eval_info2 = eval_info_of(info_map2, targets2);
    let labels = (id: Id.t) =>
      switch (EvalInfo.find_opt(id, eval_info2)) {
      | Some({elab_term, _}) => show(elab_term)
      | None => Id.to_string(id)
      };
    k(~term, ~targets1, ~after, ~targets2, ~labels);
  };

/* aM + aL: the probe samples a re-used entry splices in must agree with the
 * samples a cacheless run produces, in the same order and the same number. */
let probe_soundness = (~name: string, ~src: string, ~edit: Exp.t => Exp.t) =>
  with_probes(
    ~src,
    ~edit,
    (~term, ~targets1, ~after, ~targets2, ~labels) => {
      check(
        bool,
        name ++ ": the program actually has probe targets",
        true,
        !Id.Map.is_empty(targets2),
      );
      let (_, ref_state) =
        eval_under(~calculus=Calculus.A0, ~targets=targets2, after);
      let want = probe_report(targets2, ref_state);
      List.iter(
        (calculus: Calculus.t) => {
          let r =
            run_probes(
              ~calculus,
              ~term,
              ~targets1,
              ~after,
              ~targets2,
              ~labels,
            );
          check(
            string,
            name ++ ": " ++ Calculus.name(calculus) ++ " probe samples",
            show_report(want),
            show_report(r.report),
          );
        },
        Calculus.available,
      );
    },
  );

let test_probe_partial_tuple = () =>
  probe_soundness(
    ~name="probe under a partly-clean tuple",
    ~src=
      "let z = (3, 4) in
let (a, b) = z in
let p = ^^probe(a + 100) in
let q = ^^probe(b + 200) in
p + q",
    ~edit=replace_int_lit(~from=4, ~to_=9),
  );

let test_probe_in_calls = () =>
  probe_soundness(
    ~name="probe inside re-used calls under a partly-clean tuple",
    ~src=
      "let f : Int -> Int = fun n -> if n < 1 then 0 else n + f(n - 1) in
let z = (3, 4) in
let (a, b) = z in
let p = ^^probe(f(a)) in
let q = ^^probe(f(b)) in
p + q",
    ~edit=replace_int_lit(~from=4, ~to_=6),
  );

/* Section 8's motivating program, verbatim: editing one component of `z`
 * must leave the OTHER component's downstream call re-usable. This is the
 * whole point of aM, so if the probe on the untouched side is not replayed,
 * aM is buying nothing over aPL. */
let tuple_bottleneck_src = "let f : Int -> Int = fun n -> n * 2 in
let z = (3, 4) in
let (a, b) = z in
let p = ^^probe(f(a)) in
let q = ^^probe(f(b)) in
p + q";

/* Control for the re-use detector: an edit that cannot affect the second
 * probe at all must leave that probe's sample replayed under aPL and aM
 * alike. If this does not hold, the detector is broken rather than aM. */
let detector_control_src = "let u = 1 in
let p = ^^probe(u + 100) in
let q = ^^probe(200 + 300) in
p + q";

let test_reuse_detector_control = () =>
  with_probes(
    ~src=detector_control_src,
    ~edit=replace_int_lit(~from=1, ~to_=5),
    (~term, ~targets1, ~after, ~targets2, ~labels) =>
    List.iter(
      (calculus: Calculus.t) => {
        let r =
          run_probes(~calculus, ~term, ~targets1, ~after, ~targets2, ~labels);
        Printf.printf(
          "CONTROL %s replayed=[%s] fresh=[%s]\n",
          Calculus.name(calculus),
          String.concat(",", r.replayed),
          String.concat(",", r.fresh),
        );
        if (calculus != Calculus.A0) {
          check(
            list(string),
            "detector: unaffected probe is replayed under "
            ++ Calculus.name(calculus),
            ["200 + 300"],
            r.replayed,
          );
        };
      },
      Calculus.available,
    )
  );

/* aM's win is sensitive to where the definitions it depends on sit relative
 * to the tuple destructuring. The pre-pass that computes `reused_ids`
 * (ReusePass.reuse_pass) evaluates symbolically with `req_final` returning
 * the UNEVALUATED sub-expression, so `let (a, b) = z` matches a tuple
 * pattern against a bare `Var` -- IndetMatch -- and the walk stops dead.
 * Everything defined after the destructuring is therefore never in
 * `reused_ids`, so exp_flag reports it Dirty and it blocks re-use. */
let bottleneck_f_after = "let z = (3, 4) in
let (a, b) = z in
let f : Int -> Int = fun n -> n * 2 in
let p = ^^probe(f(a)) in
let q = ^^probe(f(b)) in
p + q";

let test_definition_order_sensitivity = () =>
  with_probes(
    ~src=bottleneck_f_after,
    ~edit=replace_int_lit(~from=3, ~to_=7),
    (~term, ~targets1, ~after, ~targets2, ~labels) => {
      let am =
        run_probes(
          ~calculus=Calculus.AM,
          ~term,
          ~targets1,
          ~after,
          ~targets2,
          ~labels,
        );
      Printf.printf(
        "ORDER f-after-destructuring: aM replayed=[%s] fresh=[%s]\n",
        String.concat(",", am.replayed),
        String.concat(",", am.fresh),
      );
      check(
        list(string),
        "aM replays f(b) when f is defined AFTER the destructuring",
        ["f(b)"],
        am.replayed,
      );
    },
  );

let test_am_actually_reuses = () =>
  with_probes(
    ~src=tuple_bottleneck_src,
    ~edit=replace_int_lit(~from=3, ~to_=7),
    (~term, ~targets1, ~after, ~targets2, ~labels) => {
      let apl =
        run_probes(
          ~calculus=Calculus.APL,
          ~term,
          ~targets1,
          ~after,
          ~targets2,
          ~labels,
        );
      let am =
        run_probes(
          ~calculus=Calculus.AM,
          ~term,
          ~targets1,
          ~after,
          ~targets2,
          ~labels,
        );
      Printf.printf(
        "REUSE aPL replayed=[%s] fresh=[%s]\n",
        String.concat(",", apl.replayed),
        String.concat(",", apl.fresh),
      );
      Printf.printf(
        "REUSE aM  replayed=[%s] fresh=[%s]\n",
        String.concat(",", am.replayed),
        String.concat(",", am.fresh),
      );
      /* Section 8: editing the first component of z must leave f(b)
       * re-usable under aM (and it is not re-usable under aPL). */
      check(
        list(string),
        "aM replays the probe on the untouched tuple component",
        ["f(b)"],
        am.replayed,
      );
    },
  );

/* Narrowing the definition-order effect: it is the tuple-pattern `let`
 * specifically that stops the pre-pass, not merely "being later in the
 * program". Prints the replay set for three placements of `f`. */
let test_where_the_prepass_stops = () => {
  let variants = [
    (
      "f before z",
      "let f : Int -> Int = fun n -> n * 2 in
let z = (3, 4) in
let (a, b) = z in
let p = ^^probe(f(a)) in
let q = ^^probe(f(b)) in
p + q",
    ),
    (
      "f between z and the destructuring",
      "let z = (3, 4) in
let f : Int -> Int = fun n -> n * 2 in
let (a, b) = z in
let p = ^^probe(f(a)) in
let q = ^^probe(f(b)) in
p + q",
    ),
    (
      "f after the destructuring",
      "let z = (3, 4) in
let (a, b) = z in
let f : Int -> Int = fun n -> n * 2 in
let p = ^^probe(f(a)) in
let q = ^^probe(f(b)) in
p + q",
    ),
    (
      "destructured inline, f after",
      "let (a, b) = (3, 4) in
let f : Int -> Int = fun n -> n * 2 in
let p = ^^probe(f(a)) in
let q = ^^probe(f(b)) in
p + q",
    ),
  ];
  List.iter(
    ((name, src)) =>
      with_probes(
        ~src,
        ~edit=replace_int_lit(~from=3, ~to_=7),
        (~term, ~targets1, ~after, ~targets2, ~labels) => {
          let am =
            run_probes(
              ~calculus=Calculus.AM,
              ~term,
              ~targets1,
              ~after,
              ~targets2,
              ~labels,
            );
          Printf.printf(
            "PREPASS %-38s aM replayed=[%s]\n",
            name,
            String.concat(",", am.replayed),
          );
        },
      ),
    variants,
  );
};

/* ---- aM + aL attacks ----------------------------------------------
 *
 * aL caches, per entry, the probe-sample slice the entry's subtree
 * contributed, and splices it back on re-use. aM makes strictly more entries
 * re-usable, so it splices strictly more slices. Each of these programs is
 * built so that aM demonstrably replays at least one probe (asserted), and
 * then the whole probe report -- every target's ordered sample values -- is
 * compared against a cacheless run. */

let probe_soundness_witnessed =
    (
      ~name: string,
      ~src: string,
      ~edit: Exp.t => Exp.t,
      ~expect_replay: list(string),
    ) =>
  with_probes(
    ~src,
    ~edit,
    (~term, ~targets1, ~after, ~targets2, ~labels) => {
      let (_, ref_state) =
        eval_under(~calculus=Calculus.A0, ~targets=targets2, after);
      let want = probe_report(targets2, ref_state);
      List.iter(
        (calculus: Calculus.t) => {
          let r =
            run_probes(
              ~calculus,
              ~term,
              ~targets1,
              ~after,
              ~targets2,
              ~labels,
            );
          if (calculus == Calculus.AM) {
            Printf.printf(
              "AML %-42s replayed=[%s] fresh=[%s]\n",
              name,
              String.concat(",", r.replayed),
              String.concat(",", r.fresh),
            );
            check(
              list(string),
              name ++ ": aM actually replays a cached log slice",
              expect_replay,
              r.replayed,
            );
          };
          check(
            string,
            name ++ ": " ++ Calculus.name(calculus) ++ " probe samples",
            show_report(want),
            show_report(r.report),
          );
        },
        Calculus.available,
      );
    },
  );

/* A probe inside a function body: the slice a re-used entry carries was
 * minted at a NON-EMPTY call stack, so replaying it re-injects samples the
 * evaluator would otherwise never produce at top level. */
let test_aml_probe_in_fn_body = () =>
  probe_soundness_witnessed(
    ~name="probe inside the function body",
    ~src=
      "let f : Int -> Int = fun n -> ^^probe(n * 2) in
let z = (3, 4) in
let (a, b) = z in
let p = f(a) in
let q = f(b) in
p * 100 + q",
    ~edit=replace_int_lit(~from=3, ~to_=7),
    ~expect_replay=["n * 2"],
  );

/* Same, with the re-used call FIRST, so the replayed slice is spliced before
 * the freshly minted one. */
let test_aml_probe_in_fn_body_reused_first = () =>
  probe_soundness_witnessed(
    ~name="probe in fn body, re-used call first",
    ~src=
      "let f : Int -> Int = fun n -> ^^probe(n * 2) in
let z = (3, 4) in
let (a, b) = z in
let q = f(b) in
let p = f(a) in
p * 100 + q",
    ~edit=replace_int_lit(~from=3, ~to_=7),
    ~expect_replay=["n * 2"],
  );

/* A probe whose subtree fires a DIFFERENT number of times after the edit:
 * the recursive call count depends on the dirtied component only. */
let test_aml_probe_count_changes = () =>
  probe_soundness_witnessed(
    ~name="probe count changes on the dirty side",
    ~src=
      "let g : Int -> Int = fun n -> if n < 1 then 0 else ^^probe(n) + g(n - 1) in
let z = (2, 3) in
let (a, b) = z in
let p = g(a) in
let q = g(b) in
p * 100 + q",
    ~edit=replace_int_lit(~from=2, ~to_=5),
    ~expect_replay=["n"],
  );

/* Probes on both sides of the tuple, one re-used and one re-computed. */
let test_aml_probe_both_sides = () =>
  probe_soundness_witnessed(
    ~name="probes on both tuple components",
    ~src=
      "let f : Int -> Int = fun n -> n * 2 in
let z = (3, 4) in
let (a, b) = z in
let p = ^^probe(f(a)) in
let q = ^^probe(f(b)) in
^^probe(p + q)",
    ~edit=replace_int_lit(~from=3, ~to_=7),
    ~expect_replay=["f(b)"],
  );

/* A probe on the tuple itself, alongside a re-used projection. */
let test_aml_probe_on_tuple = () =>
  probe_soundness_witnessed(
    ~name="probe on the tuple and on a projection",
    ~src=
      "let f : Int -> Int = fun n -> n * 2 in
let z = ^^probe((3, 4)) in
let (a, b) = z in
let q = ^^probe(f(b)) in
q + a",
    ~edit=replace_int_lit(~from=3, ~to_=7),
    ~expect_replay=["f(b)"],
  );

/* ---- aL: print statements and test results ------------------------
 *
 * Section 10's print statements land in the same sample map as probes but
 * are not in `targets`, so they need a report over the whole map. Test
 * results live in a separate per-id map that `EvaluatorState.append` also
 * splices on re-use. Both are aL-visible output that the benchmark's
 * value-only cross-check ignores entirely. */

let all_samples_report = (state: EvaluatorState.t) =>
  Sample.Map.fold(
    (id, samples, acc) =>
      [(Id.to_string(id), List.map(show_sample, samples)), ...acc],
    EvaluatorState.get_probes(state),
    [],
  )
  |> List.sort(compare);

let tests_report = (state: EvaluatorState.t) =>
  EvaluatorState.get_tests(state)
  |> List.map(((id, reports)) =>
       (
         Id.to_string(id),
         List.map(
           (r: TestMap.instance_report) => TestStatus.show(r.status),
           reports,
         ),
       )
     )
  |> List.sort(compare);

let check_side_output = (~name: string, ~src: string, ~edit: Exp.t => Exp.t) => {
  let before = parse_exp(src);
  let after = edit(before);
  let (_, ref_state) = eval_under(~calculus=Calculus.A0, after);
  let want_samples = show_report(all_samples_report(ref_state));
  let want_tests = show_report(tests_report(ref_state));
  Printf.printf(
    "SIDE %s\n  samples: %s\n  tests:   %s\n",
    name,
    want_samples,
    want_tests,
  );
  check(
    bool,
    name ++ ": the program produces some aL-visible side output",
    true,
    want_samples != "" || want_tests != "",
  );
  List.iter(
    (calculus: Calculus.t) => {
      let (_, s1) = eval_under(~calculus, before);
      let (_, s2) = eval_under(~calculus, ~prev=s1.incr_eval, after);
      check(
        string,
        name ++ ": " ++ Calculus.name(calculus) ++ " print/log samples",
        want_samples,
        show_report(all_samples_report(s2)),
      );
      check(
        string,
        name ++ ": " ++ Calculus.name(calculus) ++ " test results",
        want_tests,
        show_report(tests_report(s2)),
      );
    },
    Calculus.available,
  );
};

let test_print_under_am = () =>
  check_side_output(
    ~name="print on both tuple components",
    ~src=
      "let z = (1, 2) in
let (a, b) = z in
let p = print(a) in
let q = print(b) in
0",
    ~edit=replace_int_lit(~from=1, ~to_=5),
  );

let test_print_in_fn_under_am = () =>
  check_side_output(
    ~name="print inside a re-used call",
    ~src=
      "let f : Int -> () = fun n -> print(n * 2) in
let z = (1, 2) in
let (a, b) = z in
let p = f(a) in
let q = f(b) in
0",
    ~edit=replace_int_lit(~from=1, ~to_=5),
  );

let test_tests_under_am = () =>
  check_side_output(
    ~name="test statements on both tuple components",
    ~src=
      "let z = (1, 2) in
let (a, b) = z in
test a < 3 end;
test b < 3 end;
0",
    ~edit=replace_int_lit(~from=1, ~to_=5),
  );

/* ---- multi-step sequences ------------------------------------------
 *
 * Copy-coherence (def:cache-coherence) is about entries copied from one
 * cache to the next, so it only bites from the THIRD evaluation onwards.
 * Replay a whole edit sequence under each calculus, threading the cache, and
 * compare every step against a cacheless evaluation of that same step's
 * program. */
let check_sequence =
    (~name: string, ~src: string, ~edits: list(Exp.t => Exp.t)) => {
  let start = parse_exp(src);
  let programs =
    List.rev(
      List.fold_left(
        (acc, edit) =>
          switch (acc) {
          | [] => [edit(start)]
          | [prev, ..._] => [edit(prev), ...acc]
          },
        [start],
        edits,
      ),
    );
  let wanted = List.map(reference, programs);
  List.iter(
    (calculus: Calculus.t) => {
      let prev = ref(IncrEval.empty);
      List.iteri(
        (i, program) => {
          let (v, state) = eval_under(~calculus, ~prev=prev^, program);
          prev := state.incr_eval;
          check(
            string,
            Printf.sprintf(
              "%s step %d under %s",
              name,
              i,
              Calculus.name(calculus),
            ),
            List.nth(wanted, i),
            show(v),
          );
        },
        programs,
      );
    },
    Calculus.available,
  );
};

let lit = (from, to_) => replace_int_lit(~from, ~to_);

let test_sequences = () => {
  check_sequence(
    ~name="tuple components edited in turn",
    ~src="let z = (1, 2) in let (a, b) = z in a * 100 + b",
    ~edits=[lit(2, 7), lit(1, 9), lit(7, 3), lit(9, 1), lit(3, 2)],
  );
  check_sequence(
    ~name="nested tuples",
    ~src="let z = ((1, 2), 3) in let ((a, b), c) = z in a * 100 + b * 10 + c",
    ~edits=[lit(2, 7), lit(3, 8), lit(1, 4), lit(7, 2)],
  );
  check_sequence(
    ~name="tuple through a function",
    ~src=
      "let f : (Int, Int) -> Int = fun (x, y) -> x * 10 + y in
let z = (1, 2) in
f(z)",
    ~edits=[lit(2, 7), lit(1, 9), lit(7, 2)],
  );
  check_sequence(
    ~name="tuple scrutinised by a case",
    ~src=
      "let z = (1, 2) in
case z
  | (0, y) => y
  | (x, y) => x * 100 + y
end",
    ~edits=[lit(2, 7), lit(1, 0), lit(0, 5)],
  );
  check_sequence(
    ~name="tuple destructured by a cons pattern",
    ~src=
      "let z = [1, 2, 3] in
case z
  | [] => 0
  | hd :: tl => hd * 100
end",
    ~edits=[lit(1, 4), lit(2, 7), lit(4, 1)],
  );
  check_sequence(
    ~name="tuple under a constructor",
    ~src=
      "type T = Pair((Int, Int)) in
let z = Pair((1, 2)) in
case z
  | Pair((a, b)) => a * 100 + b
end",
    ~edits=[lit(2, 7), lit(1, 9)],
  );
  check_sequence(
    ~name="unit and singleton tuples",
    ~src="let u = () in let z = (1, ()) in let (a, b) = z in a",
    ~edits=[lit(1, 5), lit(5, 1)],
  );
  check_sequence(
    ~name="labeled tuple",
    ~src=
      "let z : (a = Int, b = Int) = (a = 1, b = 2) in
let (a = p, b = q) = z in
p * 100 + q",
    ~edits=[lit(2, 7), lit(1, 9)],
  );
};

/* ---- broad soundness sweep -----------------------------------------
 *
 * A corpus of programs crossed with id-preserving edit sequences. Every step
 * of every sequence is compared against a cacheless evaluation of that same
 * step's program, under every available calculus. Cheap, and it is the only
 * thing here that covers shapes nobody thought to attack by hand. */

let shadowing_src = "let z = (1, 2) in
let (a, b) = z in
let c = a in
let a = b in
let d = a in
c * 100 + d";

let sweep_corpus = [
  (
    "pair of calls",
    "let f : Int -> Int = fun n -> n * 3 in
let z = (1, 2) in
let (a, b) = z in
f(a) * 100 + f(b)",
    [lit(1, 5), lit(2, 7), lit(5, 1), lit(7, 2), lit(1, 4)],
  ),
  (
    "three-way tuple",
    "let z = (1, 2, 3) in
let (a, b, c) = z in
a * 10000 + b * 100 + c",
    [lit(2, 7), lit(1, 4), lit(3, 8), lit(7, 2)],
  ),
  (
    "tuple of tuples",
    "let z = ((1, 2), (3, 4)) in
let ((a, b), (c, d)) = z in
a * 1000 + b * 100 + c * 10 + d",
    [lit(4, 9), lit(1, 6), lit(9, 4), lit(2, 8)],
  ),
  (
    "shadowing through a tuple",
    shadowing_src,
    [lit(1, 5), lit(2, 7), lit(5, 1)],
  ),
  (
    "tuple rebound to a tuple",
    "let z = (1, 2) in
let w = z in
let (a, b) = w in
a * 100 + b",
    [lit(2, 7), lit(1, 9)],
  ),
  (
    "tuple in a recursive function",
    "let go : (Int, Int) -> Int =
  fun (n, acc) -> if n < 1 then acc else go((n - 1, acc + n)) in
let z = (4, 0) in
go(z)",
    [lit(4, 6), lit(6, 3)],
  ),
  (
    "option destructuring",
    "let z = (1, 2) in
let (a, b) = z in
let o = Some(b) in
case o
  | None => a
  | Some(v) => a * 100 + v
end",
    [lit(2, 7), lit(1, 9)],
  ),
  (
    "list of tuple components",
    "let z = (1, 2) in
let (a, b) = z in
let l = [a, b, a + b] in
case l
  | [] => 0
  | hd :: tl => hd * 100 + List.length(tl)
end",
    [lit(2, 7), lit(1, 9)],
  ),
  (
    "tuple passed to a lambda",
    "let z = (1, 2) in
(fun (x, y) -> x * 100 + y)(z)",
    [lit(2, 7), lit(1, 9)],
  ),
  (
    "unit components",
    "let z = ((), 1, ()) in
let (u, a, v) = z in
a",
    [lit(1, 5), lit(5, 1)],
  ),
];

/* The known-bad shape-changing edits, kept out of the sweep above so that a
 * sweep failure always means something NEW. These show the wrong value
 * persists into later steps rather than being corrected by the next edit. */
let structural_sequences = [
  (
    "tuple shrink then literal edits",
    "let z = (1, 2, 3) in
let w = z in
w",
    [drop_last_tuple_component, lit(2, 7), lit(1, 9)],
  ),
  (
    "tuple swap then literal edits",
    "let z = (1, 2) in
let (a, b) = z in
a * 100 + b",
    [swap_tuple_components, lit(2, 7)],
  ),
];

let test_structural_sequences = () =>
  List.iter(
    ((name, src, edits)) => check_sequence(~name, ~src, ~edits),
    structural_sequences,
  );

/* The same defect seen as a wrong NUMBER rather than a wrong-looking tuple:
 * permuting the components changes what `a` and `b` are bound to, but the
 * flag stays Clean, so `a * 10 + b` is served from the cache. */
let test_tuple_swap_arithmetic = () => {
  let before = parse_exp("let z = (1, 2) in let (a, b) = z in a * 10 + b");
  let after = swap_tuple_components(before);
  check(string, "sanity: a0 sees the swap", "21", reference(after));
  check_sound("permuted tuple feeds arithmetic", before, after);
};

let test_sweep = () =>
  List.iter(
    ((name, src, edits)) => check_sequence(~name, ~src, ~edits),
    sweep_corpus,
  );

let tests = (
  "IncrEval",
  [
    test_case("tuple loses a component", `Quick, test_tuple_shrink),
    test_case(
      "shrunk tuple through a binder",
      `Quick,
      test_tuple_shrink_through_binder,
    ),
    test_case("tuple components permuted", `Quick, test_tuple_swap),
    test_case("tuple emptied", `Quick, test_tuple_emptied),
    test_case(
      "one tuple component edited",
      `Quick,
      test_tuple_partial_edit_value,
    ),
    test_case("norm normalizes", `Quick, test_norm_normalizes),
    test_case("split", `Quick, test_split),
    test_case("pat_provenance opacity", `Quick, test_pat_provenance_opaque),
    test_case(
      "probe under partly-clean tuple",
      `Quick,
      test_probe_partial_tuple,
    ),
    test_case("probe inside re-used calls", `Quick, test_probe_in_calls),
    test_case("re-use detector control", `Quick, test_reuse_detector_control),
    test_case(
      "definition-order sensitivity",
      `Quick,
      test_definition_order_sensitivity,
    ),
    test_case("aM actually re-uses", `Quick, test_am_actually_reuses),
    test_case(
      "where the pre-pass stops",
      `Quick,
      test_where_the_prepass_stops,
    ),
    test_case("multi-step edit sequences", `Quick, test_sequences),
    test_case("broad soundness sweep", `Quick, test_sweep),
    test_case(
      "shape-changing edit sequences",
      `Quick,
      test_structural_sequences,
    ),
    test_case(
      "permuted tuple feeds arithmetic",
      `Quick,
      test_tuple_swap_arithmetic,
    ),
    test_case("aM+aL: probe in fn body", `Quick, test_aml_probe_in_fn_body),
    test_case(
      "aM+aL: probe in fn body, re-used first",
      `Quick,
      test_aml_probe_in_fn_body_reused_first,
    ),
    test_case(
      "aM+aL: probe count changes",
      `Quick,
      test_aml_probe_count_changes,
    ),
    test_case(
      "aM+aL: probes on both sides",
      `Quick,
      test_aml_probe_both_sides,
    ),
    test_case("aM+aL: probe on the tuple", `Quick, test_aml_probe_on_tuple),
    test_case("aM+aL: print statements", `Quick, test_print_under_am),
    test_case(
      "aM+aL: print inside a call",
      `Quick,
      test_print_in_fn_under_am,
    ),
    test_case("aM+aL: test results", `Quick, test_tests_under_am),
  ],
);

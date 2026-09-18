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

/* ==================================================================
 * Audit 2: the INTEGRATED tree (aPL + aM + a2 + aL), differential harness.
 *
 * `hazel bench-incr` compares only the PRINTED final value, and
 * `Printer.of_segment(~holes="?")` renders every hole as `?`, so two
 * different indeterminate results compare equal to it. It also compares
 * nothing that a cache entry splices in from its stored state. The harness
 * below closes both gaps: it compares the value STRUCTURALLY (ids stripped,
 * so a hole is distinguished by what it contains and where it sits), every
 * probe and print sample, every test result, and the step timeline that
 * EvaluatorState.append goes to real trouble to reproduce.
 * ================================================================== */

/* Normalize every id in a term to Id.invalid. Evaluator.finish already runs
 * Exp.replace_all_ids, which MINTS FRESH ids, so an un-normalized sexp
 * differs between any two runs. */
let strip_ids = {
  let f:
    'a.
    (IdTagged.t('a) => IdTagged.t('a), IdTagged.t('a)) => IdTagged.t('a)
   =
    (continue, x) =>
      continue({
        ...x,
        annotation: IdTagged.IdTag.temp(),
      });
  TermBase.Exp.map_term(~f_exp=f, ~f_pat=f, ~f_typ=f, ~f_tpat=f, ~f_rul=f);
};

let show_deep = (e: Exp.t): string =>
  Sexplib.Sexp.to_string(Exp.sexp_of_t(strip_ids(e)));

/* A richer sample rendering than `show_sample`: also the step window the
 * sample covers. EvaluatorState.append deliberately shifts a replayed
 * slice's step bounds onto the current timeline (`shift_sample`, and
 * step_count += ext.step_count - ext.initial_step_count), so a replayed
 * sample is supposed to land on exactly the steps a cacheless run would
 * have put it on. Nothing else compares that. */
let show_sample_timed = (s: Sample.t): string =>
  Printf.sprintf("%s steps=%d-%d", show_sample(s), s.step_start, s.step_end);

let timed_samples_report = (state: EvaluatorState.t) =>
  Sample.Map.fold(
    (id, samples, acc) =>
      [(Id.to_string(id), List.map(show_sample_timed, samples)), ...acc],
    EvaluatorState.get_probes(state),
    [],
  )
  |> List.sort(compare);

/* Compare the step timeline too? Turning it off lets a sweep separate "the
 * replayed samples are the wrong SAMPLES" from "the replayed samples are
 * right but land on the wrong STEPS". */
let compare_steps = ref(true);

let full_report = ((v: Exp.t, st: EvaluatorState.t)) => (
  show_deep(v),
  compare_steps^
    ? show_report(timed_samples_report(st))
      ++ " #steps="
      ++ string_of_int(EvaluatorState.get_step_count(st))
    : show_report(all_samples_report(st)),
  show_report(tests_report(st)),
);

/* Root entries / total entries / depth of the cache trie. Without this a
 * sweep that finds nothing is indistinguishable from a sweep in which no
 * re-use ever fired. */
let rec trie_stats = (t: IncrEval.t(EvaluatorState.t)): (int, int, int) => {
  let root = Id.Map.cardinal(t.entries);
  let (sub_total, sub_depth) =
    Id.Map.fold(
      (_, c, (tot, d)) => {
        let (_, ct, cd) = trie_stats(c);
        (tot + ct, max(d, cd + 1));
      },
      t.children,
      (0, 0),
    );
  (root, root + sub_total, sub_depth);
};

let programs_of = (term: Exp.t, edits: list(Exp.t => Exp.t)): list(Exp.t) =>
  List.rev(
    List.fold_left(
      (acc, edit) =>
        switch (acc) {
        | [] => [edit(term)]
        | [prev, ..._] => [edit(prev), ...acc]
        },
      [term],
      edits,
    ),
  );

/* Replay an edit SEQUENCE under one calculus, threading the cache, and
 * collect every place it disagrees with a cacheless run of that step's own
 * program. Probe targets come from the original zipper (every edit here
 * preserves ids), which is how `with_probes` does it. */
let diffs_for =
    (
      ~name: string,
      ~calculus: Calculus.t,
      ~src: string,
      ~edits: list(Exp.t => Exp.t),
      ~trace: bool,
    )
    : list(string) =>
  switch (Parser.to_zipper(~root=Exp, src)) {
  | None => ["could not parse: " ++ src]
  | Some(z) =>
    let MakeTerm.{term, _} = MakeTerm.from_zip_for_sem(z, ~root=Exp);
    let programs = programs_of(term, edits);
    let targets_for = (p: Exp.t) => {
      let (info_map, _) = statics_and_elab(p);
      targets_of_zipper(z, info_map);
    };
    let want =
      List.map(
        p =>
          full_report(
            eval_under(~calculus=Calculus.A0, ~targets=targets_for(p), p),
          ),
        programs,
      );
    let out = ref([]);
    let prev = ref(IncrEval.empty);
    List.iteri(
      (i, p) => {
        let targets = targets_for(p);
        let cutoff = Sample.seq_counter^;
        let (v, st) = eval_under(~calculus, ~prev=prev^, ~targets, p);
        prev := st.incr_eval;
        if (trace) {
          let (root, total, depth) = trie_stats(st.incr_eval);
          let replayed =
            Sample.Map.fold(
              (_, samples, n) =>
                n
                + List.length(
                    List.filter((sm: Sample.t) => sm.seq <= cutoff, samples),
                  ),
              EvaluatorState.get_probes(st),
              0,
            );
          Printf.printf(
            "STAT %-52s step %d %-6s root=%-4d total=%-4d depth=%d replayed=%d\n",
            name,
            i,
            Calculus.name(calculus),
            root,
            total,
            depth,
            replayed,
          );
        };
        let (wv, ws, wt) = List.nth(want, i);
        let (gv, gs, gt) = full_report((v, st));
        let tag =
          Printf.sprintf(
            "%s | step %d | %s",
            name,
            i,
            Calculus.name(calculus),
          );
        if (wv != gv) {
          out :=
            [tag ++ " VALUE\n    want " ++ wv ++ "\n    got  " ++ gv, ...out^];
        };
        if (ws != gs) {
          out :=
            [
              tag ++ " SAMPLES\n    want " ++ ws ++ "\n    got  " ++ gs,
              ...out^,
            ];
        };
        if (wt != gt) {
          out :=
            [tag ++ " TESTS\n    want " ++ wt ++ "\n    got  " ++ gt, ...out^];
        };
      },
      programs,
    );
    List.rev(out^);
  };

let diffs_of_sequence =
    (~name: string, ~src: string, ~edits: list(Exp.t => Exp.t))
    : list(string) =>
  List.concat_map(
    (calculus: Calculus.t) =>
      diffs_for(~name, ~calculus, ~src, ~edits, ~trace=false),
    Calculus.available,
  );

/* Assert that every available calculus agrees with a0 on value, samples,
 * test results and step timeline, at every step of the edit chain. */
let check_full_sequence =
    (~name: string, ~src: string, ~edits: list(Exp.t => Exp.t)) =>
  switch (diffs_of_sequence(~name, ~src, ~edits)) {
  | [] => ()
  | ds => Alcotest.fail(String.concat("\n", ds))
  };

let check_corpus = (corpus: list((string, string, list(Exp.t => Exp.t)))) => {
  let all =
    List.concat_map(
      ((name, src, edits)) => diffs_of_sequence(~name, ~src, ~edits),
      corpus,
    );
  switch (all) {
  | [] => ()
  | ds => Alcotest.fail(String.concat("\n", ds))
  };
};

/* --- more id-preserving edits -------------------------------------- */

/* Replace an integer literal with an EMPTY HOLE, keeping the leaf's
 * annotation: what deleting the only digit of a number does in the editor.
 * The result goes indeterminate, which is exactly the regime `hazel
 * bench-incr` cannot see -- Print.print renders every hole as `?`. */
let blank_int_lit = (~from: int, exp: Exp.t): Exp.t => {
  let f_exp = (continue, e: Exp.t): Exp.t =>
    switch (e.term) {
    | Atom(Int(n)) when Bigint.to_string(n) == string_of_int(from) => {
        annotation: e.annotation,
        term: EmptyHole,
      }
    | _ => continue(e)
    };
  TermBase.Exp.map_term(~f_exp, exp);
};

/* Repoint every OCCURRENCE of a variable at a different binding, keeping the
 * occurrence's id. This is what retyping ONE CHARACTER of a variable name
 * does: the token survives the edit, so its id survives with it. Cross-
 * checked end to end against real editor actions -- see the trace in the
 * audit report, which `hazel bench-incr` reports UNSOUND. */
let rename_var_occurrences =
    (~from: string, ~to_: string, exp: Exp.t): Exp.t => {
  let f_exp = (continue, e: Exp.t): Exp.t =>
    switch (e.term) {
    | Var(x) when x == from => {
        annotation: e.annotation,
        term: Var(to_),
      }
    | _ => continue(e)
    };
  TermBase.Exp.map_term(~f_exp, exp);
};

/* Swap the two binders of every 2-ary tuple PATTERN, keeping every id. */
let swap_pat_components = (exp: Exp.t): Exp.t => {
  let f_pat = (continue, p: Pat.t): Pat.t =>
    switch (p.term) {
    | Tuple([a, b]) => {
        annotation: p.annotation,
        term: Tuple([continue(b), continue(a)]),
      }
    | _ => continue(p)
    };
  TermBase.Exp.map_term(~f_pat, exp);
};

/* Drop the last binder of every tuple pattern with >2 components. */
let drop_last_pat_component = (exp: Exp.t): Exp.t => {
  let f_pat = (continue, p: Pat.t): Pat.t =>
    switch (p.term) {
    | Tuple(ps) when List.length(ps) > 2 =>
      let keep = List.filteri((i, _) => i < List.length(ps) - 1, ps);
      {
        annotation: p.annotation,
        term: Tuple(keep),
      };
    | _ => continue(p)
    };
  TermBase.Exp.map_term(~f_pat, exp);
};

let id_edit = (e: Exp.t) => e;

/* ==================================================================
 * FINDING 1 (soundness, aM and aStar).
 *
 * IncrEval.exp_flag's `Var(name)` case reports the flag of the binding that
 * `name` denotes NOW. But a flag's contract, as the Tuple case's own comment
 * states it, is a claim about THIS EXPRESSION AT THIS ID: "value_new(e) =
 * value_prev(uid(e))". Reporting the binding's flag is only a claim about
 * this id if the expression at this id was the SAME VARIABLE in the previous
 * run, and nothing checks that.
 *
 * Retyping one character of a variable name repoints an occurrence at a
 * different binding while the token -- and so the id -- survives. The
 * binding it now denotes is itself unedited, so exp_flag reports Clean;
 * pat_provenance then anchors the provenance at the occurrence's id, which
 * the edit preserved, so the recorded provenance is identical to the
 * previous run's and reuse_check's equal_reuse_map passes. The cached value
 * of the OLD variable is served.
 *
 * Every other case of exp_flag is guarded: `reused` comes from a
 * reuse_check that compares elaborations, and `Tuple` consults `prev` for
 * the cached arity and component ids. `Var` is the one that claims Clean
 * without ever looking at what used to be at this id.
 * ================================================================== */

let repoint = rename_var_occurrences(~from="x", ~to_="y");

/* The whole bug in four lines. a0 says 2; aM and aStar say 1. */
let pin_repoint_src = "let x = 1 in
let y = 2 in
let a = x in
a";

let test_pin_repoint_value = () =>
  check_full_sequence(
    ~name="FINDING 1: variable occurrence repointed by a one-char retype",
    ~src=pin_repoint_src,
    ~edits=[repoint],
  );

/* The same stale flag reached through a tuple: the tuple's own id and its
 * component ids are all preserved, so prev_tuple_components admits the
 * cached shape, and the repointed component reports Clean off its (unedited)
 * new binding. */
let test_pin_repoint_through_tuple = () =>
  check_full_sequence(
    ~name="FINDING 1: repointed occurrence inside a tuple, then destructured",
    ~src="let x = 1 in
let y = 2 in
let z = (9, x) in
let (p, q) = z in
q",
    ~edits=[repoint],
  );

/* aStar specifically: the stale flag crosses a2's call boundary. The
 * argument's flag is read off the caller's map (`flags_from`) and the
 * parameter binding lands in the body's, where a2 -- unlike aM -- does
 * maintain a map and does consult the cache. aM alone gets this one right
 * only because it caches nothing inside a call. */
let test_pin_repoint_call_argument = () =>
  check_full_sequence(
    ~name="FINDING 1: repointed occurrence as a call argument (aStar)",
    ~src="let x = 1 in
let y = 2 in
let f : Int -> Int = fun n -> n + 100 in
let a = f(x) in
a",
    ~edits=[repoint],
  );

/* The aL-visible face of the same defect, and the one `hazel bench-incr`
 * cannot see: the final value is a hole under both runs, so the printed
 * comparison agrees, while the probe sample spliced in from the cache is the
 * one the PREVIOUS program produced. */
let test_pin_repoint_probe_sample = () =>
  check_full_sequence(
    ~name="FINDING 1: stale probe sample behind an unchanged printed value",
    ~src="let k1 = 6001 in
let f1 : (Int, Int) -> Int = fun (p, q) -> q - k1 in
let v = ^^probe(f1((22, k0))) in
?",
    ~edits=[rename_var_occurrences(~from="k0", ~to_="k1")],
  );

/* ==================================================================
 * FINDING 3 (soundness, EVERY incremental calculus including aPL).
 *
 * A cache entry's `value` is a DHExp, and for an INDETERMINATE result that
 * DHExp is only meaningful relative to the environment in force where it was
 * recorded. eval_4_reuse hands it straight back. When re-use is blocked at
 * the enclosing ids but fires at an inner one, the spliced residual is an
 * open term: the result mentions `f0`, which is not bound anywhere in it,
 * where a cacheless run reports the substituted lambda.
 *
 * No edit is needed -- evaluating the same program twice with the cache
 * threaded is enough. The free variable matters: it puts a name in the
 * co_ctx that reuse_map_for_co_ctx cannot resolve, which is what blocks
 * re-use at the outer ids and pushes the hit inwards. A bare hole in the
 * same position (`let w = ? in`) does not reproduce it.
 *
 * This one is NOT specific to the integration: aPL reproduces it
 * identically, and the path it needs does not touch aM's flags, a2's trie or
 * the re-use pre-pass. I did not verify it against an earlier revision.
 * ================================================================== */

let test_pin_indeterminate_residual = () =>
  check_full_sequence(
    ~name="FINDING 3: cached indeterminate residual loses its environment",
    ~src="let f0 : Int -> Int = fun n -> 3 in
let w = k1 in
let (v2, v3) = () in
f0(f0(f0(23)))",
    ~edits=[id_edit],
  );

/* Control for FINDING 3: a hole rather than a free variable in the same
 * position leaves every calculus agreeing with a0, which is what pins the
 * co_ctx as the ingredient rather than indeterminacy as such. */
let test_indeterminate_residual_control = () =>
  check_full_sequence(
    ~name="control: hole instead of free variable",
    ~src="let f0 : Int -> Int = fun n -> 3 in
let w = ? in
let (v2, v3) = () in
f0(f0(f0(23)))",
    ~edits=[id_edit],
  );

/* ==================================================================
 * SUSPECT 2, tested head on: can the aM tuple guard find a tuple's entry
 * under a DIFFERENT callstack than the one that recorded it?
 *
 * prev_tuple_components reads `prev.entries`, which after a2 is the ROOT
 * node only, and it compares ids, not callstacks. The one way a root entry
 * can describe a tuple that is now evaluated inside a call is for an
 * id-preserving edit to MOVE the tuple into a function body -- which the
 * editor can do, since typing `fun u -> ` in front of an expression leaves
 * the expression's tokens, and so its ids, alone.
 *
 * `hoist_tail_into_fun` performs exactly that move on the AST: the whole
 * `let z = ... in ...` tail, tuple and destructuring and all, is relocated
 * into the body of the already-present function, and the function is then
 * called. Every id in the moved region survives; only the `f(0)` call node
 * is new. Run 3 repeats run 2's program, so the in-call entries recorded by
 * run 2 are live when run 3 consults them.
 * ================================================================== */
let hoist_tail_into_fun = (exp: Exp.t): Exp.t =>
  switch (exp.term) {
  | Let(fpat, fdef, rest) =>
    switch (fdef.term) {
    | Fun(upat, _old_body, ctx, name) =>
      let fname =
        switch (fpat.term) {
        | Var(v) => v
        | _ => "f"
        };
      {
        annotation: exp.annotation,
        term:
          Let(
            fpat,
            {
              annotation: fdef.annotation,
              term: Fun(upat, rest, ctx, name),
            },
            Exp.fresh(
              Ap(
                Forward,
                Exp.fresh(Var(fname)),
                Exp.fresh(Atom(Int(Bigint.of_int(0)))),
              ),
            ),
          ),
      };
    | _ => exp
    }
  | _ => exp
  };

let hoist_src = "let f : Int -> Int = fun u -> 0 in
let k = 4 in
let z = (1, k) in
let (a, b) = z in
a * 10 + b";

let test_audit2_hoist_sanity = () => {
  let before = parse_exp(hoist_src);
  let after = hoist_tail_into_fun(before);
  Printf.printf("HOIST before: %s\n", show(before));
  Printf.printf("HOIST after:  %s\n", show(after));
  Printf.printf(
    "HOIST probe after: %s\n",
    show(
      hoist_tail_into_fun(
        parse_exp(
          "let f : Int -> Int = fun u -> 0 in let g : Int -> Int = fun n -> n * 2 in let k = 4 in let z = (1, k) in let (a, b) = z in let p = g(a) in let q = g(b) in p * 100 + q",
        ),
      ),
    ),
  );
  check(
    bool,
    "the hoist really moved the tuple into the function body",
    true,
    show(after) != show(before),
  );
};

let test_audit2_tuple_moves_into_a_call = () => {
  check_full_sequence(
    ~name="tuple id moves from the root callstack into a call",
    ~src=hoist_src,
    ~edits=[hoist_tail_into_fun, id_edit, lit(4, 9), id_edit, lit(1, 6)],
  );
  /* Same move, with probes inside the relocated region, so the replayed
   * state slices are compared too and not just the value. */
  check_full_sequence(
    ~name="tuple id moves into a call, with probes",
    ~src="let f : Int -> Int = fun u -> 0 in
let g : Int -> Int = fun n -> n * 2 in
let k = 4 in
let z = (1, k) in
let (a, b) = z in
let p = ^^probe(g(a)) in
let q = ^^probe(g(b)) in
p * 100 + q",
    ~edits=[hoist_tail_into_fun, id_edit, lit(4, 9), id_edit, lit(1, 6)],
  );
};

/* --- corpus A: calls, callstack depth, probes inside calls ---------- */

let audit2_corpus_calls = [
  (
    "tuple built inside a call",
    "let f : Int -> Int = fun n -> let z = (n, 2) in let (a, b) = z in a * 10 + b in
let w = (3, 4) in
let (c, d) = w in
f(c) * 1000 + f(d)",
    [lit(4, 9), lit(3, 5), lit(2, 6)],
  ),
  (
    "probe inside a call, a2 depth",
    "let f : Int -> Int = fun n -> ^^probe(n * 2) in
let g : Int -> Int = fun n -> f(n) + f(n + 1) in
let z = (1, 2) in
let (a, b) = z in
g(a) * 100 + g(b)",
    [lit(2, 7), lit(1, 9), lit(7, 2)],
  ),
  (
    "print inside nested calls",
    "let f : Int -> Int = fun n -> let _ = print(n) in n * 2 in
let g : Int -> Int = fun n -> f(n) + f(n + 1) in
let z = (1, 2) in
let (a, b) = z in
g(a) * 100 + g(b)",
    [lit(2, 7), lit(1, 9)],
  ),
  (
    "recursion deeper than the callstack guard",
    "let r : Int -> Int = fun n -> if n < 1 then 0 else n + r(n - 1) in
let z = (6, 7) in
let (a, b) = z in
r(a) * 1000 + r(b)",
    [lit(7, 8), lit(6, 3), lit(8, 7)],
  ),
  (
    "probe inside deep recursion",
    "let r : Int -> Int = fun n -> if n < 1 then 0 else ^^probe(n) + r(n - 1) in
let z = (6, 7) in
let (a, b) = z in
r(a) * 1000 + r(b)",
    [lit(7, 8), lit(6, 3)],
  ),
  (
    "test statements inside a call",
    "let f : Int -> Int = fun n -> let _ = 0 in test n < 100 end; n * 2 in
let z = (1, 2) in
let (a, b) = z in
f(a) * 100 + f(b)",
    [lit(2, 7), lit(1, 9)],
  ),
  (
    "tuple returned from a call then destructured",
    "let mk : Int -> (Int, Int) = fun n -> (n, n + 1) in
let z = mk(3) in
let (a, b) = z in
a * 100 + b",
    [lit(3, 8), lit(1, 4)],
  ),
  (
    "higher order: tuple through a passed function",
    "let ap : (Int -> Int, Int) -> Int = fun (h, x) -> h(x) in
let f : Int -> Int = fun n -> n * 3 in
let z = (1, 2) in
let (a, b) = z in
ap((f, a)) * 100 + ap((f, b))",
    [lit(2, 7), lit(1, 9), lit(3, 5)],
  ),
  (
    "same function called at two depths",
    "let f : Int -> Int = fun n -> n + 1 in
let g : Int -> Int = fun n -> f(n) in
let z = (1, 2) in
let (a, b) = z in
f(a) * 1000 + g(b)",
    [lit(2, 7), lit(1, 9)],
  ),
  (
    "body sub-expression independent of the parameter (a2's own win)",
    "let k = 5 in
let f : Int -> Int = fun n -> let big = k * k * k in big + n in
let z = (1, 2) in
let (a, b) = z in
f(a) * 1000 + f(b)",
    [lit(1, 9), lit(2, 7), lit(5, 6)],
  ),
];

/* --- corpus B: the pre-pass walking past an indeterminate let ------- */

let audit2_corpus_prepass = [
  (
    "name rebound after the destructuring",
    "let z = (1, 2) in
let (a, b) = z in
let a = 100 in
a + b",
    [lit(2, 7), lit(1, 9), lit(100, 200)],
  ),
  (
    "name bound BEFORE and rebound by the destructuring",
    "let a = 50 in
let c = a in
let z = (1, 2) in
let (a, b) = z in
let d = a in
c * 1000 + d * 10 + b",
    [lit(2, 7), lit(50, 60), lit(1, 9)],
  ),
  (
    "pattern binds a name the body shadows again",
    "let z = (1, 2) in
let (a, b) = z in
let f : Int -> Int = fun a -> a * 3 in
let a = b in
f(a) * 100 + a",
    [lit(2, 7), lit(1, 9)],
  ),
  (
    "nested destructuring after an indeterminate let",
    "let z = ((1, 2), (3, 4)) in
let ((a, b), w) = z in
let (c, d) = w in
a * 1000 + b * 100 + c * 10 + d",
    [lit(4, 9), lit(1, 6), lit(3, 8)],
  ),
  (
    "definition after the destructuring reached by the pre-pass",
    "let z = (1, 2) in
let (a, b) = z in
let k = 5 in
let f : Int -> Int = fun n -> n * k in
f(a) * 100 + f(b)",
    [lit(2, 7), lit(5, 6), lit(1, 9)],
  ),
  (
    "two destructurings in a row",
    "let z = (1, 2) in
let (a, b) = z in
let w = (b, a) in
let (c, d) = w in
c * 100 + d",
    [lit(2, 7), lit(1, 9)],
  ),
  (
    "destructure a hole",
    "let z = (1, 2) in
let (a, b) = ? in
let c = 7 in
c * 100",
    [lit(7, 8), lit(1, 4)],
  ),
  (
    "shadow the tuple itself after destructuring",
    "let z = (1, 2) in
let (a, b) = z in
let z = (b, a) in
let (c, d) = z in
c * 100 + d",
    [lit(2, 7), lit(1, 9)],
  ),
  (
    "wild and label-only patterns after destructuring",
    "let z = (1, 2, 3) in
let (_, b, _) = z in
let k = 4 in
b * 100 + k",
    [lit(3, 8), lit(2, 7), lit(4, 6)],
  ),
  (
    "destructuring inside a function body",
    "let f : (Int, Int) -> Int = fun w -> let (a, b) = w in let k = 5 in a * k + b in
let z = (1, 2) in
f(z) * 10 + 1",
    [lit(2, 7), lit(5, 6), lit(1, 9)],
  ),
];

/* --- corpus C: tuple shapes under aStar ----------------------------- */

let audit2_corpus_shapes = [
  (
    "empty tuple bound and used",
    "let u = () in let z = (u, 1) in let (a, b) = z in b",
    [lit(1, 5), lit(5, 2)],
  ),
  (
    "singleton via parens",
    "let z = (1) in let w = (z, 2) in let (a, b) = w in a * 10 + b",
    [lit(2, 7), lit(1, 9)],
  ),
  (
    "labeled tuple through a call",
    "let f : (a = Int, b = Int) -> Int = fun (a = p, b = q) -> p * 10 + q in
let z : (a = Int, b = Int) = (a = 1, b = 2) in
f(z)",
    [lit(2, 7), lit(1, 9)],
  ),
  (
    "tuple of tuples through a call",
    "let f : ((Int, Int), (Int, Int)) -> Int =
  fun ((a, b), (c, d)) -> a * 1000 + b * 100 + c * 10 + d in
let z = ((1, 2), (3, 4)) in
f(z)",
    [lit(4, 9), lit(1, 6)],
  ),
  (
    "three-tuple partially edited through a call",
    "let f : (Int, Int, Int) -> Int = fun (a, b, c) -> a * 10000 + b * 100 + c in
let z = (1, 2, 3) in
f(z)",
    [lit(2, 7), lit(3, 8), lit(1, 9)],
  ),
  (
    "tuple inside a tuple inside a call",
    "let f : Int -> (Int, (Int, Int)) = fun n -> (n, (n + 1, n + 2)) in
let z = f(3) in
let (a, (b, c)) = z in
a * 10000 + b * 100 + c",
    [lit(3, 8), lit(1, 4)],
  ),
];

let test_audit2_sweep_calls = () => check_corpus(audit2_corpus_calls);
let test_audit2_sweep_prepass = () => check_corpus(audit2_corpus_prepass);
let test_audit2_sweep_shapes = () => check_corpus(audit2_corpus_shapes);

/* --- a randomized corpus -------------------------------------------
 *
 * The hand-written corpora only cover shapes somebody thought of. This
 * generates well-typed Int programs over the constructs the calculi actually
 * branch on -- tuple literals, tuple destructuring (including the
 * `let (a, b) = z` shape that makes the pre-pass's match indeterminate),
 * calls at several depths, `let`s inside function bodies, shadowing, probes,
 * prints and tests -- then edits them. Deterministic PRNG, so any failure is
 * reproducible from its seed. */
module Fuzz = {
  /* xorshift over 30-bit ints: exact under js_of_ocaml, unlike an LCG whose
   * multiply overflows the int and loses the low bits a seed needs in order
   * to actually diverge. */
  let state = ref(1);
  let step = (): int => {
    let x = state^;
    let x = (x lxor x lsl 13) land 0x3FFFFFFF;
    let x = x lxor x lsr 17;
    let x = (x lxor x lsl 5) land 0x3FFFFFFF;
    state := x == 0 ? 1 : x;
    state^;
  };
  let reset = (s: int) => {
    state := s == 0 ? 1 : s land 0x3FFFFFFF;
    for (_ in 1 to 8) {
      ignore(step());
    };
  };
  let next_int = (n: int): int => step() mod n;
  let pick = (xs: list('a)): 'a => List.nth(xs, next_int(List.length(xs)));

  /* Every integer literal in a generated program is distinct, so
   * replace_int_lit targets exactly one leaf. Only + and - are generated:
   * composing multiplications across bindings builds a power tower that
   * exhausts the heap long before it finds a bug. */
  let lit_pool = ref(0);
  let lits = ref([]);
  let fresh_lit = (): string => {
    lit_pool := lit_pool^ + 1;
    let v = lit_pool^;
    lits := [v, ...lits^];
    string_of_int(v);
  };
  let names = ref(0);
  let fresh_name = (): string => {
    names := names^ + 1;
    "v" ++ string_of_int(names^);
  };

  /* `funs` is which of f0/f1/f2 may be called here. A function body is
   * generated with only the STRICTLY EARLIER functions in scope, because
   * `let f : Int -> Int = ...` elaborates to a FixF and calling f inside its
   * own body would not terminate. */
  let rec gen_int =
          (
            ~depth: int,
            ~vars: list(string),
            ~tvars: list(string),
            ~funs: list(string),
          )
          : string => {
    let sub = () => gen_int(~depth=depth - 1, ~vars, ~tvars, ~funs);
    let leaf = () =>
      switch (vars) {
      | [] => fresh_lit()
      | _ => next_int(2) == 0 ? fresh_lit() : pick(vars)
      };
    if (depth <= 0) {
      leaf();
    } else {
      let choices =
        ["lit", "var", "op", "if"]
        @ (List.mem("f0", funs) ? ["f0", "f0f0"] : [])
        @ (List.mem("f1", funs) ? ["f1"] : [])
        @ (List.mem("f1", funs) && tvars != [] ? ["f1v"] : []);
      switch (pick(choices)) {
      | "lit" => fresh_lit()
      | "var" => leaf()
      | "op" => "(" ++ sub() ++ " " ++ pick(["+", "-"]) ++ " " ++ sub() ++ ")"
      | "f0" => "f0(" ++ sub() ++ ")"
      | "f0f0" => "f0(f0(" ++ sub() ++ "))"
      | "f1" => "f1((" ++ sub() ++ ", " ++ sub() ++ "))"
      | "f1v" => "f1(" ++ pick(tvars) ++ ")"
      | _ =>
        "(if "
        ++ sub()
        ++ " < "
        ++ sub()
        ++ " then "
        ++ sub()
        ++ " else "
        ++ sub()
        ++ ")"
      };
    };
  };

  let gen_tup =
      (
        ~depth: int,
        ~vars: list(string),
        ~tvars: list(string),
        ~funs: list(string),
      )
      : string =>
    switch (next_int(4), tvars, List.mem("f2", funs)) {
    | (0, [_, ..._], _) => pick(tvars)
    | (1, _, true) => "f2(" ++ gen_int(~depth, ~vars, ~tvars, ~funs) ++ ")"
    | _ =>
      "("
      ++ gen_int(~depth, ~vars, ~tvars, ~funs)
      ++ ", "
      ++ gen_int(~depth, ~vars, ~tvars, ~funs)
      ++ ")"
    };

  /* A function BODY: a few inner bindings (including tuple destructuring,
   * which is what puts aM's flags and a2's in-call re-use map in the same
   * place) and then an int expression. */
  let gen_body =
      (~vars: list(string), ~tvars: list(string), ~funs: list(string))
      : string => {
    let vars = ref(vars);
    let tvars = ref(tvars);
    let pre = ref("");
    for (_ in 1 to next_int(3)) {
      let chunk =
        switch (next_int(4)) {
        | 0 =>
          let x = fresh_name();
          let e = gen_int(~depth=2, ~vars=vars^, ~tvars=tvars^, ~funs);
          vars := [x, ...vars^];
          "let " ++ x ++ " = " ++ e ++ " in ";
        | 1 =>
          let z = fresh_name();
          let e = gen_tup(~depth=2, ~vars=vars^, ~tvars=tvars^, ~funs);
          tvars := [z, ...tvars^];
          "let " ++ z ++ " = " ++ e ++ " in ";
        | 2 =>
          let a = fresh_name();
          let b = fresh_name();
          let e =
            switch (tvars^) {
            | [_, ..._] when next_int(2) == 0 => pick(tvars^)
            | _ => gen_tup(~depth=2, ~vars=vars^, ~tvars=tvars^, ~funs)
            };
          vars := [a, b, ...vars^];
          "let (" ++ a ++ ", " ++ b ++ ") = " ++ e ++ " in ";
        | _ =>
          let e = gen_int(~depth=2, ~vars=vars^, ~tvars=tvars^, ~funs);
          "let _ = print(" ++ e ++ ") in ";
        };
      pre := pre^ ++ chunk;
    };
    pre^ ++ gen_int(~depth=2, ~vars=vars^, ~tvars=tvars^, ~funs);
  };

  /* One program: two top-level constants (so a function body can hold a
   * sub-expression that does NOT depend on the parameter, which is the only
   * thing a2 can re-use inside a call), three functions, then a chain of
   * bindings. */
  let gen_program = (~binds: int): string => {
    lits := [];
    let k0 = "let k0 = " ++ fresh_lit() ++ " in\n";
    let k1 = "let k1 = " ++ fresh_lit() ++ " in\n";
    let ks = ["k0", "k1"];
    let f0 =
      "let f0 : Int -> Int = fun n -> "
      ++ gen_body(~vars=["n", ...ks], ~tvars=[], ~funs=[])
      ++ " in\n";
    let f1 =
      "let f1 : (Int, Int) -> Int = fun (p, q) -> "
      ++ gen_body(~vars=["p", "q", ...ks], ~tvars=[], ~funs=["f0"])
      ++ " in\n";
    let f2 =
      "let f2 : Int -> (Int, Int) = fun m -> ("
      ++ gen_int(~depth=2, ~vars=["m", ...ks], ~tvars=[], ~funs=["f0", "f1"])
      ++ ", "
      ++ gen_int(~depth=2, ~vars=["m", ...ks], ~tvars=[], ~funs=["f0", "f1"])
      ++ ") in\n";
    let funs = ["f0", "f1", "f2"];
    let vars = ref(["k0", "k1"]);
    let tvars = ref([]);
    let body = ref("");
    for (_ in 1 to binds) {
      let d = 2;
      let chunk =
        switch (next_int(8)) {
        | 0 =>
          let x = fresh_name();
          let e = gen_int(~depth=d, ~vars=vars^, ~tvars=tvars^, ~funs);
          vars := [x, ...vars^];
          "let " ++ x ++ " = " ++ e ++ " in\n";
        | 1 =>
          let z = fresh_name();
          let e = gen_tup(~depth=d, ~vars=vars^, ~tvars=tvars^, ~funs);
          tvars := [z, ...tvars^];
          "let " ++ z ++ " = " ++ e ++ " in\n";
        | 2 =>
          let a = fresh_name();
          let b = fresh_name();
          let e = gen_tup(~depth=d, ~vars=vars^, ~tvars=tvars^, ~funs);
          vars := [a, b, ...vars^];
          "let (" ++ a ++ ", " ++ b ++ ") = " ++ e ++ " in\n";
        | 3 =>
          /* rebind an existing name: shadowing */
          switch (vars^) {
          | [] =>
            let x = fresh_name();
            let e = gen_int(~depth=d, ~vars=vars^, ~tvars=tvars^, ~funs);
            vars := [x, ...vars^];
            "let " ++ x ++ " = " ++ e ++ " in\n";
          | _ =>
            let x = pick(vars^);
            let e = gen_int(~depth=d, ~vars=vars^, ~tvars=tvars^, ~funs);
            "let " ++ x ++ " = " ++ e ++ " in\n";
          }
        | 4 =>
          let x = fresh_name();
          let e = gen_int(~depth=d, ~vars=vars^, ~tvars=tvars^, ~funs);
          vars := [x, ...vars^];
          "let " ++ x ++ " = ^^probe(" ++ e ++ ") in\n";
        | 5 =>
          let e = gen_int(~depth=d, ~vars=vars^, ~tvars=tvars^, ~funs);
          "let _ = print(" ++ e ++ ") in\n";
        | 6 =>
          let e = gen_int(~depth=1, ~vars=vars^, ~tvars=tvars^, ~funs);
          let e2 = gen_int(~depth=1, ~vars=vars^, ~tvars=tvars^, ~funs);
          "test " ++ e ++ " < " ++ e2 ++ " end;\n";
        | _ =>
          /* the pre-pass's indeterminate shape: a tuple pattern against a
           * bare variable */
          switch (tvars^) {
          | [] =>
            let a = fresh_name();
            let b = fresh_name();
            let e = gen_tup(~depth=d, ~vars=vars^, ~tvars=tvars^, ~funs);
            vars := [a, b, ...vars^];
            "let (" ++ a ++ ", " ++ b ++ ") = " ++ e ++ " in\n";
          | _ =>
            let a = fresh_name();
            let b = fresh_name();
            let z = pick(tvars^);
            vars := [a, b, ...vars^];
            "let (" ++ a ++ ", " ++ b ++ ") = " ++ z ++ " in\n";
          }
        };
      body := body^ ++ chunk;
    };
    k0 ++ k1 ++ f0 ++ f1 ++ f2 ++ body^ ++ gen_int(~depth=2, ~vars=vars^, ~tvars=tvars^, ~funs);
  };
};

let rec take = (k, xs) =>
  switch (k, xs) {
  | (0, _)
  | (_, []) => []
  | (k, [x, ...rest]) => [x, ...take(k - 1, rest)]
  };

/* Edit a handful of the program's (all-distinct) literals, one at a time, to
 * values that appear nowhere else. */
let fuzz_edits = (~n: int, present: list(int)): list(Exp.t => Exp.t) =>
  List.mapi(
    (i, v) => replace_int_lit(~from=v, ~to_=5000 + i),
    take(n, present),
  );

/* The same, with SHAPE-CHANGING edits mixed in: the arity and order of a
 * tuple (and of a tuple pattern) change under an id-preserving edit, a
 * literal collapses to a hole, and a variable occurrence is repointed at a
 * different binding. */
let include_var_repoint = ref(true);

let fuzz_structural_edits =
    (~n: int, present: list(int)): list(Exp.t => Exp.t) => {
  let lit_edits =
    List.mapi(
      (i, v) => replace_int_lit(~from=v, ~to_=6000 + i),
      take(n, present),
    );
  let structural =
    [
      swap_tuple_components,
      swap_pat_components,
      drop_last_tuple_component,
      empty_the_tuple,
      drop_last_pat_component,
    ]
    @ (
      include_var_repoint^
        ? [rename_var_occurrences(~from="k0", ~to_="k1")]
        : [swap_tuple_components]
    );
  let rec weave = (a, b) =>
    switch (a, b) {
    | ([], b) => b
    | (a, []) => a
    | ([x, ...a], [y, ...b]) => [x, y, ...weave(a, b)]
    };
  weave(lit_edits, [Fuzz.pick(structural), Fuzz.pick(structural)])
  @ [
    blank_int_lit(
      ~from=
        switch (present) {
        | [] => 1
        | [v, ..._] => v
        },
    ),
  ];
};

let fuzz_sweep =
    (
      ~seeds: list(int),
      ~binds: int,
      ~edits: int,
      ~mk: (~n: int, list(int)) => list(Exp.t => Exp.t),
      ~tag: string,
    ) => {
  let all = ref([]);
  List.iter(
    seed => {
      Fuzz.reset(seed);
      Fuzz.lit_pool := 0;
      Fuzz.names := 0;
      let src = Fuzz.gen_program(~binds);
      let present = List.rev(Fuzz.lits^);
      let name = Printf.sprintf("%s-seed-%d", tag, seed);
      let ds =
        try(diffs_of_sequence(~name, ~src, ~edits=mk(~n=edits, present))) {
        | e => [name ++ " RAISED " ++ Printexc.to_string(e)]
        };
      if (ds != []) {
        Printf.printf("FUZZSRC %s\n%s\n", name, src);
      };
      all := all^ @ ds;
    },
    seeds,
  );
  switch (all^) {
  | [] => ()
  | ds => Alcotest.fail(String.concat("\n", ds))
  };
};

let rec range = (a, b) => a > b ? [] : [a, ...range(a + 1, b)];

/* Literal retypes only: this one PASSES, and is the regression net. */
let test_audit2_fuzz = () =>
  fuzz_sweep(
    ~seeds=range(1, 60),
    ~binds=7,
    ~edits=4,
    ~mk=fuzz_edits,
    ~tag="fuzz",
  );

/* With shape-changing edits, including the variable repoint of FINDING 1.
 * Seeds 22 and 33 in this range fail; both are FINDING 1 (removing the
 * repoint edit from the pool makes every aStar-specific failure in seeds
 * 1..200 disappear). */
let test_audit2_fuzz_structural = () =>
  fuzz_sweep(
    ~seeds=range(1, 40),
    ~binds=7,
    ~edits=2,
    ~mk=fuzz_structural_edits,
    ~tag="fuzzS",
  );

/* The same sweep with the repoint edit removed from the pool. This PASSES
 * over seeds 1..40, which is the evidence that FINDING 1 accounts for every
 * aM/aStar-specific divergence the structural sweep found. */
let test_audit2_fuzz_structural_no_repoint = () => {
  include_var_repoint := false;
  let finally = () => include_var_repoint := true;
  switch (
    fuzz_sweep(
      ~seeds=range(1, 40),
      ~binds=7,
      ~edits=2,
      ~mk=fuzz_structural_edits,
      ~tag="fuzzS-norepoint",
    )
  ) {
  | () => finally()
  | exception e =>
    finally();
    raise(e);
  };
};

/* A visible witness that the sweeps are not vacuous: re-use really fires,
 * a2's trie really goes deeper than the root, and probe slices really get
 * replayed. Prints rather than asserts. */
let test_audit2_reuse_witness = () =>
  List.iter(
    (calculus: Calculus.t) =>
      ignore(
        diffs_for(
          ~name="witness",
          ~calculus,
          ~src=
            "let k = 5 in
let f : Int -> Int = fun n -> let big = k + k + k in ^^probe(big + n) in
let z = (1, 2) in
let (a, b) = z in
f(a) * 1000 + f(b)",
          ~edits=[lit(1, 9), lit(2, 7)],
          ~trace=true,
        ),
      ),
    Calculus.available,
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
    /* ---- audit 2 ---- */
    test_case("audit2: reuse witness", `Quick, test_audit2_reuse_witness),
    test_case("audit2: hoist sanity", `Quick, test_audit2_hoist_sanity),
    test_case(
      "audit2: tuple id moves into a call",
      `Quick,
      test_audit2_tuple_moves_into_a_call,
    ),
    test_case("audit2: sweep calls", `Quick, test_audit2_sweep_calls),
    test_case("audit2: sweep prepass", `Quick, test_audit2_sweep_prepass),
    test_case("audit2: sweep shapes", `Quick, test_audit2_sweep_shapes),
    test_case("audit2: fuzz (literal edits)", `Quick, test_audit2_fuzz),
    test_case(
      "audit2: fuzz (structural edits, no repoint)",
      `Quick,
      test_audit2_fuzz_structural_no_repoint,
    ),
    test_case(
      "audit2: control, hole not free variable",
      `Quick,
      test_indeterminate_residual_control,
    ),
    /* ---- intentional failing pins ---- */
    test_case(
      "PIN finding 1: repointed variable occurrence",
      `Quick,
      test_pin_repoint_value,
    ),
    test_case(
      "PIN finding 1: repointed occurrence through a tuple",
      `Quick,
      test_pin_repoint_through_tuple,
    ),
    test_case(
      "PIN finding 1: repointed occurrence as a call argument",
      `Quick,
      test_pin_repoint_call_argument,
    ),
    test_case(
      "PIN finding 1: stale probe sample, value unchanged",
      `Quick,
      test_pin_repoint_probe_sample,
    ),
    test_case(
      "PIN finding 2: fuzz (structural edits, with repoint)",
      `Quick,
      test_audit2_fuzz_structural,
    ),
    test_case(
      "PIN finding 3: cached indeterminate residual",
      `Quick,
      test_pin_indeterminate_residual,
    ),
  ],
);

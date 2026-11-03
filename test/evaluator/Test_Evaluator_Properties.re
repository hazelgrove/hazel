open Language;
open Test_Evaluator_Prelude;
open Alcotest;
open Poly;

let qcheck_evaluator_does_not_crash_test =
  QCheck.Test.make(
    ~name="Evaluator does not crash",
    ~count=1000,
    QCheck_Util.arb_exp_full(~minimal_idents=true, 5),
    exp => {
    switch (
      {
        let (_, elab) =
          Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), exp);
        elab;
      }
    ) {
    | exp =>
      switch (
        Evaluator.evaluate_and_limit(
          ~env=Builtins.env_init,
          ~step_limit=10000,
          exp,
        )
      ) {
      | LimitedCompleted(_)
      | StepLimitExceeded => true
      | exception e =>
        switch (e) {
        | Failure(msg)
            when
              List.exists(
                ~f=(==)(msg),
                ["type application in dynamics", "Type meet of ap"] // "type application in dynamics" https://github.com/hazelgrove/hazel/issues/1625
              ) =>
          print_endline("Skipping failure: " ++ msg);
          true;
        | _ => raise(e)
        }
      }
    | exception e =>
      print_endline(
        "Skipping statics/elaborate failure: " ++ Exn.to_string(e),
      );
      true;
    }
  });

let testable_core_exp =
  testable(
    Fmt.using(QCheck_Util.show_core_exp, Fmt.string),
    Equality.semantic.exp,
  );

/* For properties over generated terms: results compared with what each
   `proof_of` quotes erased (see QCheck_Util.erase_proof_props). */
let testable_core_exp_up_to_proofs =
  testable(Fmt.using(QCheck_Util.show_core_exp, Fmt.string), (a, b) =>
    Equality.semantic.exp(
      QCheck_Util.erase_proof_props_in_exp(a),
      QCheck_Util.erase_proof_props_in_exp(b),
    )
  );

/* Check that the evaluator and the stepper are consistent on `uexp`. The step
   limit and known issues that raise are skipped — or, with
   `require_comparison`, failed, so a fixed counterexample that stops reaching
   the comparison says so instead of passing. */
let check_evaluator_stepper_consistent =
    (
      ~step_limit: int,
      ~testable: testable(Exp.t),
      ~msg: string,
      ~require_comparison: bool,
      uexp: Exp.t,
    )
    : unit =>
  switch (
    {
      let (_, elab) =
        Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), uexp);
      elab;
    }
  ) {
  | elaborated_exp =>
    switch (
      Evaluator.evaluate_and_limit(
        ~env=Builtins.env_init,
        ~step_limit,
        elaborated_exp,
      ),
      full_small_step_reduction(~step_limit, elaborated_exp),
    ) {
    | (LimitedCompleted((bigstep_exp, _)), LimitedCompleted(smallstep_exp)) =>
      check(testable, msg, fst(smallstep_exp), bigstep_exp)
    | (_, StepLimitExceeded)
    | (StepLimitExceeded, _) =>
      if (require_comparison) {
        failf(
          "reduction did not terminate within %d steps, so nothing was compared; re-minimize the counterexample",
          step_limit,
        );
      }
    | exception e =>
      if (require_comparison) {
        failf(
          "evaluation raised %s, so nothing was compared; re-minimize the counterexample",
          Exn.to_string(e),
        );
      } else {
        print_endline("Skipping evaluation failure: " ++ Exn.to_string(e));
      }
    }
  | exception e =>
    if (require_comparison) {
      failf(
        "statics/elaborate raised %s, so nothing was compared; re-minimize the counterexample",
        Exn.to_string(e),
      );
    } else {
      print_endline(
        "Skipping statics/elaborate failure: " ++ Exn.to_string(e),
      );
    }
  };

let qcheck_stepper_confluence =
  QCheck.Test.make(
    ~name="Evaluator and stepper are consistent",
    ~count=1000,
    QCheck_Util.arb_exp_canonical(~minimal_idents=true, 5),
    uexp => {
      check_evaluator_stepper_consistent(
        ~step_limit=100,
        ~testable=testable_core_exp_up_to_proofs,
        ~msg="Small step reduction and big step reduction are equal",
        ~require_comparison=false,
        uexp,
      );
      true;
    },
  );

// Property that states let x : T = e in x is equivalent to e : T
/* `e : T` and `let x : T = e in x` evaluate to the same thing. The binder is a
   name the generator never produces (it only makes single letters), or it
   would capture a free `x` in `e`: a definition that mentions its own binder
   elaborates to a fixpoint. Statics/dynamics failures skip the case; the
   comparison itself must stay outside the catch-all. */
let pattern_equivalence_binder = "pe_bound";

/* Values compared up to leftover casts: `() : (rec o^o -> `y`)` and
   `() : `y`` are the same value under the same type, spelled differently. */
let pattern_equivalence_testable =
  testable(
    Fmt.using(QCheck_Util.show_core_exp, Fmt.string),
    Equality.(
      equality({
        ...syntactic_settings,
        ignore_parens: true,
        ignore_function_names: true,
        ignore_function_types: true,
        ignore_unknown_provenance: true,
        ignore_explicit_unlabelling: true,
        ignore_ascriptions: true,
        ignore_list_types: true,
      })
    ).
      exp,
  );

let qcheck_pattern_equivalence_test =
  QCheck.Test.make(
    ~name="Pattern equivalence",
    ~count=1000,
    QCheck.pair(
      QCheck_Util.arb_exp_canonical(~minimal_idents=true, 5),
      QCheck_Util.arb_typ_canonical(~minimal_idents=true, 5),
    ),
    ((uexp, typ)) => {
      open IdTagged.FreshGrammar;
      open Exp;
      let first = asc(uexp, typ);
      let second =
        let_(
          Pat.asc(Pat.var(pattern_equivalence_binder), typ),
          uexp,
          var(pattern_equivalence_binder),
        );
      switch (
        Evaluator.evaluate_and_limit(
          ~env=Builtins.env_init,
          ~step_limit=10000,
          elaborate(first),
        ),
        Evaluator.evaluate_and_limit(
          ~env=Builtins.env_init,
          ~step_limit=10000,
          elaborate(second),
        ),
      ) {
      | exception _ => QCheck.assume_fail()
      /* Only finished values are compared. A stuck result keeps its
         leftover ascriptions wherever evaluation stopped, which differs
         between the two forms (`o^o : (x=?)` against `(x=o^o)`) without
         either being wrong. */
      | (
          LimitedCompleted((first_exp, _)),
          LimitedCompleted((second_exp, _)),
        )
          when
            ValueChecker.is_value(first_exp)
            && ValueChecker.is_value(second_exp) =>
        Alcotest.check(
          pattern_equivalence_testable,
          "Evaluated expressions are equal",
          first_exp,
          second_exp,
        );
        true;
      | (
          LimitedCompleted((first_exp, _)),
          LimitedCompleted((second_exp, _)),
        )
          when
            !ValueChecker.is_value(first_exp)
            && !ValueChecker.is_value(second_exp) =>
        true
      | (LimitedCompleted(_), LimitedCompleted(_)) =>
        QCheck.Test.fail_report(
          "One evaluation finished as a value, the other stuck",
        )
      | (StepLimitExceeded, StepLimitExceeded) => true
      | (LimitedCompleted(_), StepLimitExceeded)
      | (StepLimitExceeded, LimitedCompleted(_)) =>
        QCheck.Test.fail_report(
          "One of the evaluations exceeded the step limit",
        )
      };
    },
  );

/* Carved out until `is_more_precise` compares recursive types as trees
   (the equirecursive stack, #2736, #2737, #2740): a `rec` whose binder its body never
   uses (`rec x -> ?`, or an invalid binder) is its body, so a step that
   sees through it (`_ : (rec x -> ?)` to `_`, type `?`) looks less precise. */
let has_rec_with_unused_binder = (ty: Typ.t): bool => {
  let found = ref(false);
  let uses = (name, body) => {
    let hit = ref(false);
    let _ =
      Typ.map_term(
        ~f_typ=
          (continue, t: Typ.t) => {
            switch (t.term) {
            | Var(v) when v == name => hit := true
            | _ => ()
            };
            continue(t);
          },
        body,
      );
    hit^;
  };
  let _ =
    Typ.map_term(
      ~f_typ=
        (continue, t: Typ.t) => {
          switch (t.term) {
          | Rec(tp, body) =>
            switch (TPat.tyvar_of_utpat(tp)) {
            | Some(name) when uses(name, body) => ()
            | _ => found := true
            }
          | _ => ()
          };
          continue(t);
        },
      ty,
    );
  found^;
};

// Taking a step should result in a consistent type that is more precise than the original type
[@warning "-52"]
let qcheck_preservation_test =
  QCheck.Test.make(
    ~name="Preservation of types",
    ~count=1000,
    QCheck_Util.arb_exp_canonical(~minimal_idents=true, 5),
    uexp => {
    switch (
      switch (
        {
          let (elab, ty) =
            QCheck_Util.skip_alias_loop(() => {
              let (statics, elab) =
                Statics.mk(
                  CoreSettings.on,
                  Builtins.ctx_init(Some(Int)),
                  uexp,
                );
              (elab, elaborated_type(statics, uexp));
            });
          QCheck.assume(!has_rec_with_unused_binder(ty));
          let stepped = single_step(elab);
          (stepped, ty);
        }
      ) {
      | (Some(next), orig_ty) =>
        /* `next` is already elaborated: re-check with no ambient
           mode, as CachedStatics does, or `Operators.replace_*` turns
           a Float negation back into an Int one. */
        switch (
          {
            let (statics, _) =
              Statics.mk(CoreSettings.on, Builtins.ctx_init(None), next);
            Statics.Map.ty_of(next.annotation.ids |> List.hd_exn, statics);
          }
        ) {
        | Some(ty) =>
          Typ.is_more_precise(
            Ctx.empty,
            QCheck_Util.erase_proof_props(ty),
            QCheck_Util.erase_proof_props(orig_ty),
          )
            ? true
            : Alcotest.fail(
                "Preservation failed: original type "
                ++ Typ.show(orig_ty)
                ++ " is not more precise than stepped type "
                ++ Typ.show(ty),
              )
        | _ =>
          Alcotest.fail("No type information found for stepped expression")
        }
      | (None, _) => true // If we can't take a step, we don't have to check preservation
      }
    ) {
    | ret => ret
    | exception (Invalid_argument("List.fold_left2")) // https://github.com/hazelgrove/hazel/issues/1673
    | exception Stack_overflow => true // Known issue with some expressions that cause infinite recursion in the stepper
    }
  });

/* Incremental evaluator correctness:
 *
 *   for all expressions E and edits δ,
 *     eval_incr(δ(E), prev = eval(E))  ==  eval(δ(E))
 *
 * i.e. evaluating the edited expression against the previous run's cache
 * must land on the same value as evaluating it from scratch. Both sides
 * use the SAME elaboration of the edited expression, so any disagreement
 * points squarely at the reuse / dirty-propagation logic rather than at
 * semantics.
 *
 * The edit we apply is a single-literal mutation (flip an Atom(Int n) to
 * Atom(Int n')) with all surrounding ids preserved — this mirrors the
 * Zipper's behaviour on a text edit, where only the touched tokens get
 * new ids. That in-place mutation is what makes the test hit the
 * reuse_check path at all; if we regenerated the tree from source the
 * id spaces would be disjoint and nothing would match prev.
 *
 * Known skips (the property still passes): expressions with no int literal
 * to edit (nothing to test), anything that hits the step limit, and anything
 * that raises from statics/evaluation (filtered the same way as the
 * other evaluator QCheck tests in this file). */

/* Collect every (id, value) pair for Atom(Int _) leaves in the tree.
 * We key on the Atom's own rep_id so the later substitute pass can
 * find and mutate exactly one leaf without touching any sibling ids. */
let collect_int_lits = (exp: Exp.t): list((Id.t, Bigint.t)) => {
  let acc = ref([]);
  let f_exp = (continue, e: Exp.t): Exp.t => {
    switch (e.term) {
    | Atom(Int(n)) => acc := [(Exp.rep_id(e), n), ...acc^]
    | _ => ()
    };
    continue(e);
  };
  let _ = TermBase.Exp.map_term(~f_exp, exp);
  acc^;
};

/* Replace the Atom(Int _) payload at the given id with `to_`, preserving
 * the id on the edited node (and on every other node). */
let replace_int_lit_by_id = (~target: Id.t, ~to_: Bigint.t, exp: Exp.t): Exp.t => {
  let f_exp = (continue, e: Exp.t): Exp.t =>
    if (Id.equal(Exp.rep_id(e), target)) {
      switch (e.term) {
      | Atom(Int(_)) => {
          ...e,
          term: Atom(Int(to_)),
        }
      | _ => continue(e)
      };
    } else {
      continue(e);
    };
  TermBase.Exp.map_term(~f_exp, exp);
};

/* Same wrapper the incremental tests use — statics plus elaboration in
 * one call, so both sides of the equation see identical elab terms. */
let statics_and_elab = (exp: Exp.t): (Statics.Map.t, Exp.t) =>
  Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), exp);

let eval_limited =
    (~prev=IncrEval.empty, ~eval_info, ~step_limit, elab: Exp.t) =>
  Evaluator.evaluate_and_limit(
    ~step_limit,
    ~prev,
    ~eval_info,
    ~env=Builtins.env_init,
    elab,
  );

/* Check that incremental eval and fresh eval agree on `exp` with one of its int
   literals bumped by one — `literal_index`, taken modulo how many there are.
   Skips and `require_comparison` as above. */
let check_incremental_against_fresh_after_edit =
    (
      ~step_limit: int=10000,
      ~compared: option(ref(int))=?,
      ~literal_index: int,
      ~testable: testable(Exp.t),
      ~msg: string,
      ~require_comparison: bool,
      exp: Exp.t,
    )
    : unit => {
  let no_comparison = reason =>
    if (require_comparison) {
      failf(
        "%s, so nothing was compared; re-minimize the counterexample",
        reason,
      );
    };
  /* Only swallow known-benign static/dynamic failures so real
   * incremental-eval disagreements surface as clean failures. */
  let try_eval = (~prev=?, eval_info, elab) =>
    try(Some(eval_limited(~prev?, ~eval_info, ~step_limit, elab))) {
    | Failure(msg)
        when
          List.exists(
            ~f=(==)(msg),
            ["type application in dynamics", "Type meet of ap"],
          ) =>
      None
    };
  let try_statics = exp =>
    try(Some(statics_and_elab(exp))) {
    | _ => None
    };
  switch (collect_int_lits(exp)) {
  | [] => no_comparison("the program has no int literal to edit")
  | lits =>
    let (target_id, old_value) =
      List.nth_exn(lits, literal_index mod List.length(lits));
    /* +1 keeps the type fixed, so the edit typechecks the same as the
     * original while still changing the value at `target_id`. */
    let new_value = Bigint.(old_value + of_int(1));
    let edited =
      replace_int_lit_by_id(~target=target_id, ~to_=new_value, exp);
    switch (try_statics(exp), try_statics(edited)) {
    | (Some((info_map_orig, elab_orig)), Some((info_map_edit, elab_edit))) =>
      let info_slice_orig =
        EvalInfo.of_info_map(
          ~probe_all=CoreSettings.on.probe_all,
          ~targets=Id.Map.empty,
          info_map_orig,
        );
      let info_slice_edit =
        EvalInfo.of_info_map(
          ~probe_all=CoreSettings.on.probe_all,
          ~targets=Id.Map.empty,
          info_map_edit,
        );
      /* Baseline run (no prev) of the original — its incr_eval becomes
       * the cache handed to the incremental run of the edited exp. */
      switch (try_eval(info_slice_orig, elab_orig)) {
      | None
      | Some(StepLimitExceeded) =>
        no_comparison("the unedited program did not evaluate to a result")
      | Some(LimitedCompleted((_, state_before))) =>
        /* Edited evaluated two ways: incrementally (reusing the baseline's
         * cache) and from scratch (empty prev). These must agree. */
        let fresh = try_eval(info_slice_edit, elab_edit);
        let incr_eval_result =
          try_eval(~prev=state_before.incr_eval, info_slice_edit, elab_edit);
        switch (fresh, incr_eval_result) {
        | (
            Some(LimitedCompleted((e_fresh, _))),
            Some(LimitedCompleted((e_incr, _))),
          ) =>
          Option.iter(compared, ~f=incr);
          check(testable, msg, e_fresh, e_incr);
        | _ =>
          no_comparison(
            "the edited program did not evaluate to a result both ways",
          )
        };
      };
    | _ => no_comparison("the program did not pass statics")
    };
  };
};

let qcheck_incremental_matches_fresh_after_edit =
  QCheck.Test.make(
    ~name="Incremental eval agrees with fresh eval after a literal edit",
    ~count=2000,
    QCheck.pair(
      QCheck.small_nat,
      QCheck_Util.arb_exp_full(~minimal_idents=true, 5),
    ),
    ((seed, exp)) => {
      check_incremental_against_fresh_after_edit(
        ~literal_index=seed,
        ~testable=testable_core_exp,
        ~msg="Incremental eval and fresh eval agree",
        ~require_comparison=false,
        exp,
      );
      true;
    },
  );

/* Recursive lets, which random programs rarely contain, and which is where
   incremental reuse needs the most care: a recursive definition's own name
   is bound by its fixpoint, while every other name it reads must still be
   tracked (an outer variable, an outer binding it shadows, a module's
   recursive member). Each template's literals take distinctive values so
   that each edit lands on the literal meant; every comparison must happen. */
let recursive_templates = [
  (
    "a recursive function",
    "let f = fun n -> if n == 0 then {a} else f(n - 1) + {b} in f({c})",
  ),
  (
    "an outer variable read inside the recursion",
    "let k = {a} in let go = fun i -> if i == 0 then k else go(i - 1) in go({c}) + {b}",
  ),
  (
    "an outer variable read in the base case",
    "let outer = {a} in let go = fun i -> if i == 0 then outer + {b} else go(i - 1) in go({c})",
  ),
  (
    "a recursive name shadowing an outer binding",
    "let go = {a} in let go = fun i -> if i == 0 then {b} else go(i - 1) in go({c})",
  ),
  (
    "a recursive helper inside a recursive function",
    "let f = fun n -> let g = fun m -> if m == 0 then {a} else g(m - 1) + 1 in if n == 0 then g({b}) else f(n - 1) in f({c})",
  ),
  (
    "a module's recursive member",
    "module M = { let sum = fun n -> if n == 0 then {a} else n + sum(n - 1); let k = {b} } in M.sum({c}) + M.k",
  ),
];

let fill = (template: string, a: int, b: int, c: int): string =>
  List.fold_left(
    [("{a}", a), ("{b}", b), ("{c}", c)],
    ~init=template,
    ~f=(s, (hole, v)) =>
    Str.global_replace(Str.regexp_string(hole), string_of_int(v), s)
  );

let incremental_recursive_lets_tests =
  List.map(recursive_templates, ~f=((name, template)) =>
    test_case(
      "Incremental eval agrees with fresh eval after a literal edit: " ++ name,
      `Quick,
      () =>
      List.iter(
        [(17, 23, 3), (17, 5, 4), (41, 23, 4), (41, 5, 3)],
        ~f=((a, b, c)) => {
          let exp = parse_exp(fill(template, a, b, c));
          let lits = collect_int_lits(exp);
          List.iter([a, b, c], ~f=v =>
            switch (
              List.findi(lits, ~f=(_, (_, n)) =>
                Bigint.equal(n, Bigint.of_int(v))
              )
            ) {
            | Some((literal_index, _)) =>
              check_incremental_against_fresh_after_edit(
                ~literal_index,
                ~testable=testable_core_exp,
                ~msg=
                  Printf.sprintf(
                    "%s, editing %d: incremental and fresh agree",
                    fill(template, a, b, c),
                    v,
                  ),
                ~require_comparison=true,
                exp,
              )
            | None =>
              failf("no literal %d in %s", v, fill(template, a, b, c))
            }
          );
        },
      )
    )
  );

/* #2457: editing the discarded literal must not leave `x` free in the stuck
   residual. Fresh and incremental both close it over `x = ()`. */
let incremental_literal_edit_regression_test =
  test_case(
    "Incremental eval agrees with fresh eval on a stuck let after a literal edit (#2457)",
    `Quick,
    () =>
    check_incremental_against_fresh_after_edit(
      ~literal_index=0,
      ~testable=testable_core_exp,
      ~msg="#2457: incremental and fresh disagree on the stuck residual",
      ~require_comparison=true,
      parse_exp("{ let x = (); let _ = 4; let true = A }"),
    )
  );

/* #2462: a function reused from the cache comes back with its closure, so
   the call goes on after a literal edit rather than sticking at
   `(fun x -> ?)(1)`. */
let incremental_reused_function_regression_test =
  test_case(
    "Incremental eval agrees with fresh eval when a function is reused (#2462)",
    `Quick,
    () =>
    check_incremental_against_fresh_after_edit(
      ~literal_index=0,
      ~testable=testable_core_exp,
      ~msg="#2462: incremental and fresh disagree on a reused function",
      ~require_comparison=true,
      parse_exp("(let ? = a in fun x -> ?)(0)"),
    )
  );

/* The gradual guarantee: annotating some of a well-typed program's
   bindings with `?` doesn't change the value it evaluates to. A `?` hides a
   type that statics would have acted on, such as lifting a value into a
   one-field labeled type, so evaluation has to act on it instead. A program
   that doesn't finish as a value is not judged. */
let with_unknown_annotations = (seed: int, e: Exp.t): Exp.t => {
  let st = Stdlib.Random.State.make([|seed|]);
  let loosen = (p: Pat.t): Pat.t =>
    Stdlib.Random.State.bool(st)
      ? Pat.fresh(Asc(p, Typ.fresh(Unknown(Hole(EmptyHole))))) : p;
  Exp.map_term(
    ~f_exp=
      (continue, e) => {
        let e = continue(e);
        switch (e.term) {
        | Let(p, def, body) => {
            ...e,
            term: Let(loosen(p), def, body),
          }
        | Fun(p, body, ty, name) => {
            ...e,
            term: Fun(loosen(p), body, ty, name),
          }
        | _ => e
        };
      },
    e,
  );
};

let gradual_guarantee_holds = ((uexp: Exp.t, seed: int)): bool => {
  let loosened = with_unknown_annotations(seed, uexp);
  let well_typed = e => {
    let (info_map, _) =
      QCheck_Util.skip_alias_loop(() =>
        Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), e)
      );
    List.is_empty(Statics.Map.error_ids(info_map));
  };
  QCheck.assume(well_typed(uexp) && well_typed(loosened));
  let run = (~step_limit, e) =>
    Evaluator.evaluate_and_limit(
      ~env=Builtins.env_init,
      ~step_limit,
      elaborate(e),
    );
  switch (run(~step_limit=1000, uexp)) {
  | LimitedCompleted((value, _)) when ValueChecker.is_value(value) =>
    switch (run(~step_limit=10000, loosened)) {
    | LimitedCompleted((loosened_value, _)) =>
      Alcotest.check(
        pattern_equivalence_testable,
        "Annotated with ?, the program evaluates to the same value",
        value,
        loosened_value,
      );
      true;
    | StepLimitExceeded =>
      QCheck.Test.fail_report("Annotated with ?, the program didn't finish")
    }
  | LimitedCompleted(_)
  | StepLimitExceeded => true
  | exception _ => QCheck.assume_fail()
  };
};

let qcheck_gradual_guarantee =
  QCheck.Test.make(
    ~name="Annotating bindings with ? keeps the value",
    ~count=1000,
    QCheck.pair(
      QCheck_Util.arb_exp_canonical(~minimal_idents=true, 5),
      QCheck.small_nat,
    ),
    gradual_guarantee_holds,
  );

/* Generated programs rarely give a one-field labeled type a value to lift,
   which is where a `?` hides the type statics lifts by. These do: a value
   of each kind meets `(a=…)` as an annotation, a pattern or a cast, and
   the program uses the field (calling it, if it is a function), since a
   record stuck inside a closure only shows when it is used. */
let qcheck_gradual_guarantee_labeled_singletons = {
  let values = [
    ("1", "Int", Fn.id),
    ({|"s"|}, "String", Fn.id),
    ("true", "Bool", Fn.id),
    ("[1]", "[Int]", Fn.id),
    ("(1, 2)", "(Int, Int)", Fn.id),
    ("(b=1)", "(b=Int)", Fn.id),
    ("(a=1)", "Int", Fn.id),
    ("fun z -> z + 1", "Int -> Int", f => f ++ "(1)"),
  ];
  let forms = [
    ((v, t, use)) =>
      "let v = " ++ v ++ " in let r : (a=" ++ t ++ ") = v in " ++ use("r.a"),
    ((v, t, use)) =>
      "let v = "
      ++ v
      ++ " in let r : (a="
      ++ t
      ++ ") = v in case r | (a=g) => "
      ++ use("g")
      ++ " end",
    ((v, _, use)) => "let v = " ++ v ++ " in let (a=x) = v in " ++ use("x"),
    ((v, t, use)) =>
      "let v = " ++ v ++ " in " ++ use("(v : (a=" ++ t ++ ")).a"),
    ((v, _, use)) =>
      "let f = fun y -> let (a=x) = y in " ++ use("x") ++ " in f(" ++ v ++ ")",
    ((v, _, use)) =>
      "let f = fun y -> case y | (a=x) => "
      ++ use("x")
      ++ " end in f("
      ++ v
      ++ ")",
  ];
  QCheck.Test.make(
    ~name="Annotating bindings with ? keeps the value: labeled singletons",
    ~count=500,
    QCheck.make(
      ~print=
        ((src, seed)) => src ++ " (seed " ++ Int.to_string(seed) ++ ")",
      QCheck.Gen.(
        map3(
          (form, value, seed) => (form(value), seed),
          oneofl(forms),
          oneofl(values),
          small_nat,
        )
      ),
    ),
    ((src, seed)) =>
    gradual_guarantee_holds((parse_exp(src), seed))
  );
};

/* Preservation excludes a `rec` type with an unused binder
   (has_rec_with_unused_binder), because of this. */
let preservation_rec_unused_binder_pin =
  test_case(
    "Known bug: `?` is not as precise as `rec x -> ?`",
    `Quick,
    () => {
      let unknown = () => Typ.fresh(Unknown(Internal));
      check(
        bool,
        "? is as precise as rec x -> ?",
        false,
        Typ.is_more_precise(
          Ctx.empty,
          unknown(),
          Typ.fresh(Rec(TPat.fresh(Var("x")), unknown())),
        ),
      );
    },
  );

let rec finish_yielding = (~remaining_slices: int, evaluation) => {
  if (remaining_slices <= 0) {
    fail("Yielding evaluation did not complete");
  };
  switch (Evaluator.run_yielding_slice(~step_budget=1, evaluation)) {
  | EvaluationCompleted(value) => value
  | EvaluationYielded(evaluation) =>
    finish_yielding(~remaining_slices=remaining_slices - 1, evaluation)
  };
};

let rec finish_yielding_with_stream =
        (~remaining_slices: int, ~stream, evaluation) => {
  if (remaining_slices <= 0) {
    fail("Yielding evaluation did not complete");
  };
  switch (Evaluator.run_yielding_slice(~step_budget=1, evaluation)) {
  | EvaluationCompleted(value) =>
    let stream =
      IncrEval.add_stream(
        Evaluator.drain_streaming_outbox(evaluation).completed,
        stream,
      );
    (value, stream);
  | EvaluationYielded(evaluation) =>
    let stream =
      IncrEval.add_stream(
        Evaluator.drain_streaming_outbox(evaluation).completed,
        stream,
      );
    finish_yielding_with_stream(
      ~remaining_slices=remaining_slices - 1,
      ~stream,
      evaluation,
    );
  };
};

let yielding_evaluation_test =
  test_case(
    "Yielding evaluation resumes to the synchronous result",
    `Quick,
    () => {
      let (_, exp) =
        Statics.mk(
          CoreSettings.on,
          Builtins.ctx_init(Some(Int)),
          parse_exp("let x = 1 in let y = 2 in x + y"),
        );
      let (sync_exp, _) = Evaluator.evaluate(~env=Builtins.env_init, exp);
      let evaluation =
        Evaluator.start_yielding_evaluation(~env=Builtins.env_init, exp);
      let evaluation =
        switch (Evaluator.run_yielding_slice(~step_budget=1, evaluation)) {
        | EvaluationYielded(evaluation) => evaluation
        | EvaluationCompleted(_) =>
          fail("Expected yielding evaluation to yield with a one-step budget")
        };
      let (yielded_exp, _) =
        finish_yielding(~remaining_slices=1000, evaluation);
      check(dhexp_typ, "yielding evaluation result", sync_exp, yielded_exp);
    },
  );

let yielding_streaming_outbox_test =
  test_case(
    "Yielding evaluation streams completed incremental entries",
    `Quick,
    () => {
      let (info_map, exp) =
        Statics.mk(
          CoreSettings.on,
          Builtins.ctx_init(Some(Int)),
          parse_exp("let x = 1 in let y = x + 2 in y"),
        );
      let eval_info =
        EvalInfo.of_info_map(
          ~probe_all=CoreSettings.on.probe_all,
          ~targets=Id.Map.empty,
          info_map,
        );
      let evaluation =
        Evaluator.start_yielding_evaluation(
          ~eval_info,
          ~env=Builtins.env_init,
          exp,
        );
      let ((_, final_state), stream) =
        finish_yielding_with_stream(
          ~remaining_slices=1000,
          ~stream=IncrEval.empty,
          evaluation,
        );
      check(
        int,
        "streamed entry count matches final entries",
        Id.Map.cardinal(final_state.incr_eval.entries),
        Id.Map.cardinal(stream.entries),
      );
      check(
        bool,
        "every streamed id appears in final entries",
        true,
        Id.Map.for_all(
          (id, _) => Id.Map.mem(id, final_state.incr_eval.entries),
          stream.entries,
        ),
      );
    },
  );

let rec yield_until_current = (~remaining_slices: int, evaluation) => {
  if (remaining_slices <= 0) {
    fail("Yielding evaluation did not produce a current outbox state");
  };
  switch (Evaluator.run_yielding_slice(~step_budget=1, evaluation)) {
  | EvaluationCompleted(_) =>
    fail("Expected yielding evaluation to yield before completion")
  | EvaluationYielded(evaluation) =>
    let outbox = Evaluator.drain_streaming_outbox(evaluation);
    switch (outbox.current) {
    | Some(_) => outbox
    | None =>
      yield_until_current(~remaining_slices=remaining_slices - 1, evaluation)
    };
  };
};

let yielding_streaming_current_state_test =
  test_case(
    "Yielding evaluation streams current partial state",
    `Quick,
    () => {
      let (info_map, exp) =
        Statics.mk(
          CoreSettings.on,
          Builtins.ctx_init(Some(Int)),
          parse_exp("let x = 1 + 2 in let y = x + 3 in y"),
        );
      let eval_info =
        EvalInfo.of_info_map(
          ~probe_all=CoreSettings.on.probe_all,
          ~targets=Id.Map.empty,
          info_map,
        );
      let evaluation =
        Evaluator.start_yielding_evaluation(
          ~eval_info,
          ~env=Builtins.env_init,
          exp,
        );
      let outbox = yield_until_current(~remaining_slices=1000, evaluation);
      switch (outbox.current) {
      | Some({state, _}) =>
        let collected = StreamCollector.collect_stream_state(outbox, exp);
        check(
          bool,
          "current state has dynamic work",
          true,
          state.step_count > 0,
        );
        check(
          bool,
          "collector includes current state",
          true,
          collected.step_count >= state.step_count,
        );
        check(
          bool,
          "current state does not recursively carry incr_eval",
          true,
          Id.Map.is_empty(state.incr_eval.entries),
        );
      | None => fail("Expected current outbox state")
      };
    },
  );

/* Regression: with probes off, stepped terms share Id.invalid (Exp.temp).
 * If outbox.current is keyed by that id, StreamCollector matches it against
 * its own walk temps and truncates — streamed test counts can go backwards
 * mid-run (e.g. 2 -> 1). current must stay keyed by a real program id. */
let rec check_streaming_tests_monotonic =
        (
          ~remaining_slices: int,
          ~prev_test_count: int,
          ~accumulated: IncrEval.outbox(EvaluatorState.t),
          ~eval_info,
          ~exp,
          evaluation,
        ) =>
  if (remaining_slices <= 0) {
    fail("Yielding evaluation did not complete");
  } else {
    switch (Evaluator.run_yielding_slice(~step_budget=1, evaluation)) {
    | EvaluationCompleted(_) =>
      let accumulated =
        IncrEval.merge_outbox(
          Evaluator.drain_streaming_outbox(evaluation),
          accumulated,
        );
      let collected = StreamCollector.collect_stream_state(accumulated, exp);
      TestMap.count(collected.tests);
    | EvaluationYielded(evaluation) =>
      let update = Evaluator.drain_streaming_outbox(evaluation);
      switch (update.current) {
      | Some({id, _}) =>
        check(
          bool,
          "current is never keyed by Id.invalid",
          false,
          Id.equal(id, Id.invalid),
        );
        check(
          bool,
          "current is keyed by a program id",
          true,
          Option.is_some(EvalInfo.find_opt(id, eval_info)),
        );
      | None => ()
      };
      let accumulated = IncrEval.merge_outbox(update, accumulated);
      let collected = StreamCollector.collect_stream_state(accumulated, exp);
      let test_count = TestMap.count(collected.tests);
      check(
        bool,
        "streamed test count is monotonic",
        true,
        test_count >= prev_test_count,
      );
      check_streaming_tests_monotonic(
        ~remaining_slices=remaining_slices - 1,
        ~prev_test_count=test_count,
        ~accumulated,
        ~eval_info,
        ~exp,
        evaluation,
      );
    };
  };

let yielding_streaming_current_id_invalid_race_test =
  test_case(
    "Stream collector current is not keyed by Id.invalid (probes off)",
    `Quick,
    () => {
      let (info_map, exp) =
        Statics.mk(
          CoreSettings.on,
          Builtins.ctx_init(Some(Int)),
          parse_exp(
            "test 1 == 1 end; test 2 == 2 end; test 3 == 3 end; test 4 == 4 end",
          ),
        );
      let eval_info =
        EvalInfo.of_info_map(
          ~probe_all=false,
          ~targets=Id.Map.empty,
          info_map,
        );
      let evaluation =
        Evaluator.start_yielding_evaluation(
          ~eval_info,
          ~env=Builtins.env_init,
          exp,
        );
      let final_count =
        check_streaming_tests_monotonic(
          ~remaining_slices=5000,
          ~prev_test_count=0,
          ~accumulated=IncrEval.empty_outbox,
          ~eval_info,
          ~exp,
          evaluation,
        );
      check(int, "all tests eventually stream", 4, final_count);
    },
  );

/* Small-step and big-step must agree. Takes a thunk so counterexamples with
   no surface syntax can be built as terms. */
let check_confluence = (~name, mk_uexp) =>
  test_case(
    name,
    `Quick,
    () => {
      let elab = elaborate(mk_uexp());
      switch (
        Evaluator.evaluate_and_limit(
          ~env=Builtins.env_init,
          ~step_limit=1000,
          elab,
        ),
        full_small_step_reduction(~step_limit=1000, elab),
      ) {
      | (LimitedCompleted((big, _)), LimitedCompleted((small, _))) =>
        check(testable_core_exp, "small step and big step agree", small, big)
      | _ =>
        fail("Expected both evaluators to complete within the step limit")
      };
    },
  );

let confluence_regression_test = (~name, src) =>
  check_confluence(~name, () => parse_exp(src));

/* Asserts only that evaluation does not raise. */
let check_evaluates = (~name, mk_uexp) =>
  test_case(
    name,
    `Quick,
    () => {
      let _ =
        Evaluator.evaluate_and_limit(
          ~env=Builtins.env_init,
          ~step_limit=10000,
          elaborate(mk_uexp()),
        );
      ();
    },
  );

/* Big-step alpha-renames the later Ap-pattern binder, small-step does not;
   duplicate binders are last-wins, so the two agree. */
let repro_forall_ap_pat_confluence =
  check_confluence(~name="forall x(x) -> x (duplicate-binder alpha)", () =>
    IdTagged.FreshGrammar.(
      Exp.forall(Pat.ap(Pat.var("x"), Pat.var("x")), Exp.var("x"))
    )
  );

/* Control for the above: a single binder was never in question. */
let repro_forall_simple_confluence =
  check_confluence(~name="forall x -> x", () =>
    IdTagged.FreshGrammar.(Exp.forall(Pat.var("x"), Exp.var("x")))
  );

/* Both evaluators keep the hole around the unit (the printer strips it, so
   it shows as `(())`). */
let repro_unit_parens_confluence =
  check_confluence(~name="DynamicErrorHole around unit", () =>
    IdTagged.FreshGrammar.(
      Exp.dynamic_error_hole(
        Exp.parens(Exp.tuple([])),
        InvalidOperationError.DivideByZero,
      )
    )
  );

/* An `_` label reaches PatternMatch, which has to know it as well as Wild. */
let repro_explicit_nonlabel_eval_crash =
  check_evaluates(~name="ExplicitNonlabel in PatternMatch", () =>
    parse_exp("{ type o^o = [String]; let _ = y; y @< _ > }")
  );

/* Applying a BuiltinFun with an unknown name raised InvalidBuiltin; statics
   now elaborates it to Invalid. No surface syntax reaches this. */
let repro_unknown_builtin_ap =
  check_evaluates(~name="unknown BuiltinFun application", () =>
    IdTagged.FreshGrammar.(
      Exp.ap(Forward, Exp.builtin_fun("x"), Exp.empty_hole())
    )
  );

/* A `Fun` whose annotation slot holds a `Sig` type: only surface ascriptions
   go through desugar_sig, so substitution met a SigLet pattern inside a type.
   Printed as ``fun B -> `a` ``, which hides the annotation. */
let repro_sig_annotated_fun_subst =
  check_evaluates(~name="Sig-annotated Fun substitution", () =>
    IdTagged.FreshGrammar.(
      Exp.fn(
        Pat.var("x"),
        Exp.int(1),
        Some(Language.Typ.fresh(Sig([Sig.sig_let(Pat.var("y"))]))),
        None,
      )
    )
  );

/* ExplicitNonlabel under a module let, through the incremental evaluator. */
let repro_explicit_nonlabel_incremental =
  test_case(
    "ExplicitNonlabel incremental eval",
    `Quick,
    () => {
      let uexp = parse_exp("(0, case ({ let _ = _ }) | _ => 0 end)");
      let (info_map, elab) =
        Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), uexp);
      let eval_info =
        EvalInfo.of_info_map(
          ~probe_all=CoreSettings.on.probe_all,
          ~targets=Id.Map.empty,
          info_map,
        );
      let _ = eval_limited(~eval_info, ~step_limit=10000, elab);
      ();
    },
  );

/* Preservation as the property checks it: one step, the stepped term
   re-checked with no ambient mode, its type at least as precise. */
let one_step_preserves = (src: string) => {
  let uexp = parse_exp(src);
  let (statics, elab) =
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), uexp);
  let orig_ty = elaborated_type(statics, uexp);
  switch (single_step(elab)) {
  /* As in the property: a final term has nothing to preserve. */
  | None => ()
  | Some(next) =>
    let (statics', _) =
      Statics.mk(CoreSettings.on, Builtins.ctx_init(None), next);
    switch (Statics.Map.ty_of(next.annotation.ids |> List.hd_exn, statics')) {
    | None => fail(src ++ ": no type for the stepped term")
    | Some(ty) =>
      check(
        bool,
        src
        ++ ": "
        ++ QCheck_Util.show_core_typ(ty)
        ++ " (after a step) is at least as precise as "
        ++ QCheck_Util.show_core_typ(orig_ty),
        true,
        Typ.is_more_precise(Ctx.empty, ty, orig_ty),
      )
    };
  };
};

let stepper_agrees = (~testable=testable_core_exp, src: string) =>
  check_evaluator_stepper_consistent(
    ~step_limit=1000,
    ~testable,
    ~msg=src ++ ": small step and big step agree",
    ~require_comparison=true,
    parse_exp(src),
  );

let evaluates_to_type = (src: string, expected: Typ.term) => {
  let result = parse_and_evaluate(src);
  let (statics, _) =
    Statics.mk(CoreSettings.on, Builtins.ctx_init(None), result);
  check(
    option(
      testable(
        Fmt.using(QCheck_Util.show_core_typ, Fmt.string),
        Typ.fast_equal,
      ),
    ),
    src ++ ": the value's type",
    Some(Typ.fresh(expected)),
    Statics.Map.ty_of(Exp.rep_id(result), statics),
  );
};

/* The value `src` evaluates to re-checks at a type at least as precise as
   the program's. */
let keeps_its_type = (src: string) => {
  let uexp = parse_exp(src);
  let (statics, elab) =
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), uexp);
  let ty = elaborated_type(statics, uexp);
  let result = evaluate(elab);
  let (statics', _) =
    Statics.mk(CoreSettings.on, Builtins.ctx_init(None), result);
  check(
    bool,
    src ++ ": the value keeps the program's type",
    true,
    switch (Statics.Map.ty_of(Exp.rep_id(result), statics')) {
    | Some(ty') => Typ.is_more_precise(Ctx.empty, ty', ty)
    | None => false
    },
  );
};

let incremental_agrees = (~testable=testable_core_exp, ~literal_index, src) =>
  check_incremental_against_fresh_after_edit(
    ~literal_index,
    ~testable,
    ~msg=src ++ ": incremental and fresh agree",
    ~require_comparison=true,
    parse_exp(src),
  );

/* One example of each cause the evaluator properties have found. */
let found_cause_tests = [
  /* Negation's kind decided by the context, then re-decided by the operand
     once a step exposes it. */
  test_case("Preservation: - undefined:Float:?_?", `Quick, () =>
    one_step_preserves("- undefined:Float:?_?")
  ),
  test_case("Preservation: negating a case that yields a float", `Quick, () =>
    one_step_preserves("- case _ | ? => 1.5| ? => proof_object x end end")
  ),
  /* And the negation chosen first must be the right one. */
  test_case("Negating a float literal is float negation", `Quick, () =>
    check(
      testable_core_exp,
      "-1.5",
      parse_and_evaluate("-1.5"),
      parse_and_evaluate("- 1.5"),
    )
  ),
  test_case("A float-annotated negation is float negation", `Quick, () =>
    check(
      testable_core_exp,
      "-5.",
      parse_and_evaluate("-5."),
      parse_and_evaluate("let x : Float = -5 in x"),
    )
  ),
  /* A `fix` stepping to a bare `_` in an argument slot. */
  test_case("Preservation: ()(_, fix _ -> _, _, `a`)", `Quick, () =>
    one_step_preserves("()(_, fix _ -> _, _, `a`)")
  ),
  /* Stepping substitutes a constructor under a module binding of the same
     name; read back by name, it would mean the module. */
  test_case("Preservation: let y = A in module A = 1 in y", `Quick, () =>
    one_step_preserves("let y = A in module A = 1 in y")
  ),
  test_case("Preservation: { module y = A; module A = 1 }", `Quick, () =>
    one_step_preserves("{ module y = A; module A = 1 }")
  ),
  /* A list carries its element type, so a list left with no elements
     still has the type its program gave it. */
  test_case("[] : [Int] evaluates to a value of type [Int]", `Quick, () =>
    evaluates_to_type("[] : [Int]", List(Atom(Int) |> Typ.fresh))
  ),
  test_case("Consing onto a typed empty list", `Quick, () =>
    parse_and_evaluate_test("[1]", "let xs : [Int] = [] in 1 :: xs")
  ),
  test_case("Matching a typed empty list against []", `Quick, () =>
    parse_and_evaluate_test("1", "case ([] : [Int]) | [] => 1 | _ => 2 end")
  ),
  test_case("Matching a typed empty list against a cons", `Quick, () =>
    parse_and_evaluate_test(
      "2",
      "case ([] : [Int]) | x :: xs => 1 | _ => 2 end",
    )
  ),
  test_case("Concatenating onto a typed empty list", `Quick, () =>
    parse_and_evaluate_test("[1]", "([] : [Int]) @ [1]")
  ),
  test_case("Concatenating two typed empty lists keeps the type", `Quick, () =>
    evaluates_to_type(
      "([] : [Int]) @ ([] : [Int])",
      List(Atom(Int) |> Typ.fresh),
    )
  ),
  test_case("The tail of a one-element list keeps the type", `Quick, () =>
    evaluates_to_type(
      "case [1] | _ :: xs => xs | [] => [] end",
      List(Atom(Int) |> Typ.fresh),
    )
  ),
  test_case("A cast to [?] evaluates to the empty list", `Quick, () =>
    parse_and_evaluate_test("[]", "[] : [?]")
  ),
  test_case("Matching a typed empty list of a recursive list type", `Quick, () =>
    parse_and_evaluate_test(
      "1",
      "type T = rec t -> [t] in let x : T = [] in case x | [] => 1 | _ => 2 end",
    )
  ),
  test_case("Stepper agrees: consing onto a typed empty list", `Quick, () =>
    stepper_agrees("let xs : [Int] = [] in 1 :: xs")
  ),
  test_case("Stepper agrees: a typed empty list", `Quick, () =>
    stepper_agrees("[] : [Int]")
  ),
  test_case("A projection from a typed empty list keeps its type", `Quick, () =>
    evaluates_to_type("([] : [(a=Int)]).a", List(Atom(Int) |> Typ.fresh))
  ),
  test_case("A projection from the tail of a list keeps its type", `Quick, () =>
    evaluates_to_type(
      "(case [(a=1, b=2)] | _ :: xs => xs | [] => [] end).a",
      List(Atom(Int) |> Typ.fresh),
    )
  ),
  /* A saved stepper state holds terms. A list literal saved before list
     literals had a type is its elements alone, `(ListLit (elements…))`. */
  test_case(
    "A list literal saved without its type still loads",
    `Quick,
    () => {
      let hole = Exp.sexp_of_t(Exp.fresh(EmptyHole));
      let loads = (saved: Sexplib.Sexp.t, n: int) =>
        switch (Exp.term_of_sexp(saved)) {
        | ListLit((es, None)) => List.length(es) == n
        | _ => false
        };
      check(
        bool,
        "[]",
        true,
        loads(Sexplib.Sexp.of_string("(ListLit ())"), 0),
      );
      check(
        bool,
        "[?]",
        true,
        loads(List([Atom("ListLit"), List([hole])]), 1),
      );
    },
  ),
  test_case(
    "A typed list survives saving and loading",
    `Quick,
    () => {
      let value = parse_and_evaluate("[] : [Int]");
      check(
        bool,
        "still a list of Int",
        true,
        switch ((value |> Exp.sexp_of_t |> Exp.t_of_sexp).term) {
        | ListLit(([], Some({term: Atom(Int), _}))) => true
        | _ => false
        },
      );
    },
  ),
  test_case("to_lvs of an empty tuple keeps its declared type", `Quick, () =>
    keeps_its_type("to_lvs(())")
  ),
  /* A proof of `[1] == [1]` meets a `proof_of` quoting the user's `[1] == [1]`:
     the proof's lists carry types the quoted ones don't. */
  test_case("A proof matches its proposition up to list types", `Quick, () =>
    parse_and_evaluate_test(
      "true",
      "theorem t = [1] == [1] in (fun (h : proof_of [1] == [1] end) -> true)(t)",
    )
  ),
  /* Through `?`, the proof meets the `proof_of` cast at runtime: its
     proposition's lists carry types the quoted ones don't. */
  test_case(
    "A proof meets its proposition's cast up to list types", `Quick, () =>
    check(
      bool,
      "the cast is gone",
      true,
      switch (
        parse_and_evaluate(
          "theorem t = [1] == [1] in let u : ? = t in (fun (p : proof_of [1] == [1] end) -> p)(u)",
        ).
          term
      ) {
      | ProofObject(_) => true
      | _ => false
      },
    )
  ),
  /* The tail of `[1]` carries `Int`; a `[]` checked against `[?]` does not.
     They are the same term, as induction joining the cases' results needs. */
  test_case(
    "Equal lists reached along different paths are the same term",
    `Quick,
    () => {
      let tail =
        parse_and_evaluate("case [1] | _ :: xs => xs | [] => [] end");
      let written = parse_and_evaluate("[] : [?]");
      check(
        bool,
        "same term",
        true,
        Exp.fast_equal_ignoring_list_types(tail, written),
      );
      check(
        bool,
        "but a changed elaboration",
        false,
        Exp.fast_equal(tail, written),
      );
    },
  ),
  /* Elaboration puts types where the runtime reads them without a context,
     so it expands every user alias: `[v]` checked against `[T]` carries
     and casts to `+ A(Int) + B`, not `T`. */
  test_case(
    "An elaborated list carries no user alias",
    `Quick,
    () => {
      let e =
        elaborate(
          parse_exp(
            "type T = + A(Int) + B in let v : ? = A(3) in let xs : [T] = [v] in xs",
          ),
        );
      let found = ref([]);
      let _ =
        Exp.map_term(
          ~f_typ=
            (cont, t: Typ.t) => {
              switch (t.term) {
              | Var("T") => found := ["T", ...found^]
              | _ => ()
              };
              cont(t);
            },
          e,
        );
      check(list(string), "user aliases in the elaboration", [], found^);
    },
  ),
  test_case("An empty range keeps its declared type", `Quick, () =>
    evaluates_to_type("range(1, 0)", List(Atom(Int) |> Typ.fresh))
  ),
  test_case("Stepper agrees: a projection from a typed empty list", `Quick, () =>
    stepper_agrees("([] : [(a=Int, b=Bool)]).a")
  ),
  /* A cast pushed into a cons tail's `let`: passes since the survey's fixes
     (seed 36); kept as a regression test. */
  test_case("Incremental: a cast pushed into a let in a cons tail", `Quick, () =>
    incremental_agrees(
      ~literal_index=0,
      "(0, proof_object 0 end:: (let `a` = y in ?))",
    )
  ),
  /* An edit that changes only the cast's type must not get the old cast
     back from the cache. */
  test_case("Incremental: a cast pushed into a module-let", `Quick, () =>
    incremental_agrees(
      ~literal_index=0,
      "(0, (module ? = _ in ()):proof_of 0 end)",
    )
  ),
  /* A cast to a `rec` type keeps the `rec` whether it reaches the value
     through a closure (stepper) or after evaluation (evaluator). */
  test_case("Stepper and evaluator agree on a cast to a rec type", `Quick, () =>
    stepper_agrees(
      "(hide undefined in if ? then _ else _):(rec ?_? -> (x=?))",
    )
  ),
];

/* A list keeps its type through every step, even once it has no elements
   left to show it. The generator writes programs that build lists and take
   them apart: literals, casts, cons, concatenation, matching a cons and
   keeping the tail, projecting a field from a list of records, and `range`. */
module ListTypes = {
  let gen_digit = QCheck.Gen.map(string_of_int, QCheck.Gen.int_bound(9));

  let rec gen_ints = (depth: int): QCheck.Gen.t(string) => {
    let leaf =
      QCheck.Gen.oneof([
        QCheck.Gen.return("[]"),
        QCheck.Gen.return("([] : [Int])"),
        QCheck.Gen.map(n => "[" ++ n ++ "]", gen_digit),
        QCheck.Gen.map2(
          (a, b) => "[" ++ a ++ ", " ++ b ++ "]",
          gen_digit,
          gen_digit,
        ),
        QCheck.Gen.map2(
          (a, b) => "range(" ++ a ++ ", " ++ b ++ ")",
          gen_digit,
          gen_digit,
        ),
      ]);
    if (depth <= 0) {
      leaf;
    } else {
      let sub = gen_ints(depth - 1);
      QCheck.Gen.frequency([
        (2, leaf),
        (
          1,
          QCheck.Gen.map2(
            (n, l) => "(" ++ n ++ " :: " ++ l ++ ")",
            gen_digit,
            sub,
          ),
        ),
        (
          1,
          QCheck.Gen.map2((a, b) => "(" ++ a ++ " @ " ++ b ++ ")", sub, sub),
        ),
        (1, QCheck.Gen.map(l => "(" ++ l ++ " : [Int])", sub)),
        (
          2,
          QCheck.Gen.map(
            l => "(case " ++ l ++ " | x :: xs => xs | [] => [] end)",
            sub,
          ),
        ),
        (
          1,
          QCheck.Gen.map(
            l =>
              "(let tl : [Int] -> [Int] = fun l -> case l | _ :: rest => rest | [] => [] end in tl("
              ++ l
              ++ "))",
            sub,
          ),
        ),
        (1, QCheck.Gen.map(l => "(" ++ l ++ ").a", gen_records(depth - 1))),
      ]);
    };
  }
  and gen_records = (depth: int): QCheck.Gen.t(string) => {
    let record =
      QCheck.Gen.map2(
        (a, b) => "(a=" ++ a ++ ", b=" ++ b ++ ")",
        gen_digit,
        gen_digit,
      );
    let leaf =
      QCheck.Gen.oneof([
        QCheck.Gen.return("([] : [(a=Int, b=Int)])"),
        QCheck.Gen.map(r => "[" ++ r ++ "]", record),
      ]);
    if (depth <= 0) {
      leaf;
    } else {
      let sub = gen_records(depth - 1);
      QCheck.Gen.frequency([
        (2, leaf),
        (
          1,
          QCheck.Gen.map2((a, b) => "(" ++ a ++ " @ " ++ b ++ ")", sub, sub),
        ),
        (
          1,
          QCheck.Gen.map(
            l => "(case " ++ l ++ " | x :: xs => xs | [] => [] end)",
            sub,
          ),
        ),
      ]);
    };
  };

  /* Each step's type is at least as precise as the program's. */
  let unpreserved = (src: string): option(string) => {
    let uexp = parse_exp(src);
    let (statics, elab) =
      Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), uexp);
    if (!List.is_empty(Statics.Map.error_ids(statics))) {
      None;
    } else {
      let orig_ty = elaborated_type(statics, uexp);
      let rec go = (steps, e) =>
        if (steps > 200) {
          None;
        } else {
          switch (single_step(e)) {
          | None => None
          | Some(next) =>
            let (statics', _) =
              Statics.mk(CoreSettings.on, Builtins.ctx_init(None), next);
            switch (Statics.Map.ty_of(Exp.rep_id(next), statics')) {
            | Some(ty) when Typ.is_more_precise(Ctx.empty, ty, orig_ty) =>
              go(steps + 1, next)
            | ty =>
              Some(
                Printf.sprintf(
                  "step %d: %s : %s, from %s",
                  steps + 1,
                  QCheck_Util.show_core_exp(next),
                  Option.value_map(
                    ~default="none",
                    ~f=QCheck_Util.show_core_typ,
                    ty,
                  ),
                  QCheck_Util.show_core_typ(orig_ty),
                ),
              )
            };
          };
        };
      go(0, elab);
    };
  };

  let property =
    QCheck.Test.make(
      ~name="A list keeps its type through every step",
      ~count=500,
      QCheck.make(~print=s => s, gen_ints(3)),
      src =>
      switch (unpreserved(src)) {
      | None => true
      | Some(msg) => QCheck.Test.fail_report(msg)
      }
    );
};

let tests = (
  "Evaluator.Properties",
  [
    repro_forall_ap_pat_confluence,
    repro_forall_simple_confluence,
    /* An indet Theorem/Forall under a fixpoint must keep its closure: in
       Environment mode the fix unrolls into a Closure, and if the Indet
       branch does not ask to keep it, big-step leaves `x` free while
       small-step substitutes. */
    confluence_regression_test(
      ~name="Confluence: indet theorem under a fixpoint keeps its closure",
      "fix x -> (theorem ? = y in x)",
    ),
    confluence_regression_test(
      ~name="Confluence: indet forall under a fixpoint keeps its closure",
      "fix x -> (forall y -> x)",
    ),
    /* #2128: duplicate binders are last-wins in both evaluators. */
    confluence_regression_test(
      ~name="Confluence: duplicate binder (#2128)",
      "fun (x::x) -> x",
    ),
    /* A cast pushed into a stuck case's branch composes with the cast the
       branch already has, whichever arrives first. */
    confluence_regression_test(
      ~name="Confluence: two casts pushed into a stuck case",
      "(fix _ -> case () | `a` => _ end):`a`",
    ),
    /* Statics accepts a parenthesized binder inside `(_=...)`, and
       elaboration turns it into `Tuple([Parens(x)])`: evaluation must bind
       `x` too. */
    test_case(
      "theorem (_=(x)) binds x",
      `Quick,
      () => {
        let (result, _) =
          Evaluator.evaluate(
            ~env=Builtins.env_init,
            elaborate(parse_exp("theorem (_=(x)) = true in x")),
          );
        switch (result.term) {
        | ProofObject(_) => ()
        | _ => fail("expected a proof, got " ++ Exp.show(result))
        };
      },
    ),
    repro_unit_parens_confluence,
    repro_explicit_nonlabel_eval_crash,
    repro_explicit_nonlabel_incremental,
    repro_unknown_builtin_ap,
    repro_sig_annotated_fun_subst,
    yielding_evaluation_test,
    yielding_streaming_outbox_test,
    yielding_streaming_current_state_test,
    yielding_streaming_current_id_invalid_race_test,
    QCheck_alcotest.to_alcotest(qcheck_evaluator_does_not_crash_test),
    QCheck_alcotest.to_alcotest(qcheck_stepper_confluence),
    QCheck_alcotest.to_alcotest(qcheck_pattern_equivalence_test),
    QCheck_alcotest.to_alcotest(qcheck_gradual_guarantee),
    QCheck_alcotest.to_alcotest(qcheck_gradual_guarantee_labeled_singletons),
    QCheck_alcotest.to_alcotest(qcheck_incremental_matches_fresh_after_edit),
    incremental_literal_edit_regression_test,
    QCheck_alcotest.to_alcotest(qcheck_preservation_test),
    QCheck_alcotest.to_alcotest(ListTypes.property),
    incremental_reused_function_regression_test,
    preservation_rec_unused_binder_pin,
  ]
  @ incremental_recursive_lets_tests
  @ found_cause_tests,
);

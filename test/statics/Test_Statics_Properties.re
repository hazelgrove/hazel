open Language;
open Test_Statics_Prelude;

let is_known_statics_failure = msg =>
  List.exists(
    (==)(msg),
    [
      "normalize exceeded 1000 recursive calls",
      "weak_head_normalize exceeded 1000 recursive calls",
      "Recursion limit exceeded in all_ctrs_of_typ",
    ],
  )
  || String.starts_with(
       ~prefix="all_ctrs_of_type called with a non-normalized type:",
       msg,
     );

let qcheck_statics_does_not_crash =
  QCheck.Test.make(
    ~name="Statics does not crash",
    ~count=1000,
    QCheck_Util.arb_exp_full(~minimal_idents=true, 5),
    exp => {
    switch (statics(exp)) {
    | _m => true
    | exception Stack_overflow => true // TODO https://github.com/hazelgrove/hazel/issues/1622
    | exception (Failure(f) as e) =>
      if (is_known_statics_failure(f)) {
        true;
      } else {
        raise(e);
      }
    }
  });

let safe_statics = (~ctx=?, exp) => {
  let ctx =
    switch (ctx) {
    | Some(c) => c
    | None => Builtins.ctx_init(Some(Int))
    };
  switch (Statics.mk(CoreSettings.on, ctx, exp)) {
  | result => `Ok(result)
  | exception Stack_overflow => `Skip
  | exception (Failure(f) as e) =>
    if (is_known_statics_failure(f)) {
      `Skip;
    } else {
      raise(e);
    }
  };
};

/* Assert that `violations` finds nothing in the info map `exp` produces.
   Programs statics cannot process are skipped, as in `safe_statics`. */
let check_no_violations =
    (
      ~violations: (Statics.Map.t, Language.Exp.t) => list('a),
      ~show: 'a => string,
      ~msg: string,
      exp: Language.Exp.t,
    )
    : unit =>
  switch (safe_statics(exp)) {
  | `Skip => ()
  | `Ok(info_map, _elab) =>
    Alcotest.(
      check(
        list(string),
        msg,
        [],
        List.map(show, violations(info_map, exp)),
      )
    )
  };

let elab_type_of =
    (info_map: Statics.Map.t, exp: Language.Exp.t): option(Typ.t) =>
  switch (Statics.Map.lookup_exp(Exp.rep_id(exp), info_map)) {
  | Some({ana, ty, ctx, _}) =>
    Some(
      Typ.match_synswitch(ana, ty) |> Typ.normalize(ctx) |> Typ.all_ids_temp,
    )
  | None => None
  };

/* Property: running statics on the elaborated expression yields the same type
   as the original expression. */
let check_elaboration_preserves_type = (exp: Language.Exp.t): unit =>
  switch (safe_statics(exp)) {
  | `Skip => ()
  | `Ok(m_user, elab) =>
    switch (elab_type_of(m_user, exp)) {
    | None => ()
    | Some(user_ty) =>
      /* Re-check with `use_mode = None`, as `CachedStatics` does for
         already-elaborated terms: `Operators.replace_*` re-specialize
         literals to the ambient mode, so a `Some(Int)` pass rewrites
         `use SInt in -?` back to an `Int` minus. */
      switch (safe_statics(~ctx=Builtins.ctx_init(None), elab)) {
      | `Skip => ()
      | `Ok(m_elab, _) =>
        switch (elab_type_of(m_elab, elab)) {
        | None => ()
        | Some(elab_ty) =>
          Alcotest.check(
            testable_typ,
            "the elaborated term synthesizes the user term's type",
            user_ty,
            elab_ty,
          )
        }
      }
    }
  };

let qcheck_elaboration_preserves_type =
  QCheck.Test.make(
    ~name="Elaboration preserves type",
    ~count=1000,
    QCheck_Util.arb_exp_full(~minimal_idents=true, 5),
    exp => {
      check_elaboration_preserves_type(exp);
      true;
    },
  );

/* Property: for each user sub-expression, the parent-recorded `elab_syn_ty`
   agrees with the `elab_syn_ty` a fresh statics run produces for that sub's
   `elab_term`. Comparing against the elaborated sub rather than re-analyzing
   the user sub means the ana-driven rewrites already baked into `elab_term` --
   number-literal replacement, constructor ADT resolution, label inference --
   agree on both sides.

   Bare labels and bare ExplicitNonlabels are skipped: neither has a type
   outside its enclosing product type, so they always "differ" for reasons
   that say nothing about elaboration. MultiHoles are skipped too (see
   is_multihole). */
type syn_disagreement = {
  sub: Language.Exp.t,
  parent_ty: Typ.t,
  fresh_ty: Typ.t,
};

let show_syn_disagreement = ({sub, parent_ty, fresh_ty}: syn_disagreement) =>
  Printf.sprintf(
    "%s: parent %s, fresh %s",
    QCheck_Util.show_core_exp(sub),
    QCheck_Util.show_core_typ(parent_ty),
    QCheck_Util.show_core_typ(fresh_ty),
  );

let syn_and_elab_of =
    (info_map: Statics.Map.t, exp: Language.Exp.t)
    : option((Typ.t, Ctx.t, Language.Exp.t)) =>
  switch (Statics.Map.lookup_exp(Exp.rep_id(exp), info_map)) {
  | Some({elab_syn_ty, ctx, elab_term, _}) =>
    Some((elab_syn_ty, ctx, elab_term))
  | None => None
  };

let is_bare_label = (e: Language.Exp.t): bool =>
  switch (Exp.term_of(e)) {
  | Label(_)
  | ExplicitNonlabel => true
  | _ => false
  };

/* A MultiHole is broken code: statics records it as `?` on purpose, but
   elaborates it to its expression children in sequence so they still run,
   and that sequence re-checks as the last child's type (`y.(74)`'s broken
   field elaborates to `74`). The property is about well-formed terms. */
let is_multihole = (e: Language.Exp.t): bool =>
  switch (Exp.term_of(e)) {
  | MultiHole(_) => true
  | _ => false
  };

let differing_subs =
    (info_map: Statics.Map.t, exp: Language.Exp.t): list(syn_disagreement) => {
  let results = ref([]);
  let _ =
    Exp.map_term(
      ~f_exp=
        (cont, sub) => {
          if (!is_bare_label(sub) && !is_multihole(sub)) {
            switch (syn_and_elab_of(info_map, sub)) {
            | None => ()
            | Some((recorded_ty, ctx, elab_term)) =>
              let parent_ty =
                recorded_ty |> Typ.normalize(ctx) |> Typ.all_ids_temp;
              /* elab_term is already elaborated: no ambient use mode, or
                 a Float negation re-specializes to Int. */
              switch (
                safe_statics(~ctx=Ctx.set_use_mode(ctx, None), elab_term)
              ) {
              | `Skip => ()
              | `Ok(m_elab, _) =>
                switch (
                  Statics.Map.lookup_exp(Exp.rep_id(elab_term), m_elab)
                ) {
                | None => ()
                | Some({elab_syn_ty, ctx: elab_ctx, _}) =>
                  let fresh_ty =
                    elab_syn_ty |> Typ.normalize(elab_ctx) |> Typ.all_ids_temp;
                  if (!Typ.fast_equal(parent_ty, fresh_ty)) {
                    let disagreement = {
                      sub,
                      parent_ty,
                      fresh_ty,
                    };
                    results := [disagreement, ...results^];
                  };
                }
              };
            };
          };
          cont(sub);
        },
      exp,
    );
  results^;
};

/* Carved out (wontfix here; modules are being reworked, see #2717): a
   `module x = def in body` whose definition mentions `x`. The binding is
   checked non-recursively but elaborates to a plain `let`, which a fresh run
   checks as recursive, so a `proof_of` quoting it differs between the two.
   Over-approximates "mentions" as any `Var(x)` in `def`. */
let has_self_referencing_module_binding = (exp: Language.Exp.t): bool => {
  let found = ref(false);
  let mentions = (names, def) => {
    let hit = ref(false);
    let _ =
      TermBase.Exp.map_term(
        ~f_exp=
          (continue, e: Language.Exp.t) => {
            switch (e.term) {
            | Var(v) when List.mem(v, names) => hit := true
            | _ => ()
            };
            continue(e);
          },
        def,
      );
    hit^;
  };
  let _ =
    TermBase.Exp.map_term(
      ~f_exp=
        (continue, e: Language.Exp.t) => {
          switch (e.term) {
          | ModuleExp(mp, def, _)
              when mentions(ModuleHelpers.mpat_names(mp), def) =>
            found := true
          | _ => ()
          };
          continue(e);
        },
      exp,
    );
  found^;
};

let qcheck_subexp_synthesis_agrees =
  QCheck.Test.make(
    ~name="Sub-expression synthesis agrees with a fresh run",
    ~count=1000,
    QCheck_Util.arb_exp_full(~minimal_idents=true, 5),
    exp => {
      /* Judge terms a user can write. The generator also emits `SInt`
         literals outside a `use SInt`, which no parse returns: under Int
         analysis `- 81` (SInt 81) records Int while a fresh run, peeking the
         operand, picks SInt negation. */
      let exp = Canonicalize.exp(exp);
      QCheck.assume(!has_self_referencing_module_binding(exp));
      check_no_violations(
        ~violations=differing_subs,
        ~show=show_syn_disagreement,
        ~msg=
          "every sub-expression's recorded elab_syn_ty agrees with a fresh synthesis",
        exp,
      );
      true;
    },
  );

/* Property: ids are persistent. Running statics twice on the same term
   records an entry at the same ids, each with an `elab_term` rooted at the
   same id.

   What this catches: an elaboration step minting a fresh id on every run
   (`Exp.fresh`), rather than reusing a source id or deriving one from it.
   The incremental evaluator keys its cache by these ids, so a subtree
   rooted at a fresh id can never be reused and silently becomes
   un-incrementalizable (the `FixF` `fresh_deterministic` fix was the first
   instance). Module lowering's export record is the case this used to fail
   on. */
type elab_id_drift = {
  info_id: Id.t,
  first: option(Id.t),
  second: option(Id.t),
};

let show_elab_id_drift = ({info_id, first, second}: elab_id_drift) => {
  let show = Option.fold(~none="no entry", ~some=Id.to_string);
  Printf.sprintf(
    "info %s: elab_term rooted at %s, then at %s",
    Id.to_string(info_id),
    show(first),
    show(second),
  );
};

let elab_roots = (info_map: Statics.Map.t): Id.Map.t(Id.t) =>
  Id.Map.fold(
    (info_id, info, acc) =>
      switch (info) {
      | Info.InfoExp({elab_term, _}) =>
        Id.Map.add(info_id, Exp.rep_id(elab_term), acc)
      | _ => acc
      },
    info_map,
    Id.Map.empty,
  );

/* `Statics.mk` is memoized, so a second call would just return the first
   run's map: run the uncached analysis twice instead. */
let uncached_info_map = (exp: Language.Exp.t): Statics.Map.t => {
  let (_, _, m) =
    Statics.uexp_to_info_map(
      ~ctx=Builtins.ctx_init(Some(Int)),
      ~ancestors=[],
      exp,
      Id.Map.empty,
    );
  m;
};

let elab_ids_drifting =
    (_: Statics.Map.t, exp: Language.Exp.t): list(elab_id_drift) =>
  Id.Map.merge(
    (info_id, first, second) =>
      first == second
        ? None
        : Some({
            info_id,
            first,
            second,
          }),
    elab_roots(uncached_info_map(exp)),
    elab_roots(uncached_info_map(exp)),
  )
  |> Id.Map.bindings
  |> List.map(snd);

let qcheck_elab_ids_persist =
  QCheck.Test.make(
    ~name="Elaboration ids are the same on every run",
    ~count=1000,
    QCheck_Util.arb_exp_full(~minimal_idents=true, 5),
    exp => {
      check_no_violations(
        ~violations=elab_ids_drifting,
        ~show=show_elab_id_drift,
        ~msg="every entry's elab_term is rooted at the same id on both runs",
        exp,
      );
      true;
    },
  );

let parse = (src: string): Language.Exp.t =>
  switch (Haz3lcore.Parser.to_term(src, ~root=Exp)) {
  | Some(e) => e
  | None => Alcotest.fail("Failed to parse expression: " ++ src)
  };

/* Long-mode counterexamples, pinned. */
let regression_tests = [
  /* A module item punning on its value's only label (`let y = (y=1)` binds
     `y : Int`) is typed the way its lowered `let` is: (y=Int). */
  Alcotest.test_case(
    "Elaboration preserves type: { module y = (y=1) }", `Quick, () =>
    check_elaboration_preserves_type(parse("{ module y = (y=1) }"))
  ),
  /* `theorem (_=x)` binds `x` to the proof, as `theorem x` does, both before
     and after elaboration (which turns the binder into a one-tuple). */
  Alcotest.test_case(
    "Elaboration preserves type: theorem (_=x) = true in x", `Quick, () =>
    check_elaboration_preserves_type(parse("theorem (_=x) = true in x"))
  ),
  /* `proof_of` quotes the elaborated program, so re-elaborating a recursive
     `let`'s fixpoint must leave it unchanged. */
  Alcotest.test_case(
    "Sub-expression synthesis: proof_object of a recursive let", `Quick, () =>
    check_no_violations(
      ~violations=differing_subs,
      ~show=show_syn_disagreement,
      ~msg=
        "every sub-expression's recorded elab_syn_ty agrees with a fresh synthesis",
      parse("proof_object let x = fun y -> x in undefined end"),
    )
  ),
];

/* The carved-out class, pinned (skipped until module bindings
   elaborate to something that re-checks as non-recursive). */
let self_referencing_module_binding_pin =
  Alcotest.test_case(
    "Sub-expression synthesis: proof_object of a self-referencing module binding (wontfix, carved out)",
    `Quick,
    () => {
    [@warning "-21"]
    {
      Alcotest.skip();
      check_no_violations(
        ~violations=differing_subs,
        ~show=show_syn_disagreement,
        ~msg=
          "every sub-expression's recorded elab_syn_ty agrees with a fresh synthesis",
        parse("proof_object module x = typfun y -> x in undefined end"),
      );
    }
  });

let tests = (
  "Statics.Properties",
  [
    QCheck_alcotest.to_alcotest(qcheck_statics_does_not_crash),
    QCheck_alcotest.to_alcotest(qcheck_elaboration_preserves_type),
    QCheck_alcotest.to_alcotest(qcheck_subexp_synthesis_agrees),
    QCheck_alcotest.to_alcotest(qcheck_elab_ids_persist),
  ]
  @ regression_tests
  @ [self_referencing_module_binding_pin],
);

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
    ~count=10000,
    QCheck_Util.arb_exp(~minimal_idents=true, 50),
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

/* Property: running statics on the elaborated expression yields the same
   type as the original expression. Rather than failing on counterexamples,
   this test counts how often the property holds across many random inputs
   and prints the distribution so we can see the pass rate. */
let elab_type_of =
    (info_map: Statics.Map.t, exp: Language.Exp.t): option(Typ.t) =>
  switch (Statics.Map.lookup_exp(Exp.rep_id(exp), info_map)) {
  | Some({ana, ty, ctx, _}) =>
    Some(
      Typ.match_synswitch(ana, ty) |> Typ.normalize(ctx) |> Typ.all_ids_temp,
    )
  | None => None
  };

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

type outcome =
  | Holds
  | Differs(Typ.t, Typ.t)
  | SkipOriginal
  | SkipElab
  | NoTypeOriginal
  | NoTypeElab;

let check_elaboration_preserves_type = (exp: Language.Exp.t): outcome =>
  switch (safe_statics(exp)) {
  | `Skip => SkipOriginal
  | `Ok(m1, elab) =>
    switch (elab_type_of(m1, exp)) {
    | None => NoTypeOriginal
    | Some(ty1) =>
      switch (safe_statics(elab)) {
      | `Skip => SkipElab
      | `Ok(m2, _) =>
        switch (elab_type_of(m2, elab)) {
        | None => NoTypeElab
        | Some(ty2) => Typ.fast_equal(ty1, ty2) ? Holds : Differs(ty1, ty2)
        }
      }
    }
  };

let is_differing = exp =>
  switch (check_elaboration_preserves_type(exp)) {
  | Differs(_, _) => true
  | _ => false
  };

/* Iteratively shrink a failing expression by repeatedly asking the
   arbitrary's shrinker for smaller candidates and keeping the first
   one that still fails the property. Capped to avoid long runs. */
let shrink_failing =
    (shrink: QCheck.Shrink.t(Language.Exp.t), exp: Language.Exp.t)
    : Language.Exp.t => {
  let max_iterations = 500;
  let current = ref(exp);
  let iterations = ref(0);
  let progress = ref(true);
  while (progress^ && iterations^ < max_iterations) {
    progress := false;
    let found = ref(None);
    try(
      shrink(current^, candidate =>
        switch (found^) {
        | Some(_) => ()
        | None =>
          incr(iterations);
          if (iterations^ >= max_iterations) {
            raise(Exit);
          };
          if (is_differing(candidate)) {
            found := Some(candidate);
            raise(Exit);
          };
        }
      )
    ) {
    | Exit => ()
    };
    switch (found^) {
    | Some(smaller) =>
      current := smaller;
      progress := true;
    | None => ()
    };
  };
  current^;
};

let show_exp = exp =>
  exp
  |> Haz3lcore.ExpToSegment.exp_to_segment(
       ~settings=Haz3lcore.ExpToSegment.Settings.editable(~inline=true),
       _,
     )
  |> Haz3lcore.Printer.of_segment(~holes="?", _);

let show_typ = typ =>
  typ
  |> Haz3lcore.ExpToSegment.typ_to_segment(
       ~settings=
         Haz3lcore.ExpToSegment.Settings.of_core(
           ~inline=true,
           CoreSettings.off,
         ),
       _,
     )
  |> Haz3lcore.Printer.of_segment(~holes="?", _);

let qcheck_elaboration_preserves_type_stats = () => {
  let total = ref(0);
  let holds = ref(0);
  let differs = ref(0);
  let skip_original = ref(0);
  let skip_elab = ref(0);
  let no_type_original = ref(0);
  let no_type_elab = ref(0);
  let count = 1000;
  let arb = QCheck_Util.arb_exp(~minimal_idents=true, 50);
  let gen = arb.QCheck.gen;
  let shrink =
    switch (arb.QCheck.shrink) {
    | Some(s) => s
    | None => ((_, _) => ())
    };
  let rand = Random.State.make([|0xC0FFEE|]);
  let sample_differs = ref([]);
  let sample_limit = 20;
  for (_ in 1 to count) {
    incr(total);
    let exp = QCheck.Gen.generate1(~rand, gen);
    switch (check_elaboration_preserves_type(exp)) {
    | Holds => incr(holds)
    | Differs(_, _) =>
      incr(differs);
      if (List.length(sample_differs^) < sample_limit) {
        let shrunk = shrink_failing(shrink, exp);
        switch (check_elaboration_preserves_type(shrunk)) {
        | Differs(ty1', ty2') =>
          sample_differs := [(shrunk, ty1', ty2'), ...sample_differs^]
        | _ => ()
        };
      };
    | SkipOriginal => incr(skip_original)
    | SkipElab => incr(skip_elab)
    | NoTypeOriginal => incr(no_type_original)
    | NoTypeElab => incr(no_type_elab)
    };
  };
  let pct = n => 100. *. float_of_int(n) /. float_of_int(total^);
  Printf.printf(
    "\n[elaboration preserves type] out of %d cases:\n"
    ^^ "  holds:              %4d (%.1f%%)\n"
    ^^ "  differs:            %4d (%.1f%%)\n"
    ^^ "  skipped (original): %4d (%.1f%%)\n"
    ^^ "  skipped (elab):     %4d (%.1f%%)\n"
    ^^ "  no type (original): %4d (%.1f%%)\n"
    ^^ "  no type (elab):     %4d (%.1f%%)\n",
    total^,
    holds^,
    pct(holds^),
    differs^,
    pct(differs^),
    skip_original^,
    pct(skip_original^),
    skip_elab^,
    pct(skip_elab^),
    no_type_original^,
    pct(no_type_original^),
    no_type_elab^,
    pct(no_type_elab^),
  );
  List.iter(
    ((exp, ty1, ty2)) =>
      Printf.printf(
        "  sample differing case:\n    exp:      %s\n    ty(orig): %s\n    ty(elab): %s\n",
        show_exp(exp),
        show_typ(ty1),
        show_typ(ty2),
      ),
    List.rev(sample_differs^),
  );
};

/* Property: for every sub-expression `sub` of the user expression, the
   parent-recorded `elab_syn_ty` for `sub` should agree with the
   `elab_syn_ty` that a fresh statics run produces for `sub`'s elaborated
   form. Concretely, for each user sub-expression:
     1. look up its info in the parent's info map to get its `elab_term`
        and the parent-recorded `elab_syn_ty`;
     2. run statics fresh (ana = syn) on `elab_term`;
     3. compare the parent's `elab_syn_ty` to the root-of-elab's
        `elab_syn_ty` in the fresh map.
   Unlike the previous form (which re-analyzed the user sub-expression),
   this compares against the elaborated sub — so ana-driven rewrites
   like number-literal replacement, constructor ADT resolution, and
   label inference that have been baked into `elab_term` will agree on
   both sides.

   Bare labels (Label(_)) are intentionally skipped: a bare label has no
   type outside its enclosing product type. */
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
  | Label(_) => true
  | _ => false
  };

/* Collect every (sub, ty_parent, ty_elab_sub) triple in `exp` where the
   parent's recorded elab_syn_ty for the user sub-expression differs
   from the elab_syn_ty synthesized on the sub's elaborated form. */
let differing_subs =
    (info_map: Statics.Map.t, exp: Language.Exp.t)
    : list((Language.Exp.t, Typ.t, Typ.t)) => {
  let results = ref([]);
  let _ =
    Exp.map_term(
      ~f_exp=
        (cont, e) => {
          if (!is_bare_label(e)) {
            switch (syn_and_elab_of(info_map, e)) {
            | None => ()
            | Some((ty_parent_raw, ctx, elab_term)) =>
              let ty_parent =
                ty_parent_raw |> Typ.normalize(ctx) |> Typ.all_ids_temp;
              switch (safe_statics(~ctx, elab_term)) {
              | `Skip => ()
              | `Ok(m_elab, _) =>
                switch (
                  Statics.Map.lookup_exp(Exp.rep_id(elab_term), m_elab)
                ) {
                | None => ()
                | Some({elab_syn_ty, ctx: ctx', _}) =>
                  let ty_elab =
                    elab_syn_ty |> Typ.normalize(ctx') |> Typ.all_ids_temp;
                  if (!Typ.fast_equal(ty_parent, ty_elab)) {
                    results := [(e, ty_parent, ty_elab), ...results^];
                  };
                }
              };
            };
          };
          cont(e);
        },
      exp,
    );
  results^;
};

type sub_outcome =
  | SubHolds
  | SubDiffers(Language.Exp.t, Typ.t, Typ.t)
  | SubSkipParent;

let check_subexp_synthesis_agrees = (exp: Language.Exp.t): sub_outcome =>
  switch (safe_statics(exp)) {
  | `Skip => SubSkipParent
  | `Ok(m_parent, _elab) =>
    switch (differing_subs(m_parent, exp)) {
    | [] => SubHolds
    | subs =>
      /* Pick the textually smallest differing sub to report. */
      let (sub, ty1, ty2) =
        List.fold_left(
          (best, cur) => {
            let (_, _, _) = best;
            let (s_best, _, _) = best;
            let (s_cur, _, _) = cur;
            String.length(show_exp(s_cur))
            < String.length(show_exp(s_best))
              ? cur : best;
          },
          List.hd(subs),
          List.tl(subs),
        );
      SubDiffers(sub, ty1, ty2);
    }
  };

let sub_is_differing = exp =>
  switch (check_subexp_synthesis_agrees(exp)) {
  | SubDiffers(_, _, _) => true
  | _ => false
  };

let shrink_failing_pred =
    (
      pred: Language.Exp.t => bool,
      shrink: QCheck.Shrink.t(Language.Exp.t),
      exp: Language.Exp.t,
    )
    : Language.Exp.t => {
  let max_iterations = 500;
  let current = ref(exp);
  let iterations = ref(0);
  let progress = ref(true);
  while (progress^ && iterations^ < max_iterations) {
    progress := false;
    let found = ref(None);
    try(
      shrink(current^, candidate =>
        switch (found^) {
        | Some(_) => ()
        | None =>
          incr(iterations);
          if (iterations^ >= max_iterations) {
            raise(Exit);
          };
          if (pred(candidate)) {
            found := Some(candidate);
            raise(Exit);
          };
        }
      )
    ) {
    | Exit => ()
    };
    switch (found^) {
    | Some(smaller) =>
      current := smaller;
      progress := true;
    | None => ()
    };
  };
  current^;
};

let qcheck_subexp_synthesis_agrees_stats = () => {
  let total = ref(0);
  let holds = ref(0);
  let differs = ref(0);
  let skip_parent = ref(0);
  let count = 10000;
  let arb = QCheck_Util.arb_exp(~minimal_idents=true, 50);
  let gen = arb.QCheck.gen;
  let shrink =
    switch (arb.QCheck.shrink) {
    | Some(s) => s
    | None => ((_, _) => ())
    };
  let rand = Random.State.make([|0xDECAF|]);
  let sample_differs = ref([]);
  let sample_limit = 20;
  for (_ in 1 to count) {
    incr(total);
    let exp = QCheck.Gen.generate1(~rand, gen);
    switch (check_subexp_synthesis_agrees(exp)) {
    | SubHolds => incr(holds)
    | SubDiffers(_, _, _) =>
      incr(differs);
      if (List.length(sample_differs^) < sample_limit) {
        let shrunk = shrink_failing_pred(sub_is_differing, shrink, exp);
        switch (check_subexp_synthesis_agrees(shrunk)) {
        | SubDiffers(sub, ty1, ty2) =>
          sample_differs := [(shrunk, sub, ty1, ty2), ...sample_differs^]
        | _ => ()
        };
      };
    | SubSkipParent => incr(skip_parent)
    };
  };
  let pct = n => 100. *. float_of_int(n) /. float_of_int(total^);
  Printf.printf(
    "\n[subexp synthesis agrees] out of %d cases:\n"
    ^^ "  holds:               %4d (%.1f%%)\n"
    ^^ "  differs:             %4d (%.1f%%)\n"
    ^^ "  skipped (parent):    %4d (%.1f%%)\n",
    total^,
    holds^,
    pct(holds^),
    differs^,
    pct(differs^),
    skip_parent^,
    pct(skip_parent^),
  );
  List.iter(
    ((exp, sub, ty1, ty2)) =>
      Printf.printf(
        "  sample differing case:\n    exp:         %s\n    sub:         %s\n    ty(parent):  %s\n    ty(sub):     %s\n",
        show_exp(exp),
        show_exp(sub),
        show_typ(ty1),
        show_typ(ty2),
      ),
    List.rev(sample_differs^),
  );
};

/* Property (weak form): for every InfoExp entry in the info_map, the stored
 * elab_term's outer rep_id must be the id of *some* subexpression of the
 * input user term. (The strict form — elab_id == key — is too strong: e.g.
 * TyAlias is stripped during elaboration so its elab_term is just the body,
 * which has the body's id rather than the TyAlias's. That's fine as long
 * as the body's id IS a user-source id.)
 *
 * What this catches: elaboration steps that mint an id that doesn't
 * correspond to any user source position. Those ids are absent from
 * info_map, so the evaluator cannot create a cache entry at them and the
 * subtree silently becomes un-incrementalizable — the same shape of bug
 * as the FixF-with-fresh_deterministic issue we just fixed.
 *
 * The set of "user subexpression ids" is collected by walking the user
 * term (the input to Statics.mk) via TermBase.Exp.map_term. */
type elab_id_outcome =
  | IdInUserTerm
  | IdNotInUserTerm(Id.t, Id.t) /* (key_in_map, elab_term_rep_id) */
  | SkipStatics;

let collect_user_term_ids = (exp: Language.Exp.t): Id.Set.t => {
  let acc = ref(Id.Set.empty);
  let f_exp = (continue, e: Language.Exp.t): Language.Exp.t => {
    acc := Id.Set.add(Exp.rep_id(e), acc^);
    continue(e);
  };
  let _ = TermBase.Exp.map_term(~f_exp, exp);
  acc^;
};

let check_elab_term_id_in_user_term = (exp: Language.Exp.t): elab_id_outcome =>
  switch (safe_statics(exp)) {
  | `Skip => SkipStatics
  | `Ok(info_map, _elab) =>
    let user_ids = collect_user_term_ids(exp);
    let mismatch =
      Id.Map.fold(
        (id, info, acc) =>
          switch (acc, info) {
          | (Some(_), _) => acc
          | (None, Info.InfoExp({elab_term, _})) =>
            let elab_id = Exp.rep_id(elab_term);
            Id.Set.mem(elab_id, user_ids) ? None : Some((id, elab_id));
          | (None, _) => None
          },
        info_map,
        None,
      );
    switch (mismatch) {
    | None => IdInUserTerm
    | Some((k, e)) => IdNotInUserTerm(k, e)
    };
  };

let qcheck_elab_term_id_in_user_term_stats = () => {
  let rand = Random.State.make([|0xC0DE, 0xFEED|]);
  let arb = QCheck_Util.arb_exp(~minimal_idents=true, 40);
  let gen = arb.QCheck.gen;
  let shrink =
    switch (arb.QCheck.shrink) {
    | Some(s) => s
    | None => ((_, _) => ())
    };
  let total = ref(0);
  let holds = ref(0);
  let mismatches = ref(0);
  let skipped = ref(0);
  let sample_limit = 5;
  let sample_mismatches = ref([]);
  let is_mismatch = exp =>
    switch (check_elab_term_id_in_user_term(exp)) {
    | IdNotInUserTerm(_, _) => true
    | _ => false
    };
  for (_ in 1 to 100000) {
    incr(total);
    let exp = QCheck.Gen.generate1(~rand, gen);
    switch (check_elab_term_id_in_user_term(exp)) {
    | IdInUserTerm => incr(holds)
    | IdNotInUserTerm(_, _) =>
      incr(mismatches);
      if (List.length(sample_mismatches^) < sample_limit) {
        let shrunk = shrink_failing_pred(is_mismatch, shrink, exp);
        switch (check_elab_term_id_in_user_term(shrunk)) {
        | IdNotInUserTerm(k, e) =>
          sample_mismatches := [(shrunk, k, e), ...sample_mismatches^]
        | _ => ()
        };
      };
    | SkipStatics => incr(skipped)
    };
  };
  let pct = n => 100. *. float_of_int(n) /. float_of_int(total^);
  Printf.printf(
    "\n[elab_term id is in user-term subexpressions] out of %d cases:\n"
    ^^ "  holds:           %4d (%.1f%%)\n"
    ^^ "  mismatches:      %4d (%.1f%%)\n"
    ^^ "  skipped:         %4d (%.1f%%)\n",
    total^,
    holds^,
    pct(holds^),
    mismatches^,
    pct(mismatches^),
    skipped^,
    pct(skipped^),
  );
  List.iter(
    ((exp, k, e)) =>
      Printf.printf(
        "  sample mismatch:\n    exp:           %s\n    info_map key:  %s\n    elab_rep_id:   %s\n",
        show_exp(exp),
        Id.to_string(k),
        Id.to_string(e),
      ),
    List.rev(sample_mismatches^),
  );
};

/* Property: no binder inside a program leaks into its type. Every module
   path in the root type (`m.T`) must be rooted at a name the program does
   not bind: a builtin, or a variable free in the program. A name that is
   bound by some pattern and also free elsewhere (present in the root
   co-context) cannot be told apart, so it is allowed. */
let qcheck_root_type_is_closed =
  QCheck.Test.make(
    ~name="Root type mentions no internal binder",
    ~count=2000,
    QCheck_Util.arb_exp(~minimal_idents=true, 50),
    exp =>
    switch (safe_statics(exp)) {
    | `Skip => true
    | `Ok(m, _) =>
      switch (Statics.Map.lookup_exp(Exp.rep_id(exp), m)) {
      | None => true
      | Some(root) =>
        let bound =
          Id.Map.fold(
            (_, info: Info.t, acc) =>
              switch (info) {
              | InfoPat({user_term, _}) => Pat.bound_vars(user_term) @ acc
              | _ => acc
              },
            m,
            [],
          );
        let free = List.map(fst, root.co_ctx);
        Typ.path_roots(root.ty)
        |> List.for_all(x => !List.mem(x, bound) || List.mem(x, free));
      }
    }
  );

let tests = (
  "Statics.Properties",
  [
    QCheck_alcotest.to_alcotest(qcheck_statics_does_not_crash),
    QCheck_alcotest.to_alcotest(qcheck_root_type_is_closed),
    Alcotest.test_case(
      "Elaboration preserves type (stats only)",
      `Slow,
      qcheck_elaboration_preserves_type_stats,
    ),
    Alcotest.test_case(
      "Sub-expression synthesis agrees (stats only)",
      `Slow,
      qcheck_subexp_synthesis_agrees_stats,
    ),
    Alcotest.test_case(
      "elab_term id is in user-term subexpressions (stats only)",
      `Slow,
      qcheck_elab_term_id_in_user_term_stats,
    ),
  ],
);

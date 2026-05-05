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

/* Invariant: for every InfoExp keyed by id `k` (where `k` is the rep_id
   of the user_term), `k` must appear somewhere — at any depth — in the
   elab_term's id set. The probe pipeline keys targets by user_term ids;
   the evaluator binds `expr_id` to whatever node it's currently
   evaluating; for a probe to fire, the user-source id must reach the
   runtime on *some* elab subterm. If it appears nowhere in the elab
   subtree, statics has dropped it.

   Exception: TupLabel user_terms are skipped. The Tuple processing path
   builds the TupLabel's elab via `... |> rewrap` where the in-scope
   `rewrap` is the parent Tuple's, so the source TupLabel's id is stamped
   over (Statics.re:925–941). This is the same shape of bug as #2264
   but for labeled tuple items rather than custom-statics Aps, and is
   out of scope for this PR. Once Statics.re uses the source TupLabel's
   own ids when building its elab, this skip can be removed.

   We skip entries where `key_id != Info.id_of(info)` to avoid reporting
   multi-tile duplicates of the same logical InfoExp. */
type id_alignment_violation = {
  user_cls: string,
  elab_cls: string,
  key_id: Id.t,
  user_rep_id: Id.t,
  user_ids: list(Id.t),
  elab_id_count: int,
};

/* Classes whose elab term *intentionally* doesn't carry the user_term's id:
   - TupLabel: source TupLabel inside a Tuple is rebuilt with the parent
     Tuple's `rewrap`, so the source TupLabel id is overwritten
     (Statics.re:925–941).
   - TyAlias: type aliases have no runtime; elab is just the body
     (Statics.re:2271). Source TyAlias node id is dropped.
   Both are real id-drift in elaboration but distinct from #2264;
   tracking them separately. Extend this list as the PBT surfaces
   more drift sites. */
let is_skipped_class = (e: Exp.t): bool =>
  switch (e.term) {
  | TupLabel(_, _)
  | TyAlias(_, _, _) => true
  | _ => false
  };

/* Collect every id appearing on any node of an Exp tree, via map_term. */
let all_ids_of_exp = (e: Exp.t): Id.Set.t => {
  let acc = ref(Id.Set.empty);
  let _ =
    Exp.map_term(
      ~f_exp=
        (cont, sub) => {
          List.iter(id => acc := Id.Set.add(id, acc^), IdTagged.ids(sub));
          cont(sub);
        },
      e,
    );
  acc^;
};

let id_alignment_violations =
    (info_map: Statics.Map.t): list(id_alignment_violation) =>
  Id.Map.fold(
    (key_id, info, acc) =>
      if (key_id != Info.id_of(info)) {
        acc;
      } else {
        switch (info) {
        | Info.InfoExp({user_term, elab_term, _})
            when !is_skipped_class(user_term) =>
          let user_ids = IdTagged.ids(user_term);
          let user_rep_id = IdTagged.rep_id(user_term);
          let elab_ids = all_ids_of_exp(elab_term);
          Id.Set.mem(user_rep_id, elab_ids)
            ? acc
            : [
              {
                user_cls: Exp.cls_of_term(user_term.term) |> Exp.show_cls,
                elab_cls: Exp.cls_of_term(elab_term.term) |> Exp.show_cls,
                key_id,
                user_rep_id,
                user_ids,
                elab_id_count: Id.Set.cardinal(elab_ids),
              },
              ...acc,
            ];
        | _ => acc
        };
      },
    info_map,
    [],
  );

let short_id = (id: Id.t): string => {
  let s = Id.show(id);
  /* Pull just the hex chunk before the first '-' from the formatted string. */
  switch (String.index_opt(s, '"')) {
  | Some(i) =>
    let after = String.sub(s, i + 1, String.length(s) - i - 1);
    switch (String.index_opt(after, '-')) {
    | Some(j) => String.sub(after, 0, j)
    | None => after
    };
  | None => s
  };
};

let show_violation = (v: id_alignment_violation): string =>
  Printf.sprintf(
    "user_cls=%s elab_cls=%s key=%s user_rep=%s user_ids=[%s] not in elab (elab has %d distinct ids)",
    v.user_cls,
    v.elab_cls,
    short_id(v.key_id),
    short_id(v.user_rep_id),
    String.concat(",", List.map(short_id, v.user_ids)),
    v.elab_id_count,
  );

/* PBT version of the id-alignment invariant.

   We can't cheaply add custom-statics builtin names to the *shared*
   menhirParser generator, because its shrinker can isolate `Var("to_lvs")`
   and similar bare names, exposing an unrelated free-var-id divergence in
   the evaluator/stepper consistency PBT. Instead, we wrap a fresh body
   expression in a Var(builtin)-headed Ap *here at the test boundary* —
   the inner generated body cannot itself contain those names, so no
   shrunk subterm becomes a bare builtin Var. This gives us PBT coverage
   of the custom-statics elaboration paths without disturbing other
   property tests. */
let custom_statics_builtins = [
  "to_lvs",
  "from_lvs",
  "project_labels",
  "select_labels",
  "omit_labels",
  "omit_all_labels",
  "group_by_label",
];

let arb_builtin_ap_exp = (~size: int) => {
  open QCheck;
  let inner = QCheck_Util.arb_exp(~minimal_idents=true, size);
  let inner_gen = inner.gen;
  let gen =
    Gen.(
      let* fn_name = oneofl(custom_statics_builtins);
      let* arg_core = inner_gen;
      /* Wrap: Ap(Var(fn_name), arg). Construct directly as an
         Exp.t with fresh ids — no menhir AST round-trip needed. */
      let fn_var = Exp.fresh(Var(fn_name));
      pure(Exp.fresh(Ap(Forward, fn_var, arg_core)))
    );
  /* No shrinker: shrinking would peel off the Ap and expose the bare
     builtin Var to other downstream tests sharing this arb (none today,
     but defensive). */
  make(~print=show_exp, gen);
};

let qcheck_user_id_preserved_in_elab =
  QCheck.Test.make(
    ~name="every InfoExp's user_term rep_id appears in its elab_term",
    ~count=2000,
    QCheck_Util.arb_exp(~minimal_idents=true, 30),
    exp =>
    switch (safe_statics(exp)) {
    | `Skip => true
    | `Ok(info_map, _) =>
      switch (id_alignment_violations(info_map)) {
      | [] => true
      | violations =>
        QCheck.Test.fail_reportf(
          "user_term id dropped from elab on:\n  %s\nviolations:\n  %s",
          show_exp(exp),
          String.concat("\n  ", List.map(show_violation, violations)),
        )
      }
    }
  );

let qcheck_user_id_preserved_in_elab_custom_statics =
  QCheck.Test.make(
    ~name=
      "every InfoExp's user_term rep_id appears in its elab_term (custom-statics Ap)",
    ~count=500,
    arb_builtin_ap_exp(~size=20),
    exp =>
    switch (safe_statics(exp)) {
    | `Skip => true
    | `Ok(info_map, _) =>
      switch (id_alignment_violations(info_map)) {
      | [] => true
      | violations =>
        QCheck.Test.fail_reportf(
          "user_term id dropped from elab on:\n  %s\nviolations:\n  %s",
          show_exp(exp),
          String.concat("\n  ", List.map(show_violation, violations)),
        )
      }
    }
  );

let tests = (
  "Statics.Properties",
  [
    QCheck_alcotest.to_alcotest(qcheck_statics_does_not_crash),
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
    QCheck_alcotest.to_alcotest(qcheck_user_id_preserved_in_elab),
    QCheck_alcotest.to_alcotest(
      qcheck_user_id_preserved_in_elab_custom_statics,
    ),
  ],
);

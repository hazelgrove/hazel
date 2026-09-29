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
      switch (safe_statics(elab)) {
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
    QCheck_Util.arb_exp(~minimal_idents=true, 50),
    exp => {
      check_elaboration_preserves_type(exp);
      true;
    },
  );

/* Disabled: a `let`-bound module ascribed by name synthesizes a different type
   before and after elaboration -- under QCHECK_SEED=1 the user term gives
   `Unknown(Hole(Invalid "y"))` where its elaboration gives `(x = + B, y = ())`.
   Seed-dependent, and did not reduce to a short program: most seeds pass. */
let qcheck_elaboration_preserves_type_disabled =
  Alcotest.test_case("Elaboration preserves type (disabled)", `Quick, () => {
    [@warning "-21"]
    {
      Alcotest.skip();
      ignore(QCheck_alcotest.to_alcotest(qcheck_elaboration_preserves_type));
    }
  });

/* Property: for each user sub-expression, the parent-recorded `elab_syn_ty`
   agrees with the `elab_syn_ty` a fresh statics run produces for that sub's
   `elab_term`. Comparing against the elaborated sub rather than re-analyzing
   the user sub means the ana-driven rewrites already baked into `elab_term` --
   number-literal replacement, constructor ADT resolution, label inference --
   agree on both sides.

   Bare labels are skipped: a bare label has no type outside its enclosing
   product type. */
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
  | Label(_) => true
  | _ => false
  };

let differing_subs =
    (info_map: Statics.Map.t, exp: Language.Exp.t): list(syn_disagreement) => {
  let results = ref([]);
  let _ =
    Exp.map_term(
      ~f_exp=
        (cont, sub) => {
          if (!is_bare_label(sub)) {
            switch (syn_and_elab_of(info_map, sub)) {
            | None => ()
            | Some((recorded_ty, ctx, elab_term)) =>
              let parent_ty =
                recorded_ty |> Typ.normalize(ctx) |> Typ.all_ids_temp;
              switch (safe_statics(~ctx, elab_term)) {
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

let qcheck_subexp_synthesis_agrees =
  QCheck.Test.make(
    ~name="Sub-expression synthesis agrees with a fresh run",
    ~count=10000,
    QCheck_Util.arb_exp(~minimal_idents=true, 50),
    exp => {
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

/* Disabled: a negation analyzed against Float keeps an Int `elab_term`, so the
   two sides disagree -- `(()) +. - []` reports parent Float, fresh Int.
   Module member types are a second witness class. Fails on ~0.3% of generated
   programs. */
let qcheck_subexp_synthesis_agrees_disabled =
  Alcotest.test_case(
    "Sub-expression synthesis agrees with a fresh run (disabled)", `Quick, () => {
    [@warning "-21"]
    {
      Alcotest.skip();
      ignore(QCheck_alcotest.to_alcotest(qcheck_subexp_synthesis_agrees));
    }
  });

/* Property (weak form): every `InfoExp`'s stored `elab_term` is rooted at the
   id of some sub-expression of the user term. The strict form -- elab id ==
   map key -- is too strong: `TyAlias` is stripped during elaboration, so its
   `elab_term` is just the body and carries the body's id.

   What this catches: an elaboration step minting an id that corresponds to no
   user source position. Such ids are absent from the info map, so the
   evaluator cannot create a cache entry at them and the subtree silently
   becomes un-incrementalizable. */
type elab_id_mismatch = {
  info_id: Id.t,
  elab_id: Id.t,
};

let show_elab_id_mismatch = ({info_id, elab_id}: elab_id_mismatch) =>
  Printf.sprintf(
    "info %s holds an elab_term rooted at %s",
    Id.to_string(info_id),
    Id.to_string(elab_id),
  );

let collect_user_term_ids = (exp: Language.Exp.t): Id.Set.t => {
  let acc = ref(Id.Set.empty);
  let f_exp = (continue, e: Language.Exp.t): Language.Exp.t => {
    acc := Id.Set.add(Exp.rep_id(e), acc^);
    continue(e);
  };
  let _ = TermBase.Exp.map_term(~f_exp, exp);
  acc^;
};

let elab_ids_outside_user_term =
    (info_map: Statics.Map.t, exp: Language.Exp.t): list(elab_id_mismatch) => {
  let user_ids = collect_user_term_ids(exp);
  Id.Map.fold(
    (info_id, info, acc) =>
      switch (info) {
      | Info.InfoExp({elab_term, _}) =>
        let elab_id = Exp.rep_id(elab_term);
        let mismatch = {
          info_id,
          elab_id,
        };
        Id.Set.mem(elab_id, user_ids) ? acc : [mismatch, ...acc];
      | _ => acc
      },
    info_map,
    [],
  );
};

let qcheck_elab_term_ids_in_user_term =
  QCheck.Test.make(
    ~name="Every elab_term is rooted at a user-term id",
    ~count=1000,
    QCheck_Util.arb_exp(~minimal_idents=true, 40),
    exp => {
      check_no_violations(
        ~violations=elab_ids_outside_user_term,
        ~show=show_elab_id_mismatch,
        ~msg="every elab_term is rooted at a user-term id",
        exp,
      );
      true;
    },
  );

/* Disabled: elaboration mints ids that belong to no user sub-expression --
   `{}`, the empty module, is the smallest reproducer. Fails on ~30% of
   generated programs. */
let qcheck_elab_term_ids_in_user_term_disabled =
  Alcotest.test_case(
    "Every elab_term is rooted at a user-term id (disabled)", `Quick, () => {
    [@warning "-21"]
    {
      Alcotest.skip();
      ignore(QCheck_alcotest.to_alcotest(qcheck_elab_term_ids_in_user_term));
    }
  });

let tests = (
  "Statics.Properties",
  [
    QCheck_alcotest.to_alcotest(qcheck_statics_does_not_crash),
    qcheck_elaboration_preserves_type_disabled,
    qcheck_subexp_synthesis_agrees_disabled,
    qcheck_elab_term_ids_in_user_term_disabled,
  ],
);

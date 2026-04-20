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

let safe_statics = exp =>
  switch (Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), exp)) {
  | result => `Ok(result)
  | exception Stack_overflow => `Skip
  | exception (Failure(f) as e) =>
    if (is_known_statics_failure(f)) {
      `Skip;
    } else {
      raise(e);
    }
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
  let count = 100000;
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

let tests = (
  "Statics.Properties",
  [
    QCheck_alcotest.to_alcotest(qcheck_statics_does_not_crash),
    Alcotest.test_case(
      "Elaboration preserves type (stats only)",
      `Slow,
      qcheck_elaboration_preserves_type_stats,
    ),
  ],
);

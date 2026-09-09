open Alcotest;

module ProblemCollection = Haz3lcore.ProblemCollection;

let from_string =
    (s: string)
    : option(
        (ProblemCollection.problem_context, list(ProblemCollection.problem)),
      ) =>
  switch (Haz3lcore.Parser.to_zipper(~root=Exp, s)) {
  | None => None
  | Some(z) =>
    let editor = Haz3lcore.Editor.Model.mk(z, ~root=Exp);
    let statics =
      Haz3lcore.CachedStatics.init(
        ~settings=Language.CoreSettings.on,
        ~is_dynamic_term=false,
        ~stitch=Fun.id,
        ~root=Exp,
        editor.state.zipper,
      );
    let ctx =
      ProblemCollection.make_problem_context(
        ~display_warnings=true,
        ~statics,
        ~syntax=editor.syntax,
      );
    let problems = ProblemCollection.collect_all_problems(ctx);
    Some((ctx, problems));
  };

let from_string_exn = (s: string) =>
  switch (from_string(s)) {
  | Some(result) => result
  | None => fail("Failed to parse: " ++ s)
  };

let count_by_category = (cat: ProblemCollection.problem_category, problems) =>
  List.length(
    List.filter(
      (p: ProblemCollection.problem) => p.category == cat,
      problems,
    ),
  );

let has_structural =
    (desc: string, problems: list(ProblemCollection.problem)) =>
  List.exists(
    (p: ProblemCollection.problem) =>
      switch (p.source) {
      | Structural(d) => d == desc
      | FromInfo(_)
      | FromProjector(_) => false
      },
    problems,
  );

let has_multihole_error = (problems: list(ProblemCollection.problem)) =>
  List.exists(
    (p: ProblemCollection.problem) =>
      switch (p.source) {
      | FromInfo(ci) =>
        List.exists(
          m =>
            switch (m) {
            | Language.Mark.IsMulti => true
            | _ => false
            },
          Language.Info.marks_of(ci),
        )
      | _ => false
      },
    problems,
  );

let clean_program = () => {
  let (_, problems) = from_string_exn("let x = 1 in x + 2");
  check(int, "no problems", 0, List.length(problems));
};

let juxtaposed_literals = () => {
  let (_, problems) = from_string_exn("1 2");
  check(
    bool,
    "has missing operator",
    true,
    has_structural("Missing operator", problems),
  );
  check(
    bool,
    "syntax category count > 0",
    true,
    count_by_category(Syntax, problems) > 0,
  );
};

let type_mismatch = () => {
  let (_, problems) = from_string_exn("1 + true");
  check(
    bool,
    "has static errors",
    true,
    count_by_category(Static, problems) > 0,
  );
};

let incomplete_tile = () => {
  let (_, problems) = from_string_exn("if true then 1");
  check(
    bool,
    "has incomplete syntax error",
    true,
    count_by_category(Syntax, problems) > 0,
  );
};

let trailing_unbound_var = () => {
  let (_, problems) = from_string_exn("1\nf");
  check(
    bool,
    "has syntax errors",
    true,
    count_by_category(Syntax, problems) > 0,
  );
  check(
    bool,
    "has multihole error for broken expression",
    true,
    has_multihole_error(problems),
  );
};

let trailing_var_after_let = () => {
  let (_, problems) = from_string_exn("let x = 1 in x\nf");
  check(bool, "has errors for trailing f", true, List.length(problems) > 0);
  check(bool, "has multihole error", true, has_multihole_error(problems));
};

/* ---------- Helpers for source/collection tests ---------- */

let source_from_string = (s: string): ProblemCollection.editor_source =>
  switch (Haz3lcore.Parser.to_zipper(~root=Exp, s)) {
  | None => fail("Failed to parse: " ++ s)
  | Some(z) =>
    let editor = Haz3lcore.Editor.Model.mk(z, ~root=Exp);
    let statics =
      Haz3lcore.CachedStatics.init(
        ~settings=Language.CoreSettings.on,
        ~is_dynamic_term=false,
        ~stitch=Fun.id,
        ~root=Exp,
        editor.state.zipper,
      );
    {
      statics,
      syntax: editor.syntax,
    };
  };

let total_count =
    (counts: list((ProblemCollection.problem_category, int))): int =>
  List.fold_left((acc, (_, n)) => acc + n, 0, counts);

/* ---------- nearest_measured_id tests ---------- */

let nearest_measured_id_self = () => {
  /* When the id is itself in measured, nearest_measured_id should return
     it without walking ancestors. */
  let (ctx, _) = from_string_exn("1 + true");
  /* Pick any id known to be in the segment & measured. */
  let any_piece_id =
    switch (ctx.segment) {
    | [p, ..._] => Haz3lcore.Piece.id(p)
    | [] => fail("empty segment")
    };
  let resolved = ctx.nearest_measured_id(any_piece_id);
  check(
    bool,
    "id present in measured resolves to itself",
    true,
    resolved == Some(any_piece_id),
  );
};

let nearest_measured_id_invalid = () => {
  /* Id.invalid is in neither measured nor info_map; ancestor walk yields
     nothing. */
  let (ctx, _) = from_string_exn("1");
  check(
    bool,
    "unknown id resolves to None",
    true,
    ctx.nearest_measured_id(Haz3lcore.Id.invalid) == None,
  );
};

let nearest_measured_id_inside_fold = () => {
  /* Walk an Exp tree and return the id of the first Int literal matching
     `n`. Mirrors ProofHacks.find_exp_id. */
  module M = {
    exception Found(Language.Exp.t);
  };
  let find_int_id = (n: int, exp: Language.Exp.t): option(Haz3lcore.Id.t) => {
    let target = Bigint.of_int(n);
    switch (
      Language.Exp.map_term(
        ~f_exp=
          (cont, exp) =>
            switch (Language.Exp.term_of(exp)) {
            | Atom(Int(m)) when Bigint.equal(m, target) =>
              raise(M.Found(exp))
            | _ => cont(exp)
            },
        exp,
      )
    ) {
    | exception (M.Found(e)) => Some(Language.Exp.rep_id(e))
    | _ => None
    };
  };

  let src = {|let a = ^^fold(1 +
2 +
3 +
4) in a|};
  let z =
    switch (Haz3lcore.Parser.to_zipper(~root=Exp, src)) {
    | Some(z) => z
    | None => fail("Failed to parse: " ++ src)
    };
  let editor = Haz3lcore.Editor.Model.mk(z, ~root=Exp);
  let statics =
    Haz3lcore.CachedStatics.init(
      ~settings=Language.CoreSettings.on,
      ~is_dynamic_term=false,
      ~stitch=Fun.id,
      ~root=Exp,
      editor.state.zipper,
    );
  let ctx =
    ProblemCollection.make_problem_context(
      ~display_warnings=true,
      ~statics,
      ~syntax=editor.syntax,
    );

  /* Find the inner literal `3`, on its own line of the folded body. */
  let exp =
    Haz3lcore.MakeTerm.from_zip_for_sem(editor.state.zipper, ~root=Exp).term;
  let inner_id =
    switch (find_int_id(3, exp)) {
    | Some(id) => id
    | None => fail("inner literal 3 not found in Exp tree")
    };
  check(
    bool,
    "inner literal id is absent from measured",
    true,
    Haz3lcore.Measured.find_by_id(inner_id, ctx.measured) == None,
  );
  let resolved =
    switch (ctx.nearest_measured_id(inner_id)) {
    | Some(id) => id
    | None => fail("nearest_measured_id returned None for inner id")
    };
  /* Look the resolved id up in the Exp tree and assert the node there
     is a Fold projector. */
  let resolved_exp =
    switch (Language.ProofHacks.find_exp_id(resolved, exp)) {
    | Some(e) => e
    | None => fail("resolved id not found in Exp tree")
    };
  check(
    bool,
    "resolved exp is a Fold projector",
    true,
    switch (Language.Exp.term_of(resolved_exp)) {
    | Projector({kind: Fold, _}, _) => true
    | _ => false
    },
  );
};

/* ---------- ProblemCollection.make tests ---------- */

let make_empty_inputs = () => {
  let coll = ProblemCollection.make(~display_warnings=true, []);
  check(int, "no groups", 0, List.length(coll.groups));
  check(int, "all aggregated counts zero", 0, total_count(coll.counts));
};

let make_single_source_single_group = () => {
  /* One source with errors, one group around it. */
  let src = source_from_string("1 + true");
  let coll =
    ProblemCollection.make(
      ~display_warnings=true,
      [
        {
          label: Some("only"),
          sources: [src],
        },
      ],
    );
  check(int, "one group", 1, List.length(coll.groups));
  let g = List.hd(coll.groups);
  check(bool, "single_source = true", true, g.single_source);
  check(bool, "label preserved", true, g.label == Some("only"));
  check(
    bool,
    "group has at least one static error",
    true,
    Option.value(
      List.assoc_opt(ProblemCollection.Static, g.counts),
      ~default=0,
    )
    > 0,
  );
  /* Aggregated counts equal the lone group's counts. */
  check(
    int,
    "aggregated total equals group total",
    total_count(g.counts),
    total_count(coll.counts),
  );
};

let make_multi_source_flag = () => {
  let src1 = source_from_string("1 + true");
  let src2 = source_from_string("let x = 1 in x");
  let coll =
    ProblemCollection.make(
      ~display_warnings=true,
      [
        {
          label: Some("multi"),
          sources: [src1, src2],
        },
      ],
    );
  check(int, "one group", 1, List.length(coll.groups));
  let g = List.hd(coll.groups);
  check(bool, "single_source = false", false, g.single_source);
};

let make_first_wins_dedup_across_groups = () => {
  /* Same source in two groups: first claims all problems, second is
     empty. This is the load-bearing behavior for shared zippers like
     test_validation / user_tests. */
  let src = source_from_string("1 + true");
  let solo =
    ProblemCollection.make(
      ~display_warnings=true,
      [
        {
          label: Some("a"),
          sources: [src],
        },
      ],
    );
  let solo_total = total_count(List.hd(solo.groups).counts);
  check(bool, "solo group has problems", true, solo_total > 0);
  let coll =
    ProblemCollection.make(
      ~display_warnings=true,
      [
        {
          label: Some("a"),
          sources: [src],
        },
        {
          label: Some("b"),
          sources: [src],
        },
      ],
    );
  check(int, "two groups", 2, List.length(coll.groups));
  let g_a = List.nth(coll.groups, 0);
  let g_b = List.nth(coll.groups, 1);
  check(
    int,
    "first group keeps all problems",
    solo_total,
    total_count(g_a.counts),
  );
  check(
    int,
    "second group is empty after dedup",
    0,
    total_count(g_b.counts),
  );
  /* Aggregated counts should not double-count. */
  check(
    int,
    "aggregated counts unaffected by duplicate group",
    solo_total,
    total_count(coll.counts),
  );
};

let make_aggregates_counts_across_groups = () => {
  /* Two distinct sources with distinct ids: counts sum across groups. */
  let src1 = source_from_string("1 + true");
  let src2 = source_from_string("if true then 1");
  let solo1 =
    ProblemCollection.make(
      ~display_warnings=true,
      [
        {
          label: Some("a"),
          sources: [src1],
        },
      ],
    );
  let solo2 =
    ProblemCollection.make(
      ~display_warnings=true,
      [
        {
          label: Some("b"),
          sources: [src2],
        },
      ],
    );
  let t1 = total_count(List.hd(solo1.groups).counts);
  let t2 = total_count(List.hd(solo2.groups).counts);
  let coll =
    ProblemCollection.make(
      ~display_warnings=true,
      [
        {
          label: Some("a"),
          sources: [src1],
        },
        {
          label: Some("b"),
          sources: [src2],
        },
      ],
    );
  check(
    int,
    "aggregated counts = sum of per-group totals",
    t1 + t2,
    total_count(coll.counts),
  );
};

let projector_error_collection = () => {
  /* Synthesize a projector error in the problem context and verify it
   * surfaces as a Projector-category problem. */
  let (ctx, _) = from_string_exn("1");
  let err: Haz3lcore.ProjectorBase.error = {message: "synthetic error"};
  let fake_id = Haz3lcore.Id.mk();
  let ctx_with_err = {
    ...ctx,
    projector_errors: [(fake_id, Language.ProjectorKind.Fold, err)],
  };
  let problems =
    Haz3lcore.ProblemCollection.collect_all_problems(ctx_with_err);
  check(
    int,
    "one projector-category problem",
    1,
    count_by_category(Projector, problems),
  );
  check(
    bool,
    "problem source is FromProjector",
    true,
    List.exists(
      (p: Haz3lcore.ProblemCollection.problem) =>
        switch (p.source) {
        | FromProjector(_, e) => e.message == "synthetic error"
        | _ => false
        },
      problems,
    ),
  );
};

/* ---------- One error, one row ----------

   A transparent wrapper restates the marks of the term it wraps, so
   without collapsing those chains the sidebar reports the same error
   once per nesting level. */

let static_ids = (problems: list(ProblemCollection.problem)) =>
  List.filter_map(
    (p: ProblemCollection.problem) =>
      p.category == Static ? Some(p.id) : None,
    problems,
  );

let parens_report_one_error = () => {
  let count = s => {
    let (_, problems) = from_string_exn(s);
    count_by_category(Static, problems);
  };
  check(int, "bare free variable", 1, count("y"));
  check(int, "free variable in parens", 1, count("(y)"));
  check(int, "free variable in nested parens", 1, count("((y))"));
};

let parens_report_the_marked_term = () => {
  /* The variable owns the mark and is visible, so it stays the row's id
     — the parens around it are what drop out. */
  let (bare_ctx, bare) = from_string_exn("y");
  let (ctx, problems) = from_string_exn("(y)");
  switch (static_ids(bare), static_ids(problems)) {
  | ([bare_id], [id]) =>
    check(
      bool,
      "reported id is in measured",
      true,
      Haz3lcore.Measured.find_by_id(id, ctx.measured) != None,
    );
    check(
      bool,
      "reported id is the variable's, not the parens'",
      true,
      Haz3lcore.Measured.find_by_id(bare_id, bare_ctx.measured) != None,
    );
  | (bare_ids, ids) =>
    fail(
      "expected one static error each, got "
      ++ string_of_int(List.length(bare_ids))
      ++ " and "
      ++ string_of_int(List.length(ids)),
    )
  };
};

/* A livelit use is always inside a projector, so its expansion error hit
   this every time: once on `^s(model)`, once on the projector. */
let livelit_def = {|{
type Model = Int;
type Action = Int;
type Expansion = String;
let init : Model = 0;
let update = fun (m, a) -> a;
let view = fun m -> 0;
let expand = fun m : Model -> m
}|};

let projected_use_reports_one_error = () => {
  let count = s => {
    let (_, problems) = from_string_exn(s);
    count_by_category(Static, problems);
  };
  check(
    int,
    "bare use of a livelit with a mistyped expansion",
    1,
    count("let ^s = " ++ livelit_def ++ " in ^s(1)"),
  );
  check(
    int,
    "the same use inside its projector",
    1,
    count("let ^s = " ++ livelit_def ++ " in ^^livelit(^s(1))"),
  );
};

let projected_use_reports_a_visible_id = () => {
  /* The marked application lives under the projector and so has no
     measurement of its own; the row must land on the projector, which
     the user can see and click. */
  let (ctx, problems) =
    from_string_exn("let ^s = " ++ livelit_def ++ " in ^^livelit(^s(1))");
  switch (static_ids(problems)) {
  | [id] =>
    check(
      bool,
      "reported id is in measured",
      true,
      Haz3lcore.Measured.find_by_id(id, ctx.measured) != None,
    )
  | ids =>
    fail(
      "expected one static error, got " ++ string_of_int(List.length(ids)),
    )
  };
};

let collect_cases = [
  test_case("Clean program has no errors", `Quick, clean_program),
  test_case("Juxtaposed literals", `Quick, juxtaposed_literals),
  test_case("Type mismatch", `Quick, type_mismatch),
  test_case("Incomplete tile", `Quick, incomplete_tile),
  test_case("Trailing unbound var", `Quick, trailing_unbound_var),
  test_case("Trailing var after let", `Quick, trailing_var_after_let),
  test_case(
    "Projector errors surface as problems",
    `Quick,
    projector_error_collection,
  ),
  test_case(
    "Parens do not multiply an error",
    `Quick,
    parens_report_one_error,
  ),
  test_case(
    "A collapsed chain reports the marked term",
    `Quick,
    parens_report_the_marked_term,
  ),
  test_case(
    "A projected livelit use reports one error",
    `Quick,
    projected_use_reports_one_error,
  ),
  test_case(
    "A hidden marked term reports at its projector",
    `Quick,
    projected_use_reports_a_visible_id,
  ),
];

let nearest_measured_id_cases = [
  test_case(
    "nearest_measured_id returns self for measured id",
    `Quick,
    nearest_measured_id_self,
  ),
  test_case(
    "nearest_measured_id returns None for unknown id",
    `Quick,
    nearest_measured_id_invalid,
  ),
  test_case(
    "nearest_measured_id resolves ids inside a fold projector",
    `Quick,
    nearest_measured_id_inside_fold,
  ),
];

let make_cases = [
  test_case("make: empty inputs", `Quick, make_empty_inputs),
  test_case(
    "make: single source produces single_source group",
    `Quick,
    make_single_source_single_group,
  ),
  test_case(
    "make: multi-source group has single_source=false",
    `Quick,
    make_multi_source_flag,
  ),
  test_case(
    "make: first-wins dedup across groups",
    `Quick,
    make_first_wins_dedup_across_groups,
  ),
  test_case(
    "make: aggregates counts across groups",
    `Quick,
    make_aggregates_counts_across_groups,
  ),
];

let tests = [
  ("ProblemCollection.Collect", collect_cases),
  ("ProblemCollection.NearestMeasuredId", nearest_measured_id_cases),
  ("ProblemCollection.Make", make_cases),
];

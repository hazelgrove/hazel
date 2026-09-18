/**
 * Displayed values can carry duplicated source ids: with probes/samples
 * showing values, a mid-edit program containing a free variable can
 * evaluate to an indet result in which the SAME source subterm (a lambda
 * body, applied once per element) appears several times, all copies
 * carrying the SAME ids. Pretty-printing that value (ExpToSegment) and
 * building an editor from it makes Segment.reassemble group the
 * duplicated tiles by id into one Aba match and die
 * (Failure("Tile.reassemble: out-of-order shards"), or the same
 * corruption caught in Highlight.of_tile).
 *
 * ExpToSegment.pad_ids dedups ids WITHIN one term's id list; duplicates
 * ACROSS SIBLING subterms are what these tests exercise
 * (ExpToSegment.uniquify_repeated_tiles is the guard).
 *
 * Core-level mirror of the production path
 * EvalResult -> CodeSelectable.Model.mk_from_exp -> ExpToSegment ->
 * (editor init) -> Segment.reassemble.
 */
open Alcotest;
open Haz3lcore;
open Test_Evaluator_Prelude;

/* Result-display-flavored settings (mirrors EvalResult's usage:
 * value printing, unknowns as holes). The exact flags matter less than
 * exercising the exp_to_segment -> reassemble pipeline. */
let display_settings: ExpToSegment.Settings.t = {
  secondary: AutoFormat,
  parenthesization: Defensive,
  label_format: QuoteWhenNecessary,
  inline: true,
  fold_case_clauses: false,
  fold_fn_bodies: `NoFold,
  hide_fixpoints: false,
  show_ascriptions: false,
  show_filters: true,
  show_unknown_as_hole: true,
  hole_tiles: false,
  project_tables: false,
};

let exp_to_segment = ExpToSegment.exp_to_segment(~settings=display_settings);

let print_seg = Printer.of_segment(~holes="?", ~refractors=[]);

/* Collect (id, shard-index) pairs over a segment, recursing into tile
 * children. A duplicate (id, shard) pair = two physical shards claiming
 * to be the same piece = the corruption Segment.reassemble dies on. */
let rec id_shard_pairs = (seg: Segment.t): list((Id.t, int)) =>
  seg
  |> List.concat_map((p: Piece.t) =>
       switch (p) {
       | Tile(t) =>
         List.map(i => (t.id, i), t.shards)
         @ List.concat_map(id_shard_pairs, t.children)
       | Projector(pr) => id_shard_pairs([pr.syntax])
       | Grout(_)
       | Secondary(_) => []
       }
     );

let duplicate_pairs = (seg: Segment.t): list((Id.t, int)) => {
  let pairs = id_shard_pairs(seg);
  let tbl = Hashtbl.create(64);
  List.filter(
    pair => {
      let seen = Hashtbl.mem(tbl, pair);
      Hashtbl.replace(tbl, pair, ());
      seen;
    },
    pairs,
  );
};

/* Evaluate a (possibly ill-typed / free-variable-containing) program the
 * way the app does, print the result value, and put the printed segment
 * through Segment.reassemble like editor construction does. The test
 * passes iff no exception escapes and no duplicate (id, shard) pairs
 * exist in the printed segment. */
let eval_print_reassemble = (msg: string, program: string) => {
  let result = parse_and_evaluate(program);
  let seg = exp_to_segment(result);
  /* First: the printed segment must not contain duplicated shards. */
  let dups = duplicate_pairs(seg);
  if (dups != []) {
    fail(
      msg
      ++ ": printed result segment contains duplicated (id, shard) pairs: "
      ++ String.concat(
           ", ",
           List.map(
             ((id, i)) => Id.to_string(id) ++ "#" ++ string_of_int(i),
             dups,
           ),
         )
      ++ "\nsegment: "
      ++ print_seg(seg),
    );
  };
  /* Second: reassembly (as run during editor init / Calculate) must not
   * raise. Guard with an explicit try so the failure message names the
   * crash rather than aborting the runner. */
  switch (Segment.reassemble(seg)) {
  | _ => ()
  | exception (Failure(f)) => fail(msg ++ ": reassemble raised: " ++ f)
  | exception (Assert_failure(_)) =>
    fail(msg ++ ": reassemble raised Assert_failure (Tile.re invariant)")
  };
};

let tests = (
  "ShardCrashRepro",
  [
    /* Minimal shape of the field crash: a lambda whose body contains a
     * free variable (`col` — exactly the transient state after deleting
     * `row` and typing `col` in task 30 before the rename is complete),
     * applied more than once. Each indet application carries the same
     * source-body ids; printing the pair duplicates the `if/then/else`
     * tile (3 shards -> [0,1,2,0,1,2]). */
    test_case(
      "free-var lambda applied twice: printed result reassembles", `Quick, () =>
      eval_print_reassemble(
        "two applications",
        "let f = fun x -> if x == col then 1 else 2 in (f(1), f(2))",
      )
    ),
    /* List/map flavor: same shape the study tasks hit (mapi over grove
     * rows). Uses explicit applications to avoid depending on stdlib. */
    test_case(
      "free-var lambda applied thrice in a list: printed result reassembles",
      `Quick,
      () =>
      eval_print_reassemble(
        "three applications in a list",
        "let f = fun x -> if x == col then 1 else 2 in [f(1), f(2), f(3)]",
      )
    ),
    /* Control: no free variable — evaluation completes, nothing indet,
     * no duplication possible. Should pass before and after the fix. */
    test_case("control: closed lambda applied twice reassembles", `Quick, () =>
      eval_print_reassemble(
        "closed control",
        "let f = fun x -> if x == 2 then 1 else 2 in (f(1), f(2))",
      )
    ),
  ],
);

/* ── Incremental-evaluator variants ──────────────────────────────────────
 * The field crashes all involved an EDIT + RE-EVALUATION cycle (delete
 * `row`, type `col`) with probes on. The plain evaluator freshens nothing
 * relevant, but the incremental evaluator's reuse/adoption machinery is
 * id-keyed and the HACK[Matt] note names "absorption paths" as a duplicate
 * -id producer. These cases mirror the UI sequence: eval, id-preserving
 * rename of a bound var use-site to an unbound name, re-eval with ~prev. */

let statics_and_elab_with =
    (~settings: Language.CoreSettings.t, exp: Language.Exp.t) =>
  Language.Statics.mk(
    settings,
    Language.Builtins.ctx_init(Some(Language.Operators.default_mode)),
    exp,
  );

let eval_incr_with =
    (
      ~settings: Language.CoreSettings.t,
      ~prev: Language.EvaluatorState.incr_eval=Language.IncrEval.empty,
      exp: Language.Exp.t,
    )
    : (Language.Exp.t, Language.EvaluatorState.incr_eval) => {
  let (info_map, elab) = statics_and_elab_with(~settings, exp);
  let eval_info =
    Language.EvalInfo.of_info_map(
      ~probe_all=settings.probe_all,
      ~targets=Id.Map.empty,
      info_map,
    );
  let (result, state) =
    Language.Evaluator.evaluate(
      ~prev,
      ~eval_info,
      ~env=Language.Builtins.env_init,
      elab,
    );
  (result, Language.EvaluatorState.get_incr_eval(state));
};

/* Id-preserving use-site rename (mirrors a Zipper token edit: only the
 * payload changes, the annotation/ids stay). */
let replace_var =
    (~from: string, ~to_: string, exp: Language.Exp.t): Language.Exp.t => {
  let f_exp = (continue, e: Language.Exp.t): Language.Exp.t =>
    switch (e.term) {
    | Var(x) when x == from => {
        ...e,
        term: Var(to_),
      }
    | _ => continue(e)
    };
  Language.TermBase.Exp.map_term(~f_exp, exp);
};

let check_result_seg = (msg: string, result: Language.Exp.t) => {
  let seg = exp_to_segment(result);
  let dups = duplicate_pairs(seg);
  if (dups != []) {
    fail(
      msg
      ++ ": duplicated (id, shard) pairs in printed result: "
      ++ String.concat(
           ", ",
           List.map(
             ((id, i)) => Id.to_string(id) ++ "#" ++ string_of_int(i),
             dups,
           ),
         ),
    );
  };
  switch (Segment.reassemble(seg)) {
  | _ => ()
  | exception (Failure(f)) => fail(msg ++ ": reassemble raised: " ++ f)
  | exception (Assert_failure(_)) =>
    fail(msg ++ ": reassemble raised Assert_failure (Tile.re invariant)")
  };
};

let incr_rename_case = (~probe_all: bool, ~passes: int, msg: string) => {
  let settings = {
    ...Language.CoreSettings.on,
    probe_all,
  };
  let prog = "let row = 1 in let f = fun x -> if x == row then 1 else 2 in [f(1), f(2), f(3)]";
  let exp = parse_exp(prog);
  /* pass 1: pristine */
  let (_, incr1) = eval_incr_with(~settings, exp);
  /* pass 2: use-site row -> col (unbound) — the mid-fix transient */
  let broken = replace_var(~from="row", ~to_="col", exp);
  let (result2, incr2) = eval_incr_with(~settings, ~prev=incr1, broken);
  check_result_seg(msg ++ " (pass 2: broken)", result2);
  if (passes >= 3) {
    /* pass 3: back to row (Damon's re-break / undo direction) */
    let (result3, _) = eval_incr_with(~settings, ~prev=incr2, exp);
    check_result_seg(msg ++ " (pass 3: restored)", result3);
  };
};

let incr_tests = [
  test_case("incr: rename to unbound, probes off", `Quick, () =>
    incr_rename_case(~probe_all=false, ~passes=2, "incr probes-off")
  ),
  test_case("incr: rename to unbound, probe_all on", `Quick, () =>
    incr_rename_case(~probe_all=true, ~passes=2, "incr probe_all")
  ),
  test_case("incr: break/fix/re-break, probe_all on", `Quick, () =>
    incr_rename_case(~probe_all=true, ~passes=3, "incr 3-pass")
  ),
];

let tests = {
  let (name, cases) = tests;
  (name, cases @ incr_tests);
};

/* ── Sample-value variants (the real display path) ──────────────────────
 * Logical pin from the field evidence: with autoprobe OFF the same
 * keystrokes did not crash (Patrick, 01:53), but calculate reassembles
 * the SOURCE regardless of probes — so the corrupt segment cannot be the
 * source; it must be a probe-sample VALUE (rendered via
 * ProjectorInfo.utility.term_to_seg -> PrettySegment.prettify, which
 * ends in Segment.reassemble — the "Exception during Calculate").
 * Samples are captured mid-eval (values NOT freshened) and incremental
 * adoption can replay prev-run fragments; an id-preserving edit that
 * makes evaluation stuck embeds source syntax in the new value while
 * adopted fragments can carry the same ids. */

let projector_display_settings = {
  ...ExpToSegment.Settings.of_core(~inline=true, Language.CoreSettings.off),
  show_unknown_as_hole: false,
  fold_fn_bodies: `NoFold,
  project_tables: false,
};

let sample_to_seg = (v: Language.Exp.t): Segment.t =>
  ExpToSegment.any_to_segment(~settings=projector_display_settings, Exp(v));

let check_sample_values = (msg: string, probes: Language.Sample.Map.t) => {
  let all: list(Language.Sample.t) =
    Id.Map.fold((_, ss, acc) => acc @ ss, probes, []);
  List.iteri(
    (n, sample: Language.Sample.t) => {
      let v = sample.value;
      let seg = sample_to_seg(v);
      let dups = duplicate_pairs(seg);
      if (dups != []) {
        fail(
          msg
          ++ ": sample #"
          ++ string_of_int(n)
          ++ " (syntax_id "
          ++ Id.to_string(sample.syntax_id)
          ++ ") printed with duplicated (id, shard) pairs: "
          ++ String.concat(
               ", ",
               List.map(
                 ((id, i)) => Id.to_string(id) ++ "#" ++ string_of_int(i),
                 dups,
               ),
             )
          ++ "\nvalue seg: "
          ++ print_seg(seg),
        );
      };
      /* Drawer path: prettify runs Segment.reassemble internally. */
      switch (PrettySegment.prettify(~width=40, seg)) {
      | _ => ()
      | exception (Failure(f)) =>
        fail(
          msg ++ ": sample #" ++ string_of_int(n) ++ " prettify raised: " ++ f,
        )
      | exception (Assert_failure(_)) =>
        fail(
          msg
          ++ ": sample #"
          ++ string_of_int(n)
          ++ " prettify raised Assert_failure",
        )
      };
    },
    all,
  );
};

let eval_incr_probes =
    (
      ~settings: Language.CoreSettings.t,
      ~prev: Language.EvaluatorState.incr_eval=Language.IncrEval.empty,
      exp: Language.Exp.t,
    )
    : (
        Language.Exp.t,
        Language.Sample.Map.t,
        Language.EvaluatorState.incr_eval,
      ) => {
  let (info_map, elab) = statics_and_elab_with(~settings, exp);
  let eval_info =
    Language.EvalInfo.of_info_map(
      ~probe_all=settings.probe_all,
      ~targets=Id.Map.empty,
      info_map,
    );
  let (result, state) =
    Language.Evaluator.evaluate(
      ~prev,
      ~eval_info,
      ~env=Language.Builtins.env_init,
      elab,
    );
  (
    result,
    Language.EvaluatorState.get_probes(state),
    Language.EvaluatorState.get_incr_eval(state),
  );
};

let incr_samples_case = (~passes: int, msg: string) => {
  let settings = {
    ...Language.CoreSettings.on,
    probe_all: true,
  };
  /* Mirrors setCell: nested per-row/per-cell lambdas comparing indices. */
  let prog = "let row = 1 in let f = fun x -> if x == row then 1 else 2 in [f(1), f(2), f(3)]";
  let exp = parse_exp(prog);
  let (_, samples1, incr1) = eval_incr_probes(~settings, exp);
  check_sample_values(msg ++ " (pass 1 pristine)", samples1);
  let broken = replace_var(~from="row", ~to_="col", exp);
  let (_, samples2, incr2) =
    eval_incr_probes(~settings, ~prev=incr1, broken);
  check_sample_values(msg ++ " (pass 2 broken)", samples2);
  if (passes >= 3) {
    let (_, samples3, _) = eval_incr_probes(~settings, ~prev=incr2, exp);
    check_sample_values(msg ++ " (pass 3 restored)", samples3);
  };
};

let sample_tests = [
  test_case("samples: pristine/broken incremental", `Quick, () =>
    incr_samples_case(~passes=2, "samples 2-pass")
  ),
  test_case("samples: break then restore (undo)", `Quick, () =>
    incr_samples_case(~passes=3, "samples 3-pass")
  ),
];

let tests = {
  let (name, cases) = tests;
  (name, cases @ sample_tests);
};

/* ── Full-fidelity action-layer repro on the (reduced) task-30 program ──
 * Drives the exact witnessed edit sequences through Perform.go on a
 * structurally faithful planting-bug program, checking after EVERY
 * action:
 *   1. no duplicated (id, shard) pairs anywhere in the zipper;
 *   2. MakeTerm/zip does not raise (source reassembly);
 *   3. probe_all incremental eval (threading ~prev like the live
 *      editor) yields samples whose printed values reassemble.
 */

let pairs_of_siblings = ((l, r): Siblings.t) =>
  id_shard_pairs(l) @ id_shard_pairs(r);

let pairs_of_ancestor = (a: Ancestor.t) => {
  let own = List.map(i => (a.id, i), fst(a.shards) @ snd(a.shards));
  let kids =
    List.concat_map(id_shard_pairs, fst(a.children) @ snd(a.children));
  own @ kids;
};

let zipper_pairs = (z: Zipper.t): list((Id.t, int)) =>
  id_shard_pairs(z.selection.content)
  @ pairs_of_siblings(z.relatives.siblings)
  @ List.concat_map(
      ((a, sibs)) => pairs_of_ancestor(a) @ pairs_of_siblings(sibs),
      z.relatives.ancestors,
    );

let zipper_dups = (z: Zipper.t): list((Id.t, int)) => {
  let pairs = zipper_pairs(z);
  let tbl = Hashtbl.create(256);
  List.filter(
    pair => {
      let seen = Hashtbl.mem(tbl, pair);
      Hashtbl.replace(tbl, pair, ());
      seen;
    },
    pairs,
  );
};

let task30 = {xyz|type Plant = String in
type Grove = [[Plant]] in
type Row = Int in
type Col = Int in
type Model = (
  grove = Grove,
  currentSeed = Plant,
  seedInventory = [Plant]
) in
type Action =
  + SelectSeed(Int)
  + PlantSeed(Row, Col)
  + ClearGrove
in
let init: Model = (
  grove = [
    ["", "", ""],
    ["", "", ""],
    ["", "", ""]
  ],
  currentSeed = "s",
  seedInventory = ["s", "t", "u"]
) in
let setCell(grove: Grove, row: Row, _col: Col, plant: Plant): Grove =
  mapi(grove, fun (i, r) ->
    if i == row
    then mapi(r, fun (j, c) ->
      if j == row
      then plant
      else c)
    else r)
in
let updateGrove(m: Model, f: Grove -> Grove): Model =
  (
    grove = f(m.grove),
    currentSeed = m.currentSeed,
    seedInventory = m.seedInventory
  )
in
let update(m: Model, action: Action): Model =
  case action
  | SelectSeed(idx) =>
      (
        grove = m.grove,
        currentSeed = nth(m.seedInventory, idx),
        seedInventory = m.seedInventory
      )
  | PlantSeed(row, col) =>
      updateGrove(m, fun g -> setCell(g, row, col, m.currentSeed))
  | ClearGrove => init
  end
in
let run(init: Model, actions: [Action]): Model =
  fold_left(actions, fun (m, a) -> update(m, a), init)
in
run(init, [SelectSeed(1), PlantSeed(1, 2)])|xyz};

/* Locate the `row` token on the seeded-bug line (`if j == row`). */
let find_bug_token = (prog: string): (int, int) => {
  let lines = String.split_on_char('\n', prog);
  let rec go = (n, ls) =>
    switch (ls) {
    | [] => Alcotest.fail("bug line not found")
    | [l, ...rest] =>
      switch (Str.search_forward(Str.regexp_string("if j == row"), l, 0)) {
      | _ =>
        let idx = Str.search_forward(Str.regexp_string("row"), l, 0);
        (n, idx);
      | exception Not_found => go(n + 1, rest)
      }
    };
  go(0, lines);
};

let check_zipper = (msg: string, z: Zipper.t) => {
  let dups = zipper_dups(z);
  if (dups != []) {
    fail(
      msg
      ++ ": zipper contains duplicated (id, shard) pairs: "
      ++ String.concat(
           ", ",
           List.map(
             ((id, i)) => Id.to_string(id) ++ "#" ++ string_of_int(i),
             dups,
           ),
         ),
    );
  };
  /* Source reassembly as run by calculate (MakeTerm). */
  switch (MakeTerm.from_zip_for_sem(z, ~root=Exp)) {
  | _ => ()
  | exception (Failure(f)) => fail(msg ++ ": MakeTerm raised: " ++ f)
  | exception (Assert_failure(_)) =>
    fail(msg ++ ": MakeTerm raised Assert_failure")
  };
};

let action_fidelity_case = (msg: string, edit_actions: list(Action.t)) => {
  let settings = {
    ...Language.CoreSettings.on,
    probe_all: true,
  };
  let z0 =
    switch (Haz3lcore.Parser.to_zipper(~root=Exp, task30)) {
    | Some(z) => z
    | None => Alcotest.fail("task30 failed to parse")
    };
  let (bug_row, bug_col) = find_bug_token(task30);
  /* Selection over the `row` token, as every witness had. */
  let setup = [
    Test_Editing.move_point(~row=bug_row, ~col=bug_col, ()),
    Test_Editing.resize_point(~row=bug_row, ~col=bug_col + 3, ()),
  ];
  let steps = setup @ edit_actions;
  let prev = ref(Language.IncrEval.empty);
  let _final =
    List.fold_left(
      (z, a) => {
        let z' = Test_Editing.perform(z, [a]);
        let step_msg = msg ++ " after " ++ Action.show(a);
        check_zipper(step_msg, z');
        /* Live-editor-style recompute: probe_all eval threading prev. */
        switch (MakeTerm.from_zip_for_sem(z', ~root=Exp).term) {
        | exp =>
          switch (eval_incr_probes(~settings, ~prev=prev^, exp)) {
          | (_, samples, incr) =>
            prev := incr;
            check_sample_values(step_msg, samples);
          | exception _ => () /* eval failures are fine; crash is in display */
          }
        };
        z';
      },
      z0,
      steps,
    );
  ();
};

/* ── Drawer-path variants (EXACT app display pipeline) ──────────────────
 * DrawerHeight.sample_rows (ProbeProj.re) is the code that actually runs
 * during Calculate: term_to_seg(~inline=false) on strip_ascriptions'd
 * sample values, PrettySegment.prettify at the drawer width, then
 * Measured.of_segment. The cases above only exercised the INLINE form.
 * New ingredient: RECURSION. A recursive call stuck on a free variable
 * (the mid-fix `col` state) embeds the recursive closure (fixpoint) in
 * the stuck sample value; with fold_fn_bodies=`NoFold the printer walks
 * the closure body — any un-freshened self-reference or repeated
 * unrolling duplicates source ids inside ONE printed segment. */

let drawer_settings = {
  ...ExpToSegment.Settings.of_core(~inline=false, Language.CoreSettings.off),
  show_unknown_as_hole: false,
  fold_fn_bodies: `NoFold,
  project_tables: false,
};

let drawer_seg_of_value = (v: Language.Exp.t): Segment.t =>
  ExpToSegment.any_to_segment(
    ~settings=drawer_settings,
    Exp(v |> Language.DHExp.strip_ascriptions),
  );

let check_sample_values_drawer = (msg: string, probes: Language.Sample.Map.t) => {
  let all: list(Language.Sample.t) =
    Id.Map.fold((_, ss, acc) => acc @ ss, probes, []);
  List.iteri(
    (n, sample: Language.Sample.t) => {
      let seg = drawer_seg_of_value(sample.value);
      let dups = duplicate_pairs(seg);
      if (dups != []) {
        fail(
          msg
          ++ ": sample #"
          ++ string_of_int(n)
          ++ " (syntax_id "
          ++ Id.to_string(sample.syntax_id)
          ++ ") drawer segment has duplicated (id, shard) pairs: "
          ++ String.concat(
               ", ",
               List.map(
                 ((id, i)) => Id.to_string(id) ++ "#" ++ string_of_int(i),
                 dups,
               ),
             )
          ++ "\nvalue seg: "
          ++ print_seg(seg),
        );
      };
      /* Full DrawerHeight.sample_rows equivalent: prettify at the
       * default drawer width, then the Measured row-count walk. */
      switch (PrettySegment.prettify(~width=30, seg)) {
      | pretty =>
        switch (
          Measured.of_segment(
            pretty,
            ProjectorCore.Shape.Map.empty,
            Id.Map.empty,
          )
        ) {
        | _ => ()
        | exception (Failure(f)) =>
          fail(
            msg
            ++ ": sample #"
            ++ string_of_int(n)
            ++ " Measured raised: "
            ++ f,
          )
        | exception (Assert_failure(_)) =>
          fail(
            msg
            ++ ": sample #"
            ++ string_of_int(n)
            ++ " Measured raised Assert_failure",
          )
        }
      | exception (Failure(f)) =>
        fail(
          msg
          ++ ": sample #"
          ++ string_of_int(n)
          ++ " drawer prettify raised: "
          ++ f,
        )
      | exception (Assert_failure(_)) =>
        fail(
          msg
          ++ ": sample #"
          ++ string_of_int(n)
          ++ " drawer prettify raised Assert_failure (Tile.re invariant)",
        )
      };
    },
    all,
  );
};

/* Recursive helper (annotated-let recursion, as the study tasks use)
 * mapped over a list, with the comparison variable UNBOUND — the exact
 * transient the witnesses were in after deleting `row` / typing `col`. */
let rec_stuck_prog = "let walk(l: [Int]): [Int] =
  case l
  | [] => []
  | x :: xs => (if x == col then 9 else x) :: walk(xs)
  end
in walk([1, 2, 3])";

/* Same but probing a FUNCTION value so a sample.value IS a recursive
 * closure (fixpoint + env), the drawer state mei had (<function> in env). */
let rec_closure_prog = "let walk(l: [Int]): [Int] =
  case l
  | [] => []
  | x :: xs => x :: walk(xs)
  end
in (walk, walk([1, 2, col]))";

let drawer_case = (~passes: int, ~prog: string, msg: string) => {
  let settings = {
    ...Language.CoreSettings.on,
    probe_all: true,
  };
  let exp = parse_exp(prog);
  let (result1, samples1, incr1) = eval_incr_probes(~settings, exp);
  check_result_seg(msg ++ " (pass 1 result)", result1);
  check_sample_values_drawer(msg ++ " (pass 1 samples)", samples1);
  if (passes >= 2) {
    /* Incremental re-eval with adoption from the broken pass — the
     * live editor always threads ~prev. */
    let (result2, samples2, _) =
      eval_incr_probes(~settings, ~prev=incr1, exp);
    check_result_seg(msg ++ " (pass 2 result)", result2);
    check_sample_values_drawer(msg ++ " (pass 2 samples)", samples2);
  };
};

/* ── THE RED TEST: duplicate-id value through the display path ──────────
 * All synchronous producers tested above freshen correctly, but the field
 * crash proves SOME producer delivers values with repeated ids (stale
 * worker results / adoption / any future producer). The display layer
 * must not die on them: it is a pretty-printer, not an invariant checker.
 * This grafts the same subterm twice into one value (ids shared — exactly
 * what the crash dumps show: out-of-order shards [0,1,2,0,1,2,...]) and
 * requires the display path to survive. RED until ExpToSegment de-dupes
 * repeated tile ids at its public exits. */

let grafted_dup_value = (): Language.Exp.t => {
  /* An if/then/else = a 3-shard tile, matching the field dump's
   * [0,1,2,0,1,2,...] signature. The SAME term object twice = shared ids. */
  let t = parse_exp("if 1 == 2 then 3 else 4");
  Language.Exp.fresh(Tuple([t, t]));
};

let dup_display_case = (msg: string) => {
  let v = grafted_dup_value();
  /* Result-view path */
  let seg = exp_to_segment(v);
  let dups = duplicate_pairs(seg);
  if (dups != []) {
    fail(
      msg
      ++ ": result segment still contains duplicated (id, shard) pairs after "
      ++ "printing: "
      ++ String.concat(
           ", ",
           List.map(
             ((id, i)) => Id.to_string(id) ++ "#" ++ string_of_int(i),
             dups,
           ),
         ),
    );
  };
  switch (Segment.reassemble(seg)) {
  | _ => ()
  | exception (Failure(f)) => fail(msg ++ ": reassemble raised: " ++ f)
  | exception (Assert_failure(_)) =>
    fail(msg ++ ": reassemble raised Assert_failure (Tile.re invariant)")
  };
  /* Drawer path (the "Exception during Calculate" stack) */
  let dseg = drawer_seg_of_value(v);
  let ddups = duplicate_pairs(dseg);
  if (ddups != []) {
    fail(msg ++ ": drawer segment contains duplicated (id, shard) pairs");
  };
  switch (PrettySegment.prettify(~width=30, dseg)) {
  | _ => ()
  | exception (Failure(f)) => fail(msg ++ ": drawer prettify raised: " ++ f)
  | exception (Assert_failure(_)) =>
    fail(msg ++ ": drawer prettify raised Assert_failure")
  };
};

let drawer_tests = [
  test_case("drawer: recursive walk stuck on free col", `Quick, () =>
    drawer_case(~passes=2, ~prog=rec_stuck_prog, "drawer rec-stuck")
  ),
  test_case("drawer: recursive closure as sample value", `Quick, () =>
    drawer_case(~passes=2, ~prog=rec_closure_prog, "drawer rec-closure")
  ),
  test_case(
    "REGRESSION: grafted duplicate-id value survives display", `Quick, () =>
    dup_display_case("grafted dup ids")
  ),
];

let tests = {
  let (name, cases) = tests;
  (name, cases @ drawer_tests);
};

let fidelity_tests = [
  test_case("task30: select row, type col (mei/P3)", `Quick, () =>
    action_fidelity_case(
      "select+type",
      [Action.Insert("c"), Action.Insert("o"), Action.Insert("l")],
    )
  ),
  test_case("task30: select row, arrow right (William)", `Quick, () =>
    action_fidelity_case(
      "select+arrow",
      [Action.Move(Local(Right, ByChar))],
    )
  ),
  test_case("task30: select, delete, retype, rapid delete (P3)", `Quick, () =>
    action_fidelity_case(
      "select+del+retype+del",
      [
        Action.Destruct(Left),
        Action.Insert("c"),
        Action.Insert("o"),
        Action.Insert("l"),
        Action.Destruct(Left),
        Action.Destruct(Left),
      ],
    )
  ),
];

let tests = {
  let (name, cases) = tests;
  (name, cases @ fidelity_tests);
};

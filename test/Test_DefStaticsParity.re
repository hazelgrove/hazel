open Alcotest;
open Haz3lcore;
open Language;

/* after each edit step, DefStatics computed from the previous step matches
   a cold calc and monolithic statics, and seeded evaluation a cold one */

let settings = CoreSettings.on;

type step = {
  edit: (~root: Sort.t, Segment.t) => Segment.t,
  /* items and members the step must re-analyze */
  analyzed: option(int),
  /* the step must change the program's value, or a stale seeded
     evaluation would pass */
  changes_value: bool,
};

let step = edit => {
  edit,
  analyzed: None,
  changes_value: false,
};
let analyzed = (n, s) => {
  ...s,
  analyzed: Some(n),
};
let changes_value = s => {
  ...s,
  changes_value: true,
};

let noop = step((~root as _, seg) => seg);

/* see CorpusUtil.edit_token */
let token = (~at=?, ~keep_id=?, needle, repl) =>
  step((~root as _, seg) =>
    switch (CorpusUtil.edit_token(~at?, ~keep_id?, ~needle, ~repl, seg)) {
    | (seg, true) => seg
    | (_, false) => fail("no token " ++ needle)
    }
  );

let op = (op: Web.OutlineEdit.def_op, path) =>
  step((~root, seg) =>
    switch (
      Web.ItemEdit.apply(
        ~mod_root=root == Sort.Mod,
        op,
        CorpusUtil.row(MakeTerm.Incr.term_of_root(~root, seg), path),
        seg,
      )
    ) {
    | Some((seg, _)) => seg
    | None => fail("op failed at " ++ String.concat("/", path))
    }
  );

/* an outline move (Alt↑↓), which can carry a member across a module edge */
let move = (~up, path) =>
  step((~root, seg) => {
    let term = MakeTerm.Incr.term_of_root(~root, seg);
    let ctx: Web.ItemEdit.ctx = {
      mod_root: root == Sort.Mod,
      term,
      info_map: lazy(Statics.Map.empty),
      is_open: _ => true,
    };
    switch (
      Web.ItemEdit.edit(
        ctx,
        Op(up ? MoveUp : MoveDown, CorpusUtil.row(term, path)),
        seg,
      )
    ) {
    | Ok((seg, _)) => seg
    | Error(why) =>
      fail("move failed at " ++ String.concat("/", path) ++ ": " ++ why)
    };
  });

let evaluate = (~prev=IncrEval.empty, info_map, elab) =>
  Evaluator.evaluate(
    ~prev,
    ~eval_info=
      EvalInfo.of_info_map(
        ~probe_all=settings.probe_all,
        ~targets=Id.Map.empty,
        info_map,
      ),
    ~env=Builtins.env_init,
    elab,
  );

let graft = (ds: DefStatics.t) =>
  switch (DefStatics.whole_elab(ds)) {
  | Some(elab) => elab
  | None => fail("whole_elab: shape gap")
  };

let tests_of = (state: EvaluatorState.t) =>
  List.sort(
    compare,
    List.map(
      ((id, rs)) =>
        (Id.show(id), TestStatus.show(TestMap.joint_status(rs))),
      state.tests,
    ),
  );

let script = (~root=Sort.Exp, ~eval=true, src: string, steps: list(step), ()) => {
  let calc = (~prev=?, seg) =>
    DefStatics.calc(
      ~settings,
      ~prev?,
      MakeTerm.Incr.term_of_root(~root, seg),
    );
  /* checks step [i]'s result, threading the seeded cache and the value */
  let exact = (i, s: step, ds: DefStatics.t, (cache, value)) => {
    let name = "step " ++ string_of_int(i);
    Option.iter(
      n => check(int, name ++ " analyzed", n, DefStatics.last_analyzed^),
      s.analyzed,
    );
    check(
      list(string),
      name,
      [],
      DefStaticsCheck.divergences(~settings, ds),
    );
    if (eval) {
      let elab = graft(ds);
      let (v, state) = evaluate(~prev=cache, ds.merged, elab);
      let (cold, state_cold) = evaluate(ds.merged, elab);
      let same = Exp.fast_equal_with_lexemes;
      check(bool, name ++ " seeded value", true, same(cold, v));
      check(
        list(pair(string, string)),
        name ++ " seeded tests",
        tests_of(state_cold),
        tests_of(state),
      );
      if (i == 0) {
        let (mono_map, mono_elab) =
          Statics.mk_unmemoized(settings, DefStatics.ctx0, ds.term);
        check(
          bool,
          "the graft evaluates as the monolithic elaboration",
          true,
          Exp.fast_equal(fst(evaluate(mono_map, mono_elab)), cold),
        );
      };
      Option.iter(
        prev =>
          if (s.changes_value) {
            check(
              bool,
              name ++ " changes the value",
              false,
              same(prev, cold),
            );
          },
        value,
      );
      (state.incr_eval, Some(cold));
    } else {
      (cache, value);
    };
  };
  let seg = CorpusUtil.seg_of(~root, src);
  let ds = calc(seg);
  /* step 0: the cold calc */
  let threaded = exact(0, noop, ds, (IncrEval.empty, None));
  ignore(
    List.fold_left(
      ((i, seg, prev, threaded), s: step) => {
        let seg = s.edit(~root, seg);
        let ds = calc(~prev, seg);
        (i + 1, seg, ds, exact(i, s, ds, threaded));
      },
      (1, seg, ds, threaded),
      steps,
    ),
  );
};

let mega = (~root, ~eval=true, ~wrap=src => src, file, steps, ()) =>
  switch (CorpusUtil.mega_src(file)) {
  | Some(src) => script(~root, ~eval, wrap(src), steps, ())
  | None => fail("corpus unreadable: " ++ file)
  };

/* the slide corpus through calc_auto, the app's path, so the check the
   runner installs compares each result with monolithic statics: cold,
   then warm after an edit to every `1` */
let corpus_through_calc_auto = () => {
  let checked = ref(0);
  List.iter(
    ((path, src)) =>
      switch (CorpusUtil.parse(src)) {
      | None => ()
      | Some(seg) =>
        let calc = seg =>
          try(
            ignore(
              DefStatics.calc_auto(
                ~settings=CoreSettings.on,
                MakeTerm.Incr.term_of(seg),
              ),
            )
          ) {
          | DefStaticsCheck.Divergence(ds) =>
            fail(path ++ ": " ++ String.concat("; ", ds))
          };
        calc(seg);
        let (edited, changed) =
          CorpusUtil.edit_token(~needle="1", ~repl="2", seg);
        if (changed) {
          calc(edited);
        };
        incr(checked);
      },
    CorpusUtil.slide_corpus(),
  );
  check(bool, "the corpus was there to check", true, checked^ >= 40);
};

/* module-rooted documents keep their own cache slots: the wrapper's id,
   the slot key, differs per document */
let separate_slots = () => {
  let term = src =>
    MakeTerm.Incr.term_of_mod(CorpusUtil.seg_of(~root=Mod, src));
  let (a, b) = (term("let x = 1;\nx"), term("let y = true;\ny"));
  check(
    bool,
    "two documents, two keys",
    false,
    Exp.rep_id(a) == Exp.rep_id(b),
  );
  let ta = DefStatics.calc_auto(~settings=CoreSettings.on, a);
  ignore(DefStatics.calc_auto(~settings=CoreSettings.on, b));
  check(
    bool,
    "the first document's calc is still cached",
    true,
    switch (DefStatics.cached(a)) {
    | Some(t) => t === ta
    | None => false
    },
  );
};

/* the app's path (init_compositional_term, through calc_auto) finds the
   document's slot, so a member edit re-analyzes just the module item and
   the member */
let app_path_reuses_its_slot = () => {
  let seg =
    CorpusUtil.seg_of(
      "module M = {\n  type Model = (wood = Int, resin = Int);\n  let init : Model = (wood = 16, resin = 0)\n} in\nlet u = M.init in\nu",
    );
  let (edited, _) = CorpusUtil.edit_token(~needle="16", ~repl="17", seg);
  let init = seg =>
    ignore(
      CachedStatics.init_compositional_term(
        ~settings,
        ~probe_ids=Id.Map.empty,
        MakeTerm.Incr.term_of(seg),
      ),
    );
  init(seg);
  init(edited);
  check(int, "item + 1 member re-analyzed", 2, DefStatics.last_analyzed^);
};

/* an item never flags its own top-level binders unused (its body hole
   could use them), so top-level unused warnings are unused_binders' */
let items_leave_binders_unflagged = () => {
  let check_calc = (what, term) => {
    let ds = DefStatics.calc(~settings, term);
    let binders =
      List.concat_map(
        (it: DefStatics.item) =>
          List.concat_map(
            fun
            | Ctx.VarEntry({id, _}) => [
                id,
                ...DefStatics.fun_wrapper_ids(it.d_node, id),
              ]
            | _ => [],
            it.d_exports,
          ),
        ds.items,
      );
    let flagged =
      List.concat_map(
        (it: DefStatics.item) =>
          List.filter(id => List.mem(id, binders), it.d_warning_ids),
        ds.items,
      );
    check(list(string), what, [], List.map(Id.show, flagged));
  };
  let exp = src => MakeTerm.Incr.term_of(CorpusUtil.seg_of(src));
  List.iter(
    ((what, src)) => check_calc(what, exp(src)),
    [
      (
        "unused definitions",
        "let f(x : Int) : Bool = x > 0 in\nlet g(y) = y in\nlet (a, b) = (1, 2) in\n1",
      ),
      (
        "an unused module",
        "module M = {\n  let x = 1;\n  let y = 2\n} in\n3",
      ),
      ("an unused module literal", "let m = {\n  let x = 1\n} in\n3"),
    ]
    @ CorpusUtil.incomplete_programs,
  );
  check_calc(
    "a module root",
    MakeTerm.Incr.term_of_mod(
      CorpusUtil.seg_of(
        ~root=Mod,
        "let a = 1;\nmodule M = {\n  let x = 1\n};\nlet f(x) = x;\n2",
      ),
    ),
  );
  List.iter(
    ((path, src)) =>
      Option.iter(
        seg => check_calc(path, MakeTerm.Incr.term_of(seg)),
        CorpusUtil.parse(src),
      ),
    CorpusUtil.slide_corpus(),
  );
  List.iter(
    ((root, file)) =>
      check_calc(
        file,
        MakeTerm.Incr.term_of_root(~root, CorpusUtil.mega_seg(~root, file)),
      ),
    [(Sort.Exp, "mega-1k.hz"), (Sort.Mod, "mega-mod-1k.hz")],
  );
};

/* with a type error and a member using an earlier binding */
let mod_root_src = "let x = 1;
type T = Int;
let bad : String = 42;
module M = {
  let y = x + 16
};
x + 1";

let mod_root_items = () => {
  let ds =
    DefStatics.calc(
      ~settings,
      MakeTerm.Incr.term_of_mod(CorpusUtil.seg_of(~root=Mod, mod_root_src)),
    );
  check(int, "5 mod items + the exports tail", 6, List.length(ds.items));
};

/* the spine root's elab_term is the whole suffix: incremental eval reuses
   a node whose elab_term is unchanged, so a hollow root would hide edits */
let spine_elab_is_whole_suffix = () => {
  let parse_exp = src => MakeTerm.Incr.term_of(CorpusUtil.seg_of(src));
  let t0 = parse_exp("let junk = 1 in\nlet x = 2 in\nx");
  let ds0 = DefStatics.calc(~settings, t0);
  let top_elab = (ds: DefStatics.t, t: Exp.t) =>
    switch (
      Statics.Map.lookup_exp(Exp.rep_id(DefStatics.strip(t)), ds.merged)
    ) {
    | Some({elab_term, _}) => elab_term
    | None => fail("no root info")
    };
  check(
    bool,
    "root elab_term is the whole program's elab",
    true,
    Exp.fast_equal(top_elab(ds0, t0), graft(ds0)),
  );
  let t1 = parse_exp("let junk = 1 in\nlet x = 3 in\nx");
  let ds1 = DefStatics.calc(~settings, ~prev=ds0, t1);
  check(
    bool,
    "editing item 2 changes the root's elab_term",
    false,
    Exp.fast_equal(top_elab(ds0, t0), top_elab(ds1, t1)),
  );
};

/* a probe in a fn body called only from a later item samples under
   compositional statics as under monolithic (fresh evaluations) */
let probe_capture_parity = (): unit => {
  let term =
    MakeTerm.go(
      CorpusUtil.seg_of("let f = fun q -> q + 1 in\nlet z = f(5) in\nz"),
    ).
      term;
  let (map0, _) = Statics.mk_unmemoized(settings, DefStatics.ctx0, term);
  let q_id =
    switch (
      Id.Map.fold(
        (id, info, acc) =>
          switch (acc, info) {
          | (None, Info.InfoExp({user_term: {term: Var("q"), _}, _})) =>
            Some(id)
          | _ => acc
          },
        map0,
        None,
      )
    ) {
    | Some(id) => id
    | None => fail("no use of q")
    };
  let probe_ids = Id.Map.singleton(q_id, ());
  let captured = (info_map, elab) => {
    let targets =
      CachedStatics.compute_targets(~settings, ~info_map, ~probe_ids, ());
    let eval_info =
      EvalInfo.of_info_map(~probe_all=false, ~targets, info_map);
    let (_, state) =
      Evaluator.evaluate(~eval_info, ~env=Builtins.env_init, elab);
    Id.Map.cardinal(EvaluatorState.get_probes(state));
  };
  let (map_m, elab_m) =
    Statics.mk_unmemoized(~probe_ids, settings, DefStatics.ctx0, term);
  let ds0 = DefStatics.calc(~settings, term);
  let ds = DefStatics.calc(~settings, ~prev=ds0, ~probe_ids, term);
  check(
    bool,
    "probe toggle re-analyzes a strict subset",
    true,
    DefStatics.last_analyzed^ < List.length(ds.items),
  );
  let root = Exp.rep_id(term);
  /* eval reuse keys on probe_targets: a root missing the witness would
     replay without samples */
  let witnessed = (map, id) =>
    switch (Statics.Map.lookup_exp(id, map)) {
    | Some(info) =>
      !SubexpProbeTargets.equal(info.probe_targets, SubexpProbeTargets.empty)
    | None => fail("no info at " ++ Id.show(id))
    };
  List.iter(
    id =>
      check(
        bool,
        "witness as monolithic",
        witnessed(map_m, id),
        witnessed(ds.merged, id),
      ),
    [root, q_id],
  );
  /* guards against the spine patch re-reading its own output */
  let root_co_size = (t: DefStatics.t) =>
    switch (Statics.Map.lookup_exp(root, t.merged)) {
    | Some(info) =>
      CoCtx.to_list(info.co_ctx)
      |> List.fold_left((n, (_, es)) => n + List.length(es), 0)
    | None => (-1)
    };
  let ds_i1 = DefStatics.calc(~settings, ~prev=ds, ~probe_ids, term);
  let ds_i2 = DefStatics.calc(~settings, ~prev=ds_i1, ~probe_ids, term);
  let ds_i3 = DefStatics.calc(~settings, ~prev=ds_i2, ~probe_ids, term);
  check(
    int,
    "spine patch idempotent (co_ctx does not grow)",
    root_co_size(ds_i1),
    root_co_size(ds_i3),
  );
  let ds_off = DefStatics.calc(~settings, ~prev=ds, term);
  check(
    bool,
    "toggle-off clears the root witness",
    false,
    witnessed(ds_off.merged, root),
  );
  check(
    bool,
    "compositional probe capture parity",
    captured(map_m, elab_m) > 0,
    captured(ds.merged, graft(ds)) > 0,
  );
};

/* a probe toggled in a ModExp chain's link re-analyzes that link (with
   its member and module), witnesses as monolithic statics does, and
   samples under a fresh evaluation */
let modexp_probe_parity = (): unit => {
  let term =
    MakeTerm.Incr.term_of(
      CorpusUtil.seg_of(
        "module M = {\n  let f = fun q -> q + 1 in\n  let z = f(5) in\n  test z == 6 end;\n  let c = 3\n} in\nM.c",
      ),
    );
  let ds0 = DefStatics.calc(~settings, term);
  let q_id =
    switch (
      Id.Map.fold(
        (id, info, acc) =>
          switch (acc, info) {
          | (None, Info.InfoExp({user_term: {term: Var("q"), _}, _})) =>
            Some(id)
          | _ => acc
          },
        ds0.merged,
        None,
      )
    ) {
    | Some(id) => id
    | None => fail("no use of q")
    };
  let probe_ids = Id.Map.singleton(q_id, ());
  let ds = DefStatics.calc(~settings, ~prev=ds0, ~probe_ids, term);
  check(int, "M, the member and f's link", 3, DefStatics.last_analyzed^);
  check(
    list(string),
    "parity",
    [],
    DefStaticsCheck.divergences(~settings, ds),
  );
  let samples = (info_map, elab) => {
    let targets =
      CachedStatics.compute_targets(~settings, ~info_map, ~probe_ids, ());
    let eval_info =
      EvalInfo.of_info_map(~probe_all=false, ~targets, info_map);
    let (_, state) =
      Evaluator.evaluate(~eval_info, ~env=Builtins.env_init, elab);
    Id.Map.cardinal(EvaluatorState.get_probes(state));
  };
  let (map_m, elab_m) =
    Statics.mk_unmemoized(~probe_ids, settings, DefStatics.ctx0, term);
  let mono = samples(map_m, elab_m);
  check(bool, "the probe samples", true, mono > 0);
  check(int, "samples as monolithic", mono, samples(ds.merged, graft(ds)));
};

/* a ModExp chain moved below what its link reads, ids kept (the outline
   moves its `let … in` lines apart, so the items are reordered here): its
   links can't be reused from above the move */
let modexp_chain_moved = (): unit => {
  let term =
    MakeTerm.Incr.term_of_mod(
      CorpusUtil.seg_of(
        ~root=Mod,
        "let a = k + 1 in\ntest a == 2 end;\nlet k = 1;\nk",
      ),
    );
  let ds0 = DefStatics.calc(~settings, term);
  let moved: Exp.t =
    switch (term.term) {
    | Module([chain, k, ...rest]) => {
        ...term,
        term: Module([k, chain, ...rest]),
      }
    | _ => fail("not a module root")
    };
  let ds = DefStatics.calc(~settings, ~prev=ds0, moved);
  check(
    list(string),
    "parity",
    [],
    DefStaticsCheck.divergences(~settings, ds),
  );
};

/* small programs: outline edits, and names resolved across items */
let small = [
  test_case(
    "deleting a member dirties its users",
    `Quick,
    script(
      "module M = {\n  let x = 1;\n  let y = 2\n} in\nM.x + M.y",
      [op(Delete, ["M", "x"])],
    ),
  ),
  test_case(
    "deleting a member its sibling uses",
    `Quick,
    script(
      "module M = {\n  let x = 1;\n  let y = x + 1\n} in\nM.y",
      [op(Delete, ["M", "x"])],
    ),
  ),
  test_case(
    "moving a member below its user",
    `Quick,
    script(
      "module M = {\n  let x = 1;\n  let y = 2;\n  let z = x + y\n} in\nM.z",
      [op(MoveDown, ["M", "x"]), op(MoveDown, ["M", "x"])],
    ),
  ),
  test_case(
    "moving a member above its dependency",
    `Quick,
    script(
      "module M = {\n  let x = 1;\n  let y = x + 1\n} in\nM.y",
      [op(MoveUp, ["M", "y"])],
    ),
  ),
  test_case(
    "an item moving above its neighbor",
    `Quick,
    script(
      "let a = 1 in\nlet b = 2 in\nlet c = a + b in\nc",
      [op(MoveUp, ["c"]), op(MoveUp, ["c"])],
    ),
  ),
  test_case(
    "items moved, deleted, duplicated and inserted",
    `Quick,
    script(
      "let a = 1 in\nlet b = a + 1 in\nlet c = b + a in\nlet d = 7 in\nc + d",
      [
        noop |> analyzed(0),
        /* b, and c, which mentions it */
        op(MoveDown, ["b"]) |> analyzed(2),
        /* only the tail mentions d */
        op(Delete, ["d"]) |> analyzed(1),
        /* the copy, and a's users b and c */
        op(Duplicate, ["a"]) |> analyzed(3),
        /* an unrelated def: just itself */
        op(NewBelow, ["c"]) |> analyzed(1),
      ],
    ),
  ),
  test_case(
    "top-level stepper filter",
    `Quick,
    script(
      "let x = 1 in\neval x in\nlet y = x + 1 in\ny",
      [token("1", "5")],
    ),
  ),
  test_case(
    "constructor resolves to the latest sum",
    `Quick,
    script(
      "type A = Foo + Bar in\nlet v = Foo in\ntype B = Qux + Baz in\nlet w = Foo in\nw",
      [token("Qux", "Foo")],
    ),
  ),
  test_case(
    "a module named like a constructor",
    `Quick,
    script(
      "type A = Foo + Bar in\nmodule Baz = {\n  let x = 1\n} in\nlet w = Foo in\nw",
      [token("Baz", "Foo"), token("Foo", "Qux")],
    ),
  ),
  test_case(
    "members moved across module edges",
    `Quick,
    script(
      ~root=Mod,
      "let a = 1;\nmodule M = {\n  let x = a + 1;\n  let y = x * 2;\n  let z = y + 1\n};\nlet b = M.y;\nb",
      [move(~up=true, ["M", "x"]), move(~up=false, ["M", "z"])],
    ),
  ),
  test_case(
    "a module's last member moved out",
    `Quick,
    script(
      ~root=Mod,
      "let a = 1;\nmodule M = {\n  let x = a + 1\n};\nlet b = 4;\nb",
      [move(~up=false, ["M", "x"])],
    ),
  ),
  test_case(
    "unused binders with a hole in scope",
    `Quick,
    script("let x = 1 in\nlet y = 2 in\n¿", [token("2", "3")]),
  ),
  test_case(
    "an unused function definition",
    `Quick,
    script("let f(x : Int) : Bool = x > 0 in\nlet g(y) = y in\n1", []),
  ),
  test_case(
    "a module used in an alias's definition",
    `Quick,
    script(
      "module M = {\n  type T = Int;\n  let x = 1\n} in\ntype U = M.T in\nlet y : U = 2 in\ny",
      [],
    ),
  ),
  test_case(
    "a module used in an annotation",
    `Quick,
    script(
      "module M = {\n  type T = Int;\n  let x = 1\n} in\nlet y : M.T = 2 in\ny",
      [],
    ),
  ),
  test_case(
    "a module and an alias of one name",
    `Quick,
    script(
      "module Point = {\n  let origin = 75\n} in\ntype Point = (Int, Int) in\nlet p : Int = Point.origin in\np",
      [],
    ),
  ),
  test_case(
    "a module and a constructor of one name",
    `Quick,
    script(
      "module Timer = {\n  type format = Float;\n  let w = 75\n} in\ntype T = Timer(Int) + Other in\nlet v : Timer.format = 1.0 in\nv",
      [],
    ),
  ),
  test_case(
    "a constructor doesn't shadow a dirty type",
    `Quick,
    /* statics only: seeded eval keeps a stale 9 under a retyped
       annotation, a gap of its own (let r : Int = 9 fails alike) */
    script(
      ~eval=false,
      "type Circle = Int in\ntype Shape = Circle(Circle) + Square(Bool) in\nlet r : Circle = 9 in\nr",
      [token("Int", "Bool")],
    ),
  ),
  test_case(
    "an alias doesn't shadow a dirty constructor",
    `Quick,
    script(
      "type S = Cc(Int) + Dd in\ntype S = Bool in\ntype Cc = Bool in\nlet v = Cc(1) in\nv",
      [token("Int", "Float")],
    ),
  ),
  test_case(
    "an alias doesn't shadow a dirty module",
    `Quick,
    script(
      "module Point = {\n  let origin = 9\n} in\ntype Point = (Bool, Bool) in\nlet p = Point.origin + 1 in\np",
      [token("9", "true")],
    ),
  ),
  test_case(
    "a lowercase module aliased by a let",
    `Quick,
    /* statics only, as above */
    script(
      ~eval=false,
      "let m = {\n  type T = Int;\n  let x = 9\n} in\nlet n = m in\nlet y : n.T = 9 in\ny",
      [token("Int", "Bool")],
    ),
  ),
  test_case(
    "deleting a member a later one shadowed",
    `Quick,
    script(
      "module M = {\n  let (y, x) = (1, 2);\n  let x = true;\n  let w = x\n} in\nM.w",
      [op(Delete, ["M", "x"])],
    ),
  ),
  test_case(
    "a module moved past what its member reads",
    `Quick,
    /* k moves above M, so M now sees k, which no dirty name says: M
       and all its members re-analyze, then the tail */
    script(
      "module M = {\n  let x = k + 1;\n  let y = 2\n} in\nlet k = 1 in\nM.x",
      [op(MoveUp, ["k"]) |> analyzed(5)],
    ),
  ),
  test_case(
    "a module moved past what its member reads, at a module root",
    `Quick,
    script(
      ~root=Mod,
      "module M = {\n  let x = k + 1;\n  let y = 2\n};\nlet k = 1;\nM.x",
      [op(MoveUp, ["k"])],
    ),
  ),
  test_case(
    "moving a member past one of the same name",
    `Quick,
    script(
      "module M = {\n  let (y, x) = (0, 0);\n  let x = 1;\n  let x = true;\n  let n = 5;\n  let u = x\n} in\nM.u",
      [op(MoveDown, ["M", "x"])],
    ),
  ),
];

/* a type edit re-analyzes only the items whose types mention it */
let type_deps = [
  test_case(
    "an alias's users",
    `Quick,
    script(
      "type T = [Int] in\nlet a : T = [1] in\nlet b = 2. in\nlet c = a in\ntype U = [T] in\nlet d : U = [[2]] in\nlet e = \"x\" in\nlet g = b +. 1. in\n1",
      /* T, a (annotation), c (a's type mentions T), U (transitive), d
         (annotation U); b, e, g and the tail stay clean */
      [token("Int", "Bool") |> analyzed(5)],
    ),
  ),
  test_case(
    "a shadowing alias stops the cascade",
    `Quick,
    script(
      "type T = [Int] in\nlet a : T = [1] in\ntype T = Bool in\nlet z : T = true in\n9",
      /* the first T and a; the second T shadows it, so z stays clean */
      [token("Int", "Float") |> analyzed(2)],
    ),
  ),
  test_case(
    "a constructor change",
    `Quick,
    script(
      "type S = Aa + Bb in\nlet h : S = Aa in\nlet k = 3 in\ncase h | Aa => 1 | Bb => 2 end",
      /* S's item, h (annotation + ctor use), the trailing case (ctor
         patterns + scrutinee type); k stays clean */
      [token("Bb", "Cc") |> analyzed(3)],
    ),
  ),
  test_case(
    "a member retype",
    `Quick,
    script(
      "module M = {\n  let f : () -> Bool = fun _ -> true;\n  let g = 1\n} in\nlet consume = M.f(()) in\n9",
      /* M, its f member, the exports-tail member, and the consumer */
      [token("Bool", "String") |> analyzed(4)],
    ),
  ),
];

/* a literal edit keeps its id, as typing into a token does: seeded
   evaluation must see the new value through unchanged ids */
let literal_edits = {
  let row = (name, src) =>
    test_case(
      name,
      `Quick,
      script(src, [token(~keep_id=true, "1", "5") |> changes_value]),
    );
  [
    row("a top-level literal", "let a = 1 in\nlet b = a + 10 in\nb"),
    row(
      "a module member's literal",
      "module M = {\nlet a = 1;\nlet b = a + 10\n} in\nM.b",
    ),
    row(
      "a let-bound module literal's",
      "let m = {\nlet a = 1;\nlet b = a + 10\n} in\nm.b",
    ),
    row(
      "a literal upstream of a member",
      "let k = 1 in\nmodule M = {\nlet b = k + 10\n} in\nM.b",
    ),
  ];
};

let module_roots = [
  test_case(
    "a module root",
    `Quick,
    script(
      ~root=Mod,
      mod_root_src,
      [
        /* the module item (a cheap surrogate) and the edited member */
        token("16", "17") |> analyzed(2),
        noop |> analyzed(0),
      ],
    ),
  ),
  test_case(
    "a recursive type export",
    `Quick,
    /* its Rec binder is fresh per analysis: a member edit that keeps the
       exports must not re-analyze the module's users */
    script(
      ~root=Mod,
      "module A = {\n  type L = Nil + Cons(Int, L);\n  let x = 180\n};\nlet u = A.x;\nu",
      [token("180", "181") |> analyzed(2)],
    ),
  ),
];

/* a ModExp member that is a let chain is a level of items: an edit
   re-analyzes the module, the member's wrapper, the edited link and the
   links that use what it changed. a let chain in a definition is one item */
let modexp_chains = [
  test_case(
    "a ModExp member that is a let chain",
    `Quick,
    script(
      "module Holes = {\n  let k = 10;\n  let a = 1 in\n  let b = k + 2 in\n  test a + b == 13 end;\n  let c = 3\n} in\nHoles.c",
      [
        noop |> analyzed(0),
        /* Holes, the member and a's link; the test now fails */
        token(~keep_id=true, "1", "5") |> analyzed(3),
        token("2", "6") |> analyzed(3),
        /* k, then b (the one link that reads it), Holes' exports tail
           and the program's tail, which reads Holes */
        token("10", "false") |> analyzed(6),
        /* the tail link (b goes unused) */
        token(~at=`Last, "b", "a") |> analyzed(3),
        /* a retyped: its link and the tail, which reads a */
        token("5", "true") |> analyzed(4),
      ],
    ),
  ),
  test_case(
    "let chains in definitions stay one item",
    `Quick,
    script(
      "module M = {\n  let _ = let a = 1 in let b = 2 in a + b;\n  let x = let p = 4 in p + 1;\n  let y = x\n} in\nlet _ = let q = 6 in q in\nM.y",
      [
        token(~at=`First, "1", "5") |> analyzed(2),
        token("4", "8") |> analyzed(2),
        token("6", "7") |> analyzed(1),
      ],
    ),
  ),
  test_case(
    "module literals in a ModExp chain",
    `Quick,
    script(
      "module Outer = {\n  let k = 1 in\n  module Inner = {\n    let x = k + 1;\n    let w = 7 in\n    test w == 7 end;\n    let y = 5\n  } in\n  test Inner.x == 2 end;\n  let z = 3\n} in\nOuter.z",
      [
        /* Outer, its member, Inner's link and Inner's y */
        token("5", "6") |> analyzed(4),
        /* ... and Inner's own chain member and w's link */
        token(~at=`First, ~keep_id=true, "7", "8") |> analyzed(5),
        /* k retyped: Inner's link and its x, which reads k */
        token(~at=`First, "1", "true") |> analyzed(5),
      ],
    ),
  ),
  test_case(
    "a ModExp member that isn't a chain",
    `Quick,
    script(
      "module M = {\n  let a = 1;\n  test a == 1 end;\n  a + 4;\n  let b = 2\n} in\nM.b",
      [
        token("4", "5") |> analyzed(2),
        token(~at=`Last, ~keep_id=true, "1", "3") |> analyzed(2),
      ],
    ),
  ),
  test_case(
    "a ModExp chain at a module root",
    `Quick,
    script(
      ~root=Mod,
      "let a = 1 in\nlet b = a + 1 in\ntest b == 2 end;\nlet c = 3;\nc",
      [
        token(~at=`First, ~keep_id=true, "1", "5") |> analyzed(2),
        token("3", "4") |> changes_value |> analyzed(1),
      ],
    ),
  ),
  test_case(
    "mega-1k as one ModExp member",
    `Quick,
    /* the program inside braces: its module chain (with the first test)
       is one ModExp member. an edit costs Mega, that member, the
       WateringTimer link and its selfcheck member */
    /* seeded evaluation at this size costs ~13s; the smaller chain cases
       check it */
    mega(
      ~root=Mod,
      ~eval=false,
      ~wrap=src => "module Mega = {\n" ++ src ++ "\n}",
      "mega-1k.hz",
      [token("180", "181") |> analyzed(4)],
    ),
  ),
];

/* the mega programs: a few edits each, at scale */
let corpus = [
  test_case(
    "mega-1k",
    `Quick,
    mega(
      ~root=Exp,
      "mega-1k.hz",
      [
        /* WateringTimer and its selfcheck member, which this breaks */
        token("180", "181") |> analyzed(2) |> changes_value,
        /* DewLedger, a labeled-tuple Model's module, and its selfcheck */
        token("morning", "evening") |> analyzed(2),
        /* the tail only */
        token(~at=`Last, ~keep_id=true, "9", "8") |> analyzed(1),
        /* retypes a selfcheck that MetaRunner consumes downstream */
        token(~at=`First, ~keep_id=true, "Bool", "String"),
        /* an export's type */
        token(~at=`First, ~keep_id=true, "Int", "Bool"),
        token("75", "\"x\""),
        op(Delete, ["WateringTimer", "format"]),
      ],
    ),
  ),
  test_case(
    "mega-mod-1k",
    `Quick,
    mega(
      ~root=Mod,
      ~eval=false,
      "mega-mod-1k.hz",
      [
        token("180", "181") |> analyzed(2),
        token("75", "\"x\""),
        op(Delete, ["WateringTimer", "format"]),
      ],
    ),
  ),
  test_case(
    "mega-mod-1k in one module",
    `Quick,
    /* a member edit costs about one member, not the whole module: App,
       its WateringTimer member and that one's edited member */
    mega(
      ~root=Mod,
      ~eval=false,
      ~wrap=src => "module App = {\n" ++ src ++ "\n}",
      "mega-mod-1k.hz",
      [token("180", "181") |> analyzed(3)],
    ),
  ),
];

let tests = (
  "DefStaticsParity",
  [
    test_case("module-rooted documents keep apart", `Quick, separate_slots),
    test_case(
      "the app's path reuses its slot",
      `Quick,
      app_path_reuses_its_slot,
    ),
    test_case(
      "the slide corpus through calc_auto",
      `Quick,
      corpus_through_calc_auto,
    ),
    test_case(
      "items leave their binders' unused warnings to the engine",
      `Quick,
      items_leave_binders_unflagged,
    ),
    test_case("a module root's items", `Quick, mod_root_items),
    test_case(
      "spine elab_term is the whole suffix",
      `Quick,
      spine_elab_is_whole_suffix,
    ),
    test_case("probe capture parity", `Quick, probe_capture_parity),
    test_case("probe parity in a ModExp chain", `Quick, modexp_probe_parity),
    test_case("a ModExp chain moved", `Quick, modexp_chain_moved),
  ]
  @ small
  @ type_deps
  @ literal_edits
  @ module_roots
  @ modexp_chains
  @ corpus,
);

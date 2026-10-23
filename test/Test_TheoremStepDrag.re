open Alcotest;
open Util;
open Language;
open Haz3lcore;
open Test_Evaluator_Prelude;
module S = Web.StepperBase;
module D = Web.TheoremStepDrag;
let visual_exp =
  testable(Fmt.using(Exp.show, Fmt.string), D.same_projection);

/* The visible surface includes collapsed function labels, not their hidden
   implementation syntax. Semantic equality alone missed these regressions. */
let rec visible_tokens = (seg: Segment.t): list(string) =>
  seg
  |> List.concat_map((p: Piece.t) =>
       switch (p) {
       | Tile(t) =>
         Aba.mk(t.shards, t.children)
         |> Aba.join(i => [Tile.token(t, i)], visible_tokens)
         |> List.concat
       | Projector(p) when p.kind == Fold => [
           FoldProj.t_of_sexp(Sexplib.Sexp.of_string(p.model)).text,
         ]
       | _ => []
       }
     );
let rec visible_layout = (~measured, seg: Segment.t) =>
  seg
  |> List.concat_map((p: Piece.t) =>
       switch (p) {
       | Tile(t) =>
         Aba.mk(t.shards, t.children)
         |> Aba.join(
              i => {
                let m =
                  Web.CodeFlip.find_meas(measured, Shard(t.id, i))
                  |> Option.get;
                [
                  (
                    Tile.token(t, i),
                    m.origin.row,
                    m.origin.col,
                    m.last.row,
                    m.last.col,
                  ),
                ];
              },
              visible_layout(~measured),
            )
         |> List.concat
       | Projector(p) when p.kind == Fold =>
         let m = Measured.find_by_id(p.id, measured) |> Option.get;
         [
           (
             FoldProj.t_of_sexp(Sexplib.Sexp.of_string(p.model)).text,
             m.origin.row,
             m.origin.col,
             m.last.row,
             m.last.col,
           ),
         ];
       | _ => []
       }
     );
let rec folds = (seg: Segment.t): list(Base.projector) =>
  seg
  |> List.concat_map((p: Piece.t) =>
       switch (p) {
       | Tile(t) => List.concat_map(folds, t.children)
       | Projector(p) when p.kind == Fold => [p]
       | _ => []
       }
     );

let settings =
  Web.Settings.Model.{
    ...init,
    core: {
      ...CoreSettings.on,
      drag_refactor: true,
      evaluation: {
        ...CoreSettings.on.evaluation,
        enable_proof: true,
      },
    },
  };
let render = exp =>
  Web.CodeWithStatics.Model.mk_from_exp(
    ~settings=settings.core,
    ~fold_fn_bodies=`Named,
    ~root=Exp,
    D.for_display(settings.core, exp),
  );
let int_type = Typ.fresh(Atom(Int));
let assoc = "forall (x : Int) -> forall (y : Int) -> forall (z : Int) -> (x + y) + z == x + (y + z)";
let fusion = "forall (items : [Int]) -> forall (f : Int -> Int) -> forall (g : Int -> Int) -> map(map(items, f), g) == map(items, fun x -> g(f(x)))";
let base_ctx = Builtins.ctx_init(Some(Int));

let context =
    (
      ~law=assoc,
      ~bindings=[("a", int_type), ("b", int_type), ("c", int_type)],
      (),
    ) => {
  let ctx = SemanticCtx.of_ctx_and_env(base_ctx, Builtins.env_init);
  let ctx =
    List.fold_left(
      (ctx, (name, typ)) =>
        SemanticCtx.add_entry(ctx, Id.mk(), name, typ, None),
      ctx,
      bindings,
    );
  fst(
    SemanticCtx.add_hypothesis(
      ctx,
      "law",
      parse_exp(law) |> Substitution.in_exp(Builtins.env_init),
    ),
  );
};
let calculate = (~ctx, ~exp, model) =>
  S.Stepper.calculate(
    ~settings=Calc.NewValue(settings.core),
    ~ctx=Calc.NewValue(ctx),
    ~exp=Calc.NewValue(exp),
    ~ana=Calc.NewValue(Typ.fresh(Unknown(Internal))),
    model,
  );

let fixture = (~ctx=context(), marked) => {
  let z = Test_Editing.parse_zipper(marked);
  let target = Indicated.index(z) |> Option.get;
  let original = MakeTerm.from_zip_for_sem(z, ~root=Exp).term;
  let grip = Exp.find_by_id(target, original) |> Option.get;
  let idx = ProofHacks.exp_idx(grip, original);
  let exp = original |> Substitution.in_exp(Builtins.env_init);
  let (model, _, _) = calculate(~ctx, ~exp, S.init_step);
  let rec frontier = (m: S.step_model) =>
    switch (m.next_step) {
    | Some(next) => frontier(next)
    | None => m
    };
  let model = frontier(model);
  let exp = Calc.get_saved_exc(model.expr);
  let target = ProofHacks.nth_exp(grip, idx, exp) |> Option.get |> Exp.rep_id;
  (ctx, exp, model, target);
};
let editor = (model: S.step_model) => Calc.get_saved_exc(model.editor);
let candidates = (~ctx, ~shard=None, model, target) => {
  let e = editor(model);
  D.candidates(
    ~settings=settings.core,
    ~info_map=e.statics.info_map,
    ~env=SemanticCtx.get_env(ctx),
    ~term=e.statics.term,
    ~measured=e.editor.syntax.measured,
    ~shape_map=e.editor.syntax.shape_map,
    ~target,
    ~shard,
    (),
  );
};
let choose =
    (
      ~ctx,
      ~name="law",
      ~direction=Direction.Right,
      ~shard=None,
      model,
      target,
    ) => {
  let cs = candidates(~ctx, ~shard, model, target);
  cs
  |> List.find((c: Refactor.DragCandidate.t) =>
       switch (c.kind) {
       | RewriteTheorem(_, d, _) =>
         d == direction && String.starts_with(~prefix=name ++ " ", c.label)
       | _ => false
       }
     );
};
let release = (~ctx, model, _target, c: Refactor.DragCandidate.t) =>
  S.Stepper.update(
    ~settings,
    DragRewrite(
      editor(model).statics.term,
      D.environment_id(SemanticCtx.get_env(ctx)),
      [(c.kind, c.at)],
    ),
    model,
  );
let same_exp = (label, expected, actual) =>
  check(testable_exp(), label, parse_exp(expected), actual);
let step = (model: S.step_model) =>
  switch (model.step_kind) {
  | AxiomStep(m) => m
  | _ => fail("expected a recorded theorem step")
  };

let history_fixture = (~ctx, ~exp, before) => {
  open Web;
  let thm_id = Id.mk();
  let thm =
    Theorems.Model.{
      ...theorem_init("demo"),
      goal_exp: Calc.Calculated(exp),
      sem_ctx: Calc.Calculated(ctx),
      stepper_view: {
        ...StepperView.Model.init,
        root: before,
      },
    };
  let source = parse_exp("theorem demo = true in 0");
  let main =
    CodeWithStatics.Model.mk_from_exp(
      ~settings=settings.core,
      ~root=Exp,
      source,
    );
  let cell: CellEditor.Model.t = {
    editor: main,
    result: {
      ...EvalResult.Model.init,
      theorems: {
        thm_map: Id.Map.singleton(thm_id, thm),
        thms: Calc.Calculated([thm_id]),
      },
    },
  };
  let editors: Editors.Model.t =
    Scratch({
      current: 0,
      scratchpads: [
        ScratchMode.Scratchpad.mk_code(~name="drag proof", ~editor=cell, ()),
      ],
    });
  let globals = {
    ...Globals.Model.init(),
    settings,
  };
  let current: Page.Model.t = {
    globals,
    editors,
    explain_this: ExplainThisModel.init,
    selection: Editors.Selection.default_selection(editors),
  };
  (
    History.Model.{
      current,
      undo_stack: [],
      redo_stack: [],
    },
    thm_id,
  );
};
let proof_action = (action: S.step_action): Web.Page.Update.t =>
  Editors(
    Scratch(
      CellAction(ResultAction(TheoremsAction(TheoremUpdate(0, action)))),
    ),
  );

let load_theorems = path => {
  let path =
    List.find(Sys.file_exists, [path, "../" ++ path, "../../../" ++ path]);
  let channel = open_in_bin(path);
  let text = really_input_string(channel, in_channel_length(channel));
  close_in(channel);
  let source = parse_exp(text);
  let statics =
    CachedStatics.init_from_term(
      ~settings=settings.core,
      ~is_dynamic_term=false,
      source,
    );
  check(
    int,
    "example has no static errors",
    0,
    List.length(statics.error_ids),
  );
  let (_, state) =
    Evaluator.evaluate(~env=Builtins.env_init, statics.elaborated);
  let dynamics: Dynamics.t = {
    probe_map: EvaluatorState.get_probes(state) |> Sample.Map.finalize,
    test_results: EvaluatorState.get_tests(state) |> TestResults.mk_results,
    theorems: EvaluatorState.get_theorems(state),
  };
  let theorems =
    Web.Theorems.Update.calculate(
      ~settings=
        Calc.NewValue({
          ...settings.core,
          drag_refactor: false,
        }),
      ~statics=Calc.NewValue(statics),
      ~dynamics=Calc.NewValue(Some(dynamics)),
      Web.Theorems.Model.init,
    );
  /* The normal UI flow loads the program before the user turns
     dragging on. Hidden evaluator steps must not consume its grips. */
  let theorems =
    Web.Theorems.Update.calculate(
      ~settings=Calc.NewValue(settings.core),
      ~statics=Calc.OldValue(statics),
      ~dynamics=Calc.OldValue(Some(dynamics)),
      theorems,
    );
  theorems;
};

let rec frontier = (m: S.step_model) =>
  switch (m.next_step) {
  | Some(next) => frontier(next)
  | None => m
  };

let tests = [
  (
    "Theorem Step Drag",
    [
      test_case(
        "matched arguments cannot escape their lambda binder",
        `Quick,
        () => {
          let ctx =
            context(
              ~law="forall (h : Int -> Int) -> (fun (x : Int) -> h(x)) == h",
              (),
            );
          let exp = parse_exp("fun (x : Int) -> (fun (y : Int) -> x)(x)");
          let (before, _, _) = calculate(~ctx, ~exp, S.init_step);
          let ed = editor(before);
          let info_map = ed.statics.info_map;
          let env = SemanticCtx.get_env(ctx);
          let rules = D.rules(~info_map, ~term=exp, ~env);
          let r = List.hd(rules);
          check(
            bool,
            "underlying matcher finds the escaping instantiation",
            true,
            ProofRule.match_equality(
              ~info_map,
              ~env,
              ~direction=Right,
              r.rule,
              exp,
            )
            |> Option.is_some,
          );
          check(
            bool,
            "drag adapter rejects binder escape",
            true,
            D.matched(
              ~info_map,
              ~env,
              ~term=exp,
              rules,
              r.id,
              Right,
              Exp.rep_id(exp),
            )
            |> Option.is_none,
          );
        },
      ),
      test_case(
        "preview is read-only; release records one replayable step",
        `Quick,
        () => {
          let (ctx, exp, before, target) = fixture("(a + ¦b) + c");
          let saved = S.Stepper.persist(before);
          let c = choose(~ctx, before, target);
          check(
            bool,
            "preview does not change proof",
            true,
            saved == S.Stepper.persist(before),
          );
          check(bool, "track has displacement", true, c.current != c.target);
          let updated = release(~ctx, before, target, c);
          check(
            bool,
            "one historic action",
            true,
            updated.historic && updated.is_edit,
          );
          check(
            string,
            "theorem citation",
            "law",
            step(updated.model).equality,
          );
          check(
            bool,
            "release creates no intermediate path",
            true,
            updated.model.next_step == None,
          );
          let (after, last, _) = calculate(~ctx, ~exp, updated.model);
          check(
            visual_exp,
            "preview equals replay",
            c.term,
            Calc.get_value(last),
          );
          same_exp("associated result", "a + (b + c)", Calc.get_value(last));
          check(
            list(string),
            "required grouping parentheses remain in preview",
            ["a", "+", "(", "b", "+", "c", ")"],
            visible_tokens(c.segment),
          );
          let reloaded = S.Stepper.persist(after) |> S.Stepper.unpersist;
          let (_, replayed, _) = calculate(~ctx, ~exp, reloaded);
          check(
            testable_exp(),
            "persistence replays the same step",
            Calc.get_value(last),
            Calc.get_value(replayed),
          );
          let undone = S.Stepper.update(~settings, RemoveStep, after).model;
          let (_, last, _) = calculate(~ctx, ~exp, undone);
          check(
            testable_exp(),
            "step back restores input",
            exp,
            Calc.get_value(last),
          );
        },
      ),
      test_case(
        "reverse direction is persisted",
        `Quick,
        () => {
          let (ctx, exp, before, target) = fixture("a + (b + ¦c)");
          let c = choose(~ctx, ~direction=Left, before, target);
          let updated = release(~ctx, before, target, c);
          check(
            bool,
            "left citation",
            true,
            step(updated.model).direction == Left,
          );
          let (_, result, _) = calculate(~ctx, ~exp, updated.model);
          same_exp("reverse result", "(a + b) + c", Calc.get_value(result));
        },
      ),
      test_case(
        "release targets the second identical occurrence",
        `Quick,
        () => {
          let (ctx, exp, before, target) =
            fixture("((a + b) + c, (a + ¦b) + c)");
          let c = choose(~ctx, before, target);
          let updated = release(~ctx, before, target, c);
          check(int, "second occurrence", 1, step(updated.model).at_idx);
          let (_, result, _) = calculate(~ctx, ~exp, updated.model);
          same_exp(
            "only second rewritten",
            "((a + b) + c, a + (b + c))",
            Calc.get_value(result),
          );
        },
      ),
      test_case(
        "wrapping and unwrapping do not reverse inline proof drags",
        `Quick,
        () => {
          let left = String.make(34, 'l');
          let right = String.make(34, 'r');
          let list_type = Typ.fresh(List(int_type));
          let ctx =
            context(
              ~law=
                "forall (left : [Int]) -> forall (right : [Int]) -> sum(left @ right) == sum(left) + sum(right)",
              ~bindings=[
                ("sum", Typ.fresh(Arrow(list_type, int_type))),
                (left, list_type),
                (right, list_type),
              ],
              (),
            );
          [
            (Direction.Right, "sum(" ++ left ++ " @ ¦" ++ right ++ ")"),
            (Direction.Left, "sum(" ++ left ++ ") + sum(¦" ++ right ++ ")"),
          ]
          |> List.iter(((direction, marked)) => {
               let (_, exp, before, target) = fixture(~ctx, marked);
               let c = choose(~ctx, ~direction, before, target);
               let placed =
                 Measured.find_by_id(target, c.measured) |> Option.get;
               check(
                 bool,
                 "normal result really moves the grip between lines",
                 true,
                 placed.origin.row != c.current.row,
               );
               check(
                 int,
                 "gesture stays on the grabbed line",
                 c.current.row,
                 c.target.row,
               );
               check(
                 bool,
                 "gesture follows logical inline displacement",
                 true,
                 direction == Right
                   ? c.target.col > c.current.col
                   : c.target.col < c.current.col,
               );
               let updated = release(~ctx, before, target, c);
               let (_, result, _) = calculate(~ctx, ~exp, updated.model);
               check(
                 visual_exp,
                 "wrapped rewrite replays",
                 c.term,
                 Calc.get_value(result),
               );
             });
        },
      ),
      test_case(
        "a call's closing handle uses its own logical displacement",
        `Quick,
        () => {
          let list_type = Typ.fresh(List(int_type));
          let ctx =
            context(
              ~law=
                "forall (left : [Int]) -> forall (right : [Int]) -> sum(left @ right) == sum(left) + sum(right)",
              ~bindings=[
                ("sum", Typ.fresh(Arrow(list_type, int_type))),
                ("xs", list_type),
                ("ys", list_type),
              ],
              (),
            );
          let (_, _, before, _) = fixture(~ctx, "sum(xs @ ¦ys)");
          let ed = editor(before);
          let target = Exp.rep_id(ed.statics.term);
          let c = choose(~ctx, ~shard=Some(1), before, target);
          let close =
            Web.CodeFlip.find_meas(
              ed.editor.syntax.measured,
              Shard(target, 1),
            )
            |> Option.get;
          check(
            int,
            "the closing delimiter is the grip",
            close.origin.col,
            c.current.col,
          );
          check(
            int,
            "closing handle loses only the append tail",
            -5,
            c.target.col - c.current.col,
          );
        },
      ),
      test_case(
        "folded function identities and widths survive preview rendering",
        `Quick,
        () => {
          let arrow = Typ.fresh(Arrow(int_type, int_type));
          let ctx =
            context(
              ~law=fusion,
              ~bindings=[
                ("xs", Typ.fresh(List(int_type))),
                ("f", arrow),
                ("g", arrow),
              ],
              (),
            );
          let (_, exp, before, target) =
            fixture(~ctx, "map(map(xs, ¦f), g)");
          let live = editor(before).editor.syntax;
          let again = render(exp).editor.syntax;
          check(
            list(string),
            "function projection identities are stable",
            List.map(
              (p: Base.projector) => Id.to_string(p.id),
              folds(live.segment),
            ),
            List.map(
              (p: Base.projector) => Id.to_string(p.id),
              folds(again.segment),
            ),
          );
          let c = choose(~ctx, before, target);
          folds(c.segment)
          |> List.iter((p: Base.projector) => {
               let text =
                 FoldProj.t_of_sexp(Sexplib.Sexp.of_string(p.model)).text;
               let m = Measured.find_by_id(p.id, c.measured) |> Option.get;
               check(
                 int,
                 "candidate reserves the displayed function width",
                 Unicode.Width.columns_of_string(text),
                 m.last.col - m.origin.col,
               );
             });
        },
      ),
      test_case(
        "fusion reuses the inferred function grips",
        `Quick,
        () => {
          let arrow = Typ.fresh(Arrow(int_type, int_type));
          let ctx =
            context(
              ~law=fusion,
              ~bindings=[
                ("xs", Typ.fresh(List(int_type))),
                ("f", arrow),
                ("g", arrow),
              ],
              (),
            );
          ["map(map(xs, ¦f), g)", "map(map(xs, f), ¦g)"]
          |> List.iter(marked => {
               let (_, exp, before, target) = fixture(~ctx, marked);
               let c = choose(~ctx, before, target);
               let updated = release(~ctx, before, target, c);
               let (_, result, _) = calculate(~ctx, ~exp, updated.model);
               check(
                 visual_exp,
                 "fusion preview equals step",
                 c.term,
                 Calc.get_value(result),
               );
             });
        },
      ),
      test_case(
        "stale source snapshot is ignored without history",
        `Quick,
        () => {
          let (ctx, _, before, target) = fixture("(a + ¦b) + c");
          let c = choose(~ctx, before, target);
          let u =
            S.Stepper.update(
              ~settings,
              DragRewrite(
                parse_exp("42"),
                D.environment_id(SemanticCtx.get_env(ctx)),
                [(c.kind, target)],
              ),
              before,
            );
          check(bool, "no change", true, u.model === before);
          check(bool, "no undo entry", false, u.historic);
        },
      ),
      test_case(
        "removed theorem is revalidated at release",
        `Quick,
        () => {
          let (ctx, _, before, target) = fixture("(a + ¦b) + c");
          let c = choose(~ctx, before, target);
          let ms =
            switch (before.step_kind) {
            | MissingStep(m) => m
            | _ => fail("missing step")
            };
          let before = {
            ...before,
            step_kind:
              MissingStep({
                ...ms,
                cached_env: Calc.Calculated(Builtins.env_init),
              }),
          };
          let u = release(~ctx, before, target, c);
          check(bool, "no change", true, u.model === before && !u.historic);
        },
      ),
      test_case(
        "cancelled, source-edit, and multi-rung paths add nothing",
        `Quick,
        () => {
          let (ctx, _, before, target) = fixture("(a + ¦b) + c");
          let c = choose(~ctx, before, target);
          [
            [],
            [(Action.ExtractLet, target)],
            [(c.kind, target), (c.kind, target)],
          ]
          |> List.iter(path => {
               let u =
                 S.Stepper.update(
                   ~settings,
                   DragRewrite(
                     editor(before).statics.term,
                     D.environment_id(SemanticCtx.get_env(ctx)),
                     path,
                   ),
                   before,
                 );
               check(
                 bool,
                 "no proof or history edit",
                 true,
                 u.model === before && !u.historic,
               );
             });
        },
      ),
      test_case(
        "historical rows and non-proof steppers reject release",
        `Quick,
        () => {
          let (ctx, _, before, target) = fixture("(a + ¦b) + c");
          let c = choose(~ctx, before, target);
          let after = release(~ctx, before, target, c).model;
          let replay = release(~ctx, after, target, c);
          check(
            bool,
            "earlier step cannot be replaced",
            true,
            replay.model === after && !replay.historic,
          );
          let off =
            Web.Settings.Model.{
              ...settings,
              core: {
                ...settings.core,
                evaluation: {
                  ...settings.core.evaluation,
                  enable_proof: false,
                },
              },
            };
          let u =
            S.Stepper.update(
              ~settings=off,
              DragRewrite(
                editor(before).statics.term,
                D.environment_id(SemanticCtx.get_env(ctx)),
                [(c.kind, target)],
              ),
              before,
            );
          check(
            bool,
            "proof mode required",
            true,
            u.model === before && !u.historic,
          );
        },
      ),
      test_case(
        "unspecified witnesses do not produce unreplayable choices",
        `Quick,
        () => {
          let ctx =
            context(
              ~law="forall (x : Int) -> forall (y : Int) -> x == x + y",
              (),
            );
          let (_, _, before, target) = fixture(~ctx, "¦a");
          check(
            int,
            "no unresolved rule",
            0,
            candidates(~ctx, before, target) |> List.length,
          );
        },
      ),
      test_case(
        "a value binding shadows a theorem with the same name",
        `Quick,
        () => {
          let ctx = context();
          let ctx =
            SemanticCtx.add_entry(
              ctx,
              Id.mk(),
              "law",
              int_type,
              Some(parse_exp("42")),
            );
          let (_, _, before, target) = fixture(~ctx, "(a + ¦b) + c");
          check(
            int,
            "shadowed rule omitted",
            0,
            candidates(~ctx, before, target) |> List.length,
          );
        },
      ),
      test_case(
        "changed theorem environment rejects an otherwise valid release",
        `Quick,
        () => {
          let (ctx, _, before, target) = fixture("(a + ¦b) + c");
          let c = choose(~ctx, before, target);
          let ms =
            switch (before.step_kind) {
            | MissingStep(m) => m
            | _ => fail("missing step")
            };
          let env =
            Environment.extend(
              SemanticCtx.get_env(ctx),
              ("another", parse_exp("0")),
            );
          let changed = {
            ...before,
            step_kind:
              MissingStep({
                ...ms,
                cached_env: Calc.Calculated(env),
              }),
          };
          let u = release(~ctx, changed, target, c);
          check(
            bool,
            "changed context adds no step or undo",
            true,
            u.model === changed && !u.historic,
          );
        },
      ),
      test_case(
        "theorem release makes one page undo entry and redo restores it",
        `Quick,
        () => {
          let (ctx, exp, before, target) = fixture("(a + ¦b) + c");
          let (h0, _) = history_fixture(~ctx, ~exp, before);
          let c = choose(~ctx, before, target);
          check(
            int,
            "preview leaves history alone",
            0,
            Test_Undo.undo_len(h0),
          );
          let snapshot = editor(before).statics.term;
          let cancelled =
            Test_Undo.apply(
              h0,
              proof_action(
                DragRewrite(
                  snapshot,
                  D.environment_id(SemanticCtx.get_env(ctx)),
                  [],
                ),
              ),
            );
          check(
            int,
            "cancelled drag adds no undo entry",
            0,
            Test_Undo.undo_len(cancelled),
          );
          let h1 =
            Test_Undo.apply(
              h0,
              proof_action(
                DragRewrite(
                  snapshot,
                  D.environment_id(SemanticCtx.get_env(ctx)),
                  [(c.kind, target)],
                ),
              ),
            );
          check(
            int,
            "release adds exactly one entry",
            1,
            Test_Undo.undo_len(h1),
          );
          check(
            string,
            "source text stays unchanged",
            Test_Undo.text_of(h0),
            Test_Undo.text_of(h1),
          );
          let h2 = Test_Undo.apply(h1, Test_Undo.undo);
          check(
            bool,
            "undo restores exact proof state",
            true,
            h2.current === h0.current,
          );
          let h3 = Test_Undo.apply(h2, Test_Undo.redo);
          check(
            bool,
            "redo restores exact proof state",
            true,
            h3.current === h1.current,
          );
        },
      ),
      test_case(
        "proof step highlights expose per-shard animation anchors",
        `Quick,
        () => {
          let (_, _, before, _) = fixture("(a + ¦b) + c");
          let ed = editor(before);
          let id = Exp.rep_id(ed.statics.term);
          let decorations =
            Web.StepperEditor.View.deco(
              ~syntax=ed.editor.syntax,
              ~font_metrics=Web.FontMetrics.init,
              ~inject=_ => Ui_effect.Ignore,
              ~selected_id=None,
              _ => Ui_effect.Ignore,
              Web.StepperEditor.Model.{
                editor: ed,
                next_steps: [id],
                taken_steps: [id],
                refls: [id],
              },
            );
          let backing_ids =
            Js_of_ocaml.Js.Unsafe.js_expr(
              "(function(nodes) { const ids = []; const walk = n => { const a = (n.properties || {}).attributes || {}; if ((a.class || '').split(' ').includes('shard')) ids.push(a.id || ''); (n.children || []).forEach(walk); }; nodes.forEach(walk); return ids; })",
            );
          let ids =
            Js_of_ocaml.Js.Unsafe.fun_call(
              backing_ids,
              [|
                Js_of_ocaml.Js.Unsafe.inject(
                  decorations
                  |> List.map(Virtual_dom.Vdom.Node.to_raw)
                  |> Array.of_list
                  |> Js_of_ocaml.Js.array,
                ),
              |],
            )
            |> Js_of_ocaml.Js.to_array
            |> Array.to_list
            |> List.map(Js_of_ocaml.Js.to_string);
          check(bool, "proof backings are rendered", true, ids != []);
          List.iter(
            dom_id =>
              check(bool, "backing can follow its token", true, dom_id != ""),
            ids,
          );
        },
      ),
      test_case(
        "proof decoration scrubs follow the pressed shard in the current row",
        `Quick,
        () => {
          let list_type = Typ.fresh(List(int_type));
          let ctx =
            context(
              ~law=
                "forall (left : [Int]) -> forall (right : [Int]) -> sum(left @ right) == sum(left) + sum(right)",
              ~bindings=[
                ("sum", Typ.fresh(Arrow(list_type, int_type))),
                ("xs", list_type),
                ("ys", list_type),
              ],
              (),
            );
          let (_, _, before, _) = fixture(~ctx, "sum(xs @ ¦ys)");
          let ed = editor(before);
          let target = Exp.rep_id(ed.statics.term);
          let c = choose(~ctx, ~shard=Some(1), before, target);
          let suffix = Id.to_string(target) ++ "-1";
          let ids = [
            "step-next-" ++ Id.to_string(target) ++ "-0",
            "step-next-" ++ suffix,
            "step-taken-" ++ suffix,
            "step-refl-" ++ suffix,
            "indication-" ++ suffix,
          ];
          /* A small DOM boundary for the actual animation builder, with
             a nested editor and a render replacing the backing elements.
             It does not open or control a browser. */
          let make_scope =
            Js_of_ocaml.Js.Unsafe.js_expr(
              {js|
            (function(ids) {
              const scope = {}, nested = {};
              const make = (id, owner) => ({
                id, animations: [],
                getAttribute: k => k === 'id' ? id : null,
                closest: k => k === '.code-container' ? owner : null,
                querySelector: () => null,
                animate(frames) {
                  const a = { frames, cancelled: false, pause() {},
                    cancel() { this.cancelled = true; } };
                  this.animations.push(a);
                  return a;
                }
              });
              scope.nodes = ids.map(id => make(id, scope));
              scope.token = make('', scope);
              scope.nested = make(ids[1], nested);
              scope.querySelectorAll = selector => {
                const prefixes = Array.from(selector.matchAll(/\[id\^="([^"]+)"\]/g), m => m[1]);
                const found = [...scope.nodes, scope.nested].filter(n => prefixes.some(p => n.id.startsWith(p)));
                found.item = i => found[i] || null;
                return found;
              };
              scope.patch = () => {
                scope.old = scope.nodes;
                scope.nodes = ids.map(id => make(id, scope));
              };
              return scope;
            })
          |js},
            );
          let scope =
            Js_of_ocaml.Js.Unsafe.fun_call(
              make_scope,
              [|
                Js_of_ocaml.Js.Unsafe.inject(
                  ids
                  |> List.map(Js_of_ocaml.Js.string)
                  |> Array.of_list
                  |> Js_of_ocaml.Js.array,
                ),
              |],
            );
          check(
            int,
            "only this row's backings",
            5,
            List.length(Web.CodeFlip.anchored_decos(~scope, ())),
          );
          let metrics = Web.FontMetrics.init;
          let close =
            Measured.find_by_id(target, ed.editor.syntax.measured)
            |> Option.get;
          let token = Js_of_ocaml.Js.Unsafe.get(scope, "token");
          let dx = (-5.) *. metrics.col_width;
          let rung: Web.CodeDrag.rung = {
            kind: c.kind,
            label: c.label,
            at: c.at,
            tgt: {
              x: 0.,
              y: 0.,
            },
            scroll_rows: 0,
            moved: [(Web.CodeFlip.Shard(target, 1), token, dx, 0.)],
            deco_delta:
              Web.CodeDrag.decoration_step(
                ~font_metrics=metrics,
                ~before=ed.editor.syntax.measured,
                ~after=c.measured,
                ~frame=c.frame,
                ~absorb=c.absorb,
              ),
            exits: [],
            enters: [],
            absorb_into: [],
            emerge_src: [],
            caret_delta: None,
            piece_delta: None,
            cand: c,
            held: (target, Some(1)),
          };
          let track: Web.CodeDrag.cand = {
            kind: c.kind,
            label: c.label,
            cur: {
              x: float_of_int(close.origin.col) *. metrics.col_width,
              y: 0.,
            },
            rungs: [rung],
            exhausted: true,
          };
          let tracks = Web.CodeDrag.node_tracks(~scope, track);
          check(
            int,
            "token and its four backings move",
            5,
            List.length(tracks),
          );
          List.iter(
            ((_, _, frames)) =>
              check(
                bool,
                "matching token and backing displacement",
                true,
                frames[1] == (dx, 0., 1.),
              ),
            tracks,
          );
          Js_of_ocaml.Js.Unsafe.meth_call(scope, "patch", [||]) |> ignore;
          let animations = Web.CodeDrag.make_anims(~scope, track);
          check(
            int,
            "animations include the current backings",
            5,
            List.length(animations),
          );
          let old_animations =
            Js_of_ocaml.Js.Unsafe.js_expr(
              "(s => [...s.old, s.nested].reduce((n, el) => n + el.animations.length, 0))",
            );
          check(
            int,
            "history, nested and replaced nodes stay untouched",
            0,
            Js_of_ocaml.Js.Unsafe.fun_call(
              old_animations,
              [|Js_of_ocaml.Js.Unsafe.inject(scope)|],
            ),
          );
          Web.CodeDrag.cancel_anims(animations);
          List.iter(
            a =>
              check(
                bool,
                "cancellation releases the decoration",
                true,
                Js_of_ocaml.Js.to_bool(
                  Js_of_ocaml.Js.Unsafe.get(a, "cancelled"),
                ),
              ),
            animations,
          );
          check(
            bool,
            "removed backings dissolve",
            true,
            Web.CodeDrag.decoration_step(
              ~font_metrics=metrics,
              ~before=ed.editor.syntax.measured,
              ~after=Measured.empty,
              ~frame=c.frame,
              ~absorb=[],
              (target, Some(1)),
            )
            == Some(Gone),
          );
        },
      ),
      test_case(
        "triple sum stays inline and records two splits and regrouping",
        `Quick,
        () => {
          let theorems =
            load_theorems(
              "hazel-programs/docs/refactoring/refactoring-sum-proof.hz",
            );
          let (_, thm) =
            Id.Map.bindings(theorems.thm_map)
            |> List.find(((_, t: Web.Theorems.Model.theorem)) =>
                 t.name == "sum_three_lists"
               );
          let ctx = Calc.get_saved_exc(thm.sem_ctx);
          let goal = Calc.get_saved_exc(thm.goal_exp);
          let root = ref(thm.stepper_view.root);
          let rec to_frontier = (m: S.step_model, action) =>
            switch (m.next_step) {
            | None => action
            | Some(next) => S.NextStep(to_frontier(next, action))
            };
          [
            ("sum_append", Direction.Right),
            ("sum_append", Direction.Right),
            ("add_associativity", Direction.Left),
          ]
          |> List.iteri((i, (name, direction)) => {
               let before = frontier(root^);
               let ed = editor(before);
               let lhs =
                 switch (Exp.term_of(ed.statics.term)) {
                 | BinOp(_, lhs, _) => lhs
                 | _ => fail("expected equality goal")
                 };
               let ids = ref([]);
               let _ =
                 Exp.map_term(
                   ~f_exp=
                     (cont, e) => {
                       ids := ids^ @ [Exp.rep_id(e)];
                       cont(e);
                     },
                   lhs,
                 );
               let c =
                 ids^
                 |> List.find_map(target =>
                      candidates(~ctx, before, target)
                      |> List.find_opt((c: Refactor.DragCandidate.t) =>
                           switch (c.kind) {
                           | RewriteTheorem(_, d, _) =>
                             d == direction
                             && String.starts_with(
                                  ~prefix=name ++ " ",
                                  c.label,
                                )
                           | _ => false
                           }
                         )
                    )
                 |> Option.get;
               check(
                 int,
                 "preview does not introduce a line break",
                 1,
                 RefactorDrag.total_rows(c.measured),
               );
               let path = [(c.kind, c.at)];
               let action =
                 S.DragRewrite(
                   ed.statics.term,
                   D.environment_id(SemanticCtx.get_env(ctx)),
                   path,
                 );
               let updated =
                 S.Stepper.update(
                   ~settings,
                   to_frontier(root^, action),
                   root^,
                 );
               let (after, result, _) =
                 calculate(~ctx, ~exp=goal, updated.model);
               root := after;
               check(
                 int,
                 "recorded row stays inline",
                 1,
                 RefactorDrag.total_rows(
                   editor(frontier(after)).editor.syntax.measured,
                 ),
               );
               check(
                 visual_exp,
                 "candidate equals recorded result",
                 c.term,
                 Calc.get_value(result),
               );
               if (i == 2) {
                 switch (Exp.term_of(Calc.get_value(result))) {
                 | BinOp(_, lhs, rhs) =>
                   check(
                     visual_exp,
                     "both sides agree after regrouping",
                     lhs,
                     rhs,
                   )
                 | _ => fail("expected final equality")
                 };
               };
             });
          let rec citations = (m: S.step_model) => {
            let here =
              switch (m.step_kind) {
              | AxiomStep(a) => [a.equality]
              | _ => []
              };
            here
            @ (
              m.next_step
              |> Option.map(citations)
              |> Option.value(~default=[])
            );
          };
          check(
            list(string),
            "three cited rewrites",
            ["sum_append", "sum_append", "add_associativity"],
            citations(root^),
          );
          let last = frontier(root^);
          let final_exp = Calc.get_saved_exc(last.expr);
          let refls =
            switch (last.step_kind) {
            | MissingStep(m) => Calc.get_saved_exc(m.refls)
            | _ => fail("expected an unfinished final row")
            };
          check(
            bool,
            "reflexivity is offered for the final equality",
            true,
            List.exists(e => Exp.rep_id(e) == Exp.rep_id(final_exp), refls),
          );
          let from_exp =
            List.find(e => Exp.rep_id(e) == Exp.rep_id(final_exp), refls);
          let closed =
            S.Stepper.update(
              ~settings,
              to_frontier(
                root^,
                S.AddAxiomStep(
                  "reflexivity",
                  ProofHacks.exp_idx(from_exp, final_exp),
                  from_exp,
                  Direction.Right,
                  "Reflexive(==)",
                ),
              ),
              root^,
            );
          let (_, result, _) = calculate(~ctx, ~exp=goal, closed.model);
          same_exp(
            "reflexivity closes the equality",
            "true",
            Calc.get_value(result),
          );
        },
      ),
      test_case(
        "demo proof goals offer and replay list, map, and sum rewrites",
        `Quick,
        () => {
          let theorems =
            load_theorems(
              "hazel-programs/docs/refactoring/refactoring-proof-drags.hz",
            );
          [
            ("regroup_demo", "list_associativity", "zs"),
            ("fusion_demo", "map_fusion", "g"),
            ("sum_demo", "sum_append", "ys"),
          ]
          |> List.iter(((demo, rule, grip)) => {
               let (_, thm) =
                 Id.Map.bindings(theorems.thm_map)
                 |> List.find(((_, t: Web.Theorems.Model.theorem)) =>
                      t.name == demo
                    );
               let ctx = Calc.get_saved_exc(thm.sem_ctx);
               let rec frontier = (m: S.step_model) =>
                 switch (m.next_step) {
                 | None => m
                 | Some(n) => frontier(n)
                 };
               let before = frontier(thm.stepper_view.root);
               let exp = Calc.get_saved_exc(before.expr);
               check(
                 visual_exp,
                 demo
                 ++ " starts at its untouched goal when dragging is enabled",
                 Calc.get_saved_exc(thm.goal_exp),
                 exp,
               );
               let lhs =
                 switch (Exp.term_of(exp)) {
                 | BinOp(_, lhs, _) => lhs
                 | _ => fail("expected equality goal")
                 };
               let target = ref(None);
               let _ =
                 Exp.map_term(
                   ~f_exp=
                     (cont, e) => {
                       switch (Exp.term_of(e), target^) {
                       | (Var(name), None) when name == grip =>
                         target := Some(Exp.rep_id(e))
                       | _ => ()
                       };
                       cont(e);
                     },
                   lhs,
                 );
               let target = Option.get(target^);
               let c = choose(~ctx, ~name=rule, before, target);
               if (demo == "sum_demo") {
                 check(
                   bool,
                   "sum split moves ys right into its own call",
                   true,
                   c.target.col > c.current.col,
                 );
               };
               let updated = release(~ctx, before, target, c);
               check(
                 string,
                 "records real theorem citation",
                 rule,
                 step(updated.model).equality,
               );
               let (_, last, _) = calculate(~ctx, ~exp, updated.model);
               let actual = render(Calc.get_value(last)).editor.syntax;
               check(
                 bool,
                 demo ++ " preview geometry equals recorded row",
                 true,
                 visible_layout(~measured=c.measured, c.segment)
                 == visible_layout(~measured=actual.measured, actual.segment),
               );
               let live = editor(before).editor.syntax;
               folds(c.segment)
               |> List.iter((p: Base.projector) => {
                    check(
                      bool,
                      "all folded function labels participate in the preview",
                      true,
                      Web.CodeDrag.token_texts(c.segment)
                      |> List.exists(((k, text, _)) =>
                           k == Web.CodeFlip.ProjectorK(p.id)
                           && text
                           == FoldProj.t_of_sexp(
                                Sexplib.Sexp.of_string(p.model),
                              ).
                                text
                         ),
                    );
                    if (!Id.Map.mem(p.id, live.measured.projectors)) {
                      check(
                        bool,
                        "a new function copy retains its live origin",
                        true,
                        List.assoc_opt(p.id, c.emerge)
                        |> Option.map(id =>
                             Id.Map.mem(id, live.measured.projectors)
                           )
                        |> Option.value(~default=false),
                      );
                    };
                  });
               check(
                 list(string),
                 demo ++ " preview displays exactly the recorded row",
                 visible_tokens(
                   render(Calc.get_value(last)).editor.syntax.segment,
                 ),
                 visible_tokens(c.segment),
               );
               check(
                 visual_exp,
                 "real goal preview equals replay",
                 c.term,
                 Calc.get_value(last),
               );
             });
        },
      ),
    ],
  ),
];

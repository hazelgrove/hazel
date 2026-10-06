open Alcotest;
open Language;
open Test_Evaluator_Prelude;

/* User-defined livelits: `let ^name = (init, update, view, expand) in ...`
   binds a livelit whose uses elaborate through the runtime binding. */

let statics = (text: string): (Statics.Map.t, Exp.t) => {
  let term = parse_exp(text);
  Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term);
};

let run = (text: string): Exp.t => {
  let (_, elaborated) = statics(text);
  Evaluator.evaluate(~env=Builtins.env_init, elaborated) |> fst;
};

let run_test = (msg, expected_text, program) =>
  check(dhexp_typ, msg, run(expected_text), run(program));

let has_mark = (pred: Mark.t => bool, m: Statics.Map.t): bool =>
  Id.Map.exists(
    (_, info) =>
      switch ((info: Info.t)) {
      | InfoExp({marks, _}) => List.exists(pred, marks)
      | _ => false
      },
    m,
  );

let dbl_def = "(0, fun (m, a) -> a, fun m -> 0, fun m -> m * 2)";

let dbl_module = "{
type Model = Int;
type Action = Int;
let init : Model = 0;
let update = fun (m, a) -> a;
let view = fun m -> 0;
let expand = fun m -> m * 2
}";

let parses_as_binder = () => {
  let term = parse_exp("let ^s = 5 in 1");
  switch (term.term) {
  | Let(p, _, _) =>
    switch (p.term) {
    | Var("^s") => ()
    | _ => fail("expected pattern Var(\"^s\"), got " ++ Pat.show(p))
    }
  | _ => fail("expected a let")
  };
};

let evaluates = () =>
  run_test(
    "^dbl(21) expands and evaluates",
    "42",
    "let ^dbl = " ++ dbl_def ++ " in ^dbl(21)",
  );

let multiple_uses = () =>
  run_test(
    "each use carries its own model",
    "12",
    "let ^dbl = " ++ dbl_def ++ " in ^dbl(1) + ^dbl(2) + ^dbl(3)",
  );

let labeled_out_of_order = () =>
  run_test(
    "labeled fields select by name",
    "42",
    "let ^dbl = (expand=fun m -> m * 2, init=0, update=fun (m, a) -> a, view=fun m -> 0) in ^dbl(21)",
  );

let helpers_in_def = () =>
  run_test(
    "helpers bind inside the definition",
    "5",
    "let ^inc = (let f = fun x -> x + 1 in (0, fun (m, a) -> a, fun m -> 0, fun m -> f(m))) in ^inc(4)",
  );

let shadows_builtin = () =>
  run_test(
    "a user ^slider shadows the builtin",
    "105",
    "let ^slider = (0, fun (m, a) -> a, fun m -> 0, fun m -> m + 100) in ^slider(5)",
  );

let nested_shadowing = () =>
  run_test(
    "inner livelit binding wins",
    "30",
    "let ^d = "
    ++ dbl_def
    ++ " in let ^d = (0, fun (m, a) -> a, fun m -> 0, fun m -> m * 3) in ^d(10)",
  );

let module_evaluates = () =>
  run_test(
    "module definition with type members",
    "42",
    "let ^dbl = " ++ dbl_module ++ " in ^dbl(21)",
  );

let module_helpers = () =>
  run_test(
    "helpers are ordinary module members",
    "15",
    "let ^inc = {
let bump = fun x -> x + 1;
let init = 0;
let update = fun (m, a) -> a;
let view = fun m -> 0;
let expand = fun m -> bump(m)
} in ^inc(4) + ^inc(9)",
  );

let module_funlet_members = () =>
  run_test(
    "funlet-form members are recognized by name",
    "8",
    "let ^dbl = {
let init = 0;
let update(m, a) = a;
let view(m) = 0;
let expand(m) = m * 2
} in ^dbl(4)",
  );

let module_missing_members = () => {
  let (m, _) =
    statics("let ^x = {let init = 0; let view = fun m -> 0} in 1");
  check(
    bool,
    "missing members reported by name",
    true,
    has_mark(
      fun
      | Mark.InvalidLivelitDef(DefMissingMembers(["update", "expand"])) =>
        true
      | _ => false,
      m,
    ),
  );
};

let module_adapter = () => {
  let def_text = "{
let init = 50;
let update = fun (m, a) -> a;
let view = fun m -> Text(\"hi\");
let expand = fun m -> m;
let shape = Tab(30, 5)
}";
  let def_user = parse_exp(def_text);
  let (_, def_elab) =
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), def_user);
  switch (
    UserLivelit.mk(
      ~name="s",
      ~id=Id.invalid,
      ~def_user,
      ~def_elab,
      ~def_ty=IdTagged.FreshGrammar.Typ.unknown(Internal),
    )
  ) {
  | Ok(ll) =>
    check(
      dhexp_typ,
      "model_default is the init member",
      parse_exp("50"),
      ll.model_default,
    );
    check(
      bool,
      "shape member sets the projector shape",
      true,
      ll.shape
      == {
           horizontal: 30,
           vertical: Tab(4) /* 5 lines = 4 linebreaks */
         },
    );
  | Error(_) => fail("adapter rejected a well-formed module definition")
  };
};

let bad_def_marked = () => {
  let (m, _) = statics("let ^x = 5 in 1");
  check(
    bool,
    "non-tuple definition gets InvalidLivelitDef",
    true,
    has_mark(
      fun
      | Mark.InvalidLivelitDef(DefNotTuple) => true
      | _ => false,
      m,
    ),
  );
};

let bad_arity_marked = () => {
  let (m, _) = statics("let ^x = (1, 2) in 1");
  check(
    bool,
    "wrong-arity tuple gets InvalidLivelitDef",
    true,
    has_mark(
      fun
      | Mark.InvalidLivelitDef(DefBadArity(2)) => true
      | _ => false,
      m,
    ),
  );
};

let unbound_use_marked = () => {
  let (m, _) = statics("^nope(3)");
  check(
    bool,
    "unbound livelit use is Free",
    true,
    has_mark(
      fun
      | Mark.Free("nope") => true
      | _ => false,
      m,
    ),
  );
};

let good_def_unmarked = () => {
  let (m, _) = statics("let ^dbl = " ++ dbl_def ++ " in ^dbl(21)");
  check(
    bool,
    "well-formed program has no livelit marks",
    false,
    has_mark(
      fun
      | Mark.InvalidLivelitDef(_)
      | Mark.Free(_) => true
      | _ => false,
      m,
    ),
  );
};

/* The adapter contract LivelitProj relies on: the captured definition
   evaluates closed, its fields extract, and view(model) yields HTML. */
let adapter = () => {
  let def_text = "(50, fun (m, a) -> a, fun m -> Text(\"hi\"), fun m -> m)";
  let def_user = parse_exp(def_text);
  let (_, def_elab) =
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), def_user);
  let ll =
    switch (
      UserLivelit.mk(
        ~name="s",
        ~id=Id.invalid,
        ~def_user,
        ~def_elab,
        ~def_ty=IdTagged.FreshGrammar.Typ.unknown(Internal),
      )
    ) {
    | Ok(ll) => ll
    | Error(_) => fail("adapter rejected a well-formed definition")
    };
  /* model_default comes from init */
  check(
    dhexp_typ,
    "model_default is the init field",
    parse_exp("50"),
    ll.model_default,
  );
  /* the stored definition evaluates and view(model) is HTML */
  let record =
    switch (ll.user_def) {
    | Some(def) => evaluate(def)
    | None => fail("user_def not captured")
    };
  switch (
    Haz3lcore.MvuShape.of_tuple(Haz3lcore.MvuShape.strip_wrappers(record))
  ) {
  | Some([_, _, view_fn, _]) =>
    let html =
      evaluate(
        IdTagged.FreshGrammar.Exp.ap(Forward, view_fn, parse_exp("50")),
      );
    check(
      bool,
      "view(model) is HTML",
      true,
      Haz3lcore.MvuShape.is_html(html),
    );
  | _ => fail("definition did not evaluate to a 4-tuple")
  };
};

let shape_field = () => {
  let def_user =
    parse_exp("(0, fun (m, a) -> a, fun m -> 0, fun m -> m, Block(30, 5))");
  let (_, def_elab) =
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), def_user);
  switch (
    UserLivelit.mk(
      ~name="s",
      ~id=Id.invalid,
      ~def_user,
      ~def_elab,
      ~def_ty=IdTagged.FreshGrammar.Typ.unknown(Internal),
    )
  ) {
  | Ok(ll) =>
    check(
      bool,
      "fifth field sets the projector shape",
      true,
      ll.shape
      == {
           horizontal: 30,
           vertical: Block(4) /* 5 lines = 4 linebreaks */
         },
    )
  | Error(_) => fail("adapter rejected a 5-field definition")
  };
};

/* The event path's commit-vs-ephemeral decision: an update result that
   carries a captured environment (a mid-run Closure, e.g. off a sampled
   value) must not commit to the program text — it stays optimistic-only
   and the widget keeps running. First-order data commits. */
let commit_decision = () => {
  open IdTagged.FreshGrammar;
  check(
    bool,
    "first-order update result commits",
    true,
    Haz3lcore.LivelitProj.commit_decision(
      run("(fun (m, a) -> (m + 1, a))((1, 2))"),
    )
    == `Commit,
  );
  let env = Environment.of_list([("y", parse_exp("3"))]);
  let closure_fn =
    Exp.closure(env, Exp.fn(Pat.var("x"), Exp.var("y"), None, None));
  check(
    bool,
    "closure-carrying update result is ephemeral",
    true,
    Haz3lcore.LivelitProj.commit_decision(
      Exp.tuple([parse_exp("1"), closure_fn]),
    )
    == `Ephemeral,
  );
};

/* View fold-in: a projected use also computes view(model) in the main run,
   so probes inside view fire and the projector's sample stream carries the
   live HTML. Pipeline mirrors the CLI probe command. */
let probe_run = (text: string) => {
  switch (Haz3lcore.Parser.to_zipper(~root=Exp, text)) {
  | None => fail("failed to parse: " ++ text)
  | Some(z) =>
    let mtr = Haz3lcore.MakeTerm.from_zip_for_sem(z, ~root=Exp);
    let probe_ids =
      Haz3lcore.CachedStatics.probe_ids_of_zipper(
        ~projectors=mtr.projectors,
        z,
      );
    let (info_map, elaborated) =
      Statics.mk(
        ~probe_ids,
        CoreSettings.on,
        Builtins.ctx_init(Some(Int)),
        mtr.term,
      );
    let targets =
      Haz3lcore.CachedStatics.compute_targets(
        ~settings=CoreSettings.on,
        ~info_map,
        ~probe_ids,
      );
    let (_, state) =
      Evaluator.evaluate(
        ~eval_info=EvalInfo.of_targets(targets),
        ~env=Builtins.env_init,
        elaborated,
      );
    (
      mtr,
      List.map(fst, z.refractors.manuals),
      EvaluatorState.get_probes(state),
    );
  };
};

let view_probe_def = "let ^dbl = {
let init = 0;
let update = fun (m, a) -> a;
let view = fun m -> Text(string_of_int(^^probe(m * 3)));
let expand = fun m -> m * 2
} in ";

let view_probes_fire = () => {
  let (_, _, probes) =
    probe_run(view_probe_def ++ "^^livelit(^dbl(21)) + ^^livelit(^dbl(4))");
  /* the manual probe inside view records once per projected use */
  let view_samples =
    Sample.Map.fold(
      (_, samples, acc) =>
        acc
        + List.length(
            List.filter(
              (s: Sample.t) =>
                switch (Haz3lcore.MvuShape.strip_wrappers(s.value).term) {
                | Atom(Int(n)) =>
                  Bigint.to_int(n) == Some(63)
                  || Bigint.to_int(n) == Some(12)
                | _ => false
                },
              samples,
            ),
          ),
      probes,
      0,
    );
  check(int, "view probe sampled once per use", 2, view_samples);
};

let projector_gets_html_sample = () => {
  let (mtr, _, probes) =
    probe_run(view_probe_def ++ "^^livelit(^dbl(21)) + 1");
  let html_samples =
    Id.Map.fold(
      (id, _, acc) =>
        acc
        + (
          switch (Sample.Map.lookup(id, probes)) {
          | Some(samples) =>
            List.length(
              List.filter(
                (s: Sample.t) =>
                  Haz3lcore.MvuShape.is_html(
                    Haz3lcore.MvuShape.strip_wrappers(s.value),
                  ),
                samples,
              ),
            )
          | None => 0
          }
        ),
      mtr.projectors,
      0,
    );
  check(int, "projector stream carries the live HTML", 1, html_samples);
};

let unprojected_view_not_run = () => {
  let (_, _, probes) = probe_run(view_probe_def ++ "^dbl(21)");
  let total =
    Sample.Map.fold((_, ss, acc) => acc + List.length(ss), probes, 0);
  check(int, "no projector, no view run, no samples", 0, total);
};

let member_access = () =>
  run_test(
    "^name.member accesses the definition record",
    "51",
    "let ^dbl = " ++ dbl_module ++ " in ^dbl.expand(21) + ^dbl.update((3, 9))",
  );

let redex_as_model = () =>
  run_test(
    "a committed transition normalizes in the main run",
    "18",
    "let ^dbl = " ++ dbl_module ++ " in ^dbl(^dbl.update(3, 9))",
  );

let update_probe_def = "let ^dbl = {
let init = 0;
let update = fun (m, a) -> ^^probe(m + a);
let view = fun m -> Text(string_of_int(m));
let expand = fun m -> m * 2
} in ";

let update_probe_fires_once = () => {
  let (_, manuals, probes) =
    probe_run(update_probe_def ++ "^^livelit(^dbl(^dbl.update(3, 9)))");
  let count_12 = ids =>
    List.fold_left(
      (acc, id) =>
        acc
        + List.length(
            List.filter(
              (s: Sample.t) =>
                switch (Haz3lcore.MvuShape.strip_wrappers(s.value).term) {
                | Atom(Int(n)) => Bigint.to_int(n) == Some(12)
                | _ => false
                },
              Option.value(Sample.Map.lookup(id, probes), ~default=[]),
            ),
          ),
      0,
      ids,
    );
  check(int, "update probe sampled exactly once", 1, count_12(manuals));
  /* the model argument is also targeted — the commit path reads its value */
  let all_ids = Sample.Map.fold((id, _, acc) => [id, ...acc], probes, []);
  check(
    int,
    "transition value also sampled at the model",
    2,
    count_12(all_ids),
  );
};

/* The commit path's product: the redex term must print to text that
   reparses and evaluates to the same transition */
let redex_roundtrip = () => {
  let redex =
    UserLivelit.mk_update_redex(
      ~name="dbl",
      ~model_value=parse_exp("3"),
      ~action=parse_exp("9"),
    );
  let seg =
    Haz3lcore.ExpToSegment.any_to_segment(
      ~settings={
        ...
          Haz3lcore.ExpToSegment.Settings.of_core(
            ~inline=true,
            CoreSettings.off,
          ),
        show_unknown_as_hole: false,
        hole_tiles: false,
        fold_fn_bodies: `NoFold,
        project_tables: false,
      },
      Exp(redex),
    );
  let text = Haz3lcore.Printer.of_segment(~holes="?", ~indent="", seg);
  run_test(
    "committed transition text round-trips: " ++ text,
    "18",
    "let ^dbl = " ++ dbl_module ++ " in ^dbl(" ++ text ++ ")",
  );
};

/* Regression (color picker): a mid-run HTML sample is Closure-wrapped with
   OPEN handler funs inside; consuming it must substitute the environment
   (close_value), not strip it, or handlers lose their definitions */
let sampled_handlers_are_closed = () => {
  let (mtr, _, probes) =
    probe_run(
      "let ^pk = {
let bump = fun x -> x + 1;
let init = 0;
let update = fun (m, a) -> a;
let view = fun m -> Div([OnClickAt(fun (x, y) -> bump(x + m))], []);
let expand = fun m -> m
} in ^^livelit(^pk(5))",
    );
  let html =
    Id.Map.fold(
      (id, _, acc) =>
        switch (acc) {
        | Some(_) => acc
        | None =>
          Option.bind(Sample.Map.lookup(id, probes), samples =>
            List.find_map(
              (s: Sample.t) => {
                let v = Haz3lcore.MvuShape.close_value(s.value);
                Haz3lcore.MvuShape.is_html(v) ? Some(v) : None;
              },
              samples,
            )
          )
        },
      mtr.projectors,
      None,
    );
  switch (html) {
  | None => fail("no HTML sample recorded")
  | Some(html) =>
    let handler =
      switch (Haz3lcore.MvuShape.of_constructor_raw(html)) {
      | Some(("Div", body)) =>
        switch (Haz3lcore.MvuShape.of_tuple(body)) {
        | Some([attrs, _children]) =>
          switch (Haz3lcore.MvuShape.of_list(attrs)) {
          | Some([attr]) =>
            switch (Haz3lcore.MvuShape.of_constructor_raw(attr)) {
            | Some(("OnClickAt", handler)) => handler
            | _ => fail("expected OnClickAt attr")
            }
          | _ => fail("expected one attr")
          }
        | _ => fail("expected (attrs, children)")
        }
      | _ => fail("expected a Div sample")
      };
    let action =
      evaluate(
        IdTagged.FreshGrammar.Exp.ap(Forward, handler, parse_exp("(2, 3)")),
      );
    check(dhexp_typ, "sampled handler evaluates closed", run("8"), action);
  };
};

/* A livelit's model argument reaches the projector-side evaluation as
   RAW syntax: its constructors carry no type. Ascribing such a
   constructor to its sum (the view's `m : Model`) must type it rather
   than stick, or every `case` on the model in the view is stuck. */
let untyped_ctor_ascription = () => {
  let gate: Typ.t =
    Typ.temp(
      Sum([
        ConstructorMap.Variant(
          "Nand",
          ConstructorMap.empty_variant_ann,
          Some(Typ.temp(Atom(Int))),
        ),
        ConstructorMap.Variant("Or", ConstructorMap.empty_variant_ann, None),
      ]),
    );
  let asc = e => Exp.fresh(Asc(e, gate));
  let bare = asc(Exp.fresh(Constructor("Or", None)));
  switch (Exp.term_of(Ascriptions.transition_multiple(bare))) {
  | Constructor("Or", Some(Some(_))) => ()
  | _ => fail("bare untyped constructor did not take the sum's type")
  };
  let applied =
    asc(
      Exp.fresh(
        Ap(
          Forward,
          Exp.fresh(Constructor("Nand", None)),
          IdTagged.FreshGrammar.Exp.int(7),
        ),
      ),
    );
  switch (Exp.term_of(Ascriptions.transition_multiple(applied))) {
  | Ap(_, {term: Constructor("Nand", Some(Some(_))), _}, _) => ()
  | _ => fail("applied untyped constructor did not take the sum's type")
  };
};

/* A livelit renders VALUES of the type it expands to (rich probes, via
   LivelitRenderer): a sampled constructor-with-payload value must go
   through wrap and view like a nullary one. */
let renders_payload_ctor = () => {
  let text = "type Point = + P(Int, Int) in
let ^point = {
  type Model = Point;
  type Action = + Nothing;
  let init : Model = P(50, 50);
  let update(m: Model, _: Action): Model = m;
  let view(p: Model): HTML =
    case p
    | P(x, y) => Node(\"svg\", [Create(\"cx\", string_of_int(x + y))], [])
    end;
  let expand(p: Model): Point = p;
  let wrap(p: Point): Model = p;
  let shape : LivelitShape = Tab(6, 4)
} in
let shift(p: Point): Point = case p | P(x, y) => P(x + 10, y + 10) end in
let p0 : Point = P(30, 40) in
shift(p0)";
  switch (Haz3lcore.Parser.to_zipper(~root=Exp, text)) {
  | None => fail("parse")
  | Some(z) =>
    let mtr = Haz3lcore.MakeTerm.from_zip_for_sem(z, ~root=Exp);
    let settings = {
      ...CoreSettings.on,
      probe_all: true,
    };
    let (info_map, elaborated) =
      Statics.mk(settings, Builtins.ctx_init(Some(Int)), mtr.term);
    let probe_ids = Haz3lcore.CachedStatics.all_probeable_ids(info_map);
    let targets =
      Haz3lcore.CachedStatics.compute_targets(
        ~settings,
        ~info_map,
        ~probe_ids,
      );
    let (_, state) =
      Evaluator.evaluate(
        ~eval_info=EvalInfo.of_targets(targets),
        ~env=Builtins.env_init,
        elaborated,
      );
    let probes = EvaluatorState.get_probes(state);
    /* a site typed Point whose sample is a P(..) application */
    let site =
      Id.Map.fold(
        (id, samples, acc) =>
          switch (acc, Id.Map.find_opt(id, info_map)) {
          /* a site OUTSIDE the livelit (its ctx binds ^point): the view's
             own parameter is Point-typed too, but ^point is not in scope
             there — and Id.Map order depends on the ids the suite has
             minted so far */
          | (None, Some(Info.InfoExp({ty, ctx, _}) as info))
              when Ctx.lookup_livelit(ctx, "point") != None =>
            switch (Typ.term_of(ty)) {
            | Var("Point") =>
              switch (samples) {
              | [s, ..._] =>
                let s: Sample.t = s;
                switch (Exp.term_of(s.value)) {
                | Ap(_, {term: Constructor("P", _), _}, _) =>
                  Some((info, s.value))
                | _ => acc
                };
              | [] => acc
              }
            | _ => acc
            }
          | _ => acc
          },
        probes,
        None,
      );
    switch (site) {
    | None => fail("no Point-typed site with a P(..) sample")
    | Some((info, value)) =>
      let cands = Haz3lcore.LivelitRenderer.candidates(Some(info));
      check(bool, "^point is a candidate", true, cands != []);
      let (ctx, _) =
        Option.get(Haz3lcore.LivelitRenderer.site(Some(info)));
      let html =
        Haz3lcore.LivelitRenderer.html_of(~ctx, List.hd(cands), value);
      check(
        bool,
        "P(..) renders through wrap and view: " ++ Exp.show(value),
        true,
        html != None,
      );
    };
  };
};

/* Probe samples drawn through a livelit: a Point livelit with a given
   view, and a Point-typed site outside it with a P(..) sample */
let point_def = (view: string) =>
  "type Point = + P(Int, Int) in
let ^point = {
  type Model = Point;
  type Action = + Nothing;
  let init : Model = P(50, 50);
  let update(m: Model, _: Action): Model = m;
  "
  ++ view
  ++ ";
  let expand(p: Model): Point = p;
  let wrap(p: Point): Model = p;
  let shape : LivelitShape = Inline(6)
} in
";

let point_program = view =>
  point_def(view)
  ++ "let shift(p: Point): Point = case p | P(x, y) => P(x + 10, y + 10) end in
let p0 : Point = P(30, 40) in
shift(p0)";

let text_of_html = (html: Exp.t): option(string) =>
  switch (Haz3lcore.MvuShape.of_constructor(html)) {
  | Some(("Text", body)) => Haz3lcore.MvuShape.of_string(body)
  | _ => None
  };

/* The site's ctx, the livelit that views it, and its sample:
   renders_payload_ctor's pipeline */
let point_sample =
    (text: string): (Ctx.t, LivelitCtx.raw_livelit, Exp.t, Info.t) =>
  switch (Haz3lcore.Parser.to_zipper(~root=Exp, text)) {
  | None => fail("parse")
  | Some(z) =>
    let mtr = Haz3lcore.MakeTerm.from_zip_for_sem(z, ~root=Exp);
    let settings = {
      ...CoreSettings.on,
      probe_all: true,
    };
    let (info_map, elaborated) =
      Statics.mk(settings, Builtins.ctx_init(Some(Int)), mtr.term);
    let probe_ids = Haz3lcore.CachedStatics.all_probeable_ids(info_map);
    let targets =
      Haz3lcore.CachedStatics.compute_targets(
        ~settings,
        ~info_map,
        ~probe_ids,
      );
    let (_, state) =
      Evaluator.evaluate(
        ~eval_info=EvalInfo.of_targets(targets),
        ~env=Builtins.env_init,
        elaborated,
      );
    let site =
      Id.Map.fold(
        (id, samples, acc) =>
          switch (acc, Id.Map.find_opt(id, info_map)) {
          | (None, Some(Info.InfoExp({ty, ctx, _}) as info))
              when Ctx.lookup_livelit(ctx, "point") != None =>
            switch (Typ.term_of(ty), samples) {
            | (Var("Point"), [s, ..._]) =>
              let s: Sample.t = s;
              switch (Exp.term_of(s.value)) {
              | Ap(_, {term: Constructor("P", _), _}, _) =>
                Some((info, s.value))
              | _ => acc
              };
            | _ => acc
            }
          | _ => acc
          },
        EvaluatorState.get_probes(state),
        None,
      );
    switch (site) {
    | None => fail("no Point-typed site with a P(..) sample")
    | Some((info, value)) =>
      let (ctx, _) =
        Option.get(Haz3lcore.LivelitRenderer.site(Some(info)));
      switch (Haz3lcore.LivelitRenderer.candidates(Some(info))) {
      | [ll, ..._] => (ctx, ll, value, info)
      | [] => fail("^point is not a candidate")
      };
    };
  };

/* The render memo is keyed on the livelit's definition too: an edit to
   its view redraws samples whose values did not change */
let edited_view_redraws = () => {
  let drawn = view => {
    let (ctx, ll, value, _) = point_sample(point_program(view));
    Option.bind(
      Haz3lcore.LivelitRenderer.html_of(~ctx, ll, value),
      text_of_html,
    );
  };
  check(
    option(string),
    "the view",
    Some("one"),
    drawn("let view(p: Model): HTML = Text(\"one\")"),
  );
  check(
    option(string),
    "the edited view",
    Some("two"),
    drawn("let view(p: Model): HTML = Text(\"two\")"),
  );
};

/* A view may take a second argument, a ViewContext: where it is drawn
   (at its Literal, Offside as a probe sample, in a probe's Drawer) and
   whether it is editable. Hazel tells the two forms apart by the view's
   type. The two-argument view below prints the context it is given, so
   each place's context shows in its HTML. */
let ctx_view = "let view(p: Model, ctx: ViewContext): HTML =
    Text((case ctx.at | Literal => \"L\" | Offside => \"O\" | Drawer => \"D\" end)
         ++ (if ctx.editable then \"+\" else \"-\"))";

let plain_view = "let view(p: Model): HTML = Text(\"plain\")";

/* the written-out context type selects the two-argument form too */
let structural_ctx_view = "let view = fun (p, ctx) : (Model, (at=Place, editable=Bool)) ->
    Text(if ctx.editable then \"+\" else \"-\")";

/* The livelit bound by a program's ^point, from the context at its use */
let lookup_point = (text: string): LivelitCtx.raw_livelit => {
  let (m, _) = statics(text);
  let found =
    Id.Map.fold(
      (_, info: Info.t, acc) =>
        switch (acc, info) {
        | (Some(_), _) => acc
        | (None, InfoExp({ctx, _})) => Ctx.lookup_livelit(ctx, "point")
        | (None, _) => None
        },
      m,
      None,
    );
  switch (found) {
  | Some(ll) => ll
  | None => fail("^point is not in any context")
  };
};

let view_form_by_type = () => {
  let takes = view => lookup_point(point_def(view) ++ "1").view_takes_ctx;
  check(
    bool,
    "(Model, ViewContext) -> HTML takes it",
    true,
    takes(ctx_view),
  );
  check(bool, "Model -> HTML does not", false, takes(plain_view));
  check(
    bool,
    "(Model, (at=Place, editable=Bool)) -> HTML takes it",
    true,
    takes(structural_ctx_view),
  );
  /* a one-argument view whose Model is itself a pair keeps one argument */
  let pair = "let ^point = {
  type Model = (Int, Int);
  type Action = + Nothing;
  let init : Model = (1, 2);
  let update(m: Model, _: Action): Model = m;
  let view(m: Model): HTML = Text(\"pair\");
  let expand(m: Model): (Int, Int) = m
} in 1";
  check(
    bool,
    "a pair Model is not a context",
    false,
    lookup_point(pair).view_takes_ctx,
  );
};

/* Offside and Drawer: a probe sample's view is told where it is drawn,
   and that it is not editable (no literal to rewrite) */
let view_context_in_probes = () => {
  let (ctx, ll, value, _) = point_sample(point_program(ctx_view));
  let at = place =>
    Option.bind(
      Haz3lcore.LivelitRenderer.html_of(~ctx, ~place, ll, value),
      text_of_html,
    );
  check(option(string), "offside sample", Some("O-"), at(Offside));
  check(option(string), "drawer sample", Some("D-"), at(Drawer));
  /* the render memo keeps the places apart */
  check(option(string), "offside again", Some("O-"), at(Offside));
};

/* the HTML texts the projectors' sample streams carry */
let projector_texts = (projectors: Id.Map.t(_), probes): list(string) =>
  Id.Map.fold(
    (id, _, acc) =>
      acc
      @ (
        switch (Sample.Map.lookup(id, probes)) {
        | Some(samples) =>
          List.filter_map(
            (s: Sample.t) => {
              let v = Haz3lcore.MvuShape.close_value(s.value);
              Haz3lcore.MvuShape.is_html(v) ? text_of_html(v) : None;
            },
            samples,
          )
        | None => []
        }
      ),
    projectors,
    [],
  );

/* Literal: a projected use's view (run in the main evaluation) is told it
   draws at its literal, which is editable */
let view_context_at_literal = () => {
  let (mtr, _, probes) =
    probe_run(point_def(ctx_view) ++ "^^livelit(^point(P(1, 2)))");
  check(
    list(string),
    "literal",
    ["L+"],
    projector_texts(mtr.projectors, probes),
  );
};

/* A one-argument view keeps working at every place */
let one_arg_view_everywhere = () => {
  let (ctx, ll, value, _) = point_sample(point_program(plain_view));
  let at = place =>
    Option.bind(
      Haz3lcore.LivelitRenderer.html_of(~ctx, ~place, ll, value),
      text_of_html,
    );
  check(option(string), "offside", Some("plain"), at(Offside));
  check(option(string), "drawer", Some("plain"), at(Drawer));
  let (mtr, _, probes) =
    probe_run(point_def(plain_view) ++ "^^livelit(^point(P(1, 2)))");
  check(
    list(string),
    "literal",
    ["plain"],
    projector_texts(mtr.projectors, probes),
  );
};

/* A literal's projector requests dynamics, so its id and its model
   argument's id land in statics.targets. The statics gate compared the
   zipper's probe pins against targets' keys, saw a probe change on
   every calculate, and reran statics twice per frame (Views / Color took
   6s to open). A calculate that changes nothing must rerun no statics. */
let literal_statics_reused = () =>
  switch (
    Haz3lcore.Parser.to_zipper(
      ~root=Exp,
      view_probe_def ++ "^^probe(^^livelit(^dbl(21)) + 1)",
    )
  ) {
  | None => fail("failed to parse")
  | Some(z) =>
    Haz3lcore.CachedStatics.offered := [];
    let calculate = (~statics_mode=?, m) =>
      Web.CodeWithStatics.Update.calculate(
        ~settings=CoreSettings.on,
        ~is_edited=false,
        ~statics_mode?,
        ~stitch=x => x,
        ~dynamics=Dynamics.Map.empty,
        ~is_dynamic_term=false,
        m,
      );
    let m =
      Web.CodeWithStatics.Model.mk(Haz3lcore.Editor.Model.mk(~root=Exp, z))
      |> calculate(~statics_mode=Force);
    let pins = Haz3lcore.CachedStatics.probe_ids_of_zipper(z);
    check(
      bool,
      "targets reach past the probe pins",
      true,
      Id.Map.cardinal(m.statics.targets) > Id.Map.cardinal(pins),
    );
    /* every init of this (unstitched) editor is remembered here */
    Haz3lcore.CachedStatics.last_inits := [];
    let m' = calculate(m);
    check(
      int,
      "an unchanged program reruns no statics",
      0,
      List.length(Haz3lcore.CachedStatics.last_inits^),
    );
    /* and the gate still sees a real change: the same statics, with the
       probe removed, are recomputed */
    let unprobed =
      Web.CodeWithStatics.Model.mk(
        ~statics=m'.statics,
        Haz3lcore.Editor.Model.mk(
          ~root=Exp,
          Haz3lcore.Zipper.update_manuals(_ => [], z),
        ),
      );
    ignore(calculate(unprobed));
    check(
      bool,
      "removing the probe is a change",
      true,
      List.length(Haz3lcore.CachedStatics.last_inits^) > 0,
    );
  };

let tests = [
  (
    "UserLivelits",
    [
      test_case("pattern parses as binder", `Quick, parses_as_binder),
      test_case("expansion evaluates", `Quick, evaluates),
      test_case("module definition", `Quick, module_evaluates),
      test_case("module helpers", `Quick, module_helpers),
      test_case("module funlet members", `Quick, module_funlet_members),
      test_case("module missing members", `Quick, module_missing_members),
      test_case("module adapter", `Quick, module_adapter),
      test_case("multiple uses", `Quick, multiple_uses),
      test_case("labeled out of order", `Quick, labeled_out_of_order),
      test_case("helpers inside definition", `Quick, helpers_in_def),
      test_case("shadows builtin", `Quick, shadows_builtin),
      test_case("nested shadowing", `Quick, nested_shadowing),
      test_case("bad definition marked", `Quick, bad_def_marked),
      test_case("bad arity marked", `Quick, bad_arity_marked),
      test_case("unbound use marked", `Quick, unbound_use_marked),
      test_case("good definition unmarked", `Quick, good_def_unmarked),
      test_case(
        "livelit renders a payload constructor value",
        `Quick,
        renders_payload_ctor,
      ),
      test_case(
        "an edited view redraws its samples",
        `Quick,
        edited_view_redraws,
      ),
      test_case("view form told apart by type", `Quick, view_form_by_type),
      test_case(
        "view context offside and in the drawer",
        `Quick,
        view_context_in_probes,
      ),
      test_case(
        "view context at the literal",
        `Quick,
        view_context_at_literal,
      ),
      test_case(
        "one-argument view at every place",
        `Quick,
        one_arg_view_everywhere,
      ),
      test_case(
        "untyped constructor takes its sum type under ascription",
        `Quick,
        untyped_ctor_ascription,
      ),
      test_case("adapter contract", `Quick, adapter),
      test_case("positional shape field", `Quick, shape_field),
      test_case("commit vs ephemeral decision", `Quick, commit_decision),
      test_case("view probes fire when projected", `Quick, view_probes_fire),
      test_case(
        "projector samples the live HTML",
        `Quick,
        projector_gets_html_sample,
      ),
      test_case(
        "unprojected uses don't run view",
        `Quick,
        unprojected_view_not_run,
      ),
      test_case("member access", `Quick, member_access),
      test_case("redex as model", `Quick, redex_as_model),
      test_case("update probe fires once", `Quick, update_probe_fires_once),
      test_case("redex round-trips", `Quick, redex_roundtrip),
      test_case(
        "sampled handlers are closed",
        `Quick,
        sampled_handlers_are_closed,
      ),
      test_case(
        "a literal's statics survive a quiet calculate",
        `Quick,
        literal_statics_reused,
      ),
    ],
  ),
];

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
      test_case("adapter contract", `Quick, adapter),
      test_case("positional shape field", `Quick, shape_field),
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
    ],
  ),
];

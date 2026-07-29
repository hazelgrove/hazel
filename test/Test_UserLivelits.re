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
let size = (30, 5)
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
      "size member sets the projector size",
      true,
      ll.size
      == {
           horizontal: 30,
           vertical: Block(5),
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

let size_field = () => {
  let def_user =
    parse_exp("(0, fun (m, a) -> a, fun m -> 0, fun m -> m, (30, 5))");
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
      "fifth field sets the projector size",
      true,
      ll.size
      == {
           horizontal: 30,
           vertical: Block(5),
         },
    )
  | Error(_) => fail("adapter rejected a 5-field definition")
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
      test_case("size field", `Quick, size_field),
    ],
  ),
];

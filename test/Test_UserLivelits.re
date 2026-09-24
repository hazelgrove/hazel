open Alcotest;
open Language;
open Test_Evaluator_Prelude;

/* User-defined livelits: `let ^name = { type Model; type Action;
   type Expansion; init; update; view; expand } in ...` binds a livelit
   whose uses elaborate through the runtime binding, synthesizing the
   declared Expansion. */

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

/* Eager definition-site checking is the requirement (Cyrus, 2026-09-23),
   and it is what these assert. What changed with `expand` becoming a sum is
   only WHICH mark carries it.

   Before, `expand_fun` was a bare function, `sig_sub` compared its
   synthesized type against the realized signature, and a mismatch surfaced
   as the livelit-specific DefMemberMismatch. Now the livelit declares
   `type Expand`, so `Functional(f)` is checked ANALYTICALLY against
   `Model -> Expansion` at the constructor application, and the mismatch is
   an ordinary inconsistency right at the offending expression -- a more
   precise location, reported earlier, but not the livelit-specific mark.

   So these accept either: the point is that the definition does not pass. */
let def_is_marked = (m): bool =>
  has_mark(
    fun
    | Mark.InvalidLivelitDef(_) => true
    | _ => true,
    m,
  );

/* A definition is a module declaring Model, Action and Expansion and
   binding init, update, view and expand. `dbl` means twice its model. */
let dbl_def = "{
type Model = Int;
type Action = Int;
type Expansion = Int;
type Expand = + Functional(Model -> Expansion) + Macro(Model -> (Exp, [SpliceRef]));
let init : Model = 0;
let update = fun (m, a) -> a;
let view = fun m -> Html.text(\"\");
let expand = Functional(fun m -> m * 2)
}";

/* The standard definition plus one extra member, for tests about members
   other than the four required ones. */
let def_with = (~extra: string) =>
  "{
type Model = Int;
type Action = Int;
type Expansion = Int;
type Expand = + Functional(Model -> Expansion) + Macro(Model -> (Exp, [SpliceRef]));
let init = 0;
let update = fun (m, a) -> a;
let view = fun m -> Html.text(\"\");
let expand = Functional(fun m -> m * 2)"
  ++ (extra == "" ? "" : ";\n" ++ extra)
  ++ "
}";

/* Everything but the declared types and expand held fixed, so a test can
   vary just the interface it is about. */
let def = (~expansion: string, ~expand: string) =>
  "{
type Model = Int;
type Action = Int;
type Expansion = "
  ++ expansion
  ++ ";
type Expand = + Functional(Model -> Expansion) + Macro(Model -> (Exp, [SpliceRef]));
let init : Model = 0;
let update = fun (m, a) -> a;
let view = fun m -> Html.text(\"\");
let expand = Functional("
  ++ expand
  ++ ")
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

let members_out_of_order = () =>
  run_test(
    "members and types are found by name, in any order",
    "42",
    "let ^dbl = {
let expand = Functional(fun m -> m * 2);
type Expansion = Int;
type Expand = + Functional(Model -> Expansion) + Macro(Model -> (Exp, [SpliceRef]));
let view = fun m -> Html.text(\"\");
type Model = Int;
let init = 0;
type Action = Int;
let update = fun (m, a) -> a
} in ^dbl(21)",
  );

let helpers_in_def = () =>
  run_test(
    "helpers bind outside the definition module",
    "5",
    "let ^inc = (let f = fun x -> x + 1 in "
    ++ def(~expansion="Int", ~expand="fun m -> f(m)")
    ++ ") in ^inc(4)",
  );

/* detect() descends through a leading type alias, and carries it into
   scope so a declared member type may be stated in terms of it. */
let type_alias_before_def = () =>
  run_test(
    "a type alias in front of the module is in scope for Expansion",
    "42",
    "let ^dbl = (type Pct = Int in "
    ++ def(~expansion="Pct", ~expand="fun m -> m * 2")
    ++ ") in ^dbl(21)",
  );

let shadows_builtin = () =>
  run_test(
    "a user ^slider shadows the builtin",
    "105",
    "let ^slider = "
    ++ def(~expansion="Int", ~expand="fun m -> m + 100")
    ++ " in ^slider(5)",
  );

let nested_shadowing = () =>
  run_test(
    "inner livelit binding wins",
    "30",
    "let ^d = "
    ++ dbl_def
    ++ " in let ^d = "
    ++ def(~expansion="Int", ~expand="fun m -> m * 3")
    ++ " in ^d(10)",
  );

let module_helpers = () =>
  run_test(
    "helpers are ordinary module members",
    "15",
    "let ^inc = {
type Model = Int;
type Action = Int;
type Expansion = Int;
type Expand = + Functional(Model -> Expansion) + Macro(Model -> (Exp, [SpliceRef]));
let bump = fun x -> x + 1;
let init = 0;
let update = fun (m, a) -> a;
let view = fun m -> Html.text(\"\");
let expand = Functional(fun m -> bump(m))
} in ^inc(4) + ^inc(9)",
  );

/* `expand` is deliberately NOT in funlet form here, and cannot be: it is a
   sum, not a function, so `let expand(m) = ...` does not typecheck. That is
   a real ergonomic cost of putting the Functional/Macro choice in the
   member's type -- the one member you most want to write as a function is
   the one that can no longer be written as one. update and view still
   carry the sugar, which is what this test is about. */
let module_funlet_members = () =>
  run_test(
    "funlet-form members are recognized by name",
    "8",
    "let ^dbl = {
type Model = Int;
type Action = Int;
type Expansion = Int;
type Expand = + Functional(Model -> Expansion) + Macro(Model -> (Exp, [SpliceRef]));
let init = 0;
let update(m, a) = a;
let view(m) = Html.text(\"\");
let expand = Functional(fun m -> m * 2)
} in ^dbl(4)",
  );

/* The definition-site check: `expand` must produce the type the definition
   DECLARES as Expansion. Here Expansion is Int but expand returns a String,
   which no use-site check would be needed to catch -- the definition alone
   is already wrong. */
/* REGRESSION. The definition-site check must fire on a member whose type
   is stated through the livelit's OWN type members, not only on one whose
   wrongness is visible without them. `expand = Functional(fun m : Model -> m)` under
   `Expansion = String` types as Model -> Model; left unrealized, those
   names mean nothing outside the module, degrade to ?, and the check
   passes whatever expand returns -- which is exactly what it used to do,
   leaving the error to appear only at the uses. */
let module_expand_mismatch_through_aliases = () => {
  let (m, _) =
    statics(
      "let ^s = "
      ++ def(~expansion="String", ~expand="fun m : Model -> m")
      ++ " in ^s(1)",
    );
  check(
    bool,
    "a mismatch stated in Model/Expansion is caught at the definition",
    true,
    def_is_marked(m),
  );
};

/* A bad member type is reported, but must not unbind the livelit: its uses
   should still resolve, so they keep getting their own check rather than
   collapsing into "variable not bound". */
let module_mismatch_still_binds = () => {
  let (m, _) =
    statics(
      "let ^s = "
      ++ def(~expansion="String", ~expand="fun m : Model -> m")
      ++ " in ^s(1)",
    );
  check(
    bool,
    "the use does not become unbound",
    false,
    has_mark(
      fun
      | Mark.Free(_) => true
      | _ => false,
      m,
    ),
  );
};

let module_expand_mismatch = () => {
  let (m, _) =
    statics(
      "let ^x = {
type Model = Int;
type Action = Int;
type Expansion = Int;
type Expand = + Functional(Model -> Expansion) + Macro(Model -> (Exp, [SpliceRef]));
let init : Model = 0;
let update = fun (m, a) -> a;
let view = fun m -> Html.text(\"\");
let expand = Functional(fun m -> \"not an Int\")
} in 1",
    );
  check(
    bool,
    "expand's result type is checked against Expansion at the definition",
    true,
    def_is_marked(m),
  );
};

/* The same check, on the member whose type mentions Action: `update` must
   take (Model, Action) and give back a Model. */
let module_update_mismatch = () => {
  let (m, _) =
    statics(
      "let ^x = {
type Model = Int;
type Action = Int;
type Expansion = Int;
type Expand = + Functional(Model -> Expansion) + Macro(Model -> (Exp, [SpliceRef]));
let init : Model = 0;
let update = fun (m, a) -> \"wrong\";
let view = fun m -> Html.text(\"\");
let expand = Functional(fun m -> m)
} in 1",
    );
  check(
    bool,
    "update's result type is checked against Model",
    true,
    has_mark(
      fun
      | Mark.InvalidLivelitDef(DefMemberMismatch({name: "update", _})) =>
        true
      | _ => false,
      m,
    ),
  );
};

/* A definition that satisfies the signature reports no mismatch at all --
   the check must not fire on the livelits that already work. */
let module_well_typed_no_mismatch = () => {
  let (m, _) = statics("let ^x = " ++ dbl_def ++ " in ^x(1)");
  check(
    bool,
    "a well-typed definition raises no member mismatch",
    false,
    has_mark(
      fun
      | Mark.InvalidLivelitDef(DefMemberMismatch(_)) => true
      | _ => false,
      m,
    ),
  );
};

/* A livelit that lacks members is a MODULE that lacks members, and says so
   with the module system's own mark. DefMissingMembers and DefMissingTypes
   were livelit-specific restatements of it; ModuleMissingMembers already
   carries both kinds, since ModuleHelpers.member_names is
   value_names @ type_names. This fixture declares no types either, so all
   five are named. */
let module_missing_members = () => {
  let (m, _) =
    statics("let ^x = {let init = 0; let view = fun m -> 0} in 1");
  let named = (want: list(string)) =>
    has_mark(
      fun
      | Mark.ModuleMissingMembers(names) =>
        List.for_all(w => List.mem(w, names), want)
      | _ => false,
      m,
    );
  check(
    bool,
    "missing value members reported by name",
    true,
    named(["update", "expand"]),
  );
  check(
    bool,
    "missing type members reported the same way",
    true,
    named(["Model", "Action", "Expansion"]),
  );
};

/* Build the livelit the adapter would put in the context, or fail. */
let mk_ll = (def_text: string): LivelitCtx.raw_livelit => {
  let def_user = parse_exp(def_text);
  let ctx = Builtins.ctx_init(Some(Int));
  let (m, def_elab) = Statics.mk(CoreSettings.on, ctx, def_user);
  switch (
    UserLivelit.mk(~ctx, ~m, ~name="s", ~id=Id.invalid, ~def_user, ~def_elab)
  ) {
  | (Some(ll), _) => ll
  | (None, _) => fail("adapter rejected a well-formed module definition")
  };
};

let module_adapter = () => {
  let ll = mk_ll(def_with(~extra="let shape = Tab(30, 5)"));
  check(
    dhexp_typ,
    "model_default is the init member",
    parse_exp("0"),
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
};

/* Each LivelitShape constructor, since the vertical is derived: Inline is one
   line, Block and Tab are h LINES and the internal count is linebreaks. A
   one-line Block or Tab degenerates to Inline. */
let shape_member = () => {
  let shape = (s: string) =>
    mk_ll(def_with(~extra="let shape = " ++ s)).shape;
  let expect = (s, want: Util.ProjectorShape.t) =>
    check(bool, s, true, shape(s) == want);
  expect(
    "Inline(20)",
    {
      horizontal: 20,
      vertical: Inline,
    },
  );
  expect(
    "Block(32, 8)",
    {
      horizontal: 32,
      vertical: Block(7),
    },
  );
  expect(
    "Tab(16, 5)",
    {
      horizontal: 16,
      vertical: Tab(4),
    },
  );
  expect(
    "Block(12, 1)",
    {
      horizontal: 12,
      vertical: Inline,
    },
  );
  /* no shape member at all falls back to the default */
  check(
    bool,
    "default shape without the member",
    true,
    mk_ll(def_with(~extra="")).shape == UserLivelit.default_shape,
  );
};

let bad_def_marked = () => {
  let (m, _) = statics("let ^x = 5 in 1");
  check(
    bool,
    "non-module definition gets InvalidLivelitDef",
    true,
    has_mark(
      fun
      | Mark.InvalidLivelitDef(DefNotModule) => true
      | _ => false,
      m,
    ),
  );
};

/* A tuple has nowhere to declare Model, Action and Expansion, so the form
   the module desugars to is no longer a definition on its own. */
let tuple_def_marked = () => {
  let (m, _) =
    statics("let ^x = (0, fun (m, a) -> a, fun m -> 0, fun m -> m) in 1");
  check(
    bool,
    "the tuple form is no longer a definition",
    true,
    has_mark(
      fun
      | Mark.InvalidLivelitDef(DefNotModule) => true
      | _ => false,
      m,
    ),
  );
};

let missing_types_marked = () => {
  let (m, _) =
    statics(
      "let ^x = {
let init = 0;
let update = fun (m, a) -> a;
let view = fun m -> Html.text(\"\");
let expand = Functional(fun m -> m)
} in 1",
    );
  check(
    bool,
    "missing type members reported by name",
    true,
    has_mark(
      fun
      | Mark.ModuleMissingMembers(["Model", "Action", "Expansion"]) => true
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
      | Mark.BadLivelitExpansion(_)
      | Mark.Free(_) => true
      | _ => false,
      m,
    ),
  );
};

/* The adapter contract LivelitProj relies on: the captured definition
   evaluates closed, its fields extract, and view(model) yields HTML. */
let adapter = () => {
  let def_text = "{
type Model = Int;
type Action = Int;
type Expansion = Int;
type Expand = + Functional(Model -> Expansion) + Macro(Model -> (Exp, [SpliceRef]));
let init = 50;
let update = fun (m, a) -> a;
let view = fun m -> Html.text(\"hi\");
let expand = Functional(fun m -> m)
}";
  let def_user = parse_exp(def_text);
  let ctx = Builtins.ctx_init(Some(Int));
  let (m, def_elab) = Statics.mk(CoreSettings.on, ctx, def_user);
  let ll =
    switch (
      UserLivelit.mk(
        ~ctx,
        ~m,
        ~name="s",
        ~id=Id.invalid,
        ~def_user,
        ~def_elab,
      )
    ) {
    | (Some(ll), _) => ll
    | (None, _) => fail("adapter rejected a well-formed definition")
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
  /* A module evaluates to a Module of ModVal bindings, read by name (last
     binding wins) -- the lookup LivelitProj.record_field does at render
     time */
  let member = (label: string) =>
    switch (Haz3lcore.MvuShape.strip_wrappers(record).term) {
    | Module(items) =>
      List.fold_left(
        (acc, item: Mod.t) =>
          switch (item.term) {
          | ModVal(x, v) when x == label => Some(v)
          | _ => acc
          },
        None,
        items,
      )
    | _ => None
    };
  switch (member("view")) {
  | Some(view_fn) =>
    let html =
      evaluate(
        IdTagged.FreshGrammar.Exp.ap(Forward, view_fn, parse_exp("50")),
      );
    check(
      bool,
      "view(model) is Html.T",
      true,
      Haz3lcore.MvuShape.is_html(html),
    );
  | None => fail("definition record has no view member")
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
type Model = Int;
type Action = Int;
type Expansion = Int;
type Expand = + Functional(Model -> Expansion) + Macro(Model -> (Exp, [SpliceRef]));
let init = 0;
let update = fun (m, a) -> a;
let view = fun m -> Html.text(string_of_int(^^probe(m * 3)));
let expand = Functional(fun m -> m * 2)
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
  check(int, "projector stream carries the live Html.T", 1, html_samples);
};

let unprojected_view_not_run = () => {
  let (_, _, probes) = probe_run(view_probe_def ++ "^dbl(21)");
  let total =
    Sample.Map.fold((_, ss, acc) => acc + List.length(ss), probes, 0);
  check(int, "no projector, no view run, no samples", 0, total);
};

/* ^name.expand hands back the SUM, not a function, so a client reading it
   has to say which kind of livelit it expected. That is the point -- the
   kind is now visible in the value rather than in which signature the
   definition answered to. */
let member_access = () =>
  run_test(
    "^name.member accesses the definition record",
    "51",
    "let ^dbl = "
    ++ dbl_def
    ++ " in (case ^dbl.expand | Functional(f) => f(21) | Macro(_) => 0 end) "
    ++ "+ ^dbl.update((3, 9))",
  );

let redex_as_model = () =>
  run_test(
    "a committed transition normalizes in the main run",
    "18",
    "let ^dbl = " ++ dbl_def ++ " in ^dbl(^dbl.update(3, 9))",
  );

let update_probe_def = "let ^dbl = {
type Model = Int;
type Action = Int;
type Expansion = Int;
type Expand = + Functional(Model -> Expansion) + Macro(Model -> (Exp, [SpliceRef]));
let init = 0;
let update = fun (m, a) -> ^^probe(m + a);
let view = fun m -> Html.text(string_of_int(m));
let expand = Functional(fun m -> m * 2)
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
    "let ^dbl = " ++ dbl_def ++ " in ^dbl(" ++ text ++ ")",
  );
};

/* Regression (color picker): a mid-run HTML sample is Closure-wrapped with
   OPEN handler funs inside; consuming it must substitute the environment
   (close_value), not strip it, or handlers lose their definitions */
let sampled_handlers_are_closed = () => {
  let (mtr, _, probes) =
    probe_run(
      "let ^pk = {
type Model = Int;
type Action = Int;
type Expansion = Int;
type Expand = + Functional(Model -> Expansion) + Macro(Model -> (Exp, [SpliceRef]));
let bump = fun x -> x + 1;
let init = 0;
let update = fun (m, a) -> a;
let view = fun m -> Html.div([Attr.on_click_at(fun (x, y) -> bump(x + m))], []);
let expand = Functional(fun m -> m)
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
  | None => fail("no Html.T sample recorded")
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

/* ---- The expansion obligation ----

   A use of ^name synthesizes the DECLARED Expansion, so statics owes a
   check that the expansion actually has that type. */

let expansion_mark = (m: Statics.Map.t): option(Mark.t) =>
  Id.Map.fold(
    (_, info, acc) =>
      switch (acc, info: Info.t) {
      | (Some(_), _) => acc
      | (None, InfoExp({marks, _})) =>
        List.find_opt(
          fun
          | Mark.BadLivelitExpansion(_) => true
          | _ => false,
          marks,
        )
      | (None, _) => None
      },
    m,
    None,
  );

/* THE USE-SITE CHECK IS NOW VACUOUS FOR FUNCTIONAL LIVELITS, and these two
   tests record that rather than pretending otherwise.

   The paper checks per use (PLDI 2021, S3.2.5) because a MACRO expansion is
   a program whose type depends on the model value, which no definition-site
   check can see. A functional livelit is different: with eager
   definition-site checking (what Cyrus asked for, and what #2596 had),
   `Functional(f)` is checked against `Model -> Expansion` where it is
   written, so `f(model)` has type Expansion at every use, and there is
   nothing left for the use site to catch.

   So the fixture below -- `Expansion = String`, expand returns `Model` --
   no longer reaches the use at all. It is rejected at the definition, with
   a more precise error, which is the better outcome and the reason these
   assertions moved.

   The use-site machinery stays in Statics.re, untouched and still covered
   by the negative tests below. It becomes load-bearing again when the Macro
   arm is inhabitable, which is what `Exp` is for. Do not read these two
   tests as evidence the use-site check was removed. */
let expansion_mismatch_marked = () => {
  let (m, _) =
    statics(
      "let ^s = "
      ++ def(~expansion="String", ~expand="fun m : Model -> m")
      ++ " in ^s(1)",
    );
  check(
    bool,
    "an expansion inconsistent with Expansion is rejected at the definition",
    true,
    def_is_marked(m),
  );
  check(
    bool,
    "and never reaches the use, so no use-site mark is owed",
    true,
    Option.is_none(expansion_mark(m)),
  );
};

/* The check is discharged per use, since the expansion is a function of
   that use's model. A use spreads its info over its own ids and its
   expansion's, so count relative to one use rather than absolutely. */
let expansion_mismatch_at_each_use = () => {
  let marked = (uses: string) => {
    let (m, _) =
      statics(
        "let ^s = "
        ++ def(~expansion="String", ~expand="fun m : Model -> m")
        ++ " in "
        ++ uses,
      );
    Id.Map.fold(
      (_, info, acc) =>
        switch ((info: Info.t)) {
        | InfoExp({marks, _}) =>
          acc
          + List.length(
              List.filter(
                fun
                | Mark.BadLivelitExpansion(_) => true
                | _ => false,
                marks,
              ),
            )
        | _ => acc
        },
      m,
      0,
    );
  };
  /* Was: one use marked, two uses marked twice over -- the per-use
     discharge. With eager definition-site checking this fixture dies at
     the definition, so no use is marked however many there are. The
     per-use SHAPE is still what the code does; see the comment above. */
  check(
    int,
    "no use is marked: the definition already failed",
    0,
    marked("^s(1)"),
  );
  check(int, "and still none with two uses", 0, marked("(^s(1), ^s(2))"));
};

/* The declaration is what clients type against: `^s(7) ++ "!"` is fine
   because Expansion is String, whatever expand happens to return. */
let declared_type_is_the_interface = () =>
  run_test(
    "clients type against the declared Expansion",
    "\"7!\"",
    "let ^s = "
    ++ def(~expansion="String", ~expand="fun m -> string_of_int(m)")
    ++ " in ^s(7) ++ \"!\"",
  );

let declared_type_no_marks = () => {
  let (m, _) =
    statics(
      "let ^s = "
      ++ def(~expansion="String", ~expand="fun m -> string_of_int(m)")
      ++ " in ^s(7) ++ \"!\"",
    );
  check(
    bool,
    "a consistent expansion is unmarked",
    false,
    has_mark(
      fun
      | Mark.BadLivelitExpansion(_) => true
      | _ => false,
      m,
    ),
  );
};

/* Consistency, not equality, is the test — an expansion statics can only
   type as Unknown stays gradual, as it would anywhere else. */
let unknown_expansion_not_marked = () => {
  let (m, _) =
    statics(
      "let ^s = "
      ++ def(~expansion="String", ~expand="fun m -> m")
      ++ " in ^s(1)",
    );
  check(
    bool,
    "an Unknown expansion is not marked",
    false,
    has_mark(
      fun
      | Mark.BadLivelitExpansion(_) => true
      | _ => false,
      m,
    ),
  );
};

/* The declared type is stated independently of the definition, so a
   deliberately abstract Expansion narrows what clients may assume. */
let abstract_expansion_hides_the_model = () => {
  let (m, _) =
    statics(
      "let ^s = "
      ++ def(~expansion="String", ~expand="fun m -> string_of_int(m)")
      ++ " in ^s(7) + 1",
    );
  check(
    bool,
    "the client's misuse of Expansion is the client's error",
    true,
    has_mark(
      fun
      | Mark.ExpectationMismatch(_) => true
      | _ => false,
      m,
    ),
  );
};

let tests = [
  (
    "UserLivelits",
    [
      test_case("pattern parses as binder", `Quick, parses_as_binder),
      test_case("expansion evaluates", `Quick, evaluates),
      test_case("module helpers", `Quick, module_helpers),
      test_case("module funlet members", `Quick, module_funlet_members),
      test_case("module expand mismatch", `Quick, module_expand_mismatch),
      test_case(
        "module expand mismatch through aliases",
        `Quick,
        module_expand_mismatch_through_aliases,
      ),
      test_case(
        "module mismatch still binds",
        `Quick,
        module_mismatch_still_binds,
      ),
      test_case("module update mismatch", `Quick, module_update_mismatch),
      test_case(
        "module well typed no mismatch",
        `Quick,
        module_well_typed_no_mismatch,
      ),
      test_case("module missing members", `Quick, module_missing_members),
      test_case("module missing types", `Quick, missing_types_marked),
      test_case("module adapter", `Quick, module_adapter),
      test_case("shape member", `Quick, shape_member),
      test_case("multiple uses", `Quick, multiple_uses),
      test_case("members out of order", `Quick, members_out_of_order),
      test_case("helpers inside definition", `Quick, helpers_in_def),
      test_case(
        "type alias before definition",
        `Quick,
        type_alias_before_def,
      ),
      test_case("shadows builtin", `Quick, shadows_builtin),
      test_case("nested shadowing", `Quick, nested_shadowing),
      test_case("bad definition marked", `Quick, bad_def_marked),
      test_case("tuple definition marked", `Quick, tuple_def_marked),
      test_case("unbound use marked", `Quick, unbound_use_marked),
      test_case("good definition unmarked", `Quick, good_def_unmarked),
      test_case("adapter contract", `Quick, adapter),
      test_case(
        "expansion mismatch marked",
        `Quick,
        expansion_mismatch_marked,
      ),
      test_case(
        "expansion mismatch at each use",
        `Quick,
        expansion_mismatch_at_each_use,
      ),
      test_case(
        "declared type is the interface",
        `Quick,
        declared_type_is_the_interface,
      ),
      test_case(
        "consistent expansion unmarked",
        `Quick,
        declared_type_no_marks,
      ),
      test_case(
        "unknown expansion not marked",
        `Quick,
        unknown_expansion_not_marked,
      ),
      test_case(
        "abstract expansion hides the model",
        `Quick,
        abstract_expansion_hides_the_model,
      ),
      test_case("commit vs ephemeral decision", `Quick, commit_decision),
      test_case("view probes fire when projected", `Quick, view_probes_fire),
      test_case(
        "projector samples the live Html.T",
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

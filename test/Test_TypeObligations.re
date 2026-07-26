open Alcotest;
open Haz3lcore;
open Language;

/* T1 semantic obligations: tuple-shape deficits derived from types.
   Derivation is term-based and caret-free — states are built by
   typing (¦ = caret), but the result must not depend on where the
   caret ends up (pinned by the caret-invariance case below). */

let string_testable = testable(Fmt.string, String.equal);

let derive = (code: string): string => {
  let z = Test_Editing.perform(Zipper.init(), Test_Editing.mk(code));
  let MakeTerm.{term, _} = MakeTerm.from_zip_for_sem(z, ~root=Sort.Exp);
  let (info_map, _) =
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term);
  switch (TypeObligations.derive(info_map)) {
  | [] => "none"
  | obs =>
    obs
    |> List.map((ob: TypeObligations.t) =>
         Printf.sprintf(
           "%d/%d owes %s%s",
           ob.present,
           ob.expected,
           ob.remaining_tys
           |> List.map(Typ.pretty_print)
           |> String.concat(","),
           switch (ob.commas_at) {
           | None => ""
           | Some(idxs) =>
             " @" ++ (idxs |> List.map(string_of_int) |> String.concat(","))
           },
         )
       )
    |> List.sort(compare)
    |> String.concat(" | ")
  };
};

let ob_case = (~name, ~code, ~expected) =>
  test_case(name, `Quick, () =>
    check(string_testable, name, expected, derive(code))
  );

let f2 = "let f : (Int, String) -> Int = fun x -> 1 in ";
let g3 = "let g : (Int, String, Bool) -> Int = fun x -> 1 in ";

/* Reification: with obligations spliced into the sem term, the arity
   inconsistency disappears and elements get per-element ana. */
let statics_of = (code: string, ~reify: bool) => {
  let z = Test_Editing.perform(Zipper.init(), Test_Editing.mk(code));
  let mk = term =>
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term);
  let MakeTerm.{term, _} = MakeTerm.from_zip_for_sem(z, ~root=Sort.Exp);
  let (info_map, _) = mk(term);
  if (reify) {
    switch (TypeObligations.derive(info_map)) {
    | [] => info_map
    | obs =>
      let MakeTerm.{term, _} =
        MakeTerm.from_zip_for_sem_spliced(
          z,
          ~root=Sort.Exp,
          ~splice=TypeObligations.reify(obs),
        );
      fst(mk(term));
    };
  } else {
    info_map;
  };
};

let error_count = (code, ~reify) =>
  statics_of(code, ~reify) |> Statics.Map.error_ids |> List.length;

let reify_case = (~name, ~code, ~raw, ~reified) =>
  test_case(
    name,
    `Quick,
    () => {
      check(
        testable(Fmt.int, Int.equal),
        name ++ " raw",
        raw,
        error_count(code, ~reify=false),
      );
      check(
        testable(Fmt.int, Int.equal),
        name ++ " reified",
        reified,
        error_count(code, ~reify=true),
      );
    },
  );

let error_clses = (code, ~reify) =>
  statics_of(code, ~reify)
  |> (im => (im, Statics.Map.error_ids(im)))
  |> (
    ((im, ids)) =>
      ids
      |> List.map(id =>
           switch (Id.Map.find_opt(id, im)) {
           | Some(info) => Info.cls_of(info) |> Cls.show
           | None => "?"
           }
         )
      |> List.sort(compare)
      |> String.concat(" | ")
  );

let reify_tests = [
  /* the reified term errors exactly as the hand-typed equivalent
     f(true, ?) would: precise element error + hazel's normal tuple
     cascade — NOT the raw state's one vague arity error. Reified
     statics == statics of the materialized program. */
  test_case("genuine error localizes to element + tuple cascade", `Quick, () =>
    check(
      string_testable,
      "sites",
      "Boolean literal | Tuple literal",
      error_clses(f2 ++ "f(true¦", ~reify=true),
    )
  ),
  reify_case(
    ~name="arity error absorbed by reification",
    ~code=f2 ++ "f(1¦",
    ~raw=1,
    ~reified=0,
  ),
  reify_case(
    ~name="genuine element type error survives reification",
    ~code=f2 ++ "f(true¦",
    ~raw=1,
    ~reified=2,
  ),
  reify_case(
    ~name="three-arity two owed absorbed",
    ~code=g3 ++ "g(1¦",
    ~raw=1,
    ~reified=0,
  ),
];

/* === Scenario traces ===
 * Ergonomic walks: what the user sees owed at each state. Format
 * pins present/expected + owed types; "none" = no chip. */
let scenario_tests = [
  /* left-to-right entry: obligations shrink monotonically, never
     flicker to something weird mid-trajectory */
  /* empty parens PRESUME (gap closed 2026-07-11): the common case
     is the comma'd application, so the promise shows from `(` on;
     a tuple-typed single element still defeats at the next pass */
  ob_case(~name="LR: f(", ~code=f2 ++ "f(¦", ~expected="1/2 owes String"),
  ob_case(~name="LR: f(1", ~code=f2 ++ "f(1¦", ~expected="1/2 owes String"),
  /* after the comma, regrout's hole IS the second element: tuple
     complete; the satisfied record stays (frame recount fact) */
  ob_case(~name="LR: f(1,", ~code=f2 ++ "f(1,¦", ~expected="2/2 owes "),
  ob_case(
    ~name="LR: f(1, \"a\"",
    ~code=f2 ++ "f(1, \"a\"¦",
    ~expected="2/2 owes ",
  ),
  ob_case(
    ~name="LR: f(1, \"a\")",
    ~code=f2 ++ "f(1, \"a\")¦",
    ~expected="2/2 owes ",
  ),
  /* mid-entry operator: incomplete element must not distort count */
  ob_case(
    ~name="mid-entry: f(1 +",
    ~code=f2 ++ "f(1 +¦",
    ~expected="1/2 owes String",
  ),
  ob_case(
    ~name="mid-entry: f(1 + 2",
    ~code=f2 ++ "f(1 + 2¦",
    ~expected="1/2 owes String",
  ),
  /* nonlinear: deficit is judged program-state-wise, lands at the
     tuple end regardless of caret (contrast: the tydi-scaffold
     branch piled it at the caret) */
  ob_case(
    ~name="nonlinear: caret mid-tuple, deficit at end",
    ~code="let h : (Int, Int, Int, Int) -> Int = fun x -> 1 in h(1, ¦2, 3",
    ~expected="3/4 owes Int",
  ),
  ob_case(
    ~name="nonlinear: caret at start of deficient tuple",
    ~code="let h : (Int, Int, Int, Int) -> Int = fun x -> 1 in h(¦1, 2",
    ~expected="2/4 owes Int,Int",
  ),
  /* overfull: no insertion can fix excess — no obligation (errors
     handle it); must not underflow */
  ob_case(
    ~name="overfull: f(1, 2, 3) at arity 2",
    ~code=f2 ++ "f(1, 2, 3¦",
    ~expected="none",
  ),
  /* multi-site persistence: BOTH deficient calls owe at once,
     regardless of where the caret is — the non-local display the
     buffer architecture could never do */
  ob_case(
    ~name="multi-site: two deficient calls owe simultaneously",
    ~code=
      f2
      ++ "let g : (Bool, Bool, Bool) -> Int = fun y -> 2 in f(1¦) + g(true)",
    ~expected="1/2 owes String | 1/3 owes Bool,Bool",
  ),
];

/* Junction evidence: an operator hole between juxtaposed elements
   is a separator slot — the comma is owed AT the junction, and
   reification realizes it in place. */
let junction_tests = [
  ob_case(
    ~name="juxtaposed elements count: g(1  2",
    ~code=g3 ++ "g(1  2¦",
    ~expected="2/3 owes Bool",
  ),
  ob_case(
    ~name="junction-only site: f(1  2 at arity 2",
    ~code="let f : (Int, Int) -> Int = fun x -> 1 in f(1  2¦",
    ~expected="2/2 owes ",
  ),
  ob_case(
    ~name="mixed: g(1, 2  3 counts three",
    ~code=g3 ++ "g(1, 2  3¦",
    ~expected="3/3 owes ",
  ),
  /* type-fitting juxtaposed elements: junction comma realizes in
     place, term is clean */
  reify_case(
    ~name="junction reifies in place",
    ~code="let h : (Int, Int, Bool) -> Int = fun x -> 1 in h(1  2¦",
    ~raw=1,
    ~reified=0,
  ),
  /* ill-fitting juxtaposed element errors exactly as the hand-typed
     g(1, 2, ?) would: element + tuple cascade */
  reify_case(
    ~name="junction reify keeps genuine element error",
    ~code=g3 ++ "g(1  2¦",
    ~raw=1,
    ~reified=2,
  ),
];

let completed_of = (code: string): string => {
  let z = Test_Editing.perform(Zipper.init(), Test_Editing.mk(code));
  let seg = Zipper.unselect_and_zip(~erase_buffer=true, z);
  CanonicalCompletion.materialize_all(~sort=Sort.Exp, seg)
  |> Printer.of_segment(~holes="?", ~concave_holes="~");
};

let probe_chips = [
  /* the juxtaposition junction belongs to the unclosed ap, not the
     enclosing let's in (crossing clamp): both contexts complete
     uniformly, closer after the juxtaposed elements */
  test_case("junction inside open paren is not an in-site", `Quick, () =>
    check(
      string_testable,
      "cmp",
      /* hole at the policy position (one space from anchor) — same
         rule as the Test_GroutPlace pin table */
      "A: let f : (Int, String, Bool)->Int = fun x -> f(1 ~ 2)in? ||| B: let g : (Int, String, Bool) -> Int = fun x -> 1 in g(1 ~ 2)",
      "A: "
      ++ completed_of("let f : (Int, String, Bool)->Int = fun x -> f(1  2¦")
      ++ " ||| B: "
      ++ completed_of(
           "let g : (Int, String, Bool) -> Int = fun x -> 1 in g(1  2¦",
         ),
    )
  ),
  test_case(
    "incomplete site: junction chip + merged end chip",
    `Quick,
    () => {
      let code = "let f : (Int, String, Bool)->Int = fun x -> f(1  2¦";
      let z = Test_Editing.perform(Zipper.init(), Test_Editing.mk(code));
      let seg = Zipper.unselect_and_zip(~erase_buffer=true, z);
      let MakeTerm.{term, _} = MakeTerm.from_zip_for_sem(z, ~root=Sort.Exp);
      let (info_map, _) =
        Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term);
      let obs = TypeObligations.derive(info_map);
      let existing = CanonicalCompletion.for_editor(seg).insertions;
      let show = (ins: CanonicalCompletion.insertion) =>
        ins.delimiters
        |> List.map((d: CanonicalCompletion.delimiter_info) => d.text)
        |> String.concat("+");
      let all =
        TypeObligations.assist_stream(z, ~info_map, obs)
        |> List.map(show)
        |> String.concat(" | ");
      let arg_shape =
        Id.Map.fold(
          (_, info: Info.t, acc) =>
            switch (info) {
            | InfoExp({user_term, _}) =>
              switch (Exp.term_of(user_term)) {
              | Ap(Forward, _, arg) =>
                let d =
                  switch (Exp.term_of(arg)) {
                  | MultiHole(ts) =>
                    Printf.sprintf("MultiHole(%d)", List.length(ts))
                  | Tuple(es) => Printf.sprintf("Tuple(%d)", List.length(es))
                  | Parens(_) => "Parens"
                  | EmptyHole => "EmptyHole"
                  | t => Cls.show(Exp(Exp.cls_of_term(t)))
                  };
                acc == "" ? d : acc ++ ";" ++ d;
              | _ => acc
              }
            | _ => acc
            },
          info_map,
          "",
        );
      check(
        string_testable,
        "chips",
        "obs=[2/3] arg=MultiHole(2) existing=[)+in] all=[,+)+in]",
        Printf.sprintf(
          "obs=[%s] arg=%s existing=[%s] all=[%s]",
          obs
          |> List.map((ob: TypeObligations.t) =>
               Printf.sprintf("%d/%d", ob.present, ob.expected)
             )
          |> String.concat(","),
          arg_shape,
          existing |> List.map(show) |> String.concat(" | "),
          all,
        ),
      );
    },
  ),
];

/* Overfull juxtaposition: comma count is forced, placement chosen
   by type fit; ambiguity stays silent. */
let f2i = "let f : (Int, String) -> Int = fun x -> 1 in ";
let overfull_tests = [
  ob_case(
    ~name="overfull 3-into-2: type-fit picks the first junction",
    ~code=f2i ++ "f(1 2 3¦",
    ~expected="2/2 owes  @0",
  ),
  ob_case(
    ~name="overfull ambiguous: no presumption",
    ~code="let g : (Int, Int) -> Int = fun x -> 1 in g(1 2 3¦",
    ~expected="none",
  ),
  reify_case(
    ~name="overfull reify localizes the leftover juxtaposition",
    ~code=f2i ++ "f(1 2 3¦",
    ~raw=1,
    ~reified=1,
  ),
];

/* Inline ghost: the merged chip's pending content as buffer text.
   Obligations MUST derive from the same zipper as the display
   (typing mints fresh ids per run — a re-typed statics harness
   produces an unrelated id universe and silently never merges). */
let ghost = (code: string): string => {
  let z = Test_Editing.perform(Zipper.init(), Test_Editing.mk(code));
  let MakeTerm.{term, _} = MakeTerm.from_zip_for_sem(z, ~root=Sort.Exp);
  let (info_map, _) =
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term);
  let obs = TypeObligations.derive(info_map);
  let assist = TypeObligations.assist_stream(z, ~info_map, obs);
  switch (CanonicalCompletion.chip_among(z, assist)) {
  | None => "NONE"
  | Some(ins) =>
    switch (TypeObligations.ghost_pieces(z, ins)) {
    | None => "NONE"
    | Some(pieces) =>
      "<" ++ Printer.of_segment(~holes="?", ~concave_holes="~", pieces) ++ ">"
    }
  };
};

let ghost_case = (~name, ~code, ~expected) =>
  test_case(name, `Quick, () =>
    check(string_testable, name, expected, ghost(code))
  );

/* Stale-pass synthesis: the frame after typing `(` has no
   obligation record for the new site — the fn's type comes from the
   LAST statics pass (id lookup; name-in-stale-ctx fallback when the
   name itself was typed since). show = the chip stream assembled
   with STALE obs + STALE info_map against the FRESH zipper. */
let stale_tests = {
  let show = ins =>
    ins
    |> List.map((i: CanonicalCompletion.insertion) =>
         i.delimiters
         |> List.map((d: CanonicalCompletion.delimiter_info) => d.text)
         |> String.concat("+")
       )
    |> String.concat(" | ");
  let statics_at = (code: string) => {
    let z = Test_Editing.perform(Zipper.init(), Test_Editing.mk(code));
    let MakeTerm.{term, _} = MakeTerm.from_zip_for_sem(z, ~root=Sort.Exp);
    fst(Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term));
  };
  let stale_case = (~name, ~stale: string, ~typed: string, ~expected) =>
    test_case(
      name,
      `Quick,
      () => {
        let info_map = statics_at(stale);
        let obs = TypeObligations.derive(info_map);
        let z =
          Test_Editing.perform(
            Zipper.init(),
            Test_Editing.mk(stale)
            @ (Token.to_list(typed) |> List.map(c => Action.Insert(c))),
          );
        check(
          string_testable,
          name,
          expected,
          show(TypeObligations.assist_stream(z, ~info_map, obs)),
        );
      },
    );
  [
    /* fn token existed at the last pass: id lookup */
    stale_case(
      ~name="stale: ( after known fn — commas instantly",
      ~stale=f2 ++ "f¦",
      ~typed="(",
      ~expected=",+)",
    ),
    /* whole name typed since the last pass: name-in-ctx fallback */
    stale_case(
      ~name="stale: burst-typed name — ctx fallback",
      ~stale=f2 ++ "¦",
      ~typed="f(",
      ~expected=",+)",
    ),
    /* unknown name: no synthesis, closer only */
    stale_case(
      ~name="stale: unknown fn — no comma presumption",
      ~stale="¦",
      ~typed="q(",
      ~expected=")",
    ),
  ];
};

/* ghost_pieces is BARE (tokens + holes, no spacing) — all display
   spacing belongs to the padding oracle, asserted by the rendered
   pins in Test_CompletionDisplay */
let ghost_tests = [
  ghost_case(~name="ghost: f(1", ~code=f2 ++ "f(1¦", ~expected="<,?)>"),
  ghost_case(
    ~name="ghost: annotated let",
    ~code="let _: (Int, Bool) ¦",
    /* the trailing body hole is ARTIFACT material now — the display
       projects it (`⟪= ? in ?⟫` pins in Test_CompletionDisplay); the
       fabricated ghost channel this probe reads no longer carries it */
    ~expected="<=?in>",
  ),
  ghost_case(
    ~name="ghost: let a = 4",
    ~code="let a = 4¦",
    ~expected="<in?>",
  ),
  ghost_case(
    ~name="ghost: none in complete code",
    ~code="1 + 2¦",
    ~expected="NONE",
  ),
];

let tests = [
  ("TypeObligations: reification", reify_tests),
  ("TypeObligations: scenarios", scenario_tests),
  ("TypeObligations: stale-pass synthesis", stale_tests),
  ("TypeObligations: junctions", junction_tests),
  ("TypeObligations: overfull", overfull_tests),
  ("TypeObligations: ghost", ghost_tests),
  ("TypeObligations: chip-probe", probe_chips),
  (
    "TypeObligations: derivation",
    [
      ob_case(
        ~name="one arg of two",
        ~code=f2 ++ "f(1¦",
        ~expected="1/2 owes String",
      ),
      ob_case(
        ~name="one arg of three",
        ~code=g3 ++ "g(1¦",
        ~expected="1/3 owes String,Bool",
      ),
      ob_case(
        ~name="two args of three",
        ~code=g3 ++ "g(1, \"a\"¦",
        ~expected="2/3 owes Bool",
      ),
      /* satisfied sites KEEP their record (deficit 0, chip-inert):
         it's the type fact the frame assembly recounts against —
         without it, deleting the last comma has no arity fact until
         the next statics pass and the promise flashes */
      ob_case(
        ~name="complete call: satisfied record kept",
        ~code=f2 ++ "f(1, \"a\")¦",
        ~expected="2/2 owes ",
      ),
      /* the DEFEATED site emits nothing; the 2/2 is the (1, "a")
         literal in p's definition — a satisfied fact, not f(p) */
      ob_case(
        ~name="tuple-typed var defeats the presumption",
        ~code=
          "let f : (Int, String) -> Int = fun x -> 1 in let p : (Int, String) = (1, \"a\") in f(p¦",
        ~expected="2/2 owes ",
      ),
      ob_case(
        ~name="wrong-arity tuple var does not defeat",
        ~code=
          "let g : (Int, String, Bool) -> Int = fun x -> 1 in let p : (Int, String) = (1, \"a\") in g(p¦",
        ~expected="1/3 owes String,Bool | 2/2 owes ",
      ),
      ob_case(
        ~name="explicit tuple parens under annotation",
        ~code="let t : (Int, Bool) = (1¦",
        ~expected="1/2 owes Bool",
      ),
      ob_case(
        ~name="nested tuple: inner and outer both owe",
        ~code="let h : ((Int, Bool), String) -> Int = fun x -> 1 in h((1¦",
        ~expected="1/2 owes Bool | 1/2 owes String",
      ),
      ob_case(
        ~name="caret-invariance: same state, caret at program start",
        ~code="¦" ++ f2 ++ "f(1",
        ~expected="1/2 owes String",
      ),
      ob_case(
        ~name="unknown fn type: no obligation",
        ~code="q(1¦",
        ~expected="none",
      ),
    ],
  ),
];

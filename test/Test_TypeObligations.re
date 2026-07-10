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
           "%d/%d owes %s",
           ob.present,
           ob.expected,
           ob.remaining_tys
           |> List.map(Typ.pretty_print)
           |> String.concat(","),
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
  /* known gap: empty parens give no element anchor (M1 skips
     k=0) — this is where the caret-local inline ghost belongs */
  ob_case(~name="LR: f(", ~code=f2 ++ "f(¦", ~expected="none"),
  ob_case(~name="LR: f(1", ~code=f2 ++ "f(1¦", ~expected="1/2 owes String"),
  /* after the comma, regrout's hole IS the second element: tuple
     complete, hole anas String — nothing owed */
  ob_case(~name="LR: f(1,", ~code=f2 ++ "f(1,¦", ~expected="none"),
  ob_case(
    ~name="LR: f(1, \"a\"",
    ~code=f2 ++ "f(1, \"a\"¦",
    ~expected="none",
  ),
  ob_case(
    ~name="LR: f(1, \"a\")",
    ~code=f2 ++ "f(1, \"a\")¦",
    ~expected="none",
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

let tests = [
  ("TypeObligations: reification", reify_tests),
  ("TypeObligations: scenarios", scenario_tests),
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
      ob_case(
        ~name="complete call: no obligation",
        ~code=f2 ++ "f(1, \"a\")¦",
        ~expected="none",
      ),
      ob_case(
        ~name="tuple-typed var defeats the presumption",
        ~code=
          "let f : (Int, String) -> Int = fun x -> 1 in let p : (Int, String) = (1, \"a\") in f(p¦",
        ~expected="none",
      ),
      ob_case(
        ~name="wrong-arity tuple var does not defeat",
        ~code=
          "let g : (Int, String, Bool) -> Int = fun x -> 1 in let p : (Int, String) = (1, \"a\") in g(p¦",
        ~expected="1/3 owes String,Bool",
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

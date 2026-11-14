open Language;
open Test_ProofMap.Harness;

/* Goals and facts name the program's definitions as written, and names
 * are resolved by scope. A proof binder that rebinds a name hides the
 * facts that mention it, and one that would capture a name the goal
 * still uses is marked. One case per kind of binder. */

let status_of = (name, src): string => {
  let (pm, proof) = run_named(name, src);
  switch (ProofMap.full_status_of_proof(pm, proof)) {
  | Proven => "Proven"
  | ProvenModulo(_) => "ProvenModulo"
  | Refuted => "Refuted"
  | Incomplete => "Incomplete"
  };
};

let check_status = (~theorem="t", expected, src) => {
  let (pm, proof) = run_named(theorem, src);
  Alcotest.check(
    Alcotest.string,
    "status\n" ++ dump(pm, proof),
    expected,
    status_of(theorem, src),
  );
};

let check_marked = (~theorem="t", what, pred, src) => {
  let (pm, proof) = run_named(theorem, src);
  Alcotest.check(
    Alcotest.bool,
    what ++ "\n" ++ dump(pm, proof),
    true,
    has_mark_kind(pm, proof, pred),
  );
};

let captures: ProofMark.t => bool =
  fun
  | BinderCaptures(_) => true
  | _ => false;

let len = "let len = fun xs -> case xs | [] => 0 | x :: t => 1 + len(t) end in ";

let tests = (
  "DefinitionNames",
  [
    Alcotest.test_case(
      "eval leaves a recursive call named by its definition",
      `Quick,
      () => {
        let (pm, proof) =
          run_named(
            "t",
            len
            ++ "theorem t = forall y: Int -> forall ys: [Int] -> len(y :: ys) == 1 + len(ys) proof from len(y :: ys) do step end; ? in t",
          );
        let d = dump(pm, proof);
        Alcotest.check(
          Alcotest.bool,
          "the goal after eval names len\n" ++ d,
          true,
          contains(d, "out: 1 + len(ys) == 1 + len(ys)"),
        );
      },
    ),
    Alcotest.test_case(
      "eval leaves a builtin's recursive call named",
      `Quick,
      () => {
        let (pm, proof) =
          run_named(
            "t",
            "let add = fun (acc, x) -> acc + x in theorem t = forall h: Int -> forall l: [Int] -> forall a: Int -> fold_left(h :: l, add, a) == fold_left(l, add, a + h) proof from fold_left(h :: l, add, a) do step end; ? in t",
          );
        let d = dump(pm, proof);
        Alcotest.check(
          Alcotest.bool,
          "the goal after eval names fold_left\n" ++ d,
          true,
          contains(d, "out: fold_left(l, add, add(a, h))"),
        );
      },
    ),
    Alcotest.test_case(
      "forall: a binder named like a definition hides lemmas about it",
      `Quick,
      () =>
      check_marked(
        "the lemma about the program's len doesn't apply to the binder",
        fun
        | RuleDoesNotApply(_)
        | UnknownEquality(_) => true
        | _ => false,
        len
        ++ "theorem len_nil = len([]) == 0 proof from len([]) do step end; step in "
        ++ "theorem t = forall len: ([Int] -> Int) -> len([]) == 0 proof rewrite len_nil end in t",
      )
    ),
    Alcotest.test_case(
      "forall: eval doesn't unfold a binder named like a definition",
      `Quick,
      () =>
      check_marked(
        "nothing to step",
        fun
        | NothingToStep => true
        | _ => false,
        len
        ++ "theorem t = forall len: ([Int] -> Int) -> len([]) == 0 proof from len([]) do step end in t",
      )
    ),
    Alcotest.test_case(
      "funext: a binder free in the functions is marked", `Quick, () =>
      check_marked(
        "captures",
        captures,
        "let k = 5 in theorem t = (fun z: Int -> k) == (fun z: Int -> 5) proof funext k => rewrite refl end in t",
      )
    ),
    Alcotest.test_case(
      "funext: a binder free in the functions is captured", `Quick, () =>
      check_status(
        ~theorem="bad",
        "Incomplete",
        "theorem bad = forall y: Int -> (fun z: Int -> y) == (fun z: Int -> z) proof funext y => rewrite refl end in bad",
      )
    ),
    Alcotest.test_case(
      "generalize: facts about the generalized variable are hidden", `Quick, () =>
      check_status(
        ~theorem="bad",
        "Incomplete",
        "theorem bad = forall x: Int -> x == 0 ==> x == 0 proof assume x == 0 => generalize x => forall x => rewrite assume end in bad",
      )
    ),
    Alcotest.test_case(
      "case pattern: a variable the goal uses outside the scrutinee is marked",
      `Quick,
      () =>
      check_marked(
        "captures",
        captures,
        "let k = 0 in theorem t = forall xs: [Int] -> xs == [] || k == 0 proof induction xs | [] => ? | k :: rest => ? end in t",
      )
    ),
    Alcotest.test_case(
      "case pattern: facts about a rebound name are hidden", `Quick, () =>
      check_status(
        ~theorem="bad",
        "Incomplete",
        "let hd0 = fun l -> case l | [] => true | y :: _ => y == 0 end in "
        ++ "theorem bad = forall x: Int -> forall xs: [Int] -> x == 0 ==> hd0(xs) proof assume x == 0 => induction xs | [] => ? | x :: t => step; rewrite assume end in bad",
      )
    ),
    Alcotest.test_case(
      "focus: under a lambda, lemmas about a definition it shadows are hidden",
      `Quick,
      () =>
      check_status(
        ~theorem="bad",
        "Incomplete",
        "let k = 0 in theorem k0 = k == 0 proof from k do step end; step in "
        ++ "theorem bad = (fun k: Int -> k) == (fun k: Int -> 0) proof from k do rewrite k0 end end; rewrite refl in bad",
      )
    ),
    Alcotest.test_case(
      "have: the hypothesis is hidden under a binder that rebinds its names",
      `Quick,
      () =>
      check_status(
        ~theorem="bad",
        "Incomplete",
        "let k = 0 in theorem bad = true ==> (forall k: Int -> k == 0) proof have k == 0 proof step => assume true => forall k => rewrite have end in bad",
      )
    ),
    Alcotest.test_case(
      "assume: the hypothesis is hidden under a binder that rebinds its names",
      `Quick,
      () =>
      check_status(
        ~theorem="bad",
        "Incomplete",
        "theorem bad = forall x: Int -> x == 0 ==> (forall x: Int -> x == 0) proof assume x == 0 => forall x => rewrite assume end in bad",
      )
    ),
    Alcotest.test_case(
      "assume: an inner assumption about a rebound name stays an obligation",
      `Quick,
      () =>
      check_status(
        ~theorem="bad",
        "ProvenModulo",
        "theorem bad = forall x: Int -> x == 0 ==> (forall x: Int -> x == 0) proof assume x == 0 => forall x => assume x == 0 as h => from x do rewrite h end end; step in bad",
      )
    ),
  ],
);

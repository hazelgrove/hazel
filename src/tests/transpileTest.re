open OUnit2;

open Printf;

open Semantics.Core;

/* See comment for `tests` value below for info on how to read the test cases */
/* var_ and varN_ create variables that don't need to be declared */
let var_ = HExp.Var "v";

let varN_ n => {
  assert (n > 0 && n < 10);
  HExp.Var (sprintf "v%d" n)
};

let (>:::%) group_name checks =>
  group_name >:::
  List.map
    (
      fun (name, hexp) =>
        group_name ^ "/" ^ name >:: (
          fun _ => {
            /* TODO We shouldn't magically know that this is being invoked from the `src` directory.
               We should either use logic to determine where the `tests` dir is relative to the cwd,
               or we should find a way to ensure that this is run from the `tests` dir, which would
               seem sensible */
            let tf = open_in ("tests/test_data/" ^ group_name ^ "/" ^ name ^ ".re");
            let fcontents = really_input_string tf (in_channel_length tf);
            close_in tf;
            assert_equal
              printer::(fun x => x)
              msg::"serialization failed"
              fcontents
              (Transpile.hz_to_string hexp);
            let rec contextualize n (hexp_, fcontents_) => {
              let h_n = n + 100;
              if (n == 0) {
                (
                  HExp.Let "v" (HExp.EmptyHole h_n) (HExp.Asc hexp_ HTyp.Hole),
                  sprintf
                    "let v = HazelPrelude.EHole %d;\n((%s) : HazelPrelude.hole)"
                    h_n
                    /* kinda hacky - we remove the trailing newline and ';' */
                    (String.sub fcontents_ 0 (String.length fcontents_ - 2))
                )
              } else {
                let (hexp__, fcontents__) = contextualize (n - 1) (hexp_, fcontents_);
                (
                  HExp.Let (sprintf "v%d" n) (HExp.EmptyHole h_n) hexp__,
                  sprintf "let v%d = HazelPrelude.EHole %d;\n%s" n h_n fcontents__
                )
              }
            };
            /* contextualize ensures that the reason has proper type ascription and all variables
               are defined, so that hazelnut type checking will work */
            let (hexp_, fcontents_) = contextualize 9 (hexp, fcontents);
            assert_equal
              msg::"parse failed" hexp_ (Transpile.hz_from_string (sprintf "{%s};" fcontents_))
          }
        )
    )
    checks;

let get_thorough_ var_mod_info => {
  let cur_n = ref 0;
  let (~:) default => {
    let cur_n_val = !cur_n;
    incr cur_n;
    switch var_mod_info {
    | None => default
    | Some (n, subst) => cur_n_val == n ? subst : default
    }
  };
  /* ~:"x" really just means "x", for the most part */
  let res =
    HExp.Let
      ~:"vNum"
      (HExp.Asc (HExp.EmptyHole 0) HTyp.Num)
      (
        HExp.Let
          ~:"_plus"
          (
            HExp.Asc
              (HExp.Lam ~:"n_" (HExp.Lam ~:"n_2" (HExp.Plus (HExp.Var ~:"n_") (HExp.Var ~:"n_2"))))
              (HTyp.Arrow HTyp.Num (HTyp.Arrow HTyp.Num HTyp.Num))
          )
          (
            HExp.Let
              ~:"l001"
              (
                HExp.Asc
                  (HExp.Inj HExp.L (HExp.Var ~:"_plus"))
                  (HTyp.Sum (HTyp.Arrow HTyp.Num (HTyp.Arrow HTyp.Num HTyp.Num)) HTyp.Num)
              )
              (
                HExp.Let
                  ~:"r"
                  (
                    HExp.Asc
                      (HExp.Inj HExp.R (HExp.Var ~:"vNum"))
                      (HTyp.Sum (HTyp.Arrow HTyp.Num (HTyp.Arrow HTyp.Num HTyp.Num)) HTyp.Hole)
                  )
                  (
                    HExp.NonEmptyHole
                      1
                      (
                        HExp.Ap
                          (
                            HExp.Ap
                              (HExp.Var ~:"_plus")
                              (
                                HExp.Case
                                  (HExp.Var ~:"l001")
                                  (
                                    ~:"l2",
                                    HExp.Ap
                                      (HExp.Ap (HExp.Var ~:"l2") (HExp.NumLit 7)) (HExp.NumLit 0)
                                  )
                                  (~:"r2", HExp.Var ~:"r2")
                              )
                          )
                          (HExp.NumLit 3)
                      )
                  )
              )
          )
      );
  switch var_mod_info {
  | None => Some res
  | Some (n, _) => n < !cur_n ? Some res : None
  }
};

let testInvalidVarName _ => {
  let rec test_bad_var bad_var n => {
    let hexp_opt = get_thorough_ (Some (n, bad_var));
    switch hexp_opt {
    | None => ()
    | Some hexp =>
      assert_raises
        msg::(
          sprintf
            "Should have raised an InvalidVar exception for invalid variable '%s' in position %d"
            bad_var
            n
        )
        (Transpile.InvalidVar bad_var)
        (fun _ => Transpile.hz_to_string hexp);
      test_bad_var bad_var (n + 1)
    }
  };
  test_bad_var "B" 0;
  test_bad_var "Bx" 0;
  test_bad_var "3" 0;
  test_bad_var "3x" 0;
  test_bad_var "-" 0;
  test_bad_var "-x" 0;
  test_bad_var "x-" 0;
  test_bad_var "x-x" 0
};


/**
 * The test cases are layed out as such:
 * ...
 * "testFolder" >:::% [
 *   ("testName1", HExp1...),
 *   ("testName2", HExp2...),
 * ],
 * ...
 * Each pair asserts that the corresponding HExp and test data should be equivalent. So HExp1
 * should serialize to the contents of 'tests/test_data/testFolder/testName1.re', and the
 * contents of that file should parse to HExp1 (roughly speaking). Each HExp can use `var_`
 * or `(varN_ d)` (0 < d < 10) to refer to variables `v` and `v1` through `v9` - these variables
 * can be referred to without having to be defined. Any other variable that is referred to must
 * be defined, or else a "Type check failure" will be raised.
 */
let tests =
  "Transpile tests" >::: [
    "testVar" >:::% [("basic", var_)],
    "testLet" >:::% [
      ("basic", HExp.Let "x" var_ (varN_ 1)),
      ("nested", HExp.Let "x" (varN_ 1) (HExp.Let "y" (varN_ 2) (HExp.Var "x"))),
      (
        "deep",
        HExp.Let
          "a" (HExp.Let "b" var_ (HExp.Var "b")) (HExp.Let "c" (HExp.Var "a") (HExp.Var "c"))
      ),
      ("shadowing", HExp.Let "a" var_ (HExp.Let "a" (HExp.Var "a") (HExp.Var "a")))
    ],
    "testLam" >:::% [
      ("basic", HExp.Lam "a" var_),
      ("deep", HExp.Lam "a" (HExp.Lam "b" (HExp.Var "b")))
    ],
    "testAp" >:::% [
      ("basic", HExp.Ap (varN_ 1) (varN_ 2)),
      ("deep", HExp.Ap (HExp.Ap (varN_ 1) (varN_ 2)) (HExp.Ap (varN_ 3) (varN_ 4)))
    ],
    "testNumLit" >:::% [("zero", HExp.NumLit 0), ("one", HExp.NumLit 1)],
    "testPlus" >:::% [
      ("basic", HExp.Plus (varN_ 1) (varN_ 2)),
      ("deep", HExp.Plus (HExp.Plus (varN_ 1) (varN_ 2)) (HExp.Plus (varN_ 3) (varN_ 4)))
    ],
    "testInjLeft" >:::% [
      ("basic", HExp.Inj HExp.L var_),
      ("deep", HExp.Inj HExp.L (HExp.Inj HExp.L var_))
    ],
    "testInjRight" >:::% [
      ("basic", HExp.Inj HExp.R var_),
      ("deep", HExp.Inj HExp.R (HExp.Inj HExp.L var_))
    ],
    /* TODO don't forget a test case for case that tries to use wrong constructor names */
    "testCase" >:::% [
      ("basic", HExp.Case var_ ("l", HExp.Var "l") ("r", HExp.Var "r")),
      (
        "deep",
        HExp.Case
          (HExp.Asc (HExp.Case var_ ("el", HExp.Var "el") ("er", HExp.Var "er")) HTyp.Hole)
          ("l", HExp.Case (HExp.Var "l") ("ll", HExp.Var "ll") ("lr", HExp.Var "lr"))
          ("r", HExp.Case (HExp.Var "r") ("rl", HExp.Var "rl") ("rr", HExp.Var "rr"))
      )
    ],
    "testEmptyHole" >:::% [("zero", HExp.EmptyHole 0), ("one", HExp.EmptyHole 1)],
    "testNonEmptyHole" >:::% [
      ("basic", HExp.NonEmptyHole 0 var_),
      ("deep", HExp.NonEmptyHole 1 (HExp.NonEmptyHole 2 var_))
    ],
    "testAscNum" >:::% [("basic", HExp.Asc var_ HTyp.Num)],
    "testAscArrow" >:::% [
      ("basic", HExp.Asc var_ (HTyp.Arrow HTyp.Num HTyp.Hole)),
      ("rightAssoc", HExp.Asc var_ (HTyp.Arrow HTyp.Hole (HTyp.Arrow HTyp.Num HTyp.Hole))),
      ("leftAssoc", HExp.Asc var_ (HTyp.Arrow (HTyp.Arrow HTyp.Hole HTyp.Num) HTyp.Hole))
    ],
    "testAscSum" >:::% [
      ("withArrow", HExp.Asc var_ (HTyp.Sum (HTyp.Arrow HTyp.Num HTyp.Hole) HTyp.Hole)),
      ("rightAssoc", HExp.Asc var_ (HTyp.Sum HTyp.Hole (HTyp.Sum HTyp.Num HTyp.Hole))),
      ("leftAssoc", HExp.Asc var_ (HTyp.Sum (HTyp.Sum HTyp.Hole HTyp.Num) HTyp.Hole))
    ],
    "testAscHole" >:::% [("basic", HExp.Asc var_ HTyp.Hole)],
    "testThorough" >:::% [
      (
        "thorough",
        /* See the HExp returned by get_thorough_ */
        switch (get_thorough_ None) {
        | Some thorough => thorough
        | _ => assert false
        }
      )
    ],
    "testInvalidVarName" >:: testInvalidVarName
  ];

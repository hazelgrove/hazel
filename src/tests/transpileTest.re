open OUnit2;

open Printf;

open Semantics.Core;

let ph_ = HExp.Var "ph";

let phn_ n => HExp.Var ("ph" ^ string_of_int n);

let wsre_ = "[ \n\r\t]";

let norm_ s => Str.global_replace (Str.regexp (wsre_ ^ "+")) " " (String.trim s);

let test_serialize_ hexp expected =>
  assert_equal printer::(fun x => x) (norm_ expected) (norm_ (Transpile.hz_to_string hexp));

let test_serialize_ hexp expected =>
  assert_equal printer::(fun x => x) (norm_ expected) (norm_ (Transpile.hz_to_string hexp));

let testVar _ => test_serialize_ (HExp.Var "var") "var;";

let testLet _ => {
  test_serialize_ (HExp.Let "x" ph_ (HExp.Var "b")) "{ let x = ph; b };";
  test_serialize_
    (HExp.Let "a" (HExp.Let "b" ph_ (HExp.Var "b")) (HExp.Let "c" (HExp.Var "a") (HExp.Var "c")))
    "{ let a = { let b = ph; b }; let c = a; c };";
  test_serialize_
    (HExp.Let "a" ph_ (HExp.Let "a" (HExp.Var "a") (HExp.Var "a"))) "{ let a = ph; let a = a; a };"
};

let testLam _ => {
  test_serialize_ (HExp.Lam "a" ph_) "fun a => ph;";
  test_serialize_ (HExp.Lam "a" (HExp.Lam "b" (HExp.Var "b"))) "fun a b => b;"
};

let testAp _ => {
  test_serialize_ (HExp.Ap (phn_ 1) (phn_ 2)) "ph1 ph2;";
  test_serialize_
    (HExp.Ap (HExp.Ap (phn_ 1) (phn_ 2)) (HExp.Ap (phn_ 3) (phn_ 4))) "(ph1 ph2) (ph3 ph4);"
};

let testNumLit _ => {
  test_serialize_ (HExp.NumLit 0) "0;";
  test_serialize_ (HExp.NumLit 1) "1;"
};

let testPlus _ => {
  test_serialize_ (HExp.Plus (phn_ 1) (phn_ 2)) "ph1 + ph2;";
  test_serialize_
    (HExp.Plus (HExp.Plus (phn_ 1) (phn_ 2)) (HExp.Plus (phn_ 3) (phn_ 4)))
    "ph1 + ph2 + (ph3 + ph4);"
};

let testInj side _ => {
  let side_str =
    switch side {
    | HExp.L => "L"
    | HExp.R => "R"
    };
  test_serialize_ (HExp.Inj side ph_) (sprintf "HazelPrelude.%s ph;" side_str);
  test_serialize_
    (HExp.Inj side (HExp.Inj HExp.L (HExp.Var "x")))
    (sprintf "HazelPrelude.%s (HazelPrelude.L x);" side_str)
};

let testCase _ => {
  test_serialize_
    (HExp.Case ph_ ("l", HExp.Var "x") ("r", HExp.Var "y"))
    "switch ph { | HazelPrelude.L l => x | HazelPrelude.R r => y };";
  test_serialize_
    (
      HExp.Case
        (HExp.Case ph_ ("el", HExp.Var "ele") ("er", HExp.Var "ere"))
        ("l", HExp.Case (phn_ 1) ("ll", HExp.Var "lle") ("lr", HExp.Var "lre"))
        ("r", HExp.Case (phn_ 2) ("rl", HExp.Var "rle") ("rr", HExp.Var "rre"))
    )
    (
      "switch ( switch ph { | HazelPrelude.L el => ele | HazelPrelude.R er => ere } ) { " ^
      "| HazelPrelude.L l => " ^
      "  switch ph1 { | HazelPrelude.L ll => lle | HazelPrelude.R lr => lre } " ^
      "| HazelPrelude.R r => " ^
      "  switch ph2 { | HazelPrelude.L rl => rle | HazelPrelude.R rr => rre } " ^ "};"
    )
};

let testEmptyHole _ => {
  test_serialize_ (HExp.EmptyHole 0) "HazelPrelude.EHole 0;";
  test_serialize_ (HExp.EmptyHole 1) "HazelPrelude.EHole 1;"
};

let testNonEmptyHole _ => {
  /* The presence of `[@implicit_arity]` is painful, but apparentally unavoidable */
  test_serialize_ (HExp.NonEmptyHole 0 ph_) "HazelPrelude.NEHole 0 ph [@implicit_arity];";
  test_serialize_
    (HExp.NonEmptyHole 1 (HExp.NonEmptyHole 2 ph_))
    "HazelPrelude.NEHole 1 (HazelPrelude.NEHole 2 ph [@implicit_arity]) [@implicit_arity];"
};

let testAscNum _ => test_serialize_ (HExp.Asc ph_ HTyp.Num) "(ph: HazelPrelude.num);";

let testAscArrow _ => {
  test_serialize_
    (HExp.Asc ph_ (HTyp.Arrow HTyp.Num HTyp.Hole)) "(ph: HazelPrelude.num => HazelPrelude.hole);";
  test_serialize_
    (HExp.Asc ph_ (HTyp.Arrow HTyp.Hole (HTyp.Arrow HTyp.Num HTyp.Hole)))
    "(ph: HazelPrelude.hole => HazelPrelude.num => HazelPrelude.hole);";
  test_serialize_
    (HExp.Asc ph_ (HTyp.Arrow (HTyp.Arrow HTyp.Hole HTyp.Num) HTyp.Hole))
    "(ph: (HazelPrelude.hole => HazelPrelude.num) => HazelPrelude.hole);"
};

let testAscSum _ => {
  test_serialize_
    (HExp.Asc ph_ (HTyp.Sum (HTyp.Arrow HTyp.Num HTyp.Hole) HTyp.Hole))
    "(ph: HazelPrelude.sum (HazelPrelude.num => HazelPrelude.hole) HazelPrelude.hole);";
  test_serialize_
    (HExp.Asc ph_ (HTyp.Sum HTyp.Hole (HTyp.Sum HTyp.Num HTyp.Hole)))
    "( ph: HazelPrelude.sum HazelPrelude.hole (HazelPrelude.sum HazelPrelude.num HazelPrelude.hole) );";
  test_serialize_
    (HExp.Asc ph_ (HTyp.Sum (HTyp.Sum HTyp.Hole HTyp.Num) HTyp.Hole))
    "( ph: HazelPrelude.sum (HazelPrelude.sum HazelPrelude.hole HazelPrelude.num) HazelPrelude.hole );"
};

let testAscHole _ => test_serialize_ (HExp.Asc ph_ HTyp.Hole) "(ph: HazelPrelude.hole);";

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

let testThorough _ =>
  test_serialize_
    (
      switch (get_thorough_ None) {
      | None => assert false
      | Some t => t
      }
    )
    (
      /* TODO move this to a data file */
      "{ " ^
      "  let vNum: HazelPrelude.num = HazelPrelude.EHole 0; " ^
      "  let _plus: HazelPrelude.num => HazelPrelude.num => HazelPrelude.num = " ^
      "    fun n_ n_2 => n_ + n_2; " ^
      "  let l001: HazelPrelude.sum " ^
      "              (HazelPrelude.num => HazelPrelude.num => HazelPrelude.num) " ^
      "              HazelPrelude.num = " ^
      "    HazelPrelude.L _plus;" ^
      "  let r: HazelPrelude.sum " ^
      "           (HazelPrelude.num => HazelPrelude.num => HazelPrelude.num) " ^
      "           HazelPrelude.hole = " ^
      "    HazelPrelude.R vNum;" ^
      "  HazelPrelude.NEHole 1 ( " ^
      "    ( " ^
      "      _plus " ^
      "      ( " ^
      "        switch l001 { " ^
      "          | HazelPrelude.L l2 => (l2 7) 0 " ^
      "          | HazelPrelude.R r2 => r2 " ^
      "        } " ^ "      ) " ^ "    ) " ^ "    3 " ^ "  ) [@implicit_arity] " ^ "};"
    );

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

let tests =
  "Transpile tests" >::: [
    "testVar" >:: testVar,
    "testLet" >:: testLet,
    "testLam" >:: testLam,
    "testAp" >:: testAp,
    "testNumLit" >:: testNumLit,
    "testPlus" >:: testPlus,
    "testInjLeft" >:: testInj HExp.L,
    "testInjRight" >:: testInj HExp.R,
    "testCase" >:: testCase,
    "testEmptyHole" >:: testEmptyHole,
    "testNonEmptyHole" >:: testNonEmptyHole,
    "testAscNum" >:: testAscNum,
    "testAscArrow" >:: testAscArrow,
    "testAscSum" >:: testAscSum,
    "testAscHole" >:: testAscHole,
    "testThorough" >:: testThorough,
    "testInvalidVarName" >:: testInvalidVarName
  ];

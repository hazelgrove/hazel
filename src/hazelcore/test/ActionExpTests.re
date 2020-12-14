/*
 * Attempting to synthesize a + operator when the cursor is on a float expression
 * 1.| => 1. +.|
 */
let%expect_test "simple int operation prediction test" = {
  let zoperand =
    ZExp.CursorE(CursorPosition.OnText(1), UHExp.IntLit(NotInHole, "1"));
  let skel = Skel.Placeholder(0);
  let zopseq = ZOpSeq.ZOpSeq(skel, ZOperand(zoperand, (E, E)));
  let zline = ZExp.ExpLineZ(zopseq);
  let e = ([], zline, []);
  switch (
    Action_Exp.syn_perform(
      Contexts.empty,
      Construct(SOp(SPlus)),
      (e, HTyp.Int, MetaVarGen.init),
    )
  ) {
  | Succeeded((ze, _, _)) => print_endline(Serialization.string_of_zexp(ze))
  | _ => print_endline("nothing")
  };

  %expect
  {|(()(ExpLineZ(ZOpSeq(BinOp NotInHole Plus(Placeholder 0)(Placeholder 1))(ZOperator((OnOp After)Plus)((S(IntLit NotInHole 1)E)(S(EmptyHole 0)E)))))())|};
};

let%expect_test "simple float operation prediction test" = {
  let zoperand =
    ZExp.CursorE(CursorPosition.OnText(2), UHExp.FloatLit(NotInHole, "1."));
  let skel = Skel.Placeholder(0);
  let zopseq = ZOpSeq.ZOpSeq(skel, ZOperand(zoperand, (E, E)));
  let zline = ZExp.ExpLineZ(zopseq);
  let e = ([], zline, []);
  switch (
    Action_Exp.syn_perform(
      Contexts.empty,
      Construct(SOp(SPlus)),
      (e, HTyp.Float, MetaVarGen.init),
    )
  ) {
  | Succeeded((ze, _, _)) => print_endline(Serialization.string_of_zexp(ze))
  | _ => print_endline("nothing")
  };
  // TODO: need to change to correct output
  %expect
  {|(()(ExpLineZ(ZOpSeq(BinOp NotInHole FPlus(Placeholder 0)(Placeholder 1))(ZOperator((OnOp After)FPlus)((S(FloatLit NotInHole 1.)E)(S(EmptyHole 0)E)))))())|};
};

/* |_ -> _ +| _ */
let%expect_test "operation prediction before empty hole test" = {
  let zoperand =
    ZExp.CursorE(CursorPosition.OnDelim(0, Before), UHExp.EmptyHole(0));
  let skel = Skel.Placeholder(0);
  let zopseq = ZOpSeq.ZOpSeq(skel, ZOperand(zoperand, (E, E)));
  let zline = ZExp.ExpLineZ(zopseq);
  let e = ([], zline, []);
  switch (
    Action_Exp.syn_perform(
      Contexts.empty,
      Construct(SOp(SPlus)),
      (e, HTyp.Int, MetaVarGen.init),
    )
  ) {
  | Succeeded((ze, _, _)) => print_endline(Serialization.string_of_zexp(ze))
  | _ => print_endline("nothing")
  };
  // TODO: need to change to correct output
  %expect
  {|(()(ExpLineZ(ZOpSeq(BinOp NotInHole Plus(Placeholder 0)(Placeholder 1))(ZOperator((OnOp After)Plus)((S(EmptyHole 1)E)(S(EmptyHole 0)E)))))())|};
};

/* let x : Float = _| in ... => let x : Float = _ +. _ in ... */

/* let x : Int = _| in ... => let x : Int = _ + _ in ... */

/* let x : Bool = _| in ... => let x : Bool = _ + _ in ... */

/* let f = λx:Int.{3.} in
     f 2|
     =>
     let f = λx:Int.{3.} in
     f 2 +.| _
   */

/* 1| 1 => 1 +| 1 */

/* 1.| 2. => 1. +.| 2. */

/* 1.| 2 => 1. +.| 2 */

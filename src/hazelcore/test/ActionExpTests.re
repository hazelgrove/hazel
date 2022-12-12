/* commenting these out for now until syntax stabilizes

                                                                                                                                   /*
                                                                                                                                    * Attempting to synthesize an operator when the cursor is on a int expression
                                                                                                                                    * 1| => 1 +|
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
                                                                                                                                         (e, HTyp.Int, IDGen.init),
                                                                                                                                       )
                                                                                                                                     ) {
                                                                                                                                     | Succeeded((ze, _, _)) => print_endline(Serialization.string_of_zexp(ze))
                                                                                                                                     | _ => print_endline("nothing")
                                                                                                                                     };

                                                                                                                                     %expect
                                                                                                                                     {|(()(ExpLineZ(ZOpSeq(BinOp NotInHole Plus(Placeholder 0)(Placeholder 1))(ZOperator((OnOp After)Plus)((S(IntLit NotInHole 1)E)(S(EmptyHole 0)E)))))())|};
                                                                                                                                   };

                                                                                                                                   /*
                                                                                                                                    * Attempting to synthesize an operator (shape `sop`) when the cursor is on RHS of a zexpression
                                                                                                                                    * e.g., 1.| => 1. +.|
                                                                                                                                    */
                                                                                                                                   let test_op_zop_rhs = (e: ZExp.zblock, sop: Action.operator_shape) => {
                                                                                                                                     switch (
                                                                                                                                       Action_Exp.syn_perform(
                                                                                                                                         Contexts.empty,
                                                                                                                                         Construct(SOp(sop)),
                                                                                                                                         (e, HTyp.Float, IDGen.init),
                                                                                                                                       )
                                                                                                                                     ) {
                                                                                                                                     | Succeeded((ze, _, _)) => print_endline(Serialization.string_of_zexp(ze))
                                                                                                                                     | _ => print_endline("nothing")
                                                                                                                                     };
                                                                                                                                   };

                                                                                                                                   /*
                                                                                                                                    * Attempting to synthesize all basic float operators when cursor is on RHS of float zexpression, i.e.,
                                                                                                                                    * e.g., 1.| => 1. +.|
                                                                                                                                    * 1.| => 1. *.|
                                                                                                                                    * 1.| => 1. <.|
                                                                                                                                    * 1.| => 1. ==.|
                                                                                                                                    */
                                                                                                                                   let%expect_test "simple float operations +. -. *. /. <. >. ==. prediction test" = {
                                                                                                                                     let zoperand =
                                                                                                                                       ZExp.CursorE(CursorPosition.OnText(2), UHExp.FloatLit(NotInHole, "1."));
                                                                                                                                     let skel = Skel.Placeholder(0);
                                                                                                                                     let zopseq = ZOpSeq.ZOpSeq(skel, ZOperand(zoperand, (E, E)));
                                                                                                                                     let zline = ZExp.ExpLineZ(zopseq);
                                                                                                                                     let e = ([], zline, []);
                                                                                                                                     test_op_zop_rhs(e, SPlus);
                                                                                                                                     %expect
                                                                                                                                     {|(()(ExpLineZ(ZOpSeq(BinOp NotInHole FPlus(Placeholder 0)(Placeholder 1))(ZOperator((OnOp After)FPlus)((S(FloatLit NotInHole 1.)E)(S(EmptyHole 0)E)))))())|};
                                                                                                                                     test_op_zop_rhs(e, SMinus);
                                                                                                                                     %expect
                                                                                                                                     {|(()(ExpLineZ(ZOpSeq(BinOp NotInHole FMinus(Placeholder 0)(Placeholder 1))(ZOperator((OnOp After)FMinus)((S(FloatLit NotInHole 1.)E)(S(EmptyHole 0)E)))))())|};
                                                                                                                                     test_op_zop_rhs(e, STimes);
                                                                                                                                     %expect
                                                                                                                                     {|(()(ExpLineZ(ZOpSeq(BinOp NotInHole FTimes(Placeholder 0)(Placeholder 1))(ZOperator((OnOp After)FTimes)((S(FloatLit NotInHole 1.)E)(S(EmptyHole 0)E)))))())|};
                                                                                                                                     test_op_zop_rhs(e, SDivide);
                                                                                                                                     %expect
                                                                                                                                     {|(()(ExpLineZ(ZOpSeq(BinOp NotInHole FDivide(Placeholder 0)(Placeholder 1))(ZOperator((OnOp After)FDivide)((S(FloatLit NotInHole 1.)E)(S(EmptyHole 0)E)))))())|};
                                                                                                                                     test_op_zop_rhs(e, SLessThan);
                                                                                                                                     %expect
                                                                                                                                     {|(()(ExpLineZ(ZOpSeq(BinOp NotInHole FLessThan(Placeholder 0)(Placeholder 1))(ZOperator((OnOp After)FLessThan)((S(FloatLit NotInHole 1.)E)(S(EmptyHole 0)E)))))())|};
                                                                                                                                     test_op_zop_rhs(e, SGreaterThan);
                                                                                                                                     %expect
                                                                                                                                     {|(()(ExpLineZ(ZOpSeq(BinOp NotInHole FGreaterThan(Placeholder 0)(Placeholder 1))(ZOperator((OnOp After)FGreaterThan)((S(FloatLit NotInHole 1.)E)(S(EmptyHole 0)E)))))())|};
                                                                                                                                     test_op_zop_rhs(e, SEquals);
                                                                                                                                     %expect
                                                                                                                                     {|(()(ExpLineZ(ZOpSeq(BinOp NotInHole FEquals(Placeholder 0)(Placeholder 1))(ZOperator((OnOp After)FEquals)((S(FloatLit NotInHole 1.)E)(S(EmptyHole 0)E)))))())|};
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
                                                                                                                                         (e, HTyp.Hole, 1),
                                                                                                                                       )
                                                                                                                                     ) {
                                                                                                                                     | Succeeded((ze, _, _)) => print_endline(Serialization.string_of_zexp(ze))
                                                                                                                                     | _ => print_endline("nothing")
                                                                                                                                     };

                                                                                                                                     %expect
                                                                                                                                     {|(()(ExpLineZ(ZOpSeq(BinOp NotInHole Plus(Placeholder 0)(Placeholder 1))(ZOperator((OnOp After)Plus)((S(EmptyHole 1)E)(S(EmptyHole 0)E)))))())|};
                                                                                                                                   };

                                                                                                                                   /* let x : Float = _| in ... => let x : Float = _ +.| _ in ... */
                                                                                                                                   let%expect_test "basic operation prediction analysis against float test" = {
                                                                                                                                     // basic case
                                                                                                                                     let zoperand =
                                                                                                                                       ZExp.CursorE(CursorPosition.OnDelim(0, After), UHExp.EmptyHole(4));
                                                                                                                                     let skel = Skel.Placeholder(0);
                                                                                                                                     let zopseq = ZOpSeq.ZOpSeq(skel, ZOperand(zoperand, (E, E)));
                                                                                                                                     let zline = ZExp.ExpLineZ(zopseq);
                                                                                                                                     let e1 = ([], zline, []);
                                                                                                                                     let expline = UHExp.ExpLine(OpSeq(Placeholder(0), S(EmptyHole(6), E))); // (ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 6)E)));
                                                                                                                                     let opseq =
                                                                                                                                       OpSeq.OpSeq(
                                                                                                                                         Placeholder(0),
                                                                                                                                         S(UHPat.Var(NotInHole, NotInVarHole, "x"), E),
                                                                                                                                       );
                                                                                                                                     let opseq2 = OpSeq.OpSeq(Placeholder(0), Seq.wrap(UHTyp.Float));
                                                                                                                                     let final = ([], ZExp.LetLineZE(opseq, Some(opseq2), e1), [expline]);
                                                                                                                                     switch (
                                                                                                                                       Action_Exp.syn_perform(
                                                                                                                                         Contexts.empty,
                                                                                                                                         Construct(SOp(SPlus)),
                                                                                                                                         (final, HTyp.Hole, 7),
                                                                                                                                       )
                                                                                                                                     ) {
                                                                                                                                     | Succeeded((ze, _, _)) => print_endline(Serialization.string_of_zexp(ze))
                                                                                                                                     | _ => print_endline("nothing")
                                                                                                                                     };

                                                                                                                                     %expect
                                                                                                                                     {|(()(LetLineZE(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole x)E))((OpSeq(Placeholder 0)(S Float E)))(()(ExpLineZ(ZOpSeq(BinOp NotInHole FPlus(Placeholder 0)(Placeholder 1))(ZOperator((OnOp After)FPlus)((S(EmptyHole 4)E)(S(EmptyHole 7)E)))))()))((ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 6)E)))))|};
                                                                                                                                   };

                                                                                                                                   /* analysis produces float operator even with int zoperand
                                                                                                                                      let x : Float = 1| in ... => let x : Float = 1 +.| _ in ... */
                                                                                                                                   let%expect_test "operation prediction analysis against float with int test" = {
                                                                                                                                     //  int zoperand case
                                                                                                                                     let zoperand =
                                                                                                                                       ZExp.CursorE(
                                                                                                                                         CursorPosition.OnText(1),
                                                                                                                                         UHExp.IntLit(InHole(TypeInconsistent, 2), "1"),
                                                                                                                                       );
                                                                                                                                     let skel = Skel.Placeholder(0);
                                                                                                                                     let zopseq = ZOpSeq.ZOpSeq(skel, ZOperand(zoperand, (E, E)));
                                                                                                                                     let zline = ZExp.ExpLineZ(zopseq);
                                                                                                                                     let e1 = ([], zline, []);
                                                                                                                                     let expline = UHExp.ExpLine(OpSeq(Placeholder(0), S(EmptyHole(6), E))); // (ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 6)E)));
                                                                                                                                     let opseq =
                                                                                                                                       OpSeq.OpSeq(
                                                                                                                                         Placeholder(0),
                                                                                                                                         S(UHPat.Var(NotInHole, NotInVarHole, "x"), E),
                                                                                                                                       );
                                                                                                                                     let opseq2 = OpSeq.OpSeq(Placeholder(0), Seq.wrap(UHTyp.Float));
                                                                                                                                     let final = ([], ZExp.LetLineZE(opseq, Some(opseq2), e1), [expline]);
                                                                                                                                     switch (
                                                                                                                                       Action_Exp.syn_perform(
                                                                                                                                         Contexts.empty,
                                                                                                                                         Construct(SOp(SPlus)),
                                                                                                                                         (final, HTyp.Int, IDGen.init),
                                                                                                                                       )
                                                                                                                                     ) {
                                                                                                                                     | Succeeded((ze, _, _)) => print_endline(Serialization.string_of_zexp(ze))
                                                                                                                                     | _ => print_endline("nothing")
                                                                                                                                     };

                                                                                                                                     %expect
                                                                                                                                     {|(()(LetLineZE(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole x)E))((OpSeq(Placeholder 0)(S Float E)))(()(ExpLineZ(ZOpSeq(BinOp NotInHole FPlus(Placeholder 0)(Placeholder 1))(ZOperator((OnOp After)FPlus)((S(IntLit(InHole TypeInconsistent 1)1)E)(S(EmptyHole 0)E)))))()))((ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 6)E)))))|};
                                                                                                                                   };

                                                                                                                                   /* let x : Int = _| in ... => let x : Int = _ + _ in ... */
                                                                                                                                   let%expect_test "operation prediction analysis against int test" = {
                                                                                                                                     let zoperand =
                                                                                                                                       ZExp.CursorE(CursorPosition.OnDelim(0, After), UHExp.EmptyHole(4));
                                                                                                                                     let skel = Skel.Placeholder(0);
                                                                                                                                     let zopseq = ZOpSeq.ZOpSeq(skel, ZOperand(zoperand, (E, E)));
                                                                                                                                     let zline = ZExp.ExpLineZ(zopseq);
                                                                                                                                     let e1 = ([], zline, []);
                                                                                                                                     let expline = UHExp.ExpLine(OpSeq(Placeholder(0), S(EmptyHole(6), E))); // (ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 6)E)));
                                                                                                                                     let opseq =
                                                                                                                                       OpSeq.OpSeq(
                                                                                                                                         Placeholder(0),
                                                                                                                                         S(UHPat.Var(NotInHole, NotInVarHole, "x"), E),
                                                                                                                                       );
                                                                                                                                     let opseq2 = OpSeq.OpSeq(Placeholder(0), Seq.wrap(UHTyp.Int));
                                                                                                                                     let final = ([], ZExp.LetLineZE(opseq, Some(opseq2), e1), [expline]);
                                                                                                                                     switch (
                                                                                                                                       Action_Exp.syn_perform(
                                                                                                                                         Contexts.empty,
                                                                                                                                         Construct(SOp(SPlus)),
                                                                                                                                         (final, HTyp.Hole, 7),
                                                                                                                                       )
                                                                                                                                     ) {
                                                                                                                                     | Succeeded((ze, _, _)) => print_endline(Serialization.string_of_zexp(ze))
                                                                                                                                     | _ => print_endline("nothing")
                                                                                                                                     };

                                                                                                                                     %expect
                                                                                                                                     {|(()(LetLineZE(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole x)E))((OpSeq(Placeholder 0)(S Int E)))(()(ExpLineZ(ZOpSeq(BinOp NotInHole Plus(Placeholder 0)(Placeholder 1))(ZOperator((OnOp After)Plus)((S(EmptyHole 4)E)(S(EmptyHole 7)E)))))()))((ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 6)E)))))|};
                                                                                                                                   };

                                                                                                                                   /* let x : Bool = _| in ... => let x : Bool = _ + _ in ... */
                                                                                                                                   let%expect_test "operation prediction analysis against irrelevant type test" = {
                                                                                                                                     let zoperand =
                                                                                                                                       ZExp.CursorE(CursorPosition.OnDelim(0, After), UHExp.EmptyHole(4));
                                                                                                                                     let skel = Skel.Placeholder(0);
                                                                                                                                     let zopseq = ZOpSeq.ZOpSeq(skel, ZOperand(zoperand, (E, E)));
                                                                                                                                     let zline = ZExp.ExpLineZ(zopseq);
                                                                                                                                     let e1 = ([], zline, []);
                                                                                                                                     let expline = UHExp.ExpLine(OpSeq(Placeholder(0), S(EmptyHole(6), E))); // (ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 6)E)));
                                                                                                                                     let opseq =
                                                                                                                                       OpSeq.OpSeq(
                                                                                                                                         Placeholder(0),
                                                                                                                                         S(UHPat.Var(NotInHole, NotInVarHole, "x"), E),
                                                                                                                                       );
                                                                                                                                     let opseq2 = OpSeq.OpSeq(Placeholder(0), Seq.wrap(UHTyp.Bool));
                                                                                                                                     let final = ([], ZExp.LetLineZE(opseq, Some(opseq2), e1), [expline]);
                                                                                                                                     switch (
                                                                                                                                       Action_Exp.syn_perform(
                                                                                                                                         Contexts.empty,
                                                                                                                                         Construct(SOp(SPlus)),
                                                                                                                                         (final, HTyp.Hole, 7),
                                                                                                                                       )
                                                                                                                                     ) {
                                                                                                                                     | Succeeded((ze, _, _)) => print_endline(Serialization.string_of_zexp(ze))
                                                                                                                                     | _ => print_endline("nothing")
                                                                                                                                     };

                                                                                                                                     %expect
                                                                                                                                     {|(()(LetLineZE(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole x)E))((OpSeq(Placeholder 0)(S Bool E)))(()(ExpLineZ(ZOpSeq(BinOp(InHole TypeInconsistent 8)Plus(Placeholder 0)(Placeholder 1))(ZOperator((OnOp After)Plus)((S(EmptyHole 4)E)(S(EmptyHole 7)E)))))()))((ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 6)E)))))|};
                                                                                                                                   };

                                                                                                                                   /* 1| 1 => 1 +| 1 */
                                                                                                                                   let%expect_test "int spaced operation prediction test" = {
                                                                                                                                     let zoperand =
                                                                                                                                       ZExp.CursorE(
                                                                                                                                         CursorPosition.OnText(1),
                                                                                                                                         UHExp.IntLit(InHole(TypeInconsistent, 2), "1"),
                                                                                                                                       );
                                                                                                                                     let skel =
                                                                                                                                       Skel.BinOp(
                                                                                                                                         NotInHole,
                                                                                                                                         Operators_Exp.Space,
                                                                                                                                         Placeholder(0),
                                                                                                                                         Placeholder(1),
                                                                                                                                       );
                                                                                                                                     let zopseq =
                                                                                                                                       ZOpSeq.ZOpSeq(
                                                                                                                                         skel,
                                                                                                                                         ZOperand(
                                                                                                                                           zoperand,
                                                                                                                                           (E, A(Space, S(UHExp.IntLit(NotInHole, "1"), E))),
                                                                                                                                         ),
                                                                                                                                       );
                                                                                                                                     let zline = ZExp.ExpLineZ(zopseq);
                                                                                                                                     let e = ([], zline, []);
                                                                                                                                     switch (
                                                                                                                                       Action_Exp.syn_perform(
                                                                                                                                         Contexts.empty,
                                                                                                                                         Construct(SOp(SPlus)),
                                                                                                                                         (e, HTyp.Int, IDGen.init),
                                                                                                                                       )
                                                                                                                                     ) {
                                                                                                                                     | Succeeded((ze, _, _)) => print_endline(Serialization.string_of_zexp(ze))
                                                                                                                                     | _ => print_endline("nothing")
                                                                                                                                     };

                                                                                                                                     %expect
                                                                                                                                     {|(()(ExpLineZ(ZOpSeq(BinOp NotInHole Plus(Placeholder 0)(Placeholder 1))(ZOperator((OnOp After)Plus)((S(IntLit NotInHole 1)E)(S(IntLit NotInHole 1)E)))))())|};
                                                                                                                                   };

                                                                                                                                   /* 1.| 2. => 1. +.| 2. */
                                                                                                                                   let%expect_test "float spaced operation prediction test" = {
                                                                                                                                     let zoperand =
                                                                                                                                       ZExp.CursorE(
                                                                                                                                         CursorPosition.OnText(2),
                                                                                                                                         UHExp.FloatLit(InHole(TypeInconsistent, 2), "1."),
                                                                                                                                       );
                                                                                                                                     let skel =
                                                                                                                                       Skel.BinOp(
                                                                                                                                         NotInHole,
                                                                                                                                         Operators_Exp.Space,
                                                                                                                                         Placeholder(0),
                                                                                                                                         Placeholder(1),
                                                                                                                                       );
                                                                                                                                     let zopseq =
                                                                                                                                       ZOpSeq.ZOpSeq(
                                                                                                                                         skel,
                                                                                                                                         ZOperand(
                                                                                                                                           zoperand,
                                                                                                                                           (E, A(Space, S(UHExp.FloatLit(NotInHole, "1."), E))),
                                                                                                                                         ),
                                                                                                                                       );
                                                                                                                                     let zline = ZExp.ExpLineZ(zopseq);
                                                                                                                                     let e = ([], zline, []);
                                                                                                                                     switch (
                                                                                                                                       Action_Exp.syn_perform(
                                                                                                                                         Contexts.empty,
                                                                                                                                         Construct(SOp(SPlus)),
                                                                                                                                         (e, HTyp.Float, IDGen.init),
                                                                                                                                       )
                                                                                                                                     ) {
                                                                                                                                     | Succeeded((ze, _, _)) => print_endline(Serialization.string_of_zexp(ze))
                                                                                                                                     | _ => print_endline("nothing")
                                                                                                                                     };

                                                                                                                                     %expect
                                                                                                                                     {|(()(ExpLineZ(ZOpSeq(BinOp NotInHole FPlus(Placeholder 0)(Placeholder 1))(ZOperator((OnOp After)FPlus)((S(FloatLit NotInHole 1.)E)(S(FloatLit NotInHole 1.)E)))))())|};
                                                                                                                                   };

                                                                                                                                   /* 1.| 2 => 1. +.| 2 */
                                                                                                                                   let%expect_test "mixed spaced operation prediction test" = {
                                                                                                                                     let zoperand =
                                                                                                                                       ZExp.CursorE(
                                                                                                                                         CursorPosition.OnText(2),
                                                                                                                                         UHExp.FloatLit(InHole(TypeInconsistent, 2), "1."),
                                                                                                                                       );
                                                                                                                                     let skel =
                                                                                                                                       Skel.BinOp(
                                                                                                                                         NotInHole,
                                                                                                                                         Operators_Exp.Space,
                                                                                                                                         Placeholder(0),
                                                                                                                                         Placeholder(1),
                                                                                                                                       );
                                                                                                                                     let zopseq =
                                                                                                                                       ZOpSeq.ZOpSeq(
                                                                                                                                         skel,
                                                                                                                                         ZOperand(
                                                                                                                                           zoperand,
                                                                                                                                           (E, A(Space, S(UHExp.IntLit(NotInHole, "2"), E))),
                                                                                                                                         ),
                                                                                                                                       );
                                                                                                                                     let zline = ZExp.ExpLineZ(zopseq);
                                                                                                                                     let e = ([], zline, []);
                                                                                                                                     switch (
                                                                                                                                       Action_Exp.syn_perform(
                                                                                                                                         Contexts.empty,
                                                                                                                                         Construct(SOp(SPlus)),
                                                                                                                                         (e, HTyp.Float, 3),
                                                                                                                                       )
                                                                                                                                     ) {
                                                                                                                                     | Succeeded((ze, _, _)) => print_endline(Serialization.string_of_zexp(ze))
                                                                                                                                     | _ => print_endline("nothing")
                                                                                                                                     };

                                                                                                                                     %expect
                                                                                                                                     {|(()(ExpLineZ(ZOpSeq(BinOp NotInHole FPlus(Placeholder 0)(Placeholder 1))(ZOperator((OnOp After)FPlus)((S(FloatLit NotInHole 1.)E)(S(IntLit(InHole TypeInconsistent 3)2)E)))))())|};
                                                                                                                                   };

                                                                                                                                   /* edge case where LHS float operand analyzes as function type
                                                                                                                                      cursor on empty hole, LHS float operand
                                                                                                                                      1. |_ => 1. +.| _ */
                                                                                                                                   let%expect_test "float operand on LHS of empty hole zoperand test" = {
                                                                                                                                     // float as left operand
                                                                                                                                     let zoperand =
                                                                                                                                       ZExp.CursorE(CursorPosition.OnDelim(0, Before), UHExp.EmptyHole(0));
                                                                                                                                     let skel =
                                                                                                                                       Skel.BinOp(
                                                                                                                                         NotInHole,
                                                                                                                                         Operators_Exp.Space,
                                                                                                                                         Placeholder(0),
                                                                                                                                         Placeholder(1),
                                                                                                                                       );
                                                                                                                                     let zopseq =
                                                                                                                                       ZOpSeq.ZOpSeq(
                                                                                                                                         skel,
                                                                                                                                         ZOperand(
                                                                                                                                           zoperand,
                                                                                                                                           (
                                                                                                                                             A(Space, S(UHExp.FloatLit(InHole(TypeInconsistent, 1), "1."), E)),
                                                                                                                                             E,
                                                                                                                                           ),
                                                                                                                                         ),
                                                                                                                                       );
                                                                                                                                     let zline = ZExp.ExpLineZ(zopseq);
                                                                                                                                     let e = ([], zline, []);
                                                                                                                                     switch (
                                                                                                                                       Action_Exp.syn_perform(
                                                                                                                                         Contexts.empty,
                                                                                                                                         Construct(SOp(SPlus)),
                                                                                                                                         (e, HTyp.Hole, IDGen.init),
                                                                                                                                       )
                                                                                                                                     ) {
                                                                                                                                     | Succeeded((ze, _, _)) => print_endline(Serialization.string_of_zexp(ze))
                                                                                                                                     | _ => print_endline("nothing")
                                                                                                                                     };

                                                                                                                                     %expect
                                                                                                                                     {|(()(ExpLineZ(ZOpSeq(BinOp NotInHole FPlus(Placeholder 0)(Placeholder 1))(ZOperator((OnOp After)FPlus)((S(FloatLit NotInHole 1.)E)(S(EmptyHole 0)E)))))())|};
                                                                                                                                   };

                                                                                                                                   /* cursor on empty hole, RHS float operand
                                                                                                                                      _| 1. => _ +.| 1. */
                                                                                                                                   let%expect_test "float operand on RHS of empty hole zoperand test" = {
                                                                                                                                     // float as right operand
                                                                                                                                     let zoperand =
                                                                                                                                       ZExp.CursorE(CursorPosition.OnDelim(0, After), UHExp.EmptyHole(0));
                                                                                                                                     let skel =
                                                                                                                                       Skel.BinOp(
                                                                                                                                         NotInHole,
                                                                                                                                         Operators_Exp.Space,
                                                                                                                                         Placeholder(0),
                                                                                                                                         Placeholder(1),
                                                                                                                                       );
                                                                                                                                     let zopseq =
                                                                                                                                       ZOpSeq.ZOpSeq(
                                                                                                                                         skel,
                                                                                                                                         ZOperand(
                                                                                                                                           zoperand,
                                                                                                                                           (E, A(Space, S(UHExp.FloatLit(NotInHole, "1."), E))),
                                                                                                                                         ),
                                                                                                                                       );
                                                                                                                                     let zline = ZExp.ExpLineZ(zopseq);
                                                                                                                                     let e = ([], zline, []);
                                                                                                                                     switch (
                                                                                                                                       Action_Exp.syn_perform(
                                                                                                                                         Contexts.empty,
                                                                                                                                         Construct(SOp(SPlus)),
                                                                                                                                         (e, HTyp.Hole, IDGen.init),
                                                                                                                                       )
                                                                                                                                     ) {
                                                                                                                                     | Succeeded((ze, _, _)) => print_endline(Serialization.string_of_zexp(ze))
                                                                                                                                     | _ => print_endline("nothing")
                                                                                                                                     };
                                                                                                                                     %expect
                                                                                                                                     {|(()(ExpLineZ(ZOpSeq(BinOp NotInHole FPlus(Placeholder 0)(Placeholder 1))(ZOperator((OnOp After)FPlus)((S(EmptyHole 0)E)(S(FloatLit NotInHole 1.)E)))))())|};
                                                                                                                                   };
                                                                                                                                   */

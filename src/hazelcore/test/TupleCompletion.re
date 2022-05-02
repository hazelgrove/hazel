// Test suite for the tuple completion
// let mvar = MetaVarGen.init;
// let%test "Parenthesize on empty hole (Int, Bool)" = {
//   // _
//   //   let single_hole_seq = Seq.S(UHExp.EmptyHole(mvar), E);
//   //   let single_hole_skel = Skel.Placeholder(0);
//   //   let single_hole_opseq = OpSeq.OpSeq(single_hole_skel, single_hole_seq);
//   let single_hole_operand = ZExp.place_before_operand(UHExp.EmptyHole(0));
//   //   let tuple_operand = ZOpSeq.ZOpSeq(Skel.BinOp(NotInHole, Operators_Exp.Comma, Skel.Placeholder(0), Skel.Placeholder(1)), ZSeq.ZOperand(ZExp.place_before_operand(UHExp.EmptyHole(1)))(E(A Comma(S(EmptyHole 7)E))));
//   let result =
//     Action_Exp.ana_perform_operand(
//       Contexts.initial,
//       Action.Construct(SParenthesized),
//       (single_hole_operand, 0),
//       HTyp.Prod([HTyp.Int, HTyp.Bool]),
//     );
//   print_endline(
//     Sexplib.Sexp.to_string_hum(
//       ActionOutcome.sexp_of_t(Action_Exp.sexp_of_ana_success, result),
//     ),
//   );
//   let expected =
//     ActionOutcome.Succeeded(
//       Action_Exp.AnaDone((
//         ZExp.ZBlock.wrap(ZExp.ExpLineZ(ZOpSeq.ZOpSeq(Placeholder(0)
//       (ZExp.ZOperand
//        (ZExp.ParenthesizedZ
//         (()
//          (ZExp.ExpLineZ
//           (ZOpSeq (BinOp NotInHole Comma (Placeholder 0) (Placeholder 1))
//            (ZOperand (CursorE (OnDelim 0 Before) (EmptyHole 0))
//             (E (A Comma (S (EmptyHole 0) E))))))
//          ()))
//        (E E)))
//     )),
//         2,
//       )),
//     );
//   print_endline(
//     Sexplib.Sexp.to_string_hum(
//       ActionOutcome.sexp_of_t(Action_Exp.sexp_of_ana_success, expected),
//     ),
//   );
//   result == expected;
// };

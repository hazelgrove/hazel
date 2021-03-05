module StringMap = Map.Make(String);
open Sexplib.Std;

let just_hole: UHExp.t = UHExp.Block.wrap(EmptyHole(0));

let holey_lambda: UHExp.t = {
  let lam =
    UHExp.(
      Parenthesized(
        Block.wrap(
          lam(
            OpSeq.wrap(UHPat.EmptyHole(0)),
            Block.wrap(UHExp.EmptyHole(1)),
          ),
        ),
      )
    );
  let arg = UHExp.EmptyHole(2);
  UHExp.Block.wrap'(
    Seq.mk(lam, [(Operators_Exp.Space, arg)]) |> UHExp.mk_OpSeq,
  );
};

let let_line: UHExp.t =
  UHExp.[
    letline(OpSeq.wrap(UHPat.var("y")), Block.wrap(EmptyHole(0))),
    EmptyLine,
    letline(OpSeq.wrap(UHPat.var("x")), Block.wrap(EmptyHole(1))),
    ExpLine(var("x") |> OpSeq.wrap),
    ExpLine(var("y") |> OpSeq.wrap),
  ];

let map_example: UHExp.t = {
  let case_node =
    UHExp.(
      case(
        Block.wrap(var("xs")),
        [
          Rule(OpSeq.wrap(UHPat.listnil()), Block.wrap(listnil())),
          Rule(
            UHPat.(
              Seq.mk(var("y"), [(Operators_Pat.Cons, var("ys"))])
              |> mk_OpSeq
            ),
            Operators_Exp.(
              Block.wrap'(
                Seq.mk(
                  Parenthesized(
                    Block.wrap'(
                      Seq.mk(var("f"), [(Space, var("y"))]) |> mk_OpSeq,
                    ),
                  ),
                  [
                    (
                      Cons,
                      Parenthesized(
                        Block.wrap'(
                          Seq.mk(
                            var("map"),
                            [(Space, var("f")), (Space, var("ys"))],
                          )
                          |> mk_OpSeq,
                        ),
                      ),
                    ),
                  ],
                )
                |> mk_OpSeq,
              )
            ),
          ),
        ],
      )
    );
  let lam_node =
    UHExp.(
      lam(
        OpSeq.wrap(UHPat.var("f")),
        Block.wrap(
          lam(OpSeq.wrap(UHPat.var("xs")), Block.wrap(case_node)),
        ),
      )
    );
  let letline_node =
    UHExp.(
      letline(
        OpSeq.wrap(
          UHPat.TypeAnn(
            NotInHole,
            UHPat.var("map"),
            Operators_Typ.(
              UHTyp.(
                Seq.mk(
                  Parenthesized(Seq.mk(Int, [(Arrow, Int)]) |> mk_OpSeq),
                  [
                    (Arrow, List(OpSeq.wrap(Int))),
                    (Arrow, List(OpSeq.wrap(Int))),
                  ],
                )
                |> mk_OpSeq
              )
            ),
          ),
        ),
        Block.wrap(lam_node),
      )
    );
  UHExp.[letline_node, ExpLine(EmptyHole(0) |> OpSeq.wrap)];
};

let qsort_example: UHExp.t = {
  let append_case =
    UHExp.(
      case(
        Block.wrap(var("xs")),
        [
          Rule(OpSeq.wrap(UHPat.listnil()), Block.wrap(var("ys"))),
          Rule(
            UHPat.(
              Seq.mk(var("z"), [(Operators_Pat.Cons, var("zs"))])
              |> mk_OpSeq
            ),
            Operators_Exp.(
              Block.wrap'(
                Seq.mk(
                  var("z"),
                  [
                    (
                      Cons,
                      Parenthesized(
                        Block.wrap'(
                          Seq.mk(
                            var("append"),
                            [(Space, var("zs")), (Space, var("ys"))],
                          )
                          |> mk_OpSeq,
                        ),
                      ),
                    ),
                  ],
                )
                |> mk_OpSeq,
              )
            ),
          ),
        ],
      )
    );
  let append_lam =
    UHExp.(
      lam(
        OpSeq.wrap(UHPat.var("xs")),
        Block.wrap(
          lam(OpSeq.wrap(UHPat.var("ys")), Block.wrap(append_case)),
        ),
      )
    );
  let append_letline =
    UHExp.(
      letline(
        OpSeq.wrap(
          UHPat.TypeAnn(
            NotInHole,
            UHPat.var("append"),
            Operators_Typ.(
              Seq.mk(
                UHTyp.List(OpSeq.wrap(UHTyp.Int)),
                [
                  (Arrow, List(OpSeq.wrap(UHTyp.Int))),
                  (Arrow, List(OpSeq.wrap(UHTyp.Int))),
                ],
              )
              |> UHTyp.mk_OpSeq
            ),
          ),
        ),
        Block.wrap(append_lam),
      )
    );
  let partition_case =
    UHExp.(
      Operators_Exp.(
        case(
          Block.wrap(var("xs")),
          [
            Rule(
              OpSeq.wrap(UHPat.listnil()),
              Block.wrap(
                Parenthesized(
                  Block.wrap'(
                    Seq.mk(listnil(), [(Comma, listnil())]) |> mk_OpSeq,
                  ),
                ),
              ),
            ),
            Rule(
              UHPat.(
                Seq.mk(var("y"), [(Operators_Pat.Cons, var("ys"))])
                |> mk_OpSeq
              ),
              [
                letline(
                  UHPat.(
                    OpSeq.wrap(
                      Parenthesized(
                        Seq.mk(
                          var("ys1"),
                          [(Operators_Pat.Comma, var("ys2"))],
                        )
                        |> mk_OpSeq,
                      ),
                    )
                  ),
                  Block.wrap'(
                    Seq.mk(
                      var("partition"),
                      [(Space, var("f")), (Space, var("ys"))],
                    )
                    |> mk_OpSeq,
                  ),
                ),
                ExpLine(
                  case(
                    Block.wrap'(
                      Seq.mk(var("f"), [(Space, var("y"))]) |> mk_OpSeq,
                    ),
                    [
                      Rule(
                        OpSeq.wrap(UHPat.boollit(true)),
                        Block.wrap(
                          Parenthesized(
                            Block.wrap'(
                              Seq.mk(
                                var("y"),
                                [(Cons, var("ys1")), (Comma, var("ys2"))],
                              )
                              |> mk_OpSeq,
                            ),
                          ),
                        ),
                      ),
                      Rule(
                        OpSeq.wrap(UHPat.boollit(false)),
                        Block.wrap(
                          Parenthesized(
                            Block.wrap'(
                              Seq.mk(
                                var("ys1"),
                                [(Comma, var("y")), (Cons, var("ys2"))],
                              )
                              |> mk_OpSeq,
                            ),
                          ),
                        ),
                      ),
                    ],
                  )
                  |> OpSeq.wrap,
                ),
              ],
            ),
          ],
        )
      )
    );
  let partition_lam =
    UHExp.(
      lam(
        OpSeq.wrap(UHPat.var("f")),
        Block.wrap(
          lam(OpSeq.wrap(UHPat.var("xs")), Block.wrap(partition_case)),
        ),
      )
    );
  let partition_letline =
    UHExp.(
      letline(
        OpSeq.wrap(
          UHPat.TypeAnn(
            NotInHole,
            UHPat.var("partition"),
            UHTyp.(
              Operators_Typ.(
                Seq.mk(
                  Parenthesized(Seq.mk(Int, [(Arrow, Bool)]) |> mk_OpSeq),
                  [
                    (Arrow, List(OpSeq.wrap(Int))),
                    (
                      Arrow,
                      Parenthesized(
                        Seq.mk(
                          List(OpSeq.wrap(Int)),
                          [(Prod, List(OpSeq.wrap(Int)))],
                        )
                        |> mk_OpSeq,
                      ),
                    ),
                  ],
                )
                |> mk_OpSeq
              )
            ),
          ),
        ),
        Block.wrap(partition_lam),
      )
    );

  let qsort_line =
    UHExp.(
      Operators_Exp.(
        ExpLine(
          Seq.mk(
            var("qsort"),
            [
              (
                Space,
                Parenthesized(
                  Block.wrap'(
                    Seq.mk(
                      intlit("4"),
                      [
                        (Cons, intlit("2")),
                        (Cons, intlit("6")),
                        (Cons, intlit("5")),
                        (Cons, intlit("3")),
                        (Cons, intlit("1")),
                        (Cons, intlit("7")),
                        (Cons, listnil()),
                      ],
                    )
                    |> mk_OpSeq,
                  ),
                ),
              ),
            ],
          )
          |> mk_OpSeq,
        )
      )
    );

  UHExp.[append_letline, EmptyLine, partition_letline, EmptyLine, qsort_line];
};

let color =
  "((LetLine(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole baseline)E))((ExpLine(OpSeq(BinOp NotInHole Space(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(Placeholder 2))(S(ApLivelit 0 NotInHole $slider $slider(57)((next 0)(splice_map())(splice_order())))(A Space(S(IntLit NotInHole 0)(A Space(S(IntLit NotInHole 255)E)))))))))(AbbrevLine $percent NotInAbbrevHole $slider((IntLit NotInHole 0)(IntLit NotInHole 100)))(LetLine(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole default_color)E))((ExpLine(OpSeq(Placeholder 0)(S(ApLivelit 1 NotInHole $color $color((rgb(0 1 2))(a 3)(selecting_sat_val false))((next 4)(splice_map((0(Int((ExpLine(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole baseline)E))))))(1(Int((ExpLine(OpSeq(BinOp NotInHole Plus(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole baseline)(A Plus(S(IntLit NotInHole 50)E))))))))(2(Int((ExpLine(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole baseline)E))))))(3(Int((ExpLine(OpSeq(Placeholder 0)(S(ApLivelit 2 NotInHole $slider $percent(92)((next 0)(splice_map())(splice_order())))E))))))))(splice_order(0 1 2 3))))E)))))(ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 50)E))))"
  |> Sexplib.Sexp.of_string
  |> UHExp.t_of_sexp;

let map_img_filter =
  // "((LetLine(OpSeq(Placeholder 0)(S(TypeAnn NotInHole(Var NotInHole NotInVarHole map)(OpSeq(BinOp NotInHole Arrow(Placeholder 0)(BinOp NotInHole Arrow(Placeholder 1)(Placeholder 2)))(S(Parenthesized(OpSeq(BinOp NotInHole Arrow(Placeholder 0)(Placeholder 1))(S String(A Arrow(S Int E)))))(A Arrow(S(List(OpSeq(Placeholder 0)(S String E)))(A Arrow(S(List(OpSeq(Placeholder 0)(S Int E)))E)))))))E))((ExpLine(OpSeq(Placeholder 0)(S(Lam NotInHole(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole f)E))((ExpLine(OpSeq(Placeholder 0)(S(Lam NotInHole(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole xs)E))((ExpLine(OpSeq(Placeholder 0)(S(Case(StandardErrStatus NotInHole)((ExpLine(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole xs)E))))((Rule(OpSeq(Placeholder 0)(S(ListNil NotInHole)E))((ExpLine(OpSeq(Placeholder 0)(S(ListNil NotInHole)E)))))(Rule(OpSeq(BinOp NotInHole Cons(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole y)(A Cons(S(Var NotInHole NotInVarHole ys)E))))((ExpLine(OpSeq(BinOp NotInHole Cons(Placeholder 0)(Placeholder 1))(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole f)(A Space(S(Var NotInHole NotInVarHole y)E)))))))(A Cons(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Space(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(Placeholder 2))(S(Var NotInHole NotInVarHole map)(A Space(S(Var NotInHole NotInVarHole f)(A Space(S(Var NotInHole NotInVarHole ys)E)))))))))E)))))))))E)))))E)))))E)))))EmptyLine(AbbrevLine $percentage NotInAbbrevHole $slider((IntLit NotInHole 0)(IntLit NotInHole 100)))EmptyLine(LetLine(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole classic_look)E))((ExpLine(OpSeq(Placeholder 0)(S(Lam NotInHole(OpSeq(Placeholder 0)(S(TypeAnn NotInHole(Var NotInHole NotInVarHole url)(OpSeq(Placeholder 0)(S String E)))E))((ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(ApLivelit 0 NotInHole $basic_adjustments $basic_adjustments((brightness 0)(grayscale 1))((next 2)(splice_map((0(Int((ExpLine(OpSeq(Placeholder 0)(S(IntLit NotInHole 90)E))))))(1(Int((ExpLine(OpSeq(Placeholder 0)(S(IntLit NotInHole 75)E))))))))(splice_order(0 1))))(A Space(S(Var NotInHole NotInVarHole url)E)))))))E)))))(ExpLine(OpSeq(BinOp NotInHole Space(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(Placeholder 2))(S(Var NotInHole NotInVarHole map)(A Space(S(Var NotInHole NotInVarHole classic_look)(A Space(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Cons(Placeholder 0)(BinOp NotInHole Cons(Placeholder 1)(Placeholder 2)))(S(StringLit NotInHole http://tinyurl.com/y8neczz3)(A Cons(S(StringLit NotInHole http://tinyurl.com/yd2dw4ww)(A Cons(S(ListNil NotInHole)E)))))))))E))))))))"
  "((LetLine(OpSeq(Placeholder 0)(S(TypeAnn NotInHole(Var NotInHole NotInVarHole map)(OpSeq(BinOp NotInHole Arrow(Placeholder 0)(BinOp NotInHole Arrow(Placeholder 1)(Placeholder 2)))(S(Parenthesized(OpSeq(BinOp NotInHole Arrow(Placeholder 0)(Placeholder 1))(S String(A Arrow(S Int E)))))(A Arrow(S(List(OpSeq(Placeholder 0)(S String E)))(A Arrow(S(List(OpSeq(Placeholder 0)(S Int E)))E)))))))E))((ExpLine(OpSeq(Placeholder 0)(S(Lam NotInHole(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole f)E))((ExpLine(OpSeq(Placeholder 0)(S(Lam NotInHole(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole xs)E))((ExpLine(OpSeq(Placeholder 0)(S(Case(StandardErrStatus NotInHole)((ExpLine(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole xs)E))))((Rule(OpSeq(Placeholder 0)(S(ListNil NotInHole)E))((ExpLine(OpSeq(Placeholder 0)(S(ListNil NotInHole)E)))))(Rule(OpSeq(BinOp NotInHole Cons(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole y)(A Cons(S(Var NotInHole NotInVarHole ys)E))))((ExpLine(OpSeq(BinOp NotInHole Cons(Placeholder 0)(Placeholder 1))(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole f)(A Space(S(Var NotInHole NotInVarHole y)E)))))))(A Cons(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Space(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(Placeholder 2))(S(Var NotInHole NotInVarHole map)(A Space(S(Var NotInHole NotInVarHole f)(A Space(S(Var NotInHole NotInVarHole ys)E)))))))))E)))))))))E)))))E)))))E)))))EmptyLine(AbbrevLine $percentage NotInAbbrevHole $slider((IntLit NotInHole 0)(IntLit NotInHole 100)))EmptyLine(LetLine(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole classic_look)E))((ExpLine(OpSeq(Placeholder 0)(S(Lam NotInHole(OpSeq(Placeholder 0)(S(TypeAnn NotInHole(Var NotInHole NotInVarHole url)(OpSeq(Placeholder 0)(S String E)))E))((ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(ApLivelit 0 NotInHole $basic_adjustments $basic_adjustments((brightness 0)(grayscale 1))((next 2)(splice_map((0(Int((ExpLine(OpSeq(Placeholder 0)(S(ApLivelit 2 NotInHole $slider $percentage(87)((next 0)(splice_map())(splice_order())))E))))))(1(Int((ExpLine(OpSeq(Placeholder 0)(S(ApLivelit 1 NotInHole $slider $percentage(73)((next 0)(splice_map())(splice_order())))E))))))))(splice_order(0 1))))(A Space(S(Var NotInHole NotInVarHole url)E)))))))E)))))(ExpLine(OpSeq(BinOp NotInHole Space(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(Placeholder 2))(S(Var NotInHole NotInVarHole map)(A Space(S(Var NotInHole NotInVarHole classic_look)(A Space(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Cons(Placeholder 0)(BinOp NotInHole Cons(Placeholder 1)(Placeholder 2)))(S(StringLit NotInHole http://tinyurl.com/y8neczz3)(A Cons(S(StringLit NotInHole http://tinyurl.com/yd2dw4ww)(A Cons(S(ListNil NotInHole)E)))))))))E))))))))"
  |> Sexplib.Sexp.of_string
  |> UHExp.t_of_sexp;

let grade_cutoffs =
  "((LetLine(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole q1_max)E))((ExpLine(OpSeq(Placeholder 0)(S(FloatLit NotInHole 36.)E)))))(LetLine(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole grades)E))((ExpLine(OpSeq(Placeholder 0)(S(ApLivelit 0 NotInHole $data_frame $data_frame((selected 0)(col_headers(18 19 20 21 22))(rows(((header 5)(cells(0 1 2 3 4)))((header 11)(cells(6 7 8 9 10)))((header 17)(cells(12 13 14 15 16)))((header 28)(cells(23 24 25 26 27))))))((next 29)(splice_map((0(Float((ExpLine(OpSeq(BinOp NotInHole FPlus(BinOp NotInHole FPlus(Placeholder 0)(Placeholder 1))(BinOp NotInHole Space(BinOp NotInHole Space(Placeholder 2)(Placeholder 3))(Placeholder 4)))(S(FloatLit NotInHole 24.)(A FPlus(S(Var NotInHole NotInVarHole q1_max)(A FPlus(S(ApLivelit 1 NotInHole $fslider $fslider(20)((next 0)(splice_map())(splice_order())))(A Space(S(FloatLit NotInHole 0.)(A Space(S(FloatLit NotInHole 40.)E))))))))))))))(1(Float((ExpLine(OpSeq(Placeholder 0)(S(FloatLit NotInHole 92.)E))))))(2(Float((ExpLine(OpSeq(Placeholder 0)(S(FloatLit NotInHole 83.5)E))))))(3(Float((ExpLine(OpSeq(Placeholder 0)(S(FloatLit NotInHole 95.)E))))))(4(Float((ExpLine(OpSeq(Placeholder 0)(S(FloatLit NotInHole 88.)E))))))(5(String((ExpLine(OpSeq(Placeholder 0)(S(StringLit NotInHole Alice)E))))))(6(Float((ExpLine(OpSeq(Placeholder 0)(S(FloatLit NotInHole 61.)E))))))(7(Float((ExpLine(OpSeq(Placeholder 0)(S(FloatLit NotInHole 64.)E))))))(8(Float((ExpLine(OpSeq(Placeholder 0)(S(FloatLit NotInHole 98.)E))))))(9(Float((ExpLine(OpSeq(Placeholder 0)(S(FloatLit NotInHole 70.)E))))))(10(Float((ExpLine(OpSeq(Placeholder 0)(S(FloatLit NotInHole 85.)E))))))(11(String((ExpLine(OpSeq(Placeholder 0)(S(StringLit NotInHole Bob)E))))))(12(Float((ExpLine(OpSeq(Placeholder 0)(S(FloatLit NotInHole 75.)E))))))(13(Float((ExpLine(OpSeq(Placeholder 0)(S(FloatLit NotInHole 81.)E))))))(14(Float((ExpLine(OpSeq(Placeholder 0)(S(FloatLit NotInHole 73.)E))))))(15(Float((ExpLine(OpSeq(Placeholder 0)(S(FloatLit NotInHole 82.)E))))))(16(Float((ExpLine(OpSeq(Placeholder 0)(S(FloatLit NotInHole 79.)E))))))(17(String((ExpLine(OpSeq(Placeholder 0)(S(StringLit NotInHole Carol)E))))))(18(String((ExpLine(OpSeq(Placeholder 0)(S(StringLit NotInHole A1)E))))))(19(String((ExpLine(OpSeq(Placeholder 0)(S(StringLit NotInHole A2)E))))))(20(String((ExpLine(OpSeq(Placeholder 0)(S(StringLit NotInHole A3)E))))))(21(String((ExpLine(OpSeq(Placeholder 0)(S(StringLit NotInHole Midterm)E))))))(22(String((ExpLine(OpSeq(Placeholder 0)(S(StringLit NotInHole Final)E))))))(23(Float((ExpLine(OpSeq(Placeholder 0)(S(FloatLit NotInHole 0.)E))))))(24(Float((ExpLine(OpSeq(Placeholder 0)(S(FloatLit NotInHole 100.)E))))))(25(Float((ExpLine(OpSeq(Placeholder 0)(S(FloatLit NotInHole 72.)E))))))(26(Float((ExpLine(OpSeq(Placeholder 0)(S(FloatLit NotInHole 81.)E))))))(27(Float((ExpLine(OpSeq(Placeholder 0)(S(FloatLit NotInHole 83.)E))))))(28(String((ExpLine(OpSeq(Placeholder 0)(S(StringLit NotInHole David)E))))))))(splice_order(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28))))E)))))(LetLine(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole weights)E))((ExpLine(OpSeq(BinOp NotInHole Cons(Placeholder 0)(BinOp NotInHole Cons(Placeholder 1)(BinOp NotInHole Cons(Placeholder 2)(BinOp NotInHole Cons(Placeholder 3)(BinOp NotInHole Cons(Placeholder 4)(Placeholder 5))))))(S(FloatLit NotInHole 0.15)(A Cons(S(FloatLit NotInHole 0.15)(A Cons(S(FloatLit NotInHole 0.2)(A Cons(S(FloatLit NotInHole 0.25)(A Cons(S(FloatLit NotInHole 0.25)(A Cons(S(ListNil NotInHole)E)))))))))))))))(LetLine(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole averages)E))((ExpLine(OpSeq(BinOp NotInHole Space(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(Placeholder 2))(S(Var NotInHole NotInVarHole compute_weighted_averages)(A Space(S(Var NotInHole NotInVarHole grades)(A Space(S(Var NotInHole NotInVarHole weights)E)))))))))(CommentLine\"hover over points to see student names\")(LetLine(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole cutoffs)E))((ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(ApLivelit 2 NotInHole $grade_cutoffs $grade_cutoffs((a 85.65625)(b 75.45625)(c 66.25625)(d 47.85625)(selecting()))((next 0)(splice_map())(splice_order())))(A Space(S(Var NotInHole NotInVarHole averages)E)))))))(ExpLine(OpSeq(BinOp NotInHole Space(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(Placeholder 2))(S(Var NotInHole NotInVarHole assign_grades)(A Space(S(Var NotInHole NotInVarHole averages)(A Space(S(Var NotInHole NotInVarHole cutoffs)E))))))))"
  |> Sexplib.Sexp.of_string
  |> UHExp.t_of_sexp;

let ll_def_slider_ill_typed_expansion =
  "((LivelitDefLine((name(NotInVarHole $slidy))(expansion_type(OpSeq(Placeholder 0)(S Int E)))(captures((ExpLine(OpSeq(Placeholder 0)(S(IntLit NotInHole 420)E)))))(model_type(OpSeq(BinOp NotInHole Prod(Placeholder 0)(Placeholder 1))(S Int(A Prod(S Int E)))))(action_type(OpSeq(Placeholder 0)(S Int E)))(init((ExpLine(OpSeq(Placeholder 0)(S(Inj NotInHole R((ExpLine(OpSeq(Placeholder 0)(S(Inj NotInHole L((ExpLine(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(StringLit NotInHole Int)(A Comma(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(StringLit NotInHole\"((ExpLine(OpSeq(Placeholder 0) (S(IntLit NotInHole 100) E))))\")(A Comma(S(Lam NotInHole(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole spliceno)E))((ExpLine(OpSeq(Placeholder 0)(S(Inj NotInHole L((ExpLine(OpSeq(Placeholder 0)(S(Inj NotInHole L((ExpLine(OpSeq(Placeholder 0)(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole spliceno)(A Comma(S(IntLit NotInHole 50)E)))))))E)))))E)))))E)))))E)))))))E)))))))E)))))E)))))(update((ExpLine(OpSeq(Placeholder 0)(S(Lam NotInHole(OpSeq(Placeholder 0)(S(TypeAnn NotInHole(Parenthesized(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(Parenthesized(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole spliceno)(A Comma(S(Wild NotInHole)E)))))(A Comma(S(Var NotInHole NotInVarHole action)E)))))(OpSeq(Placeholder 0)(S(Parenthesized(OpSeq(BinOp NotInHole Prod(Placeholder 0)(Placeholder 1))(S(Parenthesized(OpSeq(BinOp NotInHole Prod(Placeholder 0)(Placeholder 1))(S Int(A Prod(S Int E)))))(A Prod(S Int E)))))E)))E))((ExpLine(OpSeq(Placeholder 0)(S(Inj NotInHole L((ExpLine(OpSeq(Placeholder 0)(S(Inj NotInHole L((ExpLine(OpSeq(Placeholder 0)(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole spliceno)(A Comma(S(Var NotInHole NotInVarHole action)E)))))))E)))))E)))))E)))))E)))))(view((ExpLine(OpSeq(Placeholder 0)(S(Lam NotInHole(OpSeq(Placeholder 0)(S(TypeAnn NotInHole(Parenthesized(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole llu)(A Comma(S(Parenthesized(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(Wild NotInHole)(A Comma(S(Var NotInHole NotInVarHole val)E)))))E)))))(OpSeq(Placeholder 0)(S(Parenthesized(OpSeq(BinOp NotInHole Prod(Placeholder 0)(Placeholder 1))(S Int(A Prod(S(Parenthesized(OpSeq(BinOp NotInHole Prod(Placeholder 0)(Placeholder 1))(S Int(A Prod(S Int E)))))E)))))E)))E))((ExpLine(OpSeq(BinOp NotInHole Caret(BinOp NotInHole Caret(BinOp NotInHole Caret(BinOp NotInHole Caret(Placeholder 0)(Placeholder 1))(Placeholder 2))(Placeholder 3))(Placeholder 4))(S(StringLit NotInHole\"\n     <span class=\\\"slider-livelit\\\">\n       <input\n         class=\\\"slider\\\"\n         id=\\\"llu\\\"\n         type=\\\"range\\\"\n         min=0\n         max=100\n         oninput=\\\"(function(e) {\n             let action = '(IntLit ' + document.getElementById('llu').value + ')';\n             window.trigger(\")(A Caret(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole string_of_int)(A Space(S(Var NotInHole NotInVarHole llu)E)))))))(A Caret(S(StringLit NotInHole\", action);\n             return false;}) ();\n             return false;\\\"\n         value=\")(A Caret(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole string_of_int)(A Space(S(Var NotInHole NotInVarHole val)E)))))))(A Caret(S(StringLit NotInHole\">\n      </input>\n    </span>\")E)))))))))))))E)))))(shape((ExpLine(OpSeq(Placeholder 0)(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(BoolLit NotInHole true)(A Comma(S(IntLit NotInHole 14)E)))))))E)))))(expand((ExpLine(OpSeq(Placeholder 0)(S(Lam NotInHole(OpSeq(Placeholder 0)(S(TypeAnn NotInHole(Parenthesized(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(Wild NotInHole)(A Comma(S(Var NotInHole NotInVarHole val)E)))))(OpSeq(BinOp NotInHole Prod(Placeholder 0)(Placeholder 1))(S(Parenthesized(OpSeq(Placeholder 0)(S Int E)))(A Prod(S(Parenthesized(OpSeq(Placeholder 0)(S Int E)))E)))))E))((ExpLine(OpSeq(BinOp NotInHole Caret(BinOp NotInHole Caret(Placeholder 0)(Placeholder 1))(Placeholder 2))(S(StringLit NotInHole\"(Lam NotInHole\n(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole x)E))\n((ExpLine(OpSeq(Placeholder 0)(S(BoolLit NotInHole \")(A Caret(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole string_of_bool)(A Space(S(BoolLit NotInHole true)E)))))))(A Caret(S(StringLit NotInHole\")E)))))\")E)))))))))E)))))))(ExpLine(OpSeq(Placeholder 0)(S(FreeLivelit 2 $slidy)E))))"
  |> Sexplib.Sexp.of_string
  |> UHExp.t_of_sexp;

let ll_def_slider_basic =
  "((LivelitDefLine((name(NotInVarHole $slidy))(expansion_type(OpSeq(Placeholder 0)(S Int E)))(captures((ExpLine(OpSeq(Placeholder 0)(S(IntLit NotInHole 420)E)))))(model_type(OpSeq(BinOp NotInHole Prod(Placeholder 0)(Placeholder 1))(S Int(A Prod(S Int E)))))(action_type(OpSeq(Placeholder 0)(S Int E)))(init((ExpLine(OpSeq(Placeholder 0)(S(Inj NotInHole R((ExpLine(OpSeq(Placeholder 0)(S(Inj NotInHole L((ExpLine(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(StringLit NotInHole Int)(A Comma(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(StringLit NotInHole\"((ExpLine(OpSeq(Placeholder 0) (S(IntLit NotInHole 100) E))))\")(A Comma(S(Lam NotInHole(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole spliceno)E))((ExpLine(OpSeq(Placeholder 0)(S(Inj NotInHole L((ExpLine(OpSeq(Placeholder 0)(S(Inj NotInHole L((ExpLine(OpSeq(Placeholder 0)(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole spliceno)(A Comma(S(IntLit NotInHole 50)E)))))))E)))))E)))))E)))))E)))))))E)))))))E)))))E)))))(update((ExpLine(OpSeq(Placeholder 0)(S(Lam NotInHole(OpSeq(Placeholder 0)(S(TypeAnn NotInHole(Parenthesized(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(Parenthesized(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole spliceno)(A Comma(S(Wild NotInHole)E)))))(A Comma(S(Var NotInHole NotInVarHole action)E)))))(OpSeq(Placeholder 0)(S(Parenthesized(OpSeq(BinOp NotInHole Prod(Placeholder 0)(Placeholder 1))(S(Parenthesized(OpSeq(BinOp NotInHole Prod(Placeholder 0)(Placeholder 1))(S Int(A Prod(S Int E)))))(A Prod(S Int E)))))E)))E))((ExpLine(OpSeq(Placeholder 0)(S(Inj NotInHole L((ExpLine(OpSeq(Placeholder 0)(S(Inj NotInHole L((ExpLine(OpSeq(Placeholder 0)(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole spliceno)(A Comma(S(Var NotInHole NotInVarHole action)E)))))))E)))))E)))))E)))))E)))))
  (view((ExpLine(OpSeq(Placeholder 0)(S(Lam NotInHole(OpSeq(Placeholder 0)(S(TypeAnn NotInHole(Parenthesized(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole llu)(A Comma(S(Parenthesized(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(Wild NotInHole)(A Comma(S(Var NotInHole NotInVarHole val)E)))))E)))))(OpSeq(Placeholder 0)(S(Parenthesized(OpSeq(BinOp NotInHole Prod(Placeholder 0)(Placeholder 1))(S Int(A Prod(S(Parenthesized(OpSeq(BinOp NotInHole Prod(Placeholder 0)(Placeholder 1))(S Int(A Prod(S Int E)))))E)))))E)))E))
  ((LetLine(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole llu_string)E))((ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole string_of_int)(A Space(S(Var NotInHole NotInVarHole llu)E)))))))(LetLine(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole id)E))((ExpLine(OpSeq(BinOp NotInHole Caret(Placeholder 0)(Placeholder 1))(S(StringLit NotInHole slidy-input-)(A Caret(S(Var NotInHole NotInVarHole llu_string)E)))))))(LetLine(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole oninput)E))((ExpLine(OpSeq(BinOp NotInHole Caret(BinOp NotInHole Caret(Placeholder 0)(Placeholder 1))(Placeholder 2))(S(StringLit NotInHole\"(function(e) {let action = '(IntLit ' + document.getElementById('\")(A Caret(S(Var NotInHole NotInVarHole id)(A Caret(S(StringLit NotInHole\"').value + ')';\")E)))))))))(LetLine(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole oninput)E))((ExpLine(OpSeq(BinOp NotInHole Caret(BinOp NotInHole Caret(BinOp NotInHole Caret(Placeholder 0)(Placeholder 1))(Placeholder 2))(Placeholder 3))(S(Var NotInHole NotInVarHole oninput)(A Caret(S(StringLit NotInHole\"window.trigger('\")(A Caret(S(Var NotInHole NotInVarHole llu_string)(A Caret(S(StringLit NotInHole\"', action); return false;})(); return false;\")E)))))))))))(LetLine(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole attrs)E))((ExpLine(OpSeq(BinOp NotInHole Cons(Placeholder 0)(Placeholder 1))(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(StringLit NotInHole oninput)(A Comma(S(Var NotInHole NotInVarHole oninput)E)))))))(A Cons(S(ListNil NotInHole)E)))))))(LetLine(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole attrs)E))((ExpLine(OpSeq(BinOp NotInHole Cons(Placeholder 0)(Placeholder 1))(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Comma(Placeholder 0)(BinOp NotInHole Space(Placeholder 1)(Placeholder 2)))(S(StringLit NotInHole value)(A Comma(S(Var NotInHole NotInVarHole string_of_int)(A Space(S(Var NotInHole NotInVarHole val)E)))))))))(A Cons(S(Var NotInHole NotInVarHole attrs)E)))))))(LetLine(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole attrs)E))((ExpLine(OpSeq(BinOp NotInHole Cons(Placeholder 0)(Placeholder 1))(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(StringLit NotInHole class)(A Comma(S(StringLit NotInHole slider)E)))))))(A Cons(S(Var NotInHole NotInVarHole attrs)E)))))))(LetLine(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole attrs)E))((ExpLine(OpSeq(BinOp NotInHole Cons(Placeholder 0)(Placeholder 1))(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(StringLit NotInHole id)(A Comma(S(Var NotInHole NotInVarHole id)E)))))))(A Cons(S(Var NotInHole NotInVarHole attrs)E)))))))(LetLine(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole attrs)E))((ExpLine(OpSeq(BinOp NotInHole Cons(Placeholder 0)(Placeholder 1))(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(StringLit NotInHole type)(A Comma(S(StringLit NotInHole range)E)))))))(A Cons(S(Var NotInHole NotInVarHole attrs)E)))))))(LetLine(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole attrs)E))((ExpLine(OpSeq(BinOp NotInHole Cons(Placeholder 0)(Placeholder 1))(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(StringLit NotInHole min)(A Comma(S(StringLit NotInHole 0)E)))))))(A Cons(S(Var NotInHole NotInVarHole attrs)E)))))))(LetLine(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole attrs)E))((ExpLine(OpSeq(BinOp NotInHole Cons(Placeholder 0)(Placeholder 1))(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(StringLit NotInHole max)(A Comma(S(StringLit NotInHole 100)E)))))))(A Cons(S(Var NotInHole NotInVarHole attrs)E)))))))(LetLine(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole input)E))((ExpLine(OpSeq(Placeholder 0)(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Comma(Placeholder 0)(BinOp NotInHole Comma(Placeholder 1)(Placeholder 2)))(S(StringLit NotInHole input)(A Comma(S(Var NotInHole NotInVarHole attrs)(A Comma(S(ListNil NotInHole)E)))))))))E)))))(LetLine(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole span_attrs)E))((ExpLine(OpSeq(BinOp NotInHole Cons(Placeholder 0)(Placeholder 1))(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(StringLit NotInHole class)(A Comma(S(StringLit NotInHole slider-livelit)E)))))))(A Cons(S(ListNil NotInHole)E)))))))(ExpLine(OpSeq(Placeholder 0)(S(Parenthesized((ExpLine(OpSeq(BinOp
  NotInHole Comma(Placeholder 0)(BinOp NotInHole Comma(Placeholder 1)(BinOp NotInHole Cons(Placeholder 2)(Placeholder 3))))(S(StringLit NotInHole span)(A Comma(S(Var NotInHole NotInVarHole span_attrs)(A Comma(S(Var NotInHole NotInVarHole input)(A Cons(S(ListNil NotInHole)E)))))))))))E))))
  )E)))))(shape((ExpLine(OpSeq(Placeholder 0)(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(BoolLit NotInHole true)(A Comma(S(IntLit NotInHole 14)E)))))))E)))))(expand((ExpLine(OpSeq(Placeholder 0)(S(Lam NotInHole(OpSeq(Placeholder 0)(S(TypeAnn NotInHole(Parenthesized(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(Wild NotInHole)(A Comma(S(Var NotInHole NotInVarHole val)E)))))(OpSeq(BinOp NotInHole Prod(Placeholder 0)(Placeholder 1))(S(Parenthesized(OpSeq(Placeholder 0)(S Int E)))(A Prod(S(Parenthesized(OpSeq(Placeholder 0)(S Int E)))E)))))E))((ExpLine(OpSeq(BinOp NotInHole Caret(BinOp NotInHole Caret(Placeholder 0)(Placeholder 1))(Placeholder 2))(S(StringLit NotInHole\"(Lam NotInHole\n(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole x)E))\n((ExpLine(OpSeq(Placeholder 0)(S(IntLit NotInHole \")(A Caret(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole string_of_int)(A Space(S(Var NotInHole NotInVarHole val)E)))))))(A Caret(S(StringLit NotInHole\")E)))))\")E)))))))))E)))))))(ExpLine(OpSeq(Placeholder 0)(S(FreeLivelit 2 $slidy)E))))
  "
  |> Sexplib.Sexp.of_string
  |> UHExp.t_of_sexp;

let ll_def_slider_free_var_in_exp =
  "((LivelitDefLine((name(NotInVarHole $slidy))(expansion_type(OpSeq(Placeholder 0)(S Int E)))(captures((ExpLine(OpSeq(Placeholder 0)(S(IntLit NotInHole 420)E)))))(model_type(OpSeq(BinOp NotInHole Prod(Placeholder 0)(Placeholder 1))(S Int(A Prod(S Int E)))))(action_type(OpSeq(Placeholder 0)(S Int E)))(init((ExpLine(OpSeq(Placeholder 0)(S(Inj NotInHole R((ExpLine(OpSeq(Placeholder 0)(S(Inj NotInHole L((ExpLine(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(StringLit NotInHole Int)(A Comma(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(StringLit NotInHole\"((ExpLine(OpSeq(Placeholder 0) (S(IntLit NotInHole 100) E))))\")(A Comma(S(Lam NotInHole(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole spliceno)E))((ExpLine(OpSeq(Placeholder 0)(S(Inj NotInHole L((ExpLine(OpSeq(Placeholder 0)(S(Inj NotInHole L((ExpLine(OpSeq(Placeholder 0)(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole spliceno)(A Comma(S(IntLit NotInHole 50)E)))))))E)))))E)))))E)))))E)))))))E)))))))E)))))E)))))(update((ExpLine(OpSeq(Placeholder 0)(S(Lam NotInHole(OpSeq(Placeholder 0)(S(TypeAnn NotInHole(Parenthesized(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(Parenthesized(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole spliceno)(A Comma(S(Wild NotInHole)E)))))(A Comma(S(Var NotInHole NotInVarHole action)E)))))(OpSeq(Placeholder 0)(S(Parenthesized(OpSeq(BinOp NotInHole Prod(Placeholder 0)(Placeholder 1))(S(Parenthesized(OpSeq(BinOp NotInHole Prod(Placeholder 0)(Placeholder 1))(S Int(A Prod(S Int E)))))(A Prod(S Int E)))))E)))E))((ExpLine(OpSeq(Placeholder 0)(S(Inj NotInHole L((ExpLine(OpSeq(Placeholder 0)(S(Inj NotInHole L((ExpLine(OpSeq(Placeholder 0)(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole spliceno)(A Comma(S(Var NotInHole NotInVarHole action)E)))))))E)))))E)))))E)))))E)))))(view((ExpLine(OpSeq(Placeholder 0)(S(Lam NotInHole(OpSeq(Placeholder 0)(S(TypeAnn NotInHole(Parenthesized(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole llu)(A Comma(S(Parenthesized(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(Wild NotInHole)(A Comma(S(Var NotInHole NotInVarHole val)E)))))E)))))(OpSeq(Placeholder 0)(S(Parenthesized(OpSeq(BinOp NotInHole Prod(Placeholder 0)(Placeholder 1))(S Int(A Prod(S(Parenthesized(OpSeq(BinOp NotInHole Prod(Placeholder 0)(Placeholder 1))(S Int(A Prod(S Int E)))))E)))))E)))E))((ExpLine(OpSeq(BinOp NotInHole Caret(BinOp NotInHole Caret(BinOp NotInHole Caret(BinOp NotInHole Caret(Placeholder 0)(Placeholder 1))(Placeholder 2))(Placeholder 3))(Placeholder 4))(S(StringLit NotInHole\"\n     <span class=\\\"slider-livelit\\\">\n       <input\n         class=\\\"slider\\\"\n         id=\\\"llu\\\"\n         type=\\\"range\\\"\n         min=0\n         max=100\n         oninput=\\\"(function(e) {\n             let action = '(IntLit ' + document.getElementById('llu').value + ')';\n             window.trigger(\")(A Caret(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole string_of_int)(A Space(S(Var NotInHole NotInVarHole llu)E)))))))(A Caret(S(StringLit NotInHole\", action);\n             return false;}) ();\n             return false;\\\"\n         value=\")(A Caret(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole string_of_int)(A Space(S(Var NotInHole NotInVarHole val)E)))))))(A Caret(S(StringLit NotInHole\">\n      </input>\n    </span>\")E)))))))))))))E)))))(shape((ExpLine(OpSeq(Placeholder 0)(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(BoolLit NotInHole true)(A Comma(S(IntLit NotInHole 14)E)))))))E)))))
  (expand((ExpLine(OpSeq(Placeholder 0)(S(Lam NotInHole(OpSeq(Placeholder 0)(S(TypeAnn NotInHole(Parenthesized(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(Wild NotInHole)(A Comma(S(Var NotInHole NotInVarHole val)E)))))(OpSeq(BinOp NotInHole Prod(Placeholder 0)(Placeholder 1))(S(Parenthesized(OpSeq(Placeholder 0)(S Int E)))(A Prod(S(Parenthesized(OpSeq(Placeholder 0)(S Int E)))E)))))E))
  ((ExpLine(OpSeq(Placeholder 0)(S(StringLit NotInHole\"(Lam NotInHole\n(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole x)E))\n((ExpLine(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole free)E)))))\")E)))))E)))))))(ExpLine(OpSeq(Placeholder 0)(S(ApLivelit 1 NotInHole $slidy $slidy(IntLit 50)((next 0)(splice_map())(splice_order())))E))))"
  |> Sexplib.Sexp.of_string
  |> UHExp.t_of_sexp;

let ll_def_slider_unique_id =
  "((LivelitDefLine((name(NotInVarHole $slidy))(expansion_type(OpSeq(Placeholder 0)(S Int E)))(captures((ExpLine(OpSeq(Placeholder 0)(S(IntLit NotInHole 420)E)))))(model_type(OpSeq(Placeholder 0)(S Int E)))(action_type(OpSeq(Placeholder 0)(S Int E)))(init((ExpLine(OpSeq(Placeholder 0)(S(IntLit NotInHole 50)E)))))(update((ExpLine(OpSeq(Placeholder 0)(S(Lam NotInHole(OpSeq(Placeholder 0)(S(Parenthesized(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole model)(A Comma(S(Var NotInHole NotInVarHole action)E)))))E))((ExpLine(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole action)E)))))E)))))(view((ExpLine(OpSeq(Placeholder 0)(S(Lam NotInHole(OpSeq(Placeholder 0)(S(Parenthesized(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole llu)(A Comma(S(Var NotInHole NotInVarHole model)E)))))E))((ExpLine(OpSeq(BinOp NotInHole Caret(BinOp NotInHole Caret(BinOp NotInHole Caret(BinOp NotInHole Caret(BinOp NotInHole Caret(BinOp NotInHole Caret(BinOp NotInHole Caret(BinOp NotInHole Caret(Placeholder 0)(Placeholder 1))(Placeholder 2))(Placeholder 3))(Placeholder 4))(Placeholder 5))(Placeholder 6))(Placeholder 7))(Placeholder 8))(S(StringLit NotInHole\"\n     <span class=slider-livelit\n       <input\n         class=slider\n         id=\")(A Caret(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole string_of_int)(A Space(S(Var NotInHole NotInVarHole llu)E)))))))(A Caret(S(StringLit NotInHole\"\n         type=range\n         min=0\n         max=100\n         oninput=\\\"(function(e) {\n             let action = '(IntLit ' + document.getElementById('\")(A Caret(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole string_of_int)(A Space(S(Var NotInHole NotInVarHole llu)E)))))))(A Caret(S(StringLit NotInHole\"').value + ')';\n             window.trigger(\")(A Caret(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole string_of_int)(A Space(S(Var NotInHole NotInVarHole llu)E)))))))(A Caret(S(StringLit NotInHole\", action);\n             return false;}) ();\n             return false;\\\"\n         value=\")(A Caret(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole string_of_int)(A Space(S(Var NotInHole NotInVarHole model)E)))))))(A Caret(S(StringLit NotInHole\">\n      </input>\n    </span>\")E)))))))))))))))))))))E)))))(shape((ExpLine(OpSeq(Placeholder 0)(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(BoolLit NotInHole true)(A Comma(S(IntLit NotInHole 14)E)))))))E)))))(expand((ExpLine(OpSeq(Placeholder 0)(S(Lam NotInHole(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole m)E))((ExpLine(OpSeq(BinOp NotInHole Caret(BinOp NotInHole Caret(Placeholder 0)(Placeholder 1))(Placeholder 2))(S(StringLit NotInHole\"((ExpLine(OpSeq(Placeholder 0)(S(IntLit NotInHole \")(A Caret(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole string_of_int)(A Space(S(Var NotInHole NotInVarHole m)E)))))))(A Caret(S(StringLit NotInHole\")E))))\")E)))))))))E)))))))(ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 6)E))))"
  |> Sexplib.Sexp.of_string
  |> UHExp.t_of_sexp;

[@deriving sexp]
type id = string;
let examples =
  StringMap.(
    empty
    |> add("just_hole", just_hole)
    |> add("holey_lambda", holey_lambda)
    |> add("let_line", let_line)
    |> add("map_example", map_example)
    |> add("qsort_example", qsort_example)
    |> add("map_img_filter", map_img_filter)
    |> add("color", color)
    |> add("grade_cutoffs", grade_cutoffs)
    |> add("ll_def_slider_basic", ll_def_slider_basic)
    |> add(
         "ll_def_slider_ill_typed_expansion",
         ll_def_slider_ill_typed_expansion,
       )
    |> add("ll_def_slider_free_var_in_exp", ll_def_slider_free_var_in_exp)
    |> add("ll_def_slider_unique_id", ll_def_slider_unique_id)
  );
let get = id => StringMap.find(id, examples);

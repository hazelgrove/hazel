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

let rec qsort_n = (n: int): UHExp.t =>
  if (n == 0) {
    [];
  } else {
    [
      UHExp.letline(
        OpSeq.wrap(UHPat.var("qsort" ++ Int.to_string(n))),
        qsort_example,
      ),
      ...qsort_n(n - 1),
    ];
  };

let assistant_test' =
  {|((LetLine(OpSeq(Placeholder 0)(S(Wild NotInHole)E))((ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 289)E)))))(LetLine(OpSeq(Placeholder 0)(S(TypeAnn NotInHole(Var NotInHole NotInVarHole isBuddo)(OpSeq(Placeholder 0)(S Bool E)))E))((ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 1)E)))))(LetLine(OpSeq(Placeholder 0)(S(TypeAnn NotInHole(Var NotInHole NotInVarHole poggleHeft)(OpSeq(Placeholder 0)(S Float E)))E))((ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 2)E)))))(LetLine(OpSeq(Placeholder 0)(S(TypeAnn NotInHole(Var NotInHole NotInVarHole numFleens)(OpSeq(Placeholder 0)(S Int E)))E))((ExpLine(OpSeq(Placeholder 0)(S(IntLit NotInHole 6)E)))))(LetLine(OpSeq(Placeholder 0)(S(TypeAnn NotInHole(Var NotInHole NotInVarHole flarfus)(OpSeq(Placeholder 0)(S(Parenthesized(OpSeq(BinOp NotInHole Prod(Placeholder 0)(Placeholder 1))(S Bool(A Prod(S Bool E)))))E)))E))((ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 3)E)))))(LetLine(OpSeq(Placeholder 0)(S(TypeAnn NotInHole(Var NotInHole NotInVarHole bargle)(OpSeq(Placeholder 0)(S(Parenthesized(OpSeq(BinOp NotInHole Sum(Placeholder 0)(Placeholder 1))(S Bool(A Sum(S Float E)))))E)))E))((ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 84)E)))))(LetLine(OpSeq(Placeholder 0)(S(TypeAnn NotInHole(Var NotInHole NotInVarHole toogles)(OpSeq(Placeholder 0)(S(List(OpSeq(Placeholder 0)(S Int E)))E)))E))((ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 4)E)))))(LetLine(OpSeq(Placeholder 0)(S(TypeAnn NotInHole(Var NotInHole NotInVarHole fleem)(OpSeq(BinOp NotInHole Arrow(Placeholder 0)(Placeholder 1))(S Hole(A Arrow(S Int E)))))E))((ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 7)E)))))(LetLine(OpSeq(Placeholder 0)(S(TypeAnn NotInHole(Var NotInHole NotInVarHole blom)(OpSeq(BinOp NotInHole Arrow(Placeholder 0)(Placeholder 1))(S Float(A Arrow(S Int E)))))E))((ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 8)E)))))(LetLine(OpSeq(Placeholder 0)(S(TypeAnn NotInHole(Var NotInHole NotInVarHole grackle)(OpSeq(BinOp NotInHole Arrow(Placeholder 0)(BinOp NotInHole Arrow(Placeholder 1)(Placeholder 2)))(S Float(A Arrow(S(List(OpSeq(Placeholder 0)(S Bool E)))(A Arrow(S Int E)))))))E))((ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 10)E)))))(LetLine(OpSeq(Placeholder 0)(S(TypeAnn NotInHole(Var NotInHole NotInVarHole int2Float)(OpSeq(BinOp NotInHole Arrow(Placeholder 0)(Placeholder 1))(S Int(A Arrow(S Float E)))))E))((ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 5)E)))))(LetLine(OpSeq(Placeholder 0)(S(TypeAnn NotInHole(Var NotInHole NotInVarHole float2Int)(OpSeq(BinOp NotInHole Arrow(Placeholder 0)(Placeholder 1))(S Float(A Arrow(S Int E)))))E))((ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 6)E)))))(ExpLine(OpSeq(BinOp NotInHole FPlus(Placeholder 0)(Placeholder 1))(S(Parenthesized((ExpLine(OpSeq(BinOp(InHole TypeInconsistent 466)Plus(Placeholder 0)(Placeholder 1))(S(IntLit NotInHole 111)(A Plus(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(Var NotInHole(InVarHole Free 405)floatToInt)(A Space(S(IntLit NotInHole 222)E)))))))E)))))))(A FPlus(S(Var(InHole TypeInconsistent 467)NotInVarHole numFleens)E)))))(ExpLine(OpSeq(BinOp NotInHole Times(Placeholder 0)(Placeholder 1))(S(IntLit NotInHole 111)(A Times(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole fleem)(A Space(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole blom)(A Space(S(IntLit(InHole TypeInconsistent 468)4)E)))))))E)))))))E)))))(ExpLine(OpSeq(Placeholder 0)(S(Case(StandardErrStatus NotInHole)((ExpLine(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole bargle)(A Comma(S(Var NotInHole NotInVarHole numFleens)E))))))((Rule(OpSeq(Placeholder 0)(S(Parenthesized(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(EmptyHole 484)(A Comma(S(EmptyHole 485)E)))))E))((ExpLine(OpSeq(Placeholder 0)(S(BoolLit NotInHole true)E)))))(Rule(OpSeq(Placeholder 0)(S(EmptyHole 48)E))((ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 49)E)))))))E)))(ExpLine(OpSeq(BinOp NotInHole FPlus(Placeholder 0)(Placeholder 1))(S(IntLit(InHole TypeInconsistent 484)1)(A FPlus(S(Parenthesized((ExpLine(OpSeq(BinOp(InHole TypeInconsistent 486)Space(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole blom)(A Space(S(IntLit(InHole TypeInconsistent 485)4)E)))))))E)))))(ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 50)E))))|}
  |> Sexplib.Sexp.of_string
  |> UHExp.t_of_sexp;

let assistant_test_2 =
  {|((LetLine(OpSeq(Placeholder 0)(S(TypeAnn NotInHole(Parenthesized(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(IntLit NotInHole 4)(A Comma(S(Var NotInHole NotInVarHole foo)E)))))(OpSeq(Placeholder 0)(S Hole E)))E))((ExpLine(OpSeq(Placeholder 0)(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(EmptyHole 1225)(A Comma(S(BoolLit NotInHole true)E)))))))E)))))(LetLine(OpSeq(Placeholder 0)(S(TypeAnn NotInHole(Var NotInHole NotInVarHole isSpoon)(OpSeq(Placeholder 0)(S Bool E)))E))((ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 1)E)))))(LetLine(OpSeq(Placeholder 0)(S(TypeAnn NotInHole(Var NotInHole NotInVarHole numSpoons)(OpSeq(Placeholder 0)(S Int E)))E))((ExpLine(OpSeq(Placeholder 0)(S(IntLit NotInHole 6)E)))))(LetLine(OpSeq(Placeholder 0)(S(TypeAnn NotInHole(Var NotInHole NotInVarHole makeForks)(OpSeq(BinOp NotInHole Arrow(Placeholder 0)(Placeholder 1))(S Int(A Arrow(S Hole E)))))E))((ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 8)E)))))(LetLine(OpSeq(Placeholder 0)(S(TypeAnn NotInHole(Var NotInHole NotInVarHole toFloat)(OpSeq(BinOp NotInHole Arrow(Placeholder 0)(Placeholder 1))(S Int(A Arrow(S Float E)))))E))((ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 9)E)))))(LetLine(OpSeq(Placeholder 0)(S(TypeAnn NotInHole(Var NotInHole NotInVarHole toInt)(OpSeq(BinOp NotInHole Arrow(Placeholder 0)(Placeholder 1))(S Float(A Arrow(S Int E)))))E))((ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 10)E)))))(ExpLine(OpSeq(BinOp NotInHole FPlus(Placeholder 0)(Placeholder 1))(S(Parenthesized((ExpLine(OpSeq(BinOp(InHole TypeInconsistent 1261)Plus(Placeholder 0)(Placeholder 1))(S(IntLit NotInHole 111)(A Plus(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(Var NotInHole(InVarHole Free 405)floatToInt)(A Space(S(IntLit NotInHole 222)E)))))))E)))))))(A FPlus(S(Var(InHole TypeInconsistent 1262)NotInVarHole numSpoons)E)))))EmptyLine(ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 21)E))))|}
  |> Sexplib.Sexp.of_string
  |> UHExp.t_of_sexp;

let assistant_test =
  {|((LetLine(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole isOnline)E))((ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 1098)E)))))(LetLine(OpSeq(Placeholder 0)(S(TypeAnn NotInHole(Var NotInHole NotInVarHole playerData)(OpSeq(Placeholder 0)(S(Parenthesized(OpSeq(BinOp NotInHole Prod(Placeholder 0)(Placeholder 1))(S Bool(A Prod(S Int E)))))E)))E))((ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 981)E)))))(LetLine(OpSeq(Placeholder 0)(S(TypeAnn NotInHole(Var NotInHole NotInVarHole storeData)(OpSeq(Placeholder 0)(S Hole E)))E))((ExpLine(OpSeq(Placeholder 0)(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(FloatLit NotInHole 1.0)(A Comma(S(EmptyHole 2)E)))))))E)))))(LetLine(OpSeq(Placeholder 0)(S(TypeAnn NotInHole(Var NotInHole NotInVarHole numHP)(OpSeq(Placeholder 0)(S Int E)))E))((ExpLine(OpSeq(Placeholder 0)(S(IntLit NotInHole 6)E)))))(LetLine(OpSeq(Placeholder 0)(S(TypeAnn NotInHole(Var NotInHole NotInVarHole doSomething)(OpSeq(BinOp NotInHole Arrow(Placeholder 0)(Placeholder 1))(S Int(A Arrow(S Hole E)))))E))((ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 4)E)))))(LetLine(OpSeq(Placeholder 0)(S(TypeAnn NotInHole(Var NotInHole NotInVarHole round)(OpSeq(BinOp NotInHole Arrow(Placeholder 0)(Placeholder 1))(S Float(A Arrow(S Int E)))))E))((ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 5)E)))))(LetLine(OpSeq(Placeholder 0)(S(TypeAnn NotInHole(Var NotInHole NotInVarHole intToFloat)(OpSeq(BinOp NotInHole Arrow(Placeholder 0)(Placeholder 1))(S Int(A Arrow(S Float E)))))E))((ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 6)E)))))(LetLine(OpSeq(Placeholder 0)(S(TypeAnn NotInHole(Var NotInHole NotInVarHole floatToInt)(OpSeq(BinOp NotInHole Arrow(Placeholder 0)(Placeholder 1))(S Float(A Arrow(S Int E)))))E))((ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 7)E)))))(ExpLine(OpSeq(BinOp NotInHole FPlus(Placeholder 0)(BinOp NotInHole FTimes(Placeholder 1)(Placeholder 2)))(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(Var NotInHole(InVarHole Free 917)int2Float)(A Space(S(IntLit NotInHole 200)E)))))))(A FPlus(S(FloatLit NotInHole 20.)(A FTimes(S(Var(InHole TypeInconsistent 1120)NotInVarHole numHP)E)))))))(ExpLine(OpSeq(BinOp NotInHole FTimes(Placeholder 0)(Placeholder 1))(S(FloatLit NotInHole 100.)(A FTimes(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole intToFloat)(A Space(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole round)(A Space(S(IntLit(InHole TypeInconsistent 1121)4)E)))))))E)))))))E))))))|}
  //{|((LetLine(OpSeq(Placeholder 0)(S(Wild NotInHole)E))((ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 0)E)))))(LetLine(OpSeq(Placeholder 0)(S(TypeAnn NotInHole(Var NotInHole NotInVarHole isOnline)(OpSeq(Placeholder 0)(S Bool E)))E))((ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 668)E)))))(LetLine(OpSeq(Placeholder 0)(S(TypeAnn NotInHole(Var NotInHole NotInVarHole playerData)(OpSeq(Placeholder 0)(S(Parenthesized(OpSeq(BinOp NotInHole Prod(Placeholder 0)(Placeholder 1))(S Bool(A Prod(S Int E)))))E)))E))((ExpLine(OpSeq(Placeholder 0)(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(BoolLit NotInHole true)(A Comma(S(IntLit NotInHole 5)E)))))))E)))))(LetLine(OpSeq(Placeholder 0)(S(TypeAnn NotInHole(Var NotInHole NotInVarHole storeData)(OpSeq(Placeholder 0)(S Hole E)))E))((ExpLine(OpSeq(Placeholder 0)(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(FloatLit NotInHole 1.0)(A Comma(S(EmptyHole 221)E)))))))E)))))(LetLine(OpSeq(Placeholder 0)(S(TypeAnn NotInHole(Var NotInHole NotInVarHole numHitpoints)(OpSeq(Placeholder 0)(S Int E)))E))((ExpLine(OpSeq(Placeholder 0)(S(IntLit NotInHole 6)E)))))(LetLine(OpSeq(Placeholder 0)(S(TypeAnn NotInHole(Var NotInHole NotInVarHole bargle)(OpSeq(Placeholder 0)(S(Parenthesized(OpSeq(BinOp NotInHole Sum(Placeholder 0)(Placeholder 1))(S Bool(A Sum(S Float E)))))E)))E))((ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 4)E)))))(LetLine(OpSeq(Placeholder 0)(S(TypeAnn NotInHole(Var NotInHole NotInVarHole doSomething)(OpSeq(BinOp NotInHole Arrow(Placeholder 0)(Placeholder 1))(S Hole(A Arrow(S Int E)))))E))((ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 6)E)))))(LetLine(OpSeq(Placeholder 0)(S(TypeAnn NotInHole(Var NotInHole NotInVarHole round)(OpSeq(BinOp NotInHole Arrow(Placeholder 0)(Placeholder 1))(S Float(A Arrow(S Int E)))))E))((ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 7)E)))))(LetLine(OpSeq(Placeholder 0)(S(TypeAnn NotInHole(Var NotInHole NotInVarHole intToFloat)(OpSeq(BinOp NotInHole Arrow(Placeholder 0)(Placeholder 1))(S Int(A Arrow(S Float E)))))E))((ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 9)E)))))(LetLine(OpSeq(Placeholder 0)(S(TypeAnn NotInHole(Var NotInHole NotInVarHole floatToInt)(OpSeq(BinOp NotInHole Arrow(Placeholder 0)(Placeholder 1))(S Float(A Arrow(S Int E)))))E))((ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 10)E)))))(ExpLine(OpSeq(BinOp NotInHole FPlus(Placeholder 0)(BinOp NotInHole FTimes(Placeholder 1)(Placeholder 2)))(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole FPlus(Placeholder 0)(Placeholder 1))(S(IntLit(InHole TypeInconsistent 1363)100)(A FPlus(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(Var NotInHole(InVarHole Free 471)float2Int)(A Space(S(IntLit NotInHole 222)E)))))))E)))))))(A FPlus(S(FloatLit NotInHole 20.)(A FTimes(S(Var NotInHole(InVarHole Free 1092)numLives)E)))))))(ExpLine(OpSeq(BinOp NotInHole Times(Placeholder 0)(Placeholder 1))(S(IntLit NotInHole 111)(A Times(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole doSomething)(A Space(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole round)(A Space(S(IntLit(InHole TypeInconsistent 1364)4)E)))))))E)))))))E)))))(ExpLine(OpSeq(Placeholder 0)(S(Case(StandardErrStatus NotInHole)((ExpLine(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole bargle)(A Comma(S(Var NotInHole NotInVarHole numHitpoints)E))))))((Rule(OpSeq(Placeholder 0)(S(Parenthesized(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(EmptyHole 14)(A Comma(S(EmptyHole 15)E)))))E))((ExpLine(OpSeq(Placeholder 0)(S(BoolLit NotInHole true)E)))))(Rule(OpSeq(Placeholder 0)(S(EmptyHole 16)E))((ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 17)E)))))))E)))(ExpLine(OpSeq(BinOp NotInHole FPlus(Placeholder 0)(Placeholder 1))(S(IntLit(InHole TypeInconsistent 1392)1)(A FPlus(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(Var NotInHole(InVarHole Free 861)blom)(A Space(S(IntLit NotInHole 4)E)))))))E)))))(ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 21)E))))|}
  |> Sexplib.Sexp.of_string
  |> UHExp.t_of_sexp;

let inconsistent_branches: UHExp.t =
  UHExp.(
    Block.wrap(
      case(
        ~err=InconsistentBranches([Bool, Float, Float], 0),
        Block.wrap(UHExp.IntLit(NotInHole, "1")),
        [
          Rule(
            OpSeq.wrap(UHPat.IntLit(NotInHole, "0")),
            Block.wrap(UHExp.BoolLit(NotInHole, true)),
          ),
          Rule(
            OpSeq.wrap(UHPat.IntLit(NotInHole, "1")),
            Block.wrap(UHExp.FloatLit(NotInHole, "1.")),
          ),
          Rule(
            OpSeq.wrap(UHPat.IntLit(NotInHole, "2")),
            Block.wrap(UHExp.FloatLit(NotInHole, "2.")),
          ),
        ],
      ),
    )
  );

let examples = [
  ("hole", just_hole),
  ("lambda", holey_lambda),
  ("let", let_line),
  ("map", map_example),
  ("quicksort", qsort_example),
  ("assistant", assistant_test),
  ("assistant2", assistant_test_2),
  ("inconsistent branches", inconsistent_branches),
];

let example_to_card = ((name: string, e: UHExp.t)): CardInfo.t => {
  name,
  caption: Virtual_dom.Vdom.Node.div([], []),
  init_zexp: ZExp.place_before(e),
};

let cardstack: CardstackInfo.t = {
  title: "examples",
  cards: List.map(example_to_card, examples),
};

let tests = [
  ("quicksort x1", qsort_n(1)),
  ("quicksort x10", qsort_n(10)),
  ("quicksort x100", qsort_n(100)),
];

let teststack: CardstackInfo.t = {
  title: "tests",
  cards: List.map(example_to_card, tests),
};

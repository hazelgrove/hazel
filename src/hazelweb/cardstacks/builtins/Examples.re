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

let tests_test =
  {|((ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(Keyword(Typed Test NotInHole 6))(A Space(S(EmptyHole 1)E)))))(ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(Keyword(Typed Test NotInHole 4))(A Space(S(BoolLit NotInHole true)E)))))(ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(Keyword(Typed Test NotInHole 5))(A Space(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole And(Placeholder 0)(Placeholder 1))(S(BoolLit NotInHole true)(A And(S(BoolLit NotInHole false)E)))))))E)))))(LetLine(OpSeq(Placeholder 0)(S(TypeAnn NotInHole(Var NotInHole NotInVarHole fac)(OpSeq(BinOp NotInHole Arrow(Placeholder 0)(Placeholder 1))(S Hole(A Arrow(S Hole E)))))E))((ExpLine(OpSeq(Placeholder 0)(S(Lam NotInHole(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole x)E))((ExpLine(OpSeq(Placeholder 0)(S(Case(StandardErrStatus NotInHole)((ExpLine(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole x)E))))((Rule(OpSeq(Placeholder 0)(S(IntLit NotInHole 1)E))((ExpLine(OpSeq(Placeholder 0)(S(IntLit NotInHole 1)E)))))(Rule(OpSeq(Placeholder 0)(S(Wild NotInHole)E))((ExpLine(OpSeq(BinOp NotInHole Times(Placeholder 0)(BinOp NotInHole Space(Placeholder 1)(Placeholder 2)))(S(Var NotInHole NotInVarHole x)(A Times(S(Var NotInHole NotInVarHole fac)(A Space(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Minus(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole x)(A Minus(S(IntLit NotInHole 1)E)))))))E)))))))))))E)))))E)))))(ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(Keyword(Typed Test NotInHole 1))(A Space(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Equals(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(Placeholder 2))(S(Var NotInHole NotInVarHole fac)(A Space(S(IntLit NotInHole 1)(A Equals(S(IntLit NotInHole 1)E)))))))))E)))))(ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(Keyword(Typed Test NotInHole 3))(A Space(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Equals(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(BinOp NotInHole Times(BinOp NotInHole Times(BinOp NotInHole Times(BinOp NotInHole Times(BinOp NotInHole Times(Placeholder 2)(Placeholder 3))(Placeholder 4))(Placeholder 5))(Placeholder 6))(Placeholder 7)))(S(Var NotInHole NotInVarHole fac)(A Space(S(IntLit NotInHole 7)(A Equals(S(IntLit NotInHole 2)(A Times(S(IntLit NotInHole 3)(A Times(S(IntLit NotInHole 4)(A Times(S(IntLit NotInHole 5)(A Times(S(IntLit NotInHole 6)(A Times(S(IntLit NotInHole 7)E)))))))))))))))))))E)))))(ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(Keyword(Typed Test NotInHole 2))(A Space(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Equals(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(Placeholder 2))(S(Var NotInHole NotInVarHole fac)(A Space(S(IntLit NotInHole 3)(A Equals(S(IntLit NotInHole 5)E)))))))))E))))))|}
  |> Sexplib.Sexp.of_string
  |> UHExp.t_of_sexp;

// let idgen_test =
//   {|((((ExpLine(OpSeq(Placeholder 0)(S(Lam NotInHole(OpSeq(Placeholder 0)(S(EmptyHole 1)E))((ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 2)E)))))E)))(ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(Keyword(Typed Test NotInHole 3))(A Space(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Equals(Placeholder 0)(Placeholder 1))(S(IntLit NotInHole 1)(A Equals(S(IntLit NotInHole 1)E)))))))E))))))(ExpLineZ(ZOpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(ZOperand(CursorE(OnText 5)(BoolLit NotInHole false))((A Space(S(Keyword(Typed Test NotInHole 4))E))E))))((ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(Keyword(Typed Test NotInHole 7))(A Space(S(Parenthesized((ExpLine(OpSeq(Placeholder 0)(S(BoolLit NotInHole true)E)))))E)))))(ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 77)E)))))Hole(77 7))|}
//   |> Sexplib.Sexp.of_string
//   |> Statics.edit_state_of_sexp;

let examples = [
  ("hole", just_hole),
  ("lambda", holey_lambda),
  ("let", let_line),
  ("map", map_example),
  ("quicksort", qsort_example),
  ("inconsistent branches", inconsistent_branches),
  ("tests", tests_test),
];

let edit_state_of_block =
    ((name: string, block: UHExp.block)): (string, Statics.edit_state) => {
  let zippered_block = block |> ZExp.place_before;
  let block_type =
    switch (Statics_Exp.syn(Contexts.empty, block)) {
    | Some(ty) => ty
    | None => Hole
    };
  let edit_state: Statics.edit_state = {
    prelude: UHExp.empty_block,
    template: zippered_block,
    tester: UHExp.empty_block,
    ty: block_type,
    id_gen: IDGen.init,
  };

  (name, edit_state);
};

let example_to_card =
    ((name: string, init_edit_state: Statics.edit_state)): CardInfo.t => {
  name,
  caption: Virtual_dom.Vdom.Node.div([], []),
  init_edit_state,
};

let cardstack: CardstackInfo.t = {
  title: "examples",
  cards: List.map(example_to_card, List.map(edit_state_of_block, examples)),
};

let tests = [
  ("quicksort x1", qsort_n(1)),
  ("quicksort x10", qsort_n(10)),
  ("quicksort x100", qsort_n(100)),
];

let teststack: CardstackInfo.t = {
  title: "tests",
  cards: List.map(example_to_card, List.map(edit_state_of_block, tests)),
};

let filter_odds =
  {|((prelude())(template(()(LetLineZE(OpSeq(Placeholder 0)(S(TypeAnn NotInHole(Var NotInHole NotInVarHole mod)(OpSeq(BinOp NotInHole Arrow(Placeholder 0)(BinOp NotInHole Arrow(Placeholder 1)(Placeholder 2)))(S Int(A Arrow(S Int(A Arrow(S Int E)))))))E))(()(ExpLineZ(ZOpSeq(Placeholder 0)(ZOperand(LamZE NotInHole(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole x)E))(()(ExpLineZ(ZOpSeq(Placeholder 0)(ZOperand(LamZE NotInHole(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole y)E))(()(ExpLineZ(ZOpSeq(Placeholder 0)(ZOperand(CaseZR(StandardErrStatus NotInHole)((ExpLine(OpSeq(BinOp NotInHole LessThan(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole x)(A LessThan(S(Var NotInHole NotInVarHole y)E))))))(()(RuleZE(OpSeq(Placeholder 0)(S(BoolLit NotInHole true)E))(()(ExpLineZ(ZOpSeq(Placeholder 0)(ZOperand(CursorE(OnText 1)(Var NotInHole NotInVarHole x))(E E))))()))((Rule(OpSeq(Placeholder 0)(S(BoolLit NotInHole false)E))((ExpLine(OpSeq(BinOp NotInHole Space(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(Placeholder 2))(S(Var NotInHole NotInVarHole mod)(A Space(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Minus(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole x)(A Minus(S(Var NotInHole NotInVarHole y)E)))))))(A Space(S(Var NotInHole NotInVarHole y)E))))))))))))(E E))))()))(E E))))()))(E E))))()))((LetLine(OpSeq(Placeholder 0)(S(TypeAnn NotInHole(Var NotInHole NotInVarHole list_eq)(OpSeq(BinOp NotInHole Arrow(Placeholder 0)(Placeholder 1))(S(Parenthesized(OpSeq(Placeholder 0)(S(List(OpSeq(Placeholder 0)(S Int E)))E)))(A Arrow(S(Parenthesized(OpSeq(BinOp NotInHole Arrow(Placeholder 0)(Placeholder 1))(S(Parenthesized(OpSeq(Placeholder 0)(S(List(OpSeq(Placeholder 0)(S Int E)))E)))(A Arrow(S(Parenthesized(OpSeq(Placeholder 0)(S Bool E)))E)))))E)))))E))((ExpLine(OpSeq(Placeholder 0)(S(Lam NotInHole(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole xs)E))((ExpLine(OpSeq(Placeholder 0)(S(Lam NotInHole(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole ys)E))(EmptyLine(ExpLine(OpSeq(Placeholder 0)(S(Case(StandardErrStatus NotInHole)((ExpLine(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole xs)(A Comma(S(Var NotInHole NotInVarHole ys)E))))))((Rule(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(ListNil NotInHole)(A Comma(S(ListNil NotInHole)E))))((ExpLine(OpSeq(Placeholder 0)(S(BoolLit NotInHole true)E)))))(Rule(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(Wild NotInHole)(A Comma(S(ListNil NotInHole)E))))((ExpLine(OpSeq(Placeholder 0)(S(BoolLit NotInHole false)E)))))(Rule(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(ListNil NotInHole)(A Comma(S(Wild NotInHole)E))))((ExpLine(OpSeq(Placeholder 0)(S(BoolLit NotInHole false)E)))))(Rule(OpSeq(BinOp NotInHole Comma(BinOp NotInHole Cons(Placeholder 0)(Placeholder 1))(BinOp NotInHole Cons(Placeholder 2)(Placeholder 3)))(S(Var NotInHole NotInVarHole x)(A Cons(S(Var NotInHole NotInVarHole xs')(A Comma(S(Var NotInHole NotInVarHole y)(A Cons(S(Var NotInHole NotInVarHole ys')E))))))))((ExpLine(OpSeq(Placeholder 0)(S(Case(StandardErrStatus NotInHole)((ExpLine(OpSeq(BinOp NotInHole Equals(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole x)(A Equals(S(Var NotInHole NotInVarHole y)E))))))((Rule(OpSeq(Placeholder 0)(S(BoolLit NotInHole true)E))((ExpLine(OpSeq(BinOp NotInHole Space(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(Placeholder 2))(S(Var NotInHole NotInVarHole list_eq)(A Space(S(Var NotInHole NotInVarHole xs')(A Space(S(Var NotInHole NotInVarHole ys')E)))))))))(Rule(OpSeq(Placeholder 0)(S(Wild NotInHole)E))((ExpLine(OpSeq(Placeholder 0)(S(BoolLit NotInHole false)E)))))))E)))))))E)))))E)))))E)))))(CommentLine"TODO: complete the filter_odds function")(LetLine(OpSeq(Placeholder 0)(S(TypeAnn NotInHole(Var NotInHole NotInVarHole filter_odds)(OpSeq(BinOp NotInHole Arrow(Placeholder 0)(Placeholder 1))(S(Parenthesized(OpSeq(Placeholder 0)(S(List(OpSeq(Placeholder 0)(S Int E)))E)))(A Arrow(S(Parenthesized(OpSeq(Placeholder 0)(S(List(OpSeq(Placeholder 0)(S Int E)))E)))E)))))E))((ExpLine(OpSeq(Placeholder 0)(S(Lam NotInHole(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole xs)E))((ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 2)E)))))E)))))(CommentLine"TODO: write tests here")(ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(Keyword(Typed Test NotInHole 1))(A Space(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Space(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(Placeholder 2))(S(Var NotInHole NotInVarHole list_eq)(A Space(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole filter_odds)(A Space(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Cons(Placeholder 0)(BinOp NotInHole Cons(Placeholder 1)(BinOp NotInHole Cons(Placeholder 2)(BinOp NotInHole Cons(Placeholder 3)(BinOp NotInHole Cons(Placeholder 4)(Placeholder 5))))))(S(IntLit NotInHole 1)(A Cons(S(IntLit NotInHole 2)(A Cons(S(IntLit NotInHole 3)(A Cons(S(IntLit NotInHole 4)(A Cons(S(IntLit NotInHole 5)(A Cons(S(ListNil NotInHole)E)))))))))))))))E)))))))(A Space(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Cons(Placeholder 0)(BinOp NotInHole Cons(Placeholder 1)(BinOp NotInHole Cons(Placeholder 2)(Placeholder 3))))(S(IntLit NotInHole 1)(A Cons(S(IntLit NotInHole 3)(A Cons(S(IntLit NotInHole 5)(A Cons(S(ListNil NotInHole)E)))))))))))E)))))))))E))))))))(tester((CommentLine"empty list")(ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(Keyword(Typed Test NotInHole 3))(A Space(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Space(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(Placeholder 2))(S(Var NotInHole NotInVarHole list_eq)(A Space(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole filter_odds)(A Space(S(ListNil NotInHole)E)))))))(A Space(S(Parenthesized((ExpLine(OpSeq(Placeholder 0)(S(ListNil NotInHole)E)))))E)))))))))E)))))(CommentLine"all evens")(ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(Keyword(Typed Test NotInHole 4))(A Space(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Space(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(Placeholder 2))(S(Var NotInHole NotInVarHole list_eq)(A Space(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole filter_odds)(A Space(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Cons(Placeholder 0)(BinOp NotInHole Cons(Placeholder 1)(BinOp NotInHole Cons(Placeholder 2)(BinOp NotInHole Cons(Placeholder 3)(Placeholder 4)))))(S(IntLit NotInHole 2)(A Cons(S(IntLit NotInHole 4)(A Cons(S(IntLit NotInHole 6)(A Cons(S(IntLit NotInHole 8)(A Cons(S(ListNil NotInHole)E)))))))))))))E)))))))(A Space(S(Parenthesized((ExpLine(OpSeq(Placeholder 0)(S(ListNil NotInHole)E)))))E)))))))))E)))))(CommentLine"all odds")(ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(Keyword(Typed Test NotInHole 5))(A Space(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Space(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(Placeholder 2))(S(Var NotInHole NotInVarHole list_eq)(A Space(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole filter_odds)(A Space(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Cons(Placeholder 0)(BinOp NotInHole Cons(Placeholder 1)(BinOp NotInHole Cons(Placeholder 2)(Placeholder 3))))(S(IntLit NotInHole 1)(A Cons(S(IntLit NotInHole 3)(A Cons(S(IntLit NotInHole 5)(A Cons(S(ListNil NotInHole)E)))))))))))E)))))))(A Space(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Cons(Placeholder 0)(BinOp NotInHole Cons(Placeholder 1)(BinOp NotInHole Cons(Placeholder 2)(Placeholder 3))))(S(IntLit NotInHole 1)(A Cons(S(IntLit NotInHole 3)(A Cons(S(IntLit NotInHole 5)(A Cons(S(ListNil NotInHole)E)))))))))))E)))))))))E)))))(CommentLine normal)(ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(Keyword(Typed Test NotInHole 6))(A Space(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Space(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(Placeholder 2))(S(Var NotInHole NotInVarHole list_eq)(A Space(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole filter_odds)(A Space(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Cons(Placeholder 0)(BinOp NotInHole Cons(Placeholder 1)(BinOp NotInHole Cons(Placeholder 2)(BinOp NotInHole Cons(Placeholder 3)(BinOp NotInHole Cons(Placeholder 4)(BinOp NotInHole Cons(Placeholder 5)(Placeholder 6)))))))(S(IntLit NotInHole 1)(A Cons(S(IntLit NotInHole 2)(A Cons(S(IntLit NotInHole 3)(A Cons(S(IntLit NotInHole 4)(A Cons(S(IntLit NotInHole 5)(A Cons(S(IntLit NotInHole 6)(A Cons(S(ListNil NotInHole)E)))))))))))))))))E)))))))(A Space(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Cons(Placeholder 0)(BinOp NotInHole Cons(Placeholder 1)(BinOp NotInHole Cons(Placeholder 2)(Placeholder 3))))(S(IntLit NotInHole 1)(A Cons(S(IntLit NotInHole 3)(A Cons(S(IntLit NotInHole 5)(A Cons(S(ListNil NotInHole)E)))))))))))E)))))))))E)))))))(id_gen(1326 6))(ty(Prod())))|}
  |> Sexplib.Sexp.of_string
  |> Statics.edit_state_of_sexp;

let assignments = [("filter odds", filter_odds)];

let assignment_stack: CardstackInfo.t = {
  title: "assignments",
  cards: List.map(example_to_card, assignments),
};

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
            ~ann=OpSeq.wrap(UHTyp.Hole),
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
        OpSeq.wrap(UHPat.var("map")),
        ~ann=
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
        OpSeq.wrap(UHPat.var("append")),
        ~ann=
          UHTyp.(
            Operators_Typ.(
              Seq.mk(
                List(OpSeq.wrap(Int)),
                [
                  (Arrow, List(OpSeq.wrap(Int))),
                  (Arrow, List(OpSeq.wrap(Int))),
                ],
              )
              |> mk_OpSeq
            )
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
        OpSeq.wrap(UHPat.var("partition")),
        ~ann=
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

/*
 ((LetLine(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole add)E))(
   (OpSeq(BinOp NotInHole Arrow(Placeholder 0)(Placeholder 1))(S Int(A Arrow(S Hole E)))))
   ((ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 5)E)))))
   (ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S
   (AssertLit NotInHole 1)(A Space(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Equals(BinOp NotInHole Space(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(Placeholder 2))(Placeholder 3))(S(Var NotInHole NotInVarHole add)(A Space(S(IntLit NotInHole 2)(A Space(S(IntLit NotInHole 3)(A Equals(S(IntLit NotInHole 5)E)))))))))))E))))))

  ((
    ExpLine(
      OpSeq(
        BinOp NotInHole Space
        (Placeholder 0)
        (Placeholder 1)
        )
      (S
        (AssertLit NotInHole 1)
        (A Space
          (S(Parenthesized(
            (ExpLine(OpSeq(BinOp NotInHole Equals(Placeholder 0)(Placeholder 1))(S(IntLit NotInHole 4)(A Equals(S(IntLit NotInHole 2)E)))))))E))))))
  */

let add_stub: UHExp.line =
  LetLine(
    OpSeq.wrap(UHPat.var("add")),
    Some(UHTyp.contract(Arrow(Int, Arrow(Int, Int)))),
    [ExpLine(OpSeq.wrap(UHExp.EmptyHole(0)))],
  );

let assert_base = (n: int, bool_expr: UHExp.operand): UHExp.line =>
  ExpLine(
    UHExp.mk_OpSeq(
      Seq.mk(
        UHExp.AssertLit(NotInHole, n),
        [(Operators_Exp.Space, bool_expr)],
      ),
    ),
  );

let binary_application_template =
    (f: UHExp.operand, inputs: list(UHExp.operand)): UHExp.line =>
  ExpLine(
    UHExp.mk_OpSeq(
      Seq.mk(f, List.map(a => (Operators_Exp.Space, a), inputs)),
    ),
  );

let mk_app_equality_assert =
    (n: int, f_name: string, inputs: list(UHExp.operand), out: UHExp.operand)
    : UHExp.line =>
  assert_base(
    n,
    Parenthesized([
      ExpLine(
        UHExp.mk_OpSeq(
          Seq.mk(
            UHExp.Parenthesized([
              binary_application_template(UHExp.var(f_name), inputs),
            ]),
            [(Operators_Exp.Equals, out)],
          ),
        ),
      ),
    ]),
  );

let addition_template: UHExp.t = [
  add_stub,
  mk_app_equality_assert(
    1,
    "add",
    [UHExp.intlit("0"), UHExp.intlit("0")],
    UHExp.intlit("0"),
  ),
  mk_app_equality_assert(
    1,
    "add",
    [UHExp.intlit("0"), UHExp.intlit("1")],
    UHExp.intlit("1"),
  ),
  mk_app_equality_assert(
    2,
    "add",
    [UHExp.intlit("1"), UHExp.intlit("0")],
    UHExp.intlit("1"),
  ),
  mk_app_equality_assert(
    3,
    "add",
    [UHExp.intlit("2"), UHExp.intlit("2")],
    UHExp.intlit("4"),
  ),
];

let shmyth_app = (fname: string, args: list(UHExp.operand)): UHExp.opseq =>
  UHExp.mk_OpSeq(
    Seq.mk(
      UHExp.var(fname),
      List.map(a => (Operators_Exp.Space, a), args),
    ),
  );

let shmyth_app_operand =
    (f: UHExp.operand, inputs: list(UHExp.operand)): UHExp.opseq =>
  UHExp.mk_OpSeq(
    Seq.mk(f, List.map(a => (Operators_Exp.Space, a), inputs)),
  );

let shmyth_parens = (a: UHExp.opseq): UHExp.operand =>
  Parenthesized([ExpLine(a)]);

let shmyth_cons =
    (head: UHExp.operand, args: list(UHExp.operand)): UHExp.opseq =>
  UHExp.mk_OpSeq(
    Seq.mk(head, List.map(a => (Operators_Exp.Cons, a), args)),
  );

let shmyth_lam = (x: string, body: UHExp.opseq): UHExp.opseq =>
  OpSeq.wrap(UHExp.lam(OpSeq.wrap(UHPat.var(x)), [ExpLine(body)]));

let shmyth_hole = (h: int): UHExp.opseq => OpSeq.wrap(UHExp.EmptyHole(h));

let shmyth_let = (x: string, typ: UHTyp.opseq, v: UHExp.opseq): UHExp.line =>
  UHExp.letline(OpSeq.wrap(UHPat.var(x)), ~ann=typ, [ExpLine(v)]);

let shmyth_let_notype = (x: string, v: UHExp.opseq): UHExp.line =>
  UHExp.letline(OpSeq.wrap(UHPat.var(x)), [ExpLine(v)]);

let shmyth_sub1 = (a: UHExp.operand): UHExp.opseq =>
  UHExp.mk_OpSeq(
    Seq.seq_op_seq(
      Seq.wrap(a),
      Operators_Exp.Minus,
      Seq.wrap(UHExp.intlit("1")),
    ),
  );

let shmyth_add1 = (a: UHExp.operand): UHExp.opseq =>
  UHExp.mk_OpSeq(
    Seq.seq_op_seq(
      Seq.wrap(UHExp.intlit("1")),
      Operators_Exp.Plus,
      Seq.wrap(a),
    ),
  );

let shmyth_case_list =
    (
      scrutinee: string,
      on_nil: UHExp.opseq,
      hd: string,
      tl: string,
      on_cons: UHExp.opseq,
    )
    : UHExp.operand =>
  UHExp.case(
    [ExpLine(OpSeq.wrap(UHExp.var(scrutinee)))],
    [
      Rule(OpSeq.wrap(UHPat.listnil()), [ExpLine(on_nil)]),
      Rule(
        UHPat.mk_OpSeq(
          Seq.seq_op_seq(
            Seq.wrap(UHPat.var(hd)),
            Operators_Pat.Cons,
            Seq.wrap(UHPat.var(tl)),
          ),
        ),
        [ExpLine(on_cons)],
      ),
    ],
  );

let shmyth_case_nat =
    (
      scrutinee: string,
      succ_var_name: string,
      on_zero: UHExp.opseq,
      on_succ: UHExp.opseq,
    )
    : UHExp.operand =>
  UHExp.case(
    [ExpLine(OpSeq.wrap(UHExp.var(scrutinee)))],
    [
      Rule(OpSeq.wrap(UHPat.intlit("0")), [ExpLine(on_zero)]),
      Rule(
        OpSeq.wrap(UHPat.var(succ_var_name)),
        [
          shmyth_let_notype(
            succ_var_name,
            shmyth_sub1(UHExp.var(succ_var_name)),
          ),
          UHExp.ExpLine(on_succ),
        ],
      ),
    ],
  );

let append_template: UHExp.t = [
  shmyth_let(
    "append",
    UHTyp.contract(Arrow(List(Int), Arrow(List(Int), List(Int)))),
    shmyth_hole(0),
  ),
  mk_app_equality_assert(
    1,
    "append",
    [
      shmyth_parens(shmyth_cons(UHExp.intlit("1"), [UHExp.listnil()])),
      shmyth_parens(shmyth_cons(UHExp.intlit("2"), [UHExp.listnil()])),
    ],
    shmyth_parens(
      shmyth_cons(UHExp.intlit("1"), [UHExp.intlit("2"), UHExp.listnil()]),
    ),
  ),
  mk_app_equality_assert(
    1,
    "append",
    [
      Parenthesized([ExpLine(OpSeq.wrap(UHExp.listnil()))]),
      Parenthesized([ExpLine(OpSeq.wrap(UHExp.listnil()))]),
    ],
    Parenthesized([ExpLine(OpSeq.wrap(UHExp.listnil()))]),
  ),
];

let mult_template: UHExp.t = [
  shmyth_let(
    "add",
    UHTyp.contract(Arrow(Int, Arrow(Int, Int))),
    shmyth_lam(
      "x1",
      shmyth_lam(
        "x2",
        OpSeq.wrap(
          shmyth_case_nat(
            "x1",
            "y1",
            OpSeq.wrap(UHExp.var("x2")),
            shmyth_add1(
              Parenthesized([
                ExpLine(
                  shmyth_app("add", [UHExp.var("y1"), UHExp.var("x2")]),
                ),
              ]),
            ),
          ),
        ),
      ),
    ),
  ),
  shmyth_let(
    "mult",
    UHTyp.contract(Arrow(Int, Arrow(Int, Int))),
    shmyth_lam(
      "p",
      shmyth_lam(
        "q",
        OpSeq.wrap(
          shmyth_case_nat(
            "p",
            "y1",
            OpSeq.wrap(UHExp.intlit("0")),
            shmyth_app_operand(
              Parenthesized([ExpLine(shmyth_app("add", [EmptyHole(0)]))]),
              [
                Parenthesized([
                  ExpLine(
                    shmyth_app("mult", [EmptyHole(1), EmptyHole(2)]),
                  ),
                ]),
              ],
            ),
          ),
        ),
      ),
    ),
  ),
  mk_app_equality_assert(
    1,
    "mult",
    [UHExp.intlit("3"), UHExp.intlit("2")],
    UHExp.intlit("6"),
  ),
  mk_app_equality_assert(
    2,
    "mult",
    [UHExp.intlit("2"), UHExp.intlit("1")],
    UHExp.intlit("2"),
  ),
];

let max_template: UHExp.t = [
  shmyth_let(
    "max",
    UHTyp.contract(Arrow(Int, Arrow(Int, Int))),
    shmyth_lam(
      "m",
      shmyth_lam(
        "n",
        OpSeq.wrap(
          shmyth_case_nat(
            "m",
            "y1",
            OpSeq.wrap(UHExp.var("n")),
            OpSeq.wrap(
              shmyth_case_nat(
                "n",
                "y2",
                OpSeq.wrap(UHExp.var("m")),
                shmyth_hole(0),
              ),
            ),
          ),
        ),
      ),
    ),
  ),
  mk_app_equality_assert(
    1,
    "max",
    [UHExp.intlit("3"), UHExp.intlit("1")],
    UHExp.intlit("3"),
  ),
  mk_app_equality_assert(
    2,
    "max",
    [UHExp.intlit("1"), UHExp.intlit("2")],
    UHExp.intlit("2"),
  ),
  mk_app_equality_assert(
    3,
    "max",
    [UHExp.intlit("1"), UHExp.intlit("1")],
    UHExp.intlit("1"),
  ),
];

let odd_template: UHExp.t = [
  shmyth_let(
    "odd",
    UHTyp.contract(Arrow(Int, Bool)),
    shmyth_lam(
      "n",
      OpSeq.wrap(
        shmyth_case_nat(
          "n",
          "y1",
          OpSeq.wrap(UHExp.boollit(false)),
          OpSeq.wrap(UHExp.EmptyHole(0)),
        ),
      ),
    ),
  ),
  mk_app_equality_assert(
    1,
    "odd",
    [UHExp.intlit("1")],
    UHExp.boollit(true),
  ),
  mk_app_equality_assert(
    2,
    "odd",
    [UHExp.intlit("2")],
    UHExp.boollit(false),
  ),
];

let revConcat_template: UHExp.t = [
  shmyth_let(
    "revConcat",
    UHTyp.contract(Arrow(List(Int), Arrow(List(Int), List(Int)))),
    shmyth_lam(
      "xs",
      shmyth_lam(
        "ys",
        OpSeq.wrap(
          shmyth_case_list(
            "xs",
            OpSeq.wrap(UHExp.var("ys")),
            "head",
            "tail",
            shmyth_hole(0),
          ),
        ),
      ),
    ),
  ),
  mk_app_equality_assert(
    1,
    "revConcat",
    [
      shmyth_parens(
        shmyth_cons(
          UHExp.intlit("0"),
          [UHExp.intlit("1"), UHExp.listnil()],
        ),
      ),
      shmyth_parens(
        shmyth_cons(
          UHExp.intlit("2"),
          [UHExp.intlit("3"), UHExp.listnil()],
        ),
      ),
    ],
    shmyth_parens(
      shmyth_cons(
        UHExp.intlit("1"),
        [
          UHExp.intlit("0"),
          UHExp.intlit("2"),
          UHExp.intlit("3"),
          UHExp.listnil(),
        ],
      ),
    ),
  ),
  mk_app_equality_assert(
    2,
    "revConcat",
    [
      shmyth_parens(shmyth_cons(UHExp.intlit("4"), [UHExp.listnil()])),
      shmyth_parens(shmyth_cons(UHExp.intlit("5"), [UHExp.listnil()])),
    ],
    shmyth_parens(
      shmyth_cons(UHExp.intlit("4"), [UHExp.intlit("5"), UHExp.listnil()]),
    ),
  ),
  mk_app_equality_assert(
    3,
    "revConcat",
    [
      UHExp.listnil(),
      shmyth_parens(shmyth_cons(UHExp.intlit("1"), [UHExp.listnil()])),
    ],
    shmyth_parens(shmyth_cons(UHExp.intlit("1"), [UHExp.listnil()])),
  ),
];

let stutter_template: UHExp.t = [
  shmyth_let(
    "stutter",
    UHTyp.contract(Arrow(List(Int), List(Int))),
    shmyth_lam("xs", shmyth_hole(0)),
  ),
  mk_app_equality_assert(
    1,
    "stutter",
    [
      shmyth_parens(
        shmyth_cons(
          UHExp.intlit("1"),
          [UHExp.intlit("0"), UHExp.listnil()],
        ),
      ),
    ],
    shmyth_parens(
      shmyth_cons(
        UHExp.intlit("1"),
        [
          UHExp.intlit("1"),
          UHExp.intlit("0"),
          UHExp.intlit("0"),
          UHExp.listnil(),
        ],
      ),
    ),
  ),
  mk_app_equality_assert(
    2,
    "stutter",
    [shmyth_parens(shmyth_cons(UHExp.intlit("0"), [UHExp.listnil()]))],
    shmyth_parens(
      shmyth_cons(UHExp.intlit("0"), [UHExp.intlit("0"), UHExp.listnil()]),
    ),
  ),
  mk_app_equality_assert(3, "stutter", [UHExp.listnil()], UHExp.listnil()),
];

let stutterN_template: UHExp.t = [
  UHExp.letline(
    OpSeq.wrap(UHPat.var("append")),
    ~ann=UHTyp.contract(Arrow(List(Int), Arrow(List(Int), List(Int)))),
    [
      ExpLine(
        shmyth_lam(
          "xs",
          shmyth_lam(
            "ys",
            OpSeq.wrap(
              shmyth_case_list(
                "xs",
                OpSeq.wrap(UHExp.var("ys")),
                "x",
                "xs1",
                shmyth_cons(
                  UHExp.var("x"),
                  [
                    shmyth_parens(
                      shmyth_app(
                        "append",
                        [UHExp.var("xs1"), UHExp.var("ys")],
                      ),
                    ),
                  ],
                ),
              ),
            ),
          ),
        ),
      ),
    ],
  ),
  UHExp.letline(
    OpSeq.wrap(UHPat.var("replicate")),
    ~ann=UHTyp.contract(Arrow(Int, Arrow(Int, List(Int)))),
    [
      ExpLine(
        OpSeq.wrap(
          UHExp.lam(
            OpSeq.wrap(UHPat.var("n")),
            [
              ExpLine(
                OpSeq.wrap(
                  UHExp.lam(
                    OpSeq.wrap(UHPat.var("x")),
                    [
                      ExpLine(
                        OpSeq.wrap(
                          UHExp.case(
                            [ExpLine(OpSeq.wrap(UHExp.var("n")))],
                            [
                              Rule(
                                OpSeq.wrap(UHPat.intlit("0")),
                                [ExpLine(OpSeq.wrap(UHExp.EmptyHole(0)))],
                              ),
                              Rule(
                                UHPat.mk_OpSeq(
                                  Seq.seq_op_seq(
                                    Seq.wrap(UHPat.var("n_hd")),
                                    Operators_Pat.Cons,
                                    Seq.wrap(UHPat.var("n_tl")),
                                  ),
                                ),
                                [ExpLine(OpSeq.wrap(UHExp.EmptyHole(1)))],
                              ),
                            ],
                          ),
                        ),
                      ),
                    ],
                  ),
                ),
              ),
            ],
          ),
        ),
      ),
    ],
  ),
  UHExp.letline(
    OpSeq.wrap(UHPat.var("stutterN")),
    ~ann=UHTyp.contract(Arrow(Int, Arrow(List(Int), List(Int)))),
    [
      ExpLine(
        OpSeq.wrap(
          UHExp.lam(
            OpSeq.wrap(UHPat.var("n")),
            [
              ExpLine(
                OpSeq.wrap(
                  UHExp.lam(
                    OpSeq.wrap(UHPat.var("xs")),
                    [
                      ExpLine(
                        OpSeq.wrap(
                          UHExp.case(
                            [ExpLine(OpSeq.wrap(UHExp.var("xs")))],
                            [
                              Rule(
                                OpSeq.wrap(UHPat.listnil()),
                                [ExpLine(OpSeq.wrap(UHExp.listnil()))],
                              ),
                              Rule(
                                UHPat.mk_OpSeq(
                                  Seq.seq_op_seq(
                                    Seq.wrap(UHPat.var("x")),
                                    Operators_Pat.Cons,
                                    Seq.wrap(UHPat.var("xs1")),
                                  ),
                                ),
                                [
                                  ExpLine(
                                    UHExp.mk_OpSeq(
                                      Seq.mk(
                                        UHExp.var("append"),
                                        [
                                          (
                                            Operators_Exp.Space,
                                            Parenthesized([
                                              ExpLine(
                                                UHExp.mk_OpSeq(
                                                  Seq.mk(
                                                    UHExp.var("replicate"),
                                                    [
                                                      (
                                                        Operators_Exp.Space,
                                                        UHExp.var("n"),
                                                      ),
                                                      (
                                                        Operators_Exp.Space,
                                                        UHExp.var("x"),
                                                      ),
                                                    ],
                                                  ),
                                                ),
                                              ),
                                            ]),
                                          ),
                                          (
                                            Operators_Exp.Space,
                                            Parenthesized([
                                              ExpLine(
                                                UHExp.mk_OpSeq(
                                                  Seq.mk(
                                                    UHExp.var("stutterN"),
                                                    [
                                                      (
                                                        Operators_Exp.Space,
                                                        UHExp.var("n"),
                                                      ),
                                                      (
                                                        Operators_Exp.Space,
                                                        UHExp.var("xs1"),
                                                      ),
                                                    ],
                                                  ),
                                                ),
                                              ),
                                            ]),
                                          ),
                                        ],
                                      ),
                                    ),
                                  ),
                                ],
                              ),
                            ],
                          ),
                        ),
                      ),
                    ],
                  ),
                ),
              ),
            ],
          ),
        ),
      ),
    ],
  ),
  mk_app_equality_assert(
    1,
    "stutterN",
    UHExp.[
      intlit("2"),
      Parenthesized([
        ExpLine(
          mk_OpSeq(
            Seq.seq_op_seq(
              Seq.wrap(intlit("3")),
              Operators_Exp.Cons,
              Seq.wrap(listnil()),
            ),
          ),
        ),
      ]),
    ],
    UHExp.(
      Parenthesized([
        ExpLine(
          mk_OpSeq(
            Seq.seq_op_seq(
              Seq.wrap(intlit("3")),
              Operators_Exp.Cons,
              Seq.seq_op_seq(
                Seq.wrap(intlit("3")),
                Operators_Exp.Cons,
                Seq.wrap(listnil()),
              ),
            ),
          ),
        ),
      ])
    ),
  ),
];

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
    |> add("qsort_example_3", qsort_n(3))
    |> add("qsort_example_10", qsort_n(10))
    |> add("qsort_example_30", qsort_n(30))
    |> add("qsort_example_100", qsort_n(100))
    |> add("add_template", addition_template)
    |> add("max_template", max_template)
    |> add("odd_template", odd_template)
    |> add("mult_template", mult_template)
    |> add("append_template", append_template)
    |> add("revConcat_template", revConcat_template)
    |> add("stutter_template", stutter_template)
    |> add("stutterN_template", stutterN_template)
  );
let get = id => StringMap.find(id, examples);

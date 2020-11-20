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

let lltest1 = "((LivelitDefLine((name(NotInVarHole $dog))(expansion_type(OpSeq(Placeholder 0)(S Int E)))(captures((ExpLine(OpSeq(Placeholder 0)(S(ListNil NotInHole)E)))))(model_type(OpSeq(Placeholder 0)(S Int E)))(action_type(OpSeq(Placeholder 0)(S Int E)))(init((ExpLine(OpSeq(Placeholder 0)(S(IntLit NotInHole 666)E)))))(update((ExpLine(OpSeq(Placeholder 0)(S(Lam NotInHole(OpSeq(Placeholder 0)(S(Parenthesized(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole m)(A Comma(S(Var NotInHole NotInVarHole a)E)))))E))((OpSeq(Placeholder 0)(S(Parenthesized(OpSeq(BinOp NotInHole Prod(Placeholder 0)(Placeholder 1))(S Int(A Prod(S Int E)))))E)))((ExpLine(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole a)E)))))E)))))(view((ExpLine(OpSeq(Placeholder 0)(S(Lam NotInHole(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole m)E))((OpSeq(Placeholder 0)(S Int E)))((ExpLine(OpSeq(Placeholder 0)(S(StringLit NotInHole\"\")E)))))E)))))(shape((ExpLine(OpSeq(Placeholder 0)(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(BoolLit NotInHole true)(A Comma(S(IntLit NotInHole 15)E)))))))E)))))(expand((ExpLine(OpSeq(Placeholder 0)(S(Lam NotInHole(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole m)E))((OpSeq(Placeholder 0)(S Int E)))((ExpLine(OpSeq(BinOp NotInHole Caret(BinOp NotInHole Caret(Placeholder 0)(Placeholder 1))(Placeholder 2))(S(StringLit NotInHole\"((ExpLine(OpSeq(Placeholder 0)(S(IntLit NotInHole \")(A Caret(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole string_of_int)(A Space(S(Var NotInHole NotInVarHole m)E)))))))(A Caret(S(StringLit NotInHole\")E))))\")E)))))))))E)))))))(ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 0)E))))";

let lltest1p = lltest1 |> Sexplib.Sexp.of_string |> UHExp.t_of_sexp;

let mk_slidy_slice_first = "((LivelitDefLine((name(NotInVarHole $slidy))(expansion_type(OpSeq(Placeholder 0)(S Int E)))(captures((ExpLine(OpSeq(Placeholder 0)(S(ListNil NotInHole)E)))))(model_type(OpSeq(Placeholder 0)(S Int E)))(action_type(OpSeq(Placeholder 0)(S Int E)))(init((ExpLine(OpSeq(Placeholder 0)(S(IntLit NotInHole 50)E)))))(update((ExpLine(OpSeq(Placeholder 0)(S(Lam NotInHole(OpSeq(Placeholder 0)(S(Parenthesized(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole model)(A Comma(S(Var NotInHole NotInVarHole action)E)))))E))((OpSeq(Placeholder 0)(S(Parenthesized(OpSeq(BinOp NotInHole Prod(Placeholder 0)(Placeholder 1))(S Int(A Prod(S Int E)))))E)))((ExpLine(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole action)E)))))E)))))";

let mk_slidy_slice_second = "(view((ExpLine(OpSeq(Placeholder 0)(S(Lam NotInHole(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole m)E))((OpSeq(Placeholder 0)(S Int E)))((ExpLine(OpSeq(Placeholder 0)(S(StringLit NotInHole\"";

let mk_slidy_slice_view1 = "(view((ExpLine(OpSeq(Placeholder 0)(S(Lam NotInHole(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole m)E))((OpSeq(Placeholder 0)(S Int E)))((ExpLine(OpSeq(BinOp NotInHole Caret(BinOp NotInHole Caret(Placeholder 0)(Placeholder 1))(Placeholder 2))(S(StringLit NotInHole \"";

let slidy_view_first = handler =>
  "
    <span class=\\\"slider-livelit\\\">
      <input
        class=\\\"slider\\\"
        type=\\\"range\\\"
        min=0
        max=100
        onClick=\\\""
  ++ handler
  ++ "\\\"
        value=";

let mk_slidy_slice_view2 = "\")(A Caret(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole string_of_int)(A Space(S(Var NotInHole NotInVarHole m)E)))))))(A Caret(S(StringLit NotInHole \"";

let slidy_view_last = ">
      </input>
    </span>";

let mk_slidy_slice_view3 = "\")E)))))))))E)))))";

let mk_slidy_slice_second_last = "\")E)))))E)))))";

let mk_slidy_slice_last = "(shape((ExpLine(OpSeq(Placeholder 0)(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(BoolLit NotInHole true)(A Comma(S(IntLit NotInHole 14)E)))))))E)))))(expand((ExpLine(OpSeq(Placeholder 0)(S(Lam NotInHole(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole m)E))((OpSeq(Placeholder 0)(S Int E)))((ExpLine(OpSeq(BinOp NotInHole Caret(BinOp NotInHole Caret(Placeholder 0)(Placeholder 1))(Placeholder 2))(S(StringLit NotInHole\"((ExpLine(OpSeq(Placeholder 0)(S(IntLit NotInHole \")(A Caret(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole string_of_int)(A Space(S(Var NotInHole NotInVarHole m)E)))))))(A Caret(S(StringLit NotInHole\")E))))\")E)))))))))E)))))))(ExpLine(OpSeq(Placeholder 0)(S(ApLivelit 0 NotInHole $slidy $slidy(IntLit 50)((next 0)(splice_map())(splice_order())))E))))";

let model_v = "10";

let slidy_onclick = "(function(){
              alert('HHHHHZZZZZ');
              return false;
                 }) ();
            return false;";

let lltest2p =
  mk_slidy_slice_first
  ++ mk_slidy_slice_view1
  ++ slidy_view_first(slidy_onclick)
  ++ mk_slidy_slice_view2
  ++ slidy_view_last
  ++ mk_slidy_slice_view3
  ++ mk_slidy_slice_last
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
    |> add("livelit_test_1", lltest1p)
    |> add("livelit_test_2", lltest2p)
  );
let get = id => StringMap.find(id, examples);

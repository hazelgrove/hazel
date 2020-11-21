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

let color =
  "((LetLine(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole baseline)E))()((ExpLine(OpSeq(BinOp NotInHole Space(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(Placeholder 2))(S(ApLivelit 0 NotInHole $slider $slider(57)((next 0)(splice_map())(splice_order())))(A Space(S(IntLit NotInHole 0)(A Space(S(IntLit NotInHole 255)E)))))))))(AbbrevLine $percent NotInAbbrevHole $slider((IntLit NotInHole 0)(IntLit NotInHole 100)))(LetLine(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole default_color)E))()((ExpLine(OpSeq(Placeholder 0)(S(ApLivelit 1 NotInHole $color $color((rgb(0 1 2))(a 3)(selecting_sat_val false))((next 4)(splice_map((0(Int((ExpLine(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole baseline)E))))))(1(Int((ExpLine(OpSeq(BinOp NotInHole Plus(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole baseline)(A Plus(S(IntLit NotInHole 50)E))))))))(2(Int((ExpLine(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole baseline)E))))))(3(Int((ExpLine(OpSeq(Placeholder 0)(S(ApLivelit 2 NotInHole $slider $percent(92)((next 0)(splice_map())(splice_order())))E))))))))(splice_order(0 1 2 3))))E)))))(ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 50)E))))"
  |> Sexplib.Sexp.of_string
  |> UHExp.t_of_sexp;

let map_img_filter =
  "((LetLine(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole map)E))((OpSeq(BinOp NotInHole Arrow(Placeholder 0)(BinOp NotInHole Arrow(Placeholder 1)(Placeholder 2)))(S(Parenthesized(OpSeq(BinOp NotInHole Arrow(Placeholder 0)(Placeholder 1))(S String(A Arrow(S Int E)))))(A Arrow(S(List(OpSeq(Placeholder 0)(S String E)))(A Arrow(S(List(OpSeq(Placeholder 0)(S Int E)))E)))))))((ExpLine(OpSeq(Placeholder 0)(S(Lam NotInHole(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole f)E))()((ExpLine(OpSeq(Placeholder 0)(S(Lam NotInHole(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole xs)E))()((ExpLine(OpSeq(Placeholder 0)(S(Case(StandardErrStatus NotInHole)((ExpLine(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole xs)E))))((Rule(OpSeq(Placeholder 0)(S(ListNil NotInHole)E))((ExpLine(OpSeq(Placeholder 0)(S(ListNil NotInHole)E)))))(Rule(OpSeq(BinOp NotInHole Cons(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole y)(A Cons(S(Var NotInHole NotInVarHole ys)E))))((ExpLine(OpSeq(BinOp NotInHole Cons(Placeholder 0)(Placeholder 1))(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole f)(A Space(S(Var NotInHole NotInVarHole y)E)))))))(A Cons(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Space(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(Placeholder 2))(S(Var NotInHole NotInVarHole map)(A Space(S(Var NotInHole NotInVarHole f)(A Space(S(Var NotInHole NotInVarHole ys)E)))))))))E)))))))))E)))))E)))))E)))))EmptyLine(AbbrevLine $percentage NotInAbbrevHole $slider((IntLit NotInHole 0)(IntLit NotInHole 100)))(LetLine(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole f)E))()((ExpLine(OpSeq(Placeholder 0)(S(Lam NotInHole(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole url)E))((OpSeq(Placeholder 0)(S String E)))((ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(ApLivelit 0 NotInHole $img_filter $img_filter((brightness 0)(grayscale 1))((next 2)(splice_map((0(Int((ExpLine(OpSeq(Placeholder 0)(S(IntLit NotInHole 100)E))))))(1(Int((ExpLine(OpSeq(Placeholder 0)(S(IntLit NotInHole 100)E))))))))(splice_order(0 1))))(A Space(S(Var NotInHole NotInVarHole url)E)))))))E)))))(ExpLine(OpSeq(BinOp NotInHole Space(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(Placeholder 2))(S(Var NotInHole NotInVarHole map)(A Space(S(Var NotInHole NotInVarHole f)(A Space(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Cons(Placeholder 0)(BinOp NotInHole Cons(Placeholder 1)(Placeholder 2)))(S(StringLit NotInHole http://tinyurl.com/y8neczz3)(A Cons(S(StringLit NotInHole http://tinyurl.com/yd2dw4ww)(A Cons(S(ListNil NotInHole)E)))))))))E))))))))"
  |> Sexplib.Sexp.of_string
  |> UHExp.t_of_sexp;

let grade_cutoffs =
  "((LetLine(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole grades)E))()((ExpLine(OpSeq(Placeholder 0)(S(ApLivelit 0 NotInHole $data_frame $data_frame((selected 0)(col_headers(18 19 20 21 22))(rows(((header 5)(cells(0 1 2 3 4)))((header 11)(cells(6 7 8 9 10)))((header 17)(cells(12 13 14 15 16))))))((next 23)(splice_map((0(Int((ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 205)E))))))(1(Int((ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 191)E))))))(2(Int((ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 192)E))))))(3(Int((ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 193)E))))))(4(Int((ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 194)E))))))(5(String((ExpLine(OpSeq(Placeholder 0)(S(StringLit NotInHole Alice)E))))))(6(Int((ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 195)E))))))(7(Int((ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 196)E))))))(8(Int((ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 197)E))))))(9(Int((ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 198)E))))))(10(Int((ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 199)E))))))(11(String((ExpLine(OpSeq(Placeholder 0)(S(StringLit NotInHole Bob)E))))))(12(Int((ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 200)E))))))(13(Int((ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 201)E))))))(14(Int((ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 202)E))))))(15(Int((ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 203)E))))))(16(Int((ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 204)E))))))(17(String((ExpLine(OpSeq(Placeholder 0)(S(StringLit NotInHole Carol)E))))))(18(String((ExpLine(OpSeq(Placeholder 0)(S(StringLit NotInHole A1)E))))))(19(String((ExpLine(OpSeq(Placeholder 0)(S(StringLit NotInHole A2)E))))))(20(String((ExpLine(OpSeq(Placeholder 0)(S(StringLit NotInHole A3)E))))))(21(String((ExpLine(OpSeq(Placeholder 0)(S(StringLit NotInHole Midterm)E))))))(22(String((ExpLine(OpSeq(Placeholder 0)(S(StringLit NotInHole Final)E))))))))(splice_order(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22))))E)))))(ExpLine(OpSeq(Placeholder 0)(S(EmptyHole 6)E))))"
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
  );
let get = id => StringMap.find(id, examples);

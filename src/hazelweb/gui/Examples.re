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

let mk_slidy_slice_first = "((LivelitDefLine((name(NotInVarHole $slidy))(expansion_type(OpSeq(Placeholder 0)(S Int E)))(captures((ExpLine(OpSeq(Placeholder 0)(S(ListNil NotInHole)E)))))(model_type(OpSeq(Placeholder 0)(S Int E)))(action_type(OpSeq(Placeholder 0)(S Int E)))(init((ExpLine(OpSeq(Placeholder 0)(S(IntLit NotInHole 50)E)))))(update((ExpLine(OpSeq(Placeholder 0)(S(Lam NotInHole(OpSeq(Placeholder 0)(S(Parenthesized(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole model)(A Comma(S(Var NotInHole NotInVarHole action)E)))))E))((OpSeq(Placeholder 0)(S(Parenthesized(OpSeq(BinOp NotInHole Prod(Placeholder 0)(Placeholder 1))(S Int(A Prod(S Int E)))))E)))((ExpLine(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole action)E)))))E)))))";

let mk_slidy_view = "
(view((ExpLine(OpSeq(Placeholder 0)(S(Lam NotInHole

(OpSeq(Placeholder 0)(S(Parenthesized(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole llu)(A Comma(S(Var NotInHole NotInVarHole model)E)))))E))

((OpSeq(Placeholder 0)(S(Parenthesized(OpSeq(BinOp NotInHole Prod(Placeholder 0)(Placeholder 1))(S Int(A Prod(S Int E)))))E)))((ExpLine(OpSeq(BinOp NotInHole Caret(BinOp NotInHole Caret(BinOp NotInHole Caret(BinOp NotInHole Caret(Placeholder 0)(Placeholder 1))(Placeholder 2))(Placeholder 3))(Placeholder 4))(S(StringLit NotInHole\"\n     <span class=\\\"slider-livelit\\\">\n       <input\n         class=\\\"slider\\\"\n         id=\\\"llu\\\"\n         type=\\\"range\\\"\n         min=0\n         max=100\n         oninput=\\\"(function(e) {\n             let action = '(IntLit ' + document.getElementById('llu').value + ')';\n             window.trigger(\")(A Caret(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole string_of_int)(A Space(S(Var NotInHole NotInVarHole llu)E)))))))(A Caret(S(StringLit NotInHole \", action);\n             return false;}) ();\n             return false;\\\"\n         value=\")(A Caret(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole string_of_int)(A Space(S(Var NotInHole NotInVarHole model)E)))))))(A Caret(S(StringLit NotInHole\">\n      </input>\n    </span>\")E)))))))))))))E)))))
";

let mk_slidy_slice_view1 = "(view((ExpLine(OpSeq(Placeholder 0)(S(Lam NotInHole
(OpSeq(Placeholder 0)(S(Parenthesized(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole llu)(A Comma(S(Var NotInHole NotInVarHole m)E)))))E))
((OpSeq(Placeholder 0)(S Int E)))((ExpLine(OpSeq(BinOp NotInHole Caret(BinOp NotInHole Caret(Placeholder 0)(Placeholder 1))(Placeholder 2))(S(StringLit NotInHole \"";

let slidy_view_first = "
     <span class=\\\"slider-livelit\\\">
       <input
         class=\\\"slider\\\"
         id=\\\"slidyy\\\"
         type=\\\"range\\\"
         min=0
         max=100
         oninput=\\\"(function(e) {
             let action = '(IntLit ' + document.getElementById('slidyy').value + ')';
             window.trigger(";

let slidy_llu = "0";

let slidy_view_middle = ", action);
             return false;}) ();
             return false;\\\"
         value=";

let mk_slidy_slice_view2 = "\")(A Caret(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole string_of_int)(A Space(S(Var NotInHole NotInVarHole m)E)))))))(A Caret(S(StringLit NotInHole \"";

let slidy_view_last = ">
      </input>
    </span>";

let mk_slidy_slice_view3 = "\")E)))))))))E)))))";

let mk_slidy_slice_last = "(shape((ExpLine(OpSeq(Placeholder 0)(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(BoolLit NotInHole true)(A Comma(S(IntLit NotInHole 14)E)))))))E)))))(expand((ExpLine(OpSeq(Placeholder 0)(S(Lam NotInHole(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole m)E))((OpSeq(Placeholder 0)(S Int E)))((ExpLine(OpSeq(BinOp NotInHole Caret(BinOp NotInHole Caret(Placeholder 0)(Placeholder 1))(Placeholder 2))(S(StringLit NotInHole\"((ExpLine(OpSeq(Placeholder 0)(S(IntLit NotInHole \")(A Caret(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole string_of_int)(A Space(S(Var NotInHole NotInVarHole m)E)))))))(A Caret(S(StringLit NotInHole\")E))))\")E)))))))))E)))))))(ExpLine(OpSeq(Placeholder 0)(S(ApLivelit 0 NotInHole $slidy $slidy(IntLit 50)((next 0)(splice_map())(splice_order())))E))))";

/*
 This is the version with unique ids for divs; the display formatting breaks badly though.
 */
let slidy_string = "((LivelitDefLine((name(NotInVarHole $slidy))(expansion_type(OpSeq(Placeholder 0)(S Int E)))(captures((ExpLine(OpSeq(Placeholder 0)(S(ListNil NotInHole)E)))))(model_type(OpSeq(Placeholder 0)(S Int E)))(action_type(OpSeq(Placeholder 0)(S Int E)))(init((ExpLine(OpSeq(Placeholder 0)(S(IntLit NotInHole 50)E)))))(update((ExpLine(OpSeq(Placeholder 0)(S(Lam NotInHole(OpSeq(Placeholder 0)(S(Parenthesized(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole model)(A Comma(S(Var NotInHole NotInVarHole action)E)))))E))((OpSeq(Placeholder 0)(S(Parenthesized(OpSeq(BinOp NotInHole Prod(Placeholder 0)(Placeholder 1))(S Int(A Prod(S Int E)))))E)))((ExpLine(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole action)E)))))E)))))(view((ExpLine(OpSeq(Placeholder 0)(S(Lam NotInHole(OpSeq(Placeholder 0)(S(Parenthesized(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole id)(A Comma(S(Var NotInHole NotInVarHole model)E)))))E))((OpSeq(Placeholder 0)(S(Parenthesized(OpSeq(BinOp NotInHole Prod(Placeholder 0)(Placeholder 1))(S Int(A Prod(S Int E)))))E)))((ExpLine(OpSeq(BinOp NotInHole Caret(BinOp NotInHole Caret(BinOp NotInHole Caret(BinOp NotInHole Caret(BinOp NotInHole Caret(BinOp NotInHole Caret(BinOp NotInHole Caret(BinOp NotInHole Caret(Placeholder 0)(Placeholder 1))(Placeholder 2))(Placeholder 3))(Placeholder 4))(Placeholder 5))(Placeholder 6))(Placeholder 7))(Placeholder 8))(S(StringLit NotInHole \"
<span class='slider-livelit'>
   <input
     class='slider'
     id='\")(A Caret(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole string_of_int)(A Space(S(Var NotInHole NotInVarHole id)E)))))))(A Caret(S(StringLit NotInHole \"'
     type='range'
     min=0
     max=100
     oninput=\\\"(function(e) {
       let action = '(IntLit ' + document.getElementById('\")(A Caret(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole string_of_int)(A Space(S(Var NotInHole NotInVarHole id)E)))))))(A Caret(S(StringLit NotInHole\"').value + ')';
       window.trigger(\")(A Caret(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole string_of_int)(A Space(S(Var NotInHole NotInVarHole id)E)))))))(A Caret(S(StringLit NotInHole\", action);
       return false;}) ();
     return false;\\\"
     value=\")(A Caret(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole string_of_int)(A Space(S(Var NotInHole NotInVarHole model)E)))))))(A Caret(S(StringLit NotInHole\">\n
  </input>
</span>\"
)E)))))))))))))))))))))E)))))(shape((ExpLine(OpSeq(Placeholder 0)(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Comma(Placeholder 0)(Placeholder 1))(S(BoolLit NotInHole true)(A Comma(S(IntLit NotInHole 14)E)))))))E)))))(expand((ExpLine(OpSeq(Placeholder 0)(S(Lam NotInHole(OpSeq(Placeholder 0)(S(Var NotInHole NotInVarHole m)E))((OpSeq(Placeholder 0)(S Int E)))((ExpLine(OpSeq(BinOp NotInHole Caret(BinOp NotInHole Caret(Placeholder 0)(Placeholder 1))(Placeholder 2))(S(StringLit NotInHole\"((ExpLine(OpSeq(Placeholder 0)(S(IntLit NotInHole \")(A Caret(S(Parenthesized((ExpLine(OpSeq(BinOp NotInHole Space(Placeholder 0)(Placeholder 1))(S(Var NotInHole NotInVarHole string_of_int)(A Space(S(Var NotInHole NotInVarHole m)E)))))))(A Caret(S(StringLit NotInHole\")E))))\")E)))))))))E)))))))(ExpLine(OpSeq(Placeholder 0)(S(FreeLivelit 0 $slidy)E))))";

let ll_def_slider =
  mk_slidy_slice_first
  ++ mk_slidy_view
  ++ mk_slidy_slice_last
  |> Sexplib.Sexp.of_string
  |> UHExp.t_of_sexp;
//slidy_string

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
    |> add("ll_def_slider", ll_def_slider)
  );
let get = id => StringMap.find(id, examples);

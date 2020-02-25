module StringMap = Map.Make(String);
open Sexplib.Std;

let mk_OpSeq_typ = OpSeq.mk(~associate=Associator.Typ.associate);
let mk_OpSeq_pat = OpSeq.mk(~associate=Associator.Pat.associate);
let mk_OpSeq_exp = OpSeq.mk(~associate=Associator.Exp.associate);

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
  UHExp.Block.wrap'(Seq.mk(lam, [(UHExp.Space, arg)]) |> mk_OpSeq_exp);
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
            UHPat.(Seq.mk(var("y"), [(Cons, var("ys"))]) |> mk_OpSeq_pat),
            Block.wrap'(
              Seq.mk(
                Parenthesized(
                  Block.wrap'(
                    Seq.mk(var("f"), [(UHExp.Space, var("y"))])
                    |> mk_OpSeq_exp,
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
                        |> mk_OpSeq_exp,
                      ),
                    ),
                  ),
                ],
              )
              |> mk_OpSeq_exp,
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
          Seq.mk(
            UHTyp.Parenthesized(
              Seq.mk(UHTyp.Num, [(UHTyp.Arrow, Num)]) |> mk_OpSeq_typ,
            ),
            [
              (UHTyp.Arrow, List(OpSeq.wrap(UHTyp.Num))),
              (Arrow, List(OpSeq.wrap(UHTyp.Num))),
            ],
          )
          |> mk_OpSeq_typ,
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
            UHPat.(Seq.mk(var("z"), [(Cons, var("zs"))]) |> mk_OpSeq_pat),
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
                        |> mk_OpSeq_exp,
                      ),
                    ),
                  ),
                ],
              )
              |> mk_OpSeq_exp,
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
            Seq.mk(
              List(OpSeq.wrap(Num)),
              [
                (Arrow, List(OpSeq.wrap(Num))),
                (Arrow, List(OpSeq.wrap(Num))),
              ],
            )
            |> mk_OpSeq_typ
          ),
        Block.wrap(append_lam),
      )
    );

  let partition_case =
    UHExp.(
      case(
        Block.wrap(var("xs")),
        [
          Rule(
            OpSeq.wrap(UHPat.listnil()),
            Block.wrap(
              Parenthesized(
                Block.wrap'(
                  Seq.mk(listnil(), [(Comma, listnil())]) |> mk_OpSeq_exp,
                ),
              ),
            ),
          ),
          Rule(
            UHPat.(Seq.mk(var("y"), [(Cons, var("ys"))]) |> mk_OpSeq_pat),
            [
              letline(
                UHPat.(
                  OpSeq.wrap(
                    Parenthesized(
                      Seq.mk(var("ys1"), [(Comma, var("ys2"))])
                      |> mk_OpSeq_pat,
                    ),
                  )
                ),
                Block.wrap'(
                  Seq.mk(
                    var("partition"),
                    [(Space, var("f")), (Space, var("ys"))],
                  )
                  |> mk_OpSeq_exp,
                ),
              ),
              ExpLine(
                case(
                  Block.wrap'(
                    Seq.mk(var("f"), [(Space, var("y"))]) |> mk_OpSeq_exp,
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
                            |> mk_OpSeq_exp,
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
                            |> mk_OpSeq_exp,
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
            Seq.mk(
              Parenthesized(Seq.mk(Num, [(Arrow, Bool)]) |> mk_OpSeq_typ),
              [
                (Arrow, List(OpSeq.wrap(Num))),
                (
                  Arrow,
                  Parenthesized(
                    Seq.mk(
                      List(OpSeq.wrap(Num)),
                      [(Prod, List(OpSeq.wrap(Num)))],
                    )
                    |> mk_OpSeq_typ,
                  ),
                ),
              ],
            )
            |> mk_OpSeq_typ
          ),
        Block.wrap(partition_lam),
      )
    );

  let qsort_line =
    UHExp.(
      ExpLine(
        Seq.mk(
          var("qsort"),
          [
            (
              Space,
              Parenthesized(
                Block.wrap'(
                  Seq.mk(
                    numlit(4),
                    [
                      (Cons, numlit(2)),
                      (Cons, numlit(6)),
                      (Cons, numlit(5)),
                      (Cons, numlit(3)),
                      (Cons, numlit(1)),
                      (Cons, numlit(7)),
                      (Cons, listnil()),
                    ],
                  )
                  |> mk_OpSeq_exp,
                ),
              ),
            ),
          ],
        )
        |> mk_OpSeq_exp,
      )
    );

  UHExp.[append_letline, EmptyLine, partition_letline, EmptyLine, qsort_line];
};

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
  );
let get = id => StringMap.find(id, examples);

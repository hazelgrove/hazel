module StringMap = Map.Make(String);
open Sexplib.Std;

let mk_OpSeq_typ = OpSeq.mk(~associate=Associator.associate_ty);
let mk_OpSeq_pat = OpSeq.mk(~associate=Associator.associate_pat);
let mk_OpSeq_exp = OpSeq.mk(~associate=Associator.associate_exp);

let just_hole: UHExp.t = E0(EmptyHole(0));

let holey_lambda: UHExp.t = {
  let lam =
    UHExp.(
      Parenthesized(
        E0(
          lam(
            UHPat.P0(EmptyHole(0)),
            ~ann=UHTyp.T0(Hole),
            E0(UHExp.EmptyHole(1)),
          ),
        ),
      )
    );
  let arg = UHExp.EmptyHole(2);
  E1(Seq.mk(lam, [(UHExp.Space, arg)]) |> mk_OpSeq_exp);
};

let let_line: UHExp.t =
  UHExp.(
    E2([
      letline(UHPat.(P0(var("y"))), E0(EmptyHole(0))),
      EmptyLine,
      letline(UHPat.(P0(var("x"))), E0(EmptyHole(1))),
      ExpLine(var("x") |> OpSeq.wrap),
      ExpLine(var("y") |> OpSeq.wrap),
    ])
  );

let map_example: UHExp.t = {
  let case_node =
    UHExp.(
      case(
        E0(var("xs")),
        [
          Rule(UHPat.(P0(listnil())), E0(listnil())),
          Rule(
            UHPat.(
              P1(Seq.mk(var("y"), [(Cons, var("ys"))]) |> mk_OpSeq_pat)
            ),
            E1(
              Seq.mk(
                Parenthesized(
                  E1(
                    Seq.mk(var("f"), [(UHExp.Space, var("y"))])
                    |> mk_OpSeq_exp,
                  ),
                ),
                [
                  (
                    Cons,
                    Parenthesized(
                      E1(
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
        UHPat.(P0(var("f"))),
        E0(lam(UHPat.(P0(var("xs"))), E0(case_node))),
      )
    );
  let letline_node =
    UHExp.(
      letline(
        UHPat.(P0(var("map"))),
        ~ann=
          T1(
            Seq.mk(
              UHTyp.Parenthesized(
                T1(
                  Seq.mk(UHTyp.Num, [(UHTyp.Arrow, Num)]) |> mk_OpSeq_typ,
                ),
              ),
              [
                (UHTyp.Arrow, List(UHTyp.T0(Num))),
                (Arrow, List(UHTyp.T0(Num))),
              ],
            )
            |> mk_OpSeq_typ,
          ),
        E0(lam_node),
      )
    );
  UHExp.(E2([letline_node, ExpLine(EmptyHole(0) |> OpSeq.wrap)]));
};

let qsort_example: UHExp.t = {
  let append_case =
    UHExp.(
      case(
        E0(var("xs")),
        [
          Rule(UHPat.(P0(listnil())), E0(var("ys"))),
          Rule(
            UHPat.(
              P1(Seq.mk(var("z"), [(Cons, var("zs"))]) |> mk_OpSeq_pat)
            ),
            E1(
              Seq.mk(
                var("z"),
                [
                  (
                    Cons,
                    Parenthesized(
                      E1(
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
        UHPat.(P0(var("xs"))),
        E0(lam(UHPat.(P0(var("ys"))), E0(append_case))),
      )
    );
  let append_letline =
    UHExp.(
      letline(
        UHPat.(P0(var("append"))),
        ~ann=
          UHTyp.(
            T1(
              Seq.mk(
                List(T0(Num)),
                [(Arrow, List(T0(Num))), (Arrow, List(T0(Num)))],
              )
              |> mk_OpSeq_typ,
            )
          ),
        E0(append_lam),
      )
    );

  let partition_case =
    UHExp.(
      case(
        E0(var("xs")),
        [
          Rule(
            UHPat.(P0(listnil())),
            E0(
              Parenthesized(
                E1(
                  Seq.mk(listnil(), [(Comma, listnil())]) |> mk_OpSeq_exp,
                ),
              ),
            ),
          ),
          Rule(
            UHPat.(
              P1(Seq.mk(var("y"), [(Cons, var("ys"))]) |> mk_OpSeq_pat)
            ),
            E2([
              letline(
                UHPat.(
                  P0(
                    Parenthesized(
                      P1(
                        Seq.mk(var("ys1"), [(Comma, var("ys2"))])
                        |> mk_OpSeq_pat,
                      ),
                    ),
                  )
                ),
                E1(
                  Seq.mk(
                    var("partition"),
                    [(Space, var("f")), (Space, var("ys"))],
                  )
                  |> mk_OpSeq_exp,
                ),
              ),
              ExpLine(
                case(
                  E1(
                    Seq.mk(var("f"), [(Space, var("y"))]) |> mk_OpSeq_exp,
                  ),
                  [
                    Rule(
                      UHPat.(P0(boollit(true))),
                      E0(
                        Parenthesized(
                          E1(
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
                      UHPat.(P0(boollit(false))),
                      E0(
                        Parenthesized(
                          E1(
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
            ]),
          ),
        ],
      )
    );
  let partition_lam =
    UHExp.(
      lam(
        UHPat.(P0(var("f"))),
        E0(lam(UHPat.(P0(var("xs"))), E0(partition_case))),
      )
    );
  let partition_letline =
    UHExp.(
      letline(
        UHPat.(P0(var("partition"))),
        ~ann=
          UHTyp.(
            T1(
              Seq.mk(
                Parenthesized(
                  T1(Seq.mk(Num, [(Arrow, Bool)]) |> mk_OpSeq_typ),
                ),
                [
                  (Arrow, List(T0(Num))),
                  (
                    Arrow,
                    Parenthesized(
                      T1(
                        Seq.mk(List(T0(Num)), [(Prod, List(T0(Num)))])
                        |> mk_OpSeq_typ,
                      ),
                    ),
                  ),
                ],
              )
              |> mk_OpSeq_typ,
            )
          ),
        E0(partition_lam),
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
                E1(
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

  UHExp.(
    E2([append_letline, EmptyLine, partition_letline, EmptyLine, qsort_line])
  );
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

module StringMap = Map.Make(String);
open OpSeqUtil;
open Sexplib.Std;

let just_hole: UHExp.block = UHExp.wrap_in_block(EmptyHole(0));

let holey_lambda: UHExp.block = {
  let lam =
    UHExp.(
      Parenthesized(
        wrap_in_block(
          lam(EmptyHole(0), ~ann=Hole, wrap_in_block(EmptyHole(1))),
        ),
      )
    );
  let arg = UHExp.EmptyHole(2);
  let seq = Seq.ExpOpExp(lam, UHExp.Space, arg);
  let skel = Associator.associate_exp(seq);
  UHExp.wrap_in_block(OpSeq(skel, seq));
};

let let_line: UHExp.block =
  UHExp.(
    Block(
      [
        letline(UHPat.var("y"), wrap_in_block(EmptyHole(0))),
        EmptyLine,
        letline(UHPat.var("x"), wrap_in_block(EmptyHole(1))),
        ExpLine(var("x")),
      ],
      var("y"),
    )
  );

let map_example: UHExp.block = {
  open OpSeq;
  let case_node =
    UHExp.(
      case(
        wrap_in_block(var("xs")),
        [
          Rule(UHPat.listnil(), wrap_in_block(listnil())),
          Rule(
            Pat.mk_OpSeq(ExpOpExp(UHPat.var("y"), Cons, UHPat.var("ys"))),
            wrap_in_block(
              Exp.mk_OpSeq(
                ExpOpExp(
                  Parenthesized(
                    wrap_in_block(
                      Exp.mk_OpSeq(ExpOpExp(var("f"), Space, var("y"))),
                    ),
                  ),
                  Cons,
                  Parenthesized(
                    wrap_in_block(
                      Exp.mk_OpSeq(
                        SeqOpExp(
                          ExpOpExp(var("map"), Space, var("f")),
                          Space,
                          var("ys"),
                        ),
                      ),
                    ),
                  ),
                ),
              ),
            ),
          ),
        ],
      )
    );
  let lam_node =
    UHExp.(
      lam(
        UHPat.var("f"),
        wrap_in_block(lam(UHPat.var("xs"), wrap_in_block(case_node))),
      )
    );
  let letline_node =
    UHExp.(
      letline(
        UHPat.var("map"),
        ~ann=
          Typ.mk_OpSeq(
            SeqOpExp(
              ExpOpExp(
                Parenthesized(Typ.mk_OpSeq(ExpOpExp(Num, Arrow, Num))),
                Arrow,
                List(Num),
              ),
              Arrow,
              List(Num),
            ),
          ),
        wrap_in_block(lam_node),
      )
    );
  UHExp.Block([letline_node], EmptyHole(0));
};

let qsort_example: UHExp.block = {
  let append_case =
    UHExp.(
      case(
        wrap_in_block(var("xs")),
        [
          Rule(UHPat.listnil(), wrap_in_block(var("ys"))),
          Rule(
            Pat.mk_OpSeq(ExpOpExp(UHPat.var("z"), Cons, UHPat.var("zs"))),
            wrap_in_block(
              Exp.mk_OpSeq(
                ExpOpExp(
                  var("z"),
                  Cons,
                  Parenthesized(
                    wrap_in_block(
                      Exp.mk_OpSeq(
                        SeqOpExp(
                          ExpOpExp(var("append"), Space, var("zs")),
                          Space,
                          var("ys"),
                        ),
                      ),
                    ),
                  ),
                ),
              ),
            ),
          ),
        ],
      )
    );
  let append_lam =
    UHExp.(
      lam(
        UHPat.var("xs"),
        wrap_in_block(lam(UHPat.var("ys"), wrap_in_block(append_case))),
      )
    );
  let append_letline =
    UHExp.(
      letline(
        UHPat.var("append"),
        ~ann=
          Typ.mk_OpSeq(
            SeqOpExp(
              ExpOpExp(List(Num), Arrow, List(Num)),
              Arrow,
              List(Num),
            ),
          ),
        wrap_in_block(append_lam),
      )
    );

  let partition_case =
    UHExp.(
      case(
        wrap_in_block(var("xs")),
        [
          Rule(
            UHPat.listnil(),
            wrap_in_block(
              Parenthesized(
                wrap_in_block(
                  Exp.mk_OpSeq(ExpOpExp(listnil(), Comma, listnil())),
                ),
              ),
            ),
          ),
          Rule(
            Pat.mk_OpSeq(ExpOpExp(UHPat.var("y"), Cons, UHPat.var("ys"))),
            Block(
              [
                letline(
                  Parenthesized(
                    Pat.mk_OpSeq(
                      ExpOpExp(UHPat.var("ys1"), Comma, UHPat.var("ys2")),
                    ),
                  ),
                  wrap_in_block(
                    Exp.mk_OpSeq(
                      SeqOpExp(
                        ExpOpExp(var("partition"), Space, var("f")),
                        Space,
                        var("ys"),
                      ),
                    ),
                  ),
                ),
              ],
              case(
                wrap_in_block(
                  Exp.mk_OpSeq(ExpOpExp(var("f"), Space, var("y"))),
                ),
                [
                  Rule(
                    UHPat.boollit(true),
                    wrap_in_block(
                      Parenthesized(
                        wrap_in_block(
                          Exp.mk_OpSeq(
                            SeqOpExp(
                              ExpOpExp(var("y"), Cons, var("ys1")),
                              Comma,
                              var("ys2"),
                            ),
                          ),
                        ),
                      ),
                    ),
                  ),
                  Rule(
                    UHPat.boollit(false),
                    wrap_in_block(
                      Parenthesized(
                        wrap_in_block(
                          Exp.mk_OpSeq(
                            SeqOpExp(
                              ExpOpExp(var("ys1"), Comma, var("y")),
                              Cons,
                              var("ys2"),
                            ),
                          ),
                        ),
                      ),
                    ),
                  ),
                ],
              ),
            ),
          ),
        ],
      )
    );
  let partition_lam =
    UHExp.(
      lam(
        UHPat.var("f"),
        wrap_in_block(lam(UHPat.var("xs"), wrap_in_block(partition_case))),
      )
    );
  let partition_letline =
    UHExp.(
      letline(
        UHPat.var("partition"),
        ~ann=
          Typ.mk_OpSeq(
            SeqOpExp(
              ExpOpExp(
                Parenthesized(Typ.mk_OpSeq(ExpOpExp(Num, Arrow, Bool))),
                Arrow,
                List(Num),
              ),
              Arrow,
              Parenthesized(
                Typ.mk_OpSeq(ExpOpExp(List(Num), Prod, List(Num))),
              ),
            ),
          ),
        wrap_in_block(partition_lam),
      )
    );

  let qsort_line =
    UHExp.(
      Exp.mk_OpSeq(
        ExpOpExp(
          var("qsort"),
          Space,
          Parenthesized(
            wrap_in_block(
              Exp.mk_OpSeq(
                SeqOpExp(
                  SeqOpExp(
                    SeqOpExp(
                      SeqOpExp(
                        SeqOpExp(
                          SeqOpExp(
                            ExpOpExp(numlit(4), Cons, numlit(2)),
                            Cons,
                            numlit(6),
                          ),
                          Cons,
                          numlit(5),
                        ),
                        Cons,
                        numlit(3),
                      ),
                      Cons,
                      numlit(1),
                    ),
                    Cons,
                    numlit(7),
                  ),
                  Cons,
                  listnil(),
                ),
              ),
            ),
          ),
        ),
      )
    );

  Block(
    [append_letline, EmptyLine, partition_letline, EmptyLine],
    qsort_line,
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

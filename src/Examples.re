open OpSeqUtil;

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
  let seq = OperatorSeq.ExpOpExp(lam, UHExp.Space, arg);
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
  open OperatorSeq;
  let case_node =
    UHExp.(
      case(
        wrap_in_block(var("xs")),
        [
          Rule(UHPat.listnil(), wrap_in_block(listnil())),
          Rule(
            opseq_pat(ExpOpExp(UHPat.var("y"), Cons, UHPat.var("ys"))),
            wrap_in_block(
              opseq_exp(
                ExpOpExp(
                  Parenthesized(
                    wrap_in_block(
                      opseq_exp(ExpOpExp(var("f"), Space, var("y"))),
                    ),
                  ),
                  Cons,
                  Parenthesized(
                    wrap_in_block(
                      opseq_exp(
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
        (),
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
          opseq_typ(
            SeqOpExp(
              ExpOpExp(
                Parenthesized(opseq_typ(ExpOpExp(Num, Arrow, Num))),
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
            opseq_pat(ExpOpExp(UHPat.var("z"), Cons, UHPat.var("zs"))),
            wrap_in_block(
              opseq_exp(
                ExpOpExp(
                  var("z"),
                  Cons,
                  Parenthesized(
                    wrap_in_block(
                      opseq_exp(
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
        (),
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
          opseq_typ(
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
                  opseq_exp(ExpOpExp(listnil(), Comma, listnil())),
                ),
              ),
            ),
          ),
          Rule(
            opseq_pat(ExpOpExp(UHPat.var("y"), Cons, UHPat.var("ys"))),
            Block(
              [
                letline(
                  Parenthesized(
                    opseq_pat(
                      ExpOpExp(UHPat.var("ys1"), Comma, UHPat.var("ys2")),
                    ),
                  ),
                  wrap_in_block(
                    opseq_exp(
                      SeqOpExp(
                        ExpOpExp(var("partition"), Space, var("ys")),
                        Space,
                        var("ys"),
                      ),
                    ),
                  ),
                ),
              ],
              case(
                wrap_in_block(
                  opseq_exp(ExpOpExp(var("f"), Space, var("y"))),
                ),
                [
                  Rule(
                    UHPat.boollit(true),
                    wrap_in_block(
                      Parenthesized(
                        wrap_in_block(
                          opseq_exp(
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
                          opseq_exp(
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
                (),
              ),
            ),
          ),
        ],
        (),
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
          opseq_typ(
            SeqOpExp(
              ExpOpExp(
                Parenthesized(opseq_typ(ExpOpExp(Num, Arrow, Bool))),
                Arrow,
                List(Num),
              ),
              Arrow,
              Parenthesized(
                opseq_typ(ExpOpExp(List(Num), Arrow, List(Num))),
              ),
            ),
          ),
        wrap_in_block(partition_lam),
      )
    );

  let qsort_line =
    UHExp.(
      opseq_exp(
        ExpOpExp(
          var("qsort"),
          Space,
          Parenthesized(
            wrap_in_block(
              opseq_exp(
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

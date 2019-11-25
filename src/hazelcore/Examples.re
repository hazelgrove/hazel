module StringMap = Map.Make(String);
open Sexplib.Std;

let mk_OpSeq_typ = OpSeq.mk(~associate=Associator.associate_ty);
let mk_OpSeq_pat = OpSeq.mk(~associate=Associator.associate_pat);
let mk_OpSeq_exp = OpSeq.mk(~associate=Associator.associate_exp);

let just_hole: UHExp.block =
  UHExp.EmptyHole(0) |> OpSeq.wrap |> UHExp.wrap_in_block;

let opseq = OpSeq.wrap;
let block = (operand: UHExp.operand) =>
  operand |> OpSeq.wrap |> UHExp.wrap_in_block;

let holey_lambda: UHExp.block = {
  let lam =
    UHExp.(
      Parenthesized(
        lam(
          UHPat.EmptyHole(0) |> OpSeq.wrap,
          ~ann=UHTyp.Hole |> OpSeq.wrap,
          UHExp.EmptyHole(1) |> OpSeq.wrap |> wrap_in_block,
        )
        |> OpSeq.wrap
        |> wrap_in_block,
      )
    );
  let arg = UHExp.EmptyHole(2);
  Seq.mk(lam, [(UHExp.Space, arg)]) |> mk_OpSeq_exp |> UHExp.wrap_in_block;
};

let let_line: UHExp.block =
  UHExp.[
    letline(UHPat.var("y") |> opseq, EmptyHole(0) |> block),
    EmptyLine,
    letline(UHPat.var("x") |> opseq, EmptyHole(1) |> block),
    ExpLine(var("x") |> opseq),
    ExpLine(var("y") |> opseq),
  ];

let map_example: UHExp.block = {
  let case_node =
    UHExp.(
      case(
        var("xs") |> block,
        [
          Rule(UHPat.listnil() |> opseq, listnil() |> block),
          Rule(
            Seq.mk(UHPat.var("y"), [(UHPat.Cons, UHPat.var("ys"))])
            |> mk_OpSeq_pat,
            Seq.mk(
              Parenthesized(
                Seq.mk(var("f"), [(UHExp.Space, var("y"))])
                |> mk_OpSeq_exp
                |> wrap_in_block,
              ),
              [
                (
                  Cons,
                  Parenthesized(
                    Seq.mk(
                      var("map"),
                      [(Space, var("f")), (Space, var("ys"))],
                    )
                    |> mk_OpSeq_exp
                    |> wrap_in_block,
                  ),
                ),
              ],
            )
            |> mk_OpSeq_exp
            |> wrap_in_block,
          ),
        ],
      )
    );
  let lam_node =
    UHExp.(
      lam(
        UHPat.var("f") |> opseq,
        lam(UHPat.var("xs") |> opseq, case_node |> block) |> block,
      )
    );
  let letline_node =
    UHExp.(
      letline(
        UHPat.var("map") |> opseq,
        ~ann=
          Seq.mk(
            UHTyp.Parenthesized(
              Seq.mk(UHTyp.Num, [(UHTyp.Arrow, Num)]) |> mk_OpSeq_typ,
            ),
            [
              (UHTyp.Arrow, List(UHTyp.Num |> opseq)),
              (Arrow, List(UHTyp.Num |> opseq)),
            ],
          )
          |> mk_OpSeq_typ,
        lam_node |> block,
      )
    );
  UHExp.[letline_node, ExpLine(EmptyHole(0) |> opseq)];
};

let qsort_example: UHExp.block = {
  let append_case =
    UHExp.(
      case(
        var("xs") |> block,
        [
          Rule(UHPat.listnil() |> opseq, var("ys") |> block),
          Rule(
            Seq.mk(UHPat.var("z"), [(UHPat.Cons, UHPat.var("zs"))])
            |> mk_OpSeq_pat,
            Seq.mk(
              var("z"),
              [
                (
                  Cons,
                  Parenthesized(
                    Seq.mk(
                      var("append"),
                      [(Space, var("zs")), (Space, var("ys"))],
                    )
                    |> mk_OpSeq_exp
                    |> wrap_in_block,
                  ),
                ),
              ],
            )
            |> mk_OpSeq_exp
            |> wrap_in_block,
          ),
        ],
      )
    );
  let append_lam =
    UHExp.(
      lam(
        UHPat.var("xs") |> opseq,
        lam(UHPat.var("ys") |> opseq, append_case |> block) |> block,
      )
    );
  let append_letline =
    UHExp.(
      letline(
        UHPat.var("append") |> opseq,
        ~ann=
          Seq.mk(
            UHTyp.List(UHTyp.Num |> opseq),
            [
              (UHTyp.Arrow, List(UHTyp.Num |> opseq)),
              (Arrow, List(UHTyp.Num |> opseq)),
            ],
          )
          |> mk_OpSeq_typ,
        append_lam |> block,
      )
    );

  let partition_case =
    UHExp.(
      case(
        var("xs") |> block,
        [
          Rule(
            UHPat.listnil() |> opseq,
            Parenthesized(
              Seq.mk(listnil(), [(Comma, listnil())])
              |> mk_OpSeq_exp
              |> wrap_in_block,
            )
            |> block,
          ),
          Rule(
            Seq.mk(UHPat.var("y"), [(UHPat.Cons, UHPat.var("ys"))])
            |> mk_OpSeq_pat,
            [
              letline(
                UHPat.Parenthesized(
                  Seq.mk(
                    UHPat.var("ys1"),
                    [(UHPat.Comma, UHPat.var("ys2"))],
                  )
                  |> mk_OpSeq_pat,
                )
                |> opseq,
                Seq.mk(
                  var("partition"),
                  [(Space, var("f")), (Space, var("ys"))],
                )
                |> mk_OpSeq_exp
                |> wrap_in_block,
              ),
              ExpLine(
                case(
                  Seq.mk(var("f"), [(Space, var("y"))])
                  |> mk_OpSeq_exp
                  |> wrap_in_block,
                  [
                    Rule(
                      UHPat.boollit(true) |> opseq,
                      Parenthesized(
                        Seq.mk(
                          var("y"),
                          [(Cons, var("ys1")), (Comma, var("ys2"))],
                        )
                        |> mk_OpSeq_exp
                        |> wrap_in_block,
                      )
                      |> block,
                    ),
                    Rule(
                      UHPat.boollit(false) |> opseq,
                      Parenthesized(
                        Seq.mk(
                          var("ys1"),
                          [(Comma, var("y")), (Cons, var("ys2"))],
                        )
                        |> mk_OpSeq_exp
                        |> wrap_in_block,
                      )
                      |> block,
                    ),
                  ],
                )
                |> opseq,
              ),
            ],
          ),
        ],
      )
    );
  let partition_lam =
    UHExp.(
      lam(
        UHPat.var("f") |> opseq,
        lam(UHPat.var("xs") |> opseq, partition_case |> block) |> block,
      )
    );
  let partition_letline =
    UHExp.(
      letline(
        UHPat.var("partition") |> opseq,
        ~ann=
          Seq.mk(
            UHTyp.Parenthesized(
              Seq.mk(UHTyp.Num, [(UHTyp.Arrow, Bool)]) |> mk_OpSeq_typ,
            ),
            [
              (UHTyp.Arrow, List(UHTyp.Num |> opseq)),
              (
                Arrow,
                Parenthesized(
                  Seq.mk(
                    UHTyp.List(UHTyp.Num |> opseq),
                    [(UHTyp.Prod, List(UHTyp.Num |> opseq))],
                  )
                  |> mk_OpSeq_typ,
                ),
              ),
            ],
          )
          |> mk_OpSeq_typ,
        partition_lam |> block,
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
                |> mk_OpSeq_exp
                |> wrap_in_block,
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

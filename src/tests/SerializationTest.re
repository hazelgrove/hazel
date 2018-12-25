open OUnit2;
open Printf;
open SemanticsCore;
let nih = tm => UHExp.Tm(NotInHole, tm);
let nihVar = s => nih(UHExp.Var(NotInVHole, s));
let var' = nihVar("v");
let varN' = n => nihVar(sprintf("v%d", n));
type ezOpTree('op, 'v) =
  | Op('op, ezOpTree('op, 'v), ezOpTree('op, 'v))
  | V('v);
type seqInfo('op, 'v) =
  | Empty
  | LoneTm('v)
  | FullSeq(OperatorSeq.opseq('v, 'op));
let updateSeq = (mSeq, optOp, tm) =>
  switch (mSeq, optOp) {
  | (Empty, None) => LoneTm(tm)
  | (LoneTm(tm1), Some(op)) => FullSeq(OperatorSeq.ExpOpExp(tm1, op, tm))
  | (FullSeq(seq), Some(op)) => FullSeq(OperatorSeq.SeqOpExp(seq, op, tm))
  | _ => assert_failure("Invalid seqInfo state for updateSeq")
  };
let skelAndSeqOfEZOpTree = eztree => {
  let rec skelAndSeqOfEZOpTree' = (eztree', n, mSeq, mOp) =>
    switch (eztree') {
    | Op(op, l, r) =>
      let (lSkel, lSeq, n') = skelAndSeqOfEZOpTree'(l, n, mSeq, mOp);
      let (rSkel, rSeq, n'') = skelAndSeqOfEZOpTree'(r, n', lSeq, Some(op));
      (Skel.BinOp(NotInHole, op, lSkel, rSkel), rSeq, n'');
    | V(v) => (Skel.Placeholder(n), updateSeq(mSeq, mOp, v), n + 1)
    };

  switch (skelAndSeqOfEZOpTree'(eztree, 0, Empty, None)) {
  | (skel, FullSeq(seq), _) => (skel, seq)
  | _ =>
    assert_failure(
      "skelOfEZOpTree must return a FullSeq, not any other type of seqInfo",
    )
  };
};

let tyOpSeqOfEZOpTree = eztree => {
  let (skel, seq) = skelAndSeqOfEZOpTree(eztree);
  UHTyp.OpSeq(skel, seq);
};

let expOpSeqOfEZOpTree = eztree => {
  let (skel, seq) = skelAndSeqOfEZOpTree(eztree);
  nih(UHExp.OpSeq(skel, seq));
};
let rec contextualize' = (start, end', uhexp) =>
  if (start < end') {
    let uhexp' = contextualize'(start + 1, end', uhexp);
    UHExp.(
      nih(Let(sprintf("v%d", start), nih(EmptyHole(start)), uhexp'))
    );
  } else {
    uhexp;
  };
let contextualize = (end', uhexp) => contextualize'(0, end', uhexp);
let contextualize_v = uhexp =>
  UHExp.(nih(Let("v", nih(EmptyHole(0)), uhexp)));

let (|=>) = (name, uhexp) => (name, uhexp, None);
let (>=>) = ((inName, uhexp, _), outName) => (
  inName,
  uhexp,
  Some(outName),
);
let (<=>) = (name, uhexp) => (name, uhexp, Some(name));
let (>:::%) = (group_name, checks) =>
  group_name
  >::: List.map(
         ((inName, uhexp, outNameOpt)) =>
           group_name
           ++ "/"
           ++ inName
           >:: (
             _ => {
               let getContents = name => {
                 let tf =
                   open_in(
                     "tests/test_data/"
                     ++ group_name
                     ++ "/"
                     ++ name
                     ++ ".hazel",
                   );

                 let filecontents =
                   really_input_string(tf, in_channel_length(tf));
                 close_in(tf);
                 filecontents;
               };
               let inContents = getContents(inName);
               assert_equal(
                 ~msg="deserialization failed",
                 uhexp,
                 Deserialize.uhexp_of_string(inContents),
               );
               switch (outNameOpt) {
               | Some(outName) =>
                 let outContents =
                   inName == outName ? inContents : getContents(outName);
                 assert_equal(
                   ~printer=x => x,
                   ~msg="serialization failed",
                   outContents,
                   Serialize.string_of_uhexp(uhexp),
                 );
               | None => ()
               };
             }
           ),
         checks,
       );

let tests =
  "Serialization tests"
  >::: [
    "testAscParens"
    >:::% [
      "AutoParens"
      |=> contextualize_v(
            nih(
              UHExp.(
                UHTyp.(
                  Asc(
                    Parenthesized(
                      nih(Asc(Parenthesized(nih(Asc(var', Num))), Hole)),
                    ),
                    Num,
                  )
                )
              ),
            ),
          )
      >=> "AutoParensOut",
      "ExplicitParens"
      <=> contextualize_v(
            nih(
              UHExp.(
                Asc(
                  var',
                  UHTyp.(
                    tyOpSeqOfEZOpTree(
                      Op(
                        Arrow,
                        V(Parenthesized(Num)),
                        V(
                          Parenthesized(
                            Parenthesized(
                              tyOpSeqOfEZOpTree(Op(Sum, V(Hole), V(Hole))),
                            ),
                          ),
                        ),
                      ),
                    )
                  ),
                )
              ),
            ),
          ),
    ],
    "testAscHole"
    >:::% ["Basic" <=> contextualize_v(nih(UHExp.(Asc(var', UHTyp.Hole))))],
    "testAscNum"
    >:::% ["Basic" <=> contextualize_v(nih(UHExp.(Asc(var', UHTyp.Num))))],
    "testAscOpSeq"
    >:::% [
      "BasicArrow"
      <=> contextualize_v(
            nih(
              UHExp.(
                Asc(
                  var',
                  tyOpSeqOfEZOpTree(UHTyp.(Op(Arrow, V(Num), V(Hole)))),
                )
              ),
            ),
          ),
      "BasicSum"
      <=> contextualize_v(
            nih(
              UHExp.(
                Asc(
                  var',
                  tyOpSeqOfEZOpTree(UHTyp.(Op(Sum, V(Num), V(Num)))),
                )
              ),
            ),
          ),
      "DeepArrow"
      <=> contextualize_v(
            nih(
              UHExp.(
                Asc(
                  var',
                  tyOpSeqOfEZOpTree(
                    UHTyp.(
                      Op(Arrow, V(Hole), Op(Arrow, V(Hole), V(Hole)))
                    ),
                  ),
                )
              ),
            ),
          ),
      "DeepSum"
      <=> contextualize_v(
            nih(
              UHExp.(
                Asc(
                  var',
                  tyOpSeqOfEZOpTree(
                    UHTyp.(Op(Sum, V(Hole), Op(Sum, V(Num), V(Hole)))),
                  ),
                )
              ),
            ),
          ),
      "PrecedenceLeft"
      <=> contextualize_v(
            nih(
              UHExp.(
                Asc(
                  var',
                  tyOpSeqOfEZOpTree(
                    UHTyp.(Op(Arrow, Op(Sum, V(Hole), V(Num)), V(Num))),
                  ),
                )
              ),
            ),
          ),
      "PrecedenceRight"
      <=> contextualize_v(
            nih(
              UHExp.(
                Asc(
                  var',
                  tyOpSeqOfEZOpTree(
                    UHTyp.(Op(Arrow, V(Hole), Op(Sum, V(Num), V(Num)))),
                  ),
                )
              ),
            ),
          ),
      "Thorough"
      <=> contextualize_v(
            nih(
              UHExp.(
                Asc(
                  var',
                  tyOpSeqOfEZOpTree(
                    UHTyp.(
                      Op(
                        Arrow,
                        Op(Sum, V(Num), V(Num)),
                        Op(
                          Arrow,
                          Op(Sum, V(Hole), Op(Sum, V(Num), V(Hole))),
                          V(Num),
                        ),
                      )
                    ),
                  ),
                )
              ),
            ),
          ),
    ],
    "testCase"
    >:::% [
      "Basic"
      <=> nih(
            UHExp.(
              Asc(
                Parenthesized(
                  contextualize_v(
                    nih(
                      Case(var', ("l", nihVar("l")), ("r", nihVar("r"))),
                    ),
                  ),
                ),
                UHTyp.Hole,
              )
            ),
          ),
      "Deep"
      <=> nih(
            UHExp.(
              Asc(
                Parenthesized(
                  contextualize_v(
                    nih(
                      Case(
                        var',
                        (
                          "l",
                          nih(
                            Case(
                              nihVar("l"),
                              ("ll", nihVar("ll")),
                              ("lr", nihVar("lr")),
                            ),
                          ),
                        ),
                        (
                          "r",
                          nih(
                            Case(
                              nihVar("r"),
                              ("rl", nihVar("rl")),
                              ("rr", nihVar("rr")),
                            ),
                          ),
                        ),
                      ),
                    ),
                  ),
                ),
                UHTyp.Hole,
              )
            ),
          ),
      "WithAsc"
      <=> nih(
            UHExp.(
              Asc(
                Parenthesized(
                  contextualize_v(
                    nih(
                      Case(
                        var',
                        ("l", nihVar("l")),
                        ("r", nih(Asc(nihVar("r"), UHTyp.Num))),
                      ),
                    ),
                  ),
                ),
                UHTyp.Num,
              )
            ),
          ),
    ],
    "testEmptyHole" >:::% ["Basic" <=> nih(UHExp.(EmptyHole(0)))],
    "testInjLeft"
    >:::% [
      "Basic" <=> contextualize_v(nih(UHExp.(Inj(L, var')))),
      "Deep" <=> contextualize_v(nih(UHExp.(Inj(L, nih(Inj(L, var')))))),
    ],
    "testInjRight"
    >:::% [
      "Basic" <=> contextualize_v(nih(UHExp.(Inj(R, var')))),
      "Deep" <=> contextualize_v(nih(UHExp.(Inj(R, nih(Inj(L, var')))))),
    ],
    "testLam"
    >:::% [
      "Basic" <=> contextualize_v(nih(UHExp.(Lam("a", var')))),
      "Deep" <=> nih(UHExp.(Lam("a", nih(Lam("b", nihVar("b")))))),
      "Shadow" <=> nih(UHExp.(Lam("a", nih(Lam("a", nihVar("a")))))),
    ],
    "testLet"
    >:::% [
      "Basic" <=> nih(UHExp.(Let("x", nih(EmptyHole(0)), nihVar("x")))),
      "Deep"
      <=> nih(
            UHExp.(
              Let(
                "a",
                nih(Let("b", nih(EmptyHole(0)), nihVar("b"))),
                nih(Let("c", nihVar("a"), nihVar("c"))),
              )
            ),
          ),
      "Nested"
      <=> nih(
            UHExp.(
              Let(
                "x",
                nih(EmptyHole(0)),
                nih(Let("y", nih(EmptyHole(1)), nihVar("x"))),
              )
            ),
          ),
      "Shadowing"
      <=> nih(
            UHExp.(
              Let(
                "a",
                nih(EmptyHole(0)),
                nih(Let("a", nihVar("a"), nihVar("a"))),
              )
            ),
          ),
    ],
    "testNumLit"
    >:::% [
      "Zero" <=> nih(UHExp.NumLit(0)),
      "One" <=> nih(UHExp.NumLit(1)),
      "Twelve" <=> nih(UHExp.NumLit(12)),
    ],
    "testOpSeq"
    >:::% [
      "BasicPlus"
      <=> contextualize(
            2,
            UHExp.(
              expOpSeqOfEZOpTree(Op(Plus, V(varN'(0)), V(varN'(1))))
            ),
          ),
      "BasicSpace"
      <=> contextualize(
            2,
            UHExp.(
              expOpSeqOfEZOpTree(Op(Space, V(varN'(0)), V(varN'(1))))
            ),
          ),
      "BasicTimes"
      <=> contextualize(
            2,
            UHExp.(
              expOpSeqOfEZOpTree(Op(Times, V(varN'(0)), V(varN'(1))))
            ),
          ),
      "DeepPlus"
      <=> contextualize(
            3,
            UHExp.(
              expOpSeqOfEZOpTree(
                Op(
                  Plus,
                  Op(Plus, V(varN'(0)), V(varN'(1))),
                  V(varN'(2)),
                ),
              )
            ),
          ),
      "DeepSpace"
      <=> contextualize(
            3,
            UHExp.(
              expOpSeqOfEZOpTree(
                Op(
                  Space,
                  Op(Space, V(varN'(0)), V(varN'(1))),
                  V(varN'(2)),
                ),
              )
            ),
          ),
      "DeepTimes"
      <=> contextualize(
            3,
            UHExp.(
              expOpSeqOfEZOpTree(
                Op(
                  Times,
                  Op(Times, V(varN'(0)), V(varN'(1))),
                  V(varN'(2)),
                ),
              )
            ),
          ),
      "PrecedenceSpaceTimes"
      <=> contextualize(
            3,
            UHExp.(
              expOpSeqOfEZOpTree(
                Op(
                  Times,
                  V(varN'(0)),
                  Op(Space, V(varN'(1)), V(varN'(2))),
                ),
              )
            ),
          ),
      "PrecedenceTimesPlus"
      <=> contextualize(
            3,
            UHExp.(
              expOpSeqOfEZOpTree(
                Op(
                  Plus,
                  V(varN'(0)),
                  Op(Times, V(varN'(1)), V(varN'(2))),
                ),
              )
            ),
          ),
      "AutoParens"
      |=> contextualize_v(
            UHExp.(
              expOpSeqOfEZOpTree(
                Op(
                  Plus,
                  V(Parenthesized(nih(Asc(var', UHTyp.Num)))),
                  V(
                    Parenthesized(
                      nih(
                        Let(
                          "x",
                          var',
                          expOpSeqOfEZOpTree(
                            Op(
                              Plus,
                              V(var'),
                              V(
                                Parenthesized(
                                  Tm(
                                    InHole(1),
                                    Lam(
                                      "y",
                                      expOpSeqOfEZOpTree(
                                        Op(
                                          Plus,
                                          V(var'),
                                          V(
                                            Parenthesized(
                                              nih(
                                                Case(
                                                  var',
                                                  ("l", nihVar("l")),
                                                  ("r", nihVar("r")),
                                                ),
                                              ),
                                            ),
                                          ),
                                        ),
                                      ),
                                    ),
                                  ),
                                ),
                              ),
                            ),
                          ),
                        ),
                      ),
                    ),
                  ),
                ),
              )
            ),
          )
      >=> "AutoParensOut",
      "ExplicitParens"
      <=> contextualize_v(
            UHExp.(
              Parenthesized(
                Parenthesized(
                  expOpSeqOfEZOpTree(
                    Op(
                      Plus,
                      Op(
                        Plus,
                        Op(Plus, V(var'), V(nih(NumLit(1)))),
                        V(Parenthesized(nih(NumLit(2)))),
                      ),
                      Op(
                        Times,
                        V(
                          Parenthesized(
                            Parenthesized(
                              expOpSeqOfEZOpTree(
                                Op(Plus, V(var'), V(var')),
                              ),
                            ),
                          ),
                        ),
                        V(var'),
                      ),
                    ),
                  ),
                ),
              )
            ),
          ),
      "Thorough"
      <=> contextualize(
            10,
            UHExp.(
              expOpSeqOfEZOpTree(
                Op(
                  Plus,
                  Op(
                    Plus,
                    Op(
                      Plus,
                      Op(
                        Plus,
                        V(varN'(0)),
                        Op(
                          Times,
                          Op(Times, V(varN'(1)), V(varN'(2))),
                          Op(Space, V(varN'(3)), V(varN'(4))),
                        ),
                      ),
                      Op(
                        Times,
                        Op(Space, V(varN'(5)), V(varN'(6))),
                        V(varN'(7)),
                      ),
                    ),
                    V(varN'(8)),
                  ),
                  V(varN'(9)),
                ),
              )
            ),
          ),
    ],
    "testVar" >:::% ["Basic" <=> contextualize_v(var')],
    "testNonEmptyHoles"
    >:::% [
      "Basic"
      <=> contextualize_v(
            nih(UHExp.(Asc(Tm(InHole(1), Inj(L, var')), UHTyp.Num))),
          ),
    ],
  ];

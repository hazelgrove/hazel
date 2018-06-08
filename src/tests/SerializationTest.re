open OUnit2;

open Printf;

open Semantics.Core;

/* See comment for `tests` value below for info on how to read the test cases */
let nih tm => UHExp.Tm NotInHole tm;

let nihVar s => nih (UHExp.Var s);

let var' = nihVar "v";

let varN' n => nihVar (sprintf "v%d" n);

type ezOpTree 'op 'v =
  | Op 'op (ezOpTree 'op 'v) (ezOpTree 'op 'v)
  | V 'v;

type seqInfo 'op 'v =
  | Empty
  | LoneTm 'v
  | FullSeq (OperatorSeq.opseq 'v 'op);

let updateSeq mSeq optOp tm =>
  switch (mSeq, optOp) {
  | (Empty, None) => LoneTm tm
  | (LoneTm tm1, Some op) => FullSeq (OperatorSeq.ExpOpExp tm1 op tm)
  | (FullSeq seq, Some op) => FullSeq (OperatorSeq.SeqOpExp seq op tm)
  | _ => assert_failure "Invalid seqInfo state for updateSeq"
  };

let skelAndSeqOfEZOpTree eztree => {
  let rec skelAndSeqOfEZOpTree' eztree' n mSeq mOp =>
    switch eztree' {
    | Op op l r =>
      let (lSkel, lSeq, n') = skelAndSeqOfEZOpTree' l n mSeq mOp;
      let (rSkel, rSeq, n'') = skelAndSeqOfEZOpTree' r n' lSeq (Some op);
      (Skel.BinOp NotInHole op lSkel rSkel, rSeq, n'')
    | V v => (Skel.Placeholder n, updateSeq mSeq mOp v, n + 1)
    };
  switch (skelAndSeqOfEZOpTree' eztree 0 Empty None) {
  | (skel, FullSeq seq, _) => (skel, seq)
  | _ =>
    assert_failure "skelOfEZOpTree must return a FullSeq, not any other type of seqInfo"
  }
};

let tyOpSeqOfEZOpTree eztree => {
  let (skel, seq) = skelAndSeqOfEZOpTree eztree;
  UHTyp.OpSeq skel seq
};

let expOpSeqOfEZOpTree eztree => {
  let (skel, seq) = skelAndSeqOfEZOpTree eztree;
  nih (UHExp.OpSeq skel seq)
};

let rec contextualize' start end' uhexp =>
  if (start < end') {
    let uhexp' = contextualize' (start + 1) end' uhexp;
    UHExp.(nih (Let (sprintf "v%d" start) (nih (EmptyHole start)) uhexp'))
  } else {
    uhexp
  };

/* Wraps the uhexp with definitions for the variables `v0` through `vend' - 1`.
   If used, remaining hole numbers should start at `end'`. */
let contextualize end' uhexp => contextualize' 0 end' uhexp;

/* Wraps the uhexp with a definition for the variables `v`.
   If used, remaining hole numbers should start at 1. */
let contextualize_v uhexp => UHExp.(nih (Let "v" (nih (EmptyHole 0)) uhexp));

/* TODO
   /* it is necessary to add one extra space of indentation to
      fcontents' due to the box-style formatting */
   sprintf
     "let v = {} in\n(%s) : {}\n" (increase_indent fcontents' 1)

   let increase_indent s indent =>
     Str.global_replace (Str.regexp "\n") ("\n" ^ String.make indent ' ') s;
     */
let (|=>) name uhexp => (name, uhexp, None);

let (>=>) (inName, uhexp, _) outName => (inName, uhexp, Some outName);

let (<=>) name uhexp => (name, uhexp, Some name);

let (>:::%) group_name checks =>
  group_name >:::
  List.map
    (
      fun (inName, uhexp, outNameOpt) =>
        group_name ^ "/" ^ inName >:: (
          fun _ => {
            let getContents name => {
              /* TODO We shouldn't magically know that this is being invoked from the `src` directory.
                 We should either use logic to determine where the `tests` dir is relative to the cwd,
                 or we should find a way to ensure that this is run from the `tests` dir, which would
                 seem sensible */
              let tf =
                open_in (
                  "tests/test_data/" ^ group_name ^ "/" ^ name ^ ".hazel"
                );
              let filecontents = really_input_string tf (in_channel_length tf);
              close_in tf;
              filecontents
            };
            let inContents = getContents inName;
            assert_equal
              msg::"deserialization failed"
              /* Useful for debugging, but can't be on all the time because then a serializer crash can
                       cause parser tests to fail
                 printer::Serialize.string_of_uhexp */
              uhexp
              (Deserialize.uhexp_of_string inContents);
            switch outNameOpt {
            | Some outName =>
              let outContents =
                inName == outName ? inContents : getContents outName;
              assert_equal
                printer::(fun x => x)
                msg::"serialization failed"
                outContents
                (Serialize.string_of_uhexp uhexp)
            | None => ()
            }
          }
        )
    )
    checks;


/**
 * The test cases are layed out as such:
 * "testFolder" >:::% [
 *   "TestName1" <=> uhexp1,
 *   "TestName2" |=> uhexp2,
 *   "TestName3" |=> uhexp3 >=> "TestName3Out",
 *   ...
 * ],
 * ...
 *
 * `"TestName1" <=> uhexp1` asserts that the concrete syntax in
 * 'tests/test_data/testFolder/TestName1.re' should deserialize to uhexp1, and that uhexp1 should
 * serialize to the exact contents of that file.
 *
 * `"TestName2" |=> uhexp2` asserts only that "TestName2" must deserialize to uhexp2, but doesn't
 * test the serialization of uhexp2.
 *
 * `"TestName3" |=> uhexp3 >=> "TestName3Out"` asserts that "TestName3" must deserialize to uhexp3,
 * which must serialize to the exact contents of "TestName3Out"
 *
 * Note that some of the test data have too much indentation on some lines. This is likely
 * because the unicode characters trick the formatter about how long lines are, causing some
 * subsequent lines to have excess indentation. The test cases that exhibit this issue
 * document its present status as a won't-fix.
 */
let tests = {
  let basicArrowUHExp =
    contextualize_v (
      nih
        UHExp.(Asc var' (tyOpSeqOfEZOpTree UHTyp.(Op Arrow (V Num) (V Hole))))
    );
  let basicCaseUHExp =
    nih
      UHExp.(
        Asc
          (
            Parenthesized (
              contextualize_v (
                nih (Case var' ("l", nihVar "l") ("r", nihVar "r"))
              )
            )
          )
          UHTyp.Hole
      );
  let basicLamUHExp = contextualize_v (nih UHExp.(Lam "a" var'));
  "Serialization tests" >::: [
    "testAscParens" >:::% [
      "AutoParens" |=>
      contextualize_v (
        nih
          UHExp.(
            UHTyp.(
              Asc
                (
                  Parenthesized (
                    nih (Asc (Parenthesized (nih (Asc var' Num))) Hole)
                  )
                )
                Num
            )
          )
      ) >=> "AutoParensOut",
      "ExplicitParens" <=>
      contextualize_v (
        nih
          UHExp.(
            Asc
              var'
              UHTyp.(
                tyOpSeqOfEZOpTree (
                  Op
                    Arrow
                    (V (Parenthesized Num))
                    (
                      V (
                        Parenthesized (
                          Parenthesized (
                            tyOpSeqOfEZOpTree (Op Sum (V Hole) (V Hole))
                          )
                        )
                      )
                    )
                )
              )
          )
      )
    ],
    "testAscHole" >:::% [
      "Basic" <=> contextualize_v (nih UHExp.(Asc var' UHTyp.Hole))
    ],
    "testAscNum" >:::% [
      "Basic" <=> contextualize_v (nih UHExp.(Asc var' UHTyp.Num))
    ],
    "testAscOpSeq" >:::% [
      "BasicArrow" <=> basicArrowUHExp,
      "BasicArrowWithKeyword" |=> basicArrowUHExp >=> "BasicArrow",
      "BasicSum" <=>
      contextualize_v (
        nih UHExp.(Asc var' (tyOpSeqOfEZOpTree UHTyp.(Op Sum (V Num) (V Num))))
      ),
      "DeepArrow" <=>
      contextualize_v (
        nih
          UHExp.(
            Asc
              var'
              (
                tyOpSeqOfEZOpTree
                  UHTyp.(Op Arrow (V Hole) (Op Arrow (V Hole) (V Hole)))
              )
          )
      ),
      "DeepSum" <=>
      contextualize_v (
        nih
          UHExp.(
            Asc
              var'
              (
                tyOpSeqOfEZOpTree
                  UHTyp.(Op Sum (V Hole) (Op Sum (V Num) (V Hole)))
              )
          )
      ),
      "PrecedenceLeft" <=>
      contextualize_v (
        nih
          UHExp.(
            Asc
              var'
              (
                tyOpSeqOfEZOpTree
                  UHTyp.(Op Arrow (Op Sum (V Hole) (V Num)) (V Num))
              )
          )
      ),
      "PrecedenceRight" <=>
      contextualize_v (
        nih
          UHExp.(
            Asc
              var'
              (
                tyOpSeqOfEZOpTree
                  UHTyp.(Op Arrow (V Hole) (Op Sum (V Num) (V Num)))
              )
          )
      ),
      "Thorough" <=>
      contextualize_v (
        nih
          UHExp.(
            Asc
              var'
              (
                tyOpSeqOfEZOpTree
                  UHTyp.(
                    Op
                      Arrow
                      (Op Sum (V Num) (V Num))
                      (
                        Op
                          Arrow
                          (Op Sum (V Hole) (Op Sum (V Num) (V Hole)))
                          (V Num)
                      )
                  )
              )
          )
      )
    ],
    "testCase" >:::% [
      "Basic" <=> basicCaseUHExp,
      "BasicWithKeyword" |=> basicCaseUHExp >=> "Basic",
      "Deep" <=>
      nih
        UHExp.(
          Asc
            (
              Parenthesized (
                contextualize_v (
                  nih (
                    Case
                      var'
                      (
                        "l",
                        nih (
                          Case
                            (nihVar "l")
                            ("ll", nihVar "ll")
                            ("lr", nihVar "lr")
                        )
                      )
                      (
                        "r",
                        nih (
                          Case
                            (nihVar "r")
                            ("rl", nihVar "rl")
                            ("rr", nihVar "rr")
                        )
                      )
                  )
                )
              )
            )
            UHTyp.Hole
        ),
      "WithAsc" <=>
      nih
        UHExp.(
          Asc
            (
              Parenthesized (
                contextualize_v (
                  nih (
                    Case
                      var'
                      ("l", nihVar "l")
                      ("r", nih (Asc (nihVar "r") UHTyp.Num))
                  )
                )
              )
            )
            UHTyp.Num
        )
    ],
    "testEmptyHole" >:::% ["Basic" <=> nih UHExp.(EmptyHole 0)],
    "testInjLeft" >:::% [
      "Basic" <=> contextualize_v (nih UHExp.(Inj L var')),
      "Deep" <=> contextualize_v (nih UHExp.(Inj L (nih (Inj L var'))))
    ],
    "testInjRight" >:::% [
      "Basic" <=> contextualize_v (nih UHExp.(Inj R var')),
      "Deep" <=> contextualize_v (nih UHExp.(Inj R (nih (Inj L var'))))
    ],
    "testLam" >:::% [
      "Basic" <=> basicLamUHExp,
      "BasicWithKeyword" |=> basicLamUHExp >=> "Basic",
      "Deep" <=> nih UHExp.(Lam "a" (nih (Lam "b" (nihVar "b")))),
      "Shadow" <=> nih UHExp.(Lam "a" (nih (Lam "a" (nihVar "a"))))
    ],
    "testLet" >:::% [
      "Basic" <=> nih UHExp.(Let "x" (nih (EmptyHole 0)) (nihVar "x")),
      "Deep" <=>
      nih
        UHExp.(
          Let
            "a"
            (nih (Let "b" (nih (EmptyHole 0)) (nihVar "b")))
            (nih (Let "c" (nihVar "a") (nihVar "c")))
        ),
      "Nested" <=>
      nih
        UHExp.(
          Let
            "x"
            (nih (EmptyHole 0))
            (nih (Let "y" (nih (EmptyHole 1)) (nihVar "x")))
        ),
      "Shadowing" <=>
      nih
        UHExp.(
          Let "a" (nih (EmptyHole 0)) (nih (Let "a" (nihVar "a") (nihVar "a")))
        )
    ],
    "testNumLit" >:::% [
      "Zero" <=> nih (UHExp.NumLit 0),
      "One" <=> nih (UHExp.NumLit 1),
      "Twelve" <=> nih (UHExp.NumLit 12)
    ],
    "testOpSeq" >:::% [
      "BasicPlus" <=>
      contextualize
        2 UHExp.(expOpSeqOfEZOpTree (Op Plus (V (varN' 0)) (V (varN' 1)))),
      "BasicSpace" <=>
      contextualize
        2 UHExp.(expOpSeqOfEZOpTree (Op Space (V (varN' 0)) (V (varN' 1)))),
      "BasicTimes" <=>
      contextualize
        2 UHExp.(expOpSeqOfEZOpTree (Op Times (V (varN' 0)) (V (varN' 1)))),
      "DeepPlus" <=>
      contextualize
        3
        UHExp.(
          expOpSeqOfEZOpTree (
            Op Plus (Op Plus (V (varN' 0)) (V (varN' 1))) (V (varN' 2))
          )
        ),
      "DeepSpace" <=>
      contextualize
        3
        UHExp.(
          expOpSeqOfEZOpTree (
            Op Space (Op Space (V (varN' 0)) (V (varN' 1))) (V (varN' 2))
          )
        ),
      "DeepTimes" <=>
      contextualize
        3
        UHExp.(
          expOpSeqOfEZOpTree (
            Op Times (Op Times (V (varN' 0)) (V (varN' 1))) (V (varN' 2))
          )
        ),
      "PrecedenceSpaceTimes" <=>
      contextualize
        3
        UHExp.(
          expOpSeqOfEZOpTree (
            Op Times (V (varN' 0)) (Op Space (V (varN' 1)) (V (varN' 2)))
          )
        ),
      "PrecedenceTimesPlus" <=>
      contextualize
        3
        UHExp.(
          expOpSeqOfEZOpTree (
            Op Plus (V (varN' 0)) (Op Times (V (varN' 1)) (V (varN' 2)))
          )
        ),
      "AutoParens" |=>
      contextualize_v
        UHExp.(
          expOpSeqOfEZOpTree (
            Op
              Plus
              (V (Parenthesized (nih (Asc var' UHTyp.Num))))
              (
                V (
                  Parenthesized (
                    nih (
                      Let
                        "x"
                        var'
                        (
                          expOpSeqOfEZOpTree (
                            Op
                              Plus
                              (V var')
                              (
                                V (
                                  Parenthesized (
                                    Tm
                                      (InHole 1)
                                      (
                                        Lam
                                          "y"
                                          (
                                            expOpSeqOfEZOpTree (
                                              Op
                                                Plus
                                                (V var')
                                                (
                                                  V (
                                                    Parenthesized (
                                                      nih (
                                                        Case
                                                          var'
                                                          ("l", nihVar "l")
                                                          ("r", nihVar "r")
                                                      )
                                                    )
                                                  )
                                                )
                                            )
                                          )
                                      )
                                  )
                                )
                              )
                          )
                        )
                    )
                  )
                )
              )
          )
        ) >=> "AutoParensOut",
      "ExplicitParens" <=>
      contextualize_v
        UHExp.(
          Parenthesized (
            Parenthesized (
              expOpSeqOfEZOpTree (
                Op
                  Plus
                  (
                    Op
                      Plus
                      (Op Plus (V var') (V (nih (NumLit 1))))
                      (V (Parenthesized (nih (NumLit 2))))
                  )
                  (
                    Op
                      Times
                      (
                        V (
                          Parenthesized (
                            Parenthesized (
                              expOpSeqOfEZOpTree (Op Plus (V var') (V var'))
                            )
                          )
                        )
                      )
                      (V var')
                  )
              )
            )
          )
        ),
      "Thorough" <=>
      contextualize
        10
        UHExp.(
          expOpSeqOfEZOpTree (
            Op
              Plus
              (
                Op
                  Plus
                  (
                    Op
                      Plus
                      (
                        Op
                          Plus
                          (V (varN' 0))
                          (
                            Op
                              Times
                              (Op Times (V (varN' 1)) (V (varN' 2)))
                              (Op Space (V (varN' 3)) (V (varN' 4)))
                          )
                      )
                      (
                        Op
                          Times
                          (Op Space (V (varN' 5)) (V (varN' 6)))
                          (V (varN' 7))
                      )
                  )
                  (V (varN' 8))
              )
              (V (varN' 9))
          )
        )
    ],
    "testVar" >:::% ["Basic" <=> contextualize_v var'],
    "testNonEmptyHoles" >:::% [
      "Basic" <=>
      contextualize_v (nih UHExp.(Asc (Tm (InHole 1) (Inj L var')) UHTyp.Num))
    ]
  ]
};

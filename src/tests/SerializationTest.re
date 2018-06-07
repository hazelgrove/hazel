open OUnit2;

open Printf;

open Semantics.Core;

/* See comment for `tests` value below for info on how to read the test cases */
let nih tm => UHExp.Tm NotInHole tm;

let nihVar s => nih (UHExp.Var s);

/* var' and varN' create variables that don't need to be declared */
let var' = nihVar "v";

let varN' n => {
  assert (n > 0 && n < 10);
  nihVar (sprintf "v%d" n)
};

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

let (|=>) name uhexp => (name, uhexp, None);

let (>=>) (inName, uhexp, _) outName => (inName, uhexp, Some outName);

let (<=>) name uhexp => (name, uhexp, Some name);

let trim_trailing_newline s => {
  let last_ind = String.length s - 1;
  let last_char = s.[last_ind];
  assert_equal
    msg::(
      sprintf
        "%s should have ended in a new line but instead ends in %c" s last_char
    )
    '\n'
    last_char;
  String.sub s 0 last_ind
};

let increase_indent s indent =>
  Str.global_replace (Str.regexp "\n") ("\n" ^ String.make indent ' ') s;

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
              trim_trailing_newline filecontents
            };
            /* contextualize wraps the concrete and abstract syntax with definitions for the
               variables `v` and `vstart` through `vend'` */
            let rec contextualize' start end' (uhexp', fcontents') =>
              if (start == end') {
                (
                  UHExp.(
                    nih (
                      Let
                        "v"
                        (nih (EmptyHole (start - 1)))
                        (nih (Asc (Parenthesized uhexp') UHTyp.Hole))
                    )
                  ),
                  /* it is necessary to add one extra space of indentation to
                     fcontents' due to the box-style formatting */
                  sprintf
                    "let v = {} in\n(%s) : {}\n" (increase_indent fcontents' 1)
                )
              } else {
                let (uhexp'', fcontents'') =
                  contextualize' (start + 1) end' (uhexp', fcontents');
                (
                  UHExp.(
                    nih (
                      Let
                        (sprintf "v%d" start)
                        (nih (EmptyHole (start - 1)))
                        uhexp''
                    )
                  ),
                  sprintf "let v%d = {} in\n%s" start fcontents''
                )
              };
            let contextualize uhexp' fcontents' =>
              contextualize' 1 10 (uhexp', fcontents');
            let inContents = getContents inName;
            let (uhexp', inContents') = contextualize uhexp inContents;
            assert_equal
              msg::"deserialization failed"
              uhexp'
              (Deserialize.uhexp_of_string inContents');
            switch outNameOpt {
            | Some outName =>
              let outContents' =
                inName == outName ?
                  inContents' : snd (contextualize uhexp (getContents outName));
              assert_equal
                printer::(fun x => x)
                msg::"serialization failed"
                outContents'
                (Serialize.string_of_uhexp uhexp')
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
 * Each UHExp can use `var'` or `(varN' d)` (0 < d < 10) to refer to variables `v` and `v1`
 * through `v9` - these variables can be referred to without having to be defined. Any other
 * variable that is referred to must be defined, or else a LangUtil.IllFormed exception will be
 * raised.
 *
 * Because of the context variables, EmptyHole numbers in the uhexps should start at 10.
 *
 * Note that some of the test data have too much indentation on some lines. This is likely
 * because the unicode characters trick the formatter about how long lines are, causing some
 * subsequent lines to have excess indentation. The test cases that exhibit this issue
 * document its present status as a won't-fix.
 */
let tests = {
  let basicArrowUHExp =
    nih UHExp.(Asc var' (tyOpSeqOfEZOpTree UHTyp.(Op Arrow (V Num) (V Hole))));
  let basicCaseUHExp =
    nih UHExp.(Case var' ("l", nihVar "l") ("r", nihVar "r"));
  let basicLamUHExp = nih UHExp.(Lam "a" var');
  "Serialization tests" >::: [
    "testAscParens" >:::% [
      "AutoParens" |=>
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
        ) >=> "AutoParensOut",
      "ExplicitParens" <=>
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
    ],
    "testAscHole" >:::% ["Basic" <=> nih UHExp.(Asc var' UHTyp.Hole)],
    "testAscNum" >:::% ["Basic" <=> nih UHExp.(Asc var' UHTyp.Num)],
    "testAscOpSeq" >:::% [
      "BasicArrow" <=> basicArrowUHExp,
      "BasicArrowWithKeyword" |=> basicArrowUHExp >=> "BasicArrow",
      "BasicSum" <=>
      nih UHExp.(Asc var' (tyOpSeqOfEZOpTree UHTyp.(Op Sum (V Num) (V Num)))),
      "DeepArrow" <=>
      nih
        UHExp.(
          Asc
            var'
            (
              tyOpSeqOfEZOpTree
                UHTyp.(Op Arrow (V Hole) (Op Arrow (V Hole) (V Hole)))
            )
        ),
      "DeepSum" <=>
      nih
        UHExp.(
          Asc
            var'
            (
              tyOpSeqOfEZOpTree
                UHTyp.(Op Sum (V Hole) (Op Sum (V Num) (V Hole)))
            )
        ),
      "PrecedenceLeft" <=>
      nih
        UHExp.(
          Asc
            var'
            (
              tyOpSeqOfEZOpTree
                UHTyp.(Op Arrow (Op Sum (V Hole) (V Num)) (V Num))
            )
        ),
      "PrecedenceRight" <=>
      nih
        UHExp.(
          Asc
            var'
            (
              tyOpSeqOfEZOpTree
                UHTyp.(Op Arrow (V Hole) (Op Sum (V Num) (V Num)))
            )
        ),
      "Thorough" <=>
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
    ],
    "testCase" >:::% [
      "Basic" <=> basicCaseUHExp,
      "BasicWithKeyword" |=> basicCaseUHExp >=> "Basic",
      "Deep" <=>
      nih
        UHExp.(
          Case
            var'
            (
              "l",
              nih (Case (nihVar "l") ("ll", nihVar "ll") ("lr", nihVar "lr"))
            )
            (
              "r",
              nih (Case (nihVar "r") ("rl", nihVar "rl") ("rr", nihVar "rr"))
            )
        ),
      "WithAsc" <=>
      nih
        UHExp.(
          Case var' ("l", nihVar "l") ("r", nih (Asc (nihVar "r") UHTyp.Num))
        )
    ],
    "testEmptyHole" >:::% ["Basic" <=> nih UHExp.(EmptyHole 10)],
    "testInjLeft" >:::% [
      "Basic" <=> nih UHExp.(Inj L var'),
      "Deep" <=> nih UHExp.(Inj L (nih (Inj L var')))
    ],
    "testInjRight" >:::% [
      "Basic" <=> nih UHExp.(Inj R var'),
      "Deep" <=> nih UHExp.(Inj R (nih (Inj L var')))
    ],
    "testLam" >:::% [
      "Basic" <=> basicLamUHExp,
      "BasicWithKeyword" |=> basicLamUHExp >=> "Basic",
      "Deep" <=> nih UHExp.(Lam "a" (nih (Lam "b" (nihVar "b")))),
      "Shadow" <=> nih UHExp.(Lam "a" (nih (Lam "a" (nihVar "a"))))
    ],
    "testLet" >:::% [
      "Basic" <=> nih UHExp.(Let "x" var' (varN' 1)),
      "Deep" <=>
      nih
        UHExp.(
          Let
            "a"
            (nih (Let "b" var' (nihVar "b")))
            (nih (Let "c" (nihVar "a") (nihVar "c")))
        ),
      "Nested" <=>
      nih UHExp.(Let "x" (varN' 1) (nih (Let "y" (varN' 2) (nihVar "x")))),
      "Shadowing" <=>
      nih UHExp.(Let "a" var' (nih (Let "a" (nihVar "a") (nihVar "a"))))
    ],
    "testNumLit" >:::% [
      "Zero" <=> nih (UHExp.NumLit 0),
      "One" <=> nih (UHExp.NumLit 1),
      "Twelve" <=> nih (UHExp.NumLit 12)
    ],
    "testOpSeq" >:::% [
      "BasicPlus" <=>
      UHExp.(expOpSeqOfEZOpTree (Op Plus (V (varN' 1)) (V (varN' 2)))),
      "BasicSpace" <=>
      UHExp.(expOpSeqOfEZOpTree (Op Space (V (varN' 1)) (V (varN' 2)))),
      "BasicTimes" <=>
      UHExp.(expOpSeqOfEZOpTree (Op Times (V (varN' 1)) (V (varN' 2)))),
      "DeepPlus" <=>
      UHExp.(
        expOpSeqOfEZOpTree (
          Op Plus (Op Plus (V (varN' 1)) (V (varN' 2))) (V (varN' 3))
        )
      ),
      "DeepSpace" <=>
      UHExp.(
        expOpSeqOfEZOpTree (
          Op Space (Op Space (V (varN' 1)) (V (varN' 2))) (V (varN' 3))
        )
      ),
      "DeepTimes" <=>
      UHExp.(
        expOpSeqOfEZOpTree (
          Op Times (Op Times (V (varN' 1)) (V (varN' 2))) (V (varN' 3))
        )
      ),
      "PrecedenceSpaceTimes" <=>
      UHExp.(
        expOpSeqOfEZOpTree (
          Op Times (V (varN' 1)) (Op Space (V (varN' 2)) (V (varN' 3)))
        )
      ),
      "PrecedenceTimesPlus" <=>
      UHExp.(
        expOpSeqOfEZOpTree (
          Op Plus (V (varN' 1)) (Op Times (V (varN' 2)) (V (varN' 3)))
        )
      ),
      "AutoParens" |=>
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
                                    (InHole 10)
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
                        (V (varN' 1))
                        (
                          Op
                            Times
                            (Op Times (V (varN' 2)) (V (varN' 3)))
                            (Op Space (V (varN' 4)) (V (varN' 5)))
                        )
                    )
                    (
                      Op
                        Times
                        (Op Space (V (varN' 6)) (V (varN' 7)))
                        (V (varN' 8))
                    )
                )
                (V (varN' 9))
            )
            (V var')
        )
      )
    ],
    "testVar" >:::% ["Basic" <=> var'],
    "testNonEmptyHoles" >:::% [
      "Basic" <=> nih UHExp.(Asc (Tm (InHole 10) (Inj L var')) UHTyp.Num)
    ]
  ]
};

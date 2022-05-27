// TODO change this type to have the right kind of expression
module Memo = Core_kernel.Memo;
open Sexplib;
open Sexplib.Std;

// General type for a single example
[@deriving sexp]
type quest = {
  idz: string,
  expressionz: UHExp.t,
  caption: string,
  rankz: int,
  result: DHExp.t,
};

[@deriving sexp]
type explain = {
  id: string,
  expression: string,
  rank: int,
};

/*
 TODO: The centralized model should keep track of all of the different prompts (so add to the type t at the top of Model.re something like prompt: list(Prompt.t)
  */
[@deriving sexp]
type t = {
  key: string,
  program: ZExp.t,
  prompt_message: string,
  explanation: list(explain),
  examples: list(quest),
  explanation_text_box: string,
  example_text_box: string,
  syntactic_form_level: int,
};

// TODO - Ardi - This how to create a prompt
// Sexp.to_string(sexp_of_t(Prompt))

let print_to_console = stuff => {
  List.fold_left(
    (++),
    "",
    List.map(x => Sexp.to_string(sexp_of_t(x)), stuff),
  );
};

/****************** FUNCTION APPLICATION WITH CURRYING ***********************************************/
let curry_fun_app_snippet =
  UHExp.[
    letline(OpSeq.wrap(UHPat.var("foo")), Block.wrap(UHExp.EmptyHole(1))),
    ExpLine(
      OpSeq.wrap(
        Parenthesized(
          Block.wrap'(
            Seq.mk(
              Parenthesized(
                Block.wrap'(
                  Seq.mk(var("foo"), [(Operators_Exp.Space, intlit("1"))])
                  |> UHExp.mk_OpSeq,
                ),
              ),
              [(Operators_Exp.Space, boollit(true))],
            )
            |> UHExp.mk_OpSeq,
          ),
        ),
      ),
    ),
  ];

let curry_fun_app_expl_1 = {
  id: "explanation_1",
  expression: "Apply function [`foo 1`](0) to argument [`true`](1).",
  rank: (-1),
};

let curry_fun_app_expl_2 = {
  id: "explanation_2",
  expression: "Apply function [`foo`](0 0) to argument [`1`](0 1) and then apply the resulting function to argument [`true`](1).",
  rank: (-1),
};

let curry_fun_app_expl_3 = {
  id: "explanation_3",
  expression: "Apply function [`foo 1`](0) to argument [`true`](1).\n- Function [`foo 1`](0) is the result of applying function `foo` to argument `1`.",
  rank: (-1),
};

let curry_fun_app_expl_4 = {
  id: "explanation_4",
  expression: "Apply function [`foo`](0 0) to arguments [`1`](0 1) and [`true`](1).",
  rank: (-1),
};

let curry_fun_app_ex_1 = {
  idz: "example_1",
  expressionz:
    UHExp.(
      Block.wrap'(
        Seq.mk(
          lam(
            UHPat.(OpSeq.wrap(var("x"))),
            Block.wrap'(
              Seq.mk(var("x"), [(Operators_Exp.Plus, intlit("1"))])
              |> mk_OpSeq,
            ),
          ),
          [(Operators_Exp.Space, intlit("2"))],
        )
        |> mk_OpSeq,
      )
    ),
  caption: "The function is applied to argument 2, and evaluating the body of the function gives 2 + 1 which equals 3.",
  rankz: (-1),
  result: DHExp.IntLit(3),
};

let curry_fun_app_ex_2 = {
  idz: "example_2",
  expressionz:
    UHExp.[
      letline(
        UHPat.(OpSeq.wrap(var("add_one"))),
        Block.wrap(
          lam(
            UHPat.(OpSeq.wrap(var("x"))),
            Block.wrap'(
              Seq.mk(var("x"), [(Operators_Exp.Plus, intlit("1"))])
              |> mk_OpSeq,
            ),
          ),
        ),
      ),
      ExpLine(
        Seq.mk(var("add_one"), [(Operators_Exp.Space, intlit("2"))])
        |> mk_OpSeq,
      ),
    ],
  caption: "The function add_one is applied to argument 2, and evaluating the body of the function gives 2 + 1 which equals 3.",
  rankz: (-1),
  result: DHExp.IntLit(3),
};

let curry_fun_app_ex_3 = {
  idz: "example_3",
  expressionz:
    UHExp.(
      Block.wrap'(
        Seq.mk(
          lam(
            UHPat.(OpSeq.wrap(var("x"))),
            Block.wrap(
              lam(
                UHPat.(OpSeq.wrap(var("y"))),
                Block.wrap'(
                  Seq.mk(var("x"), [(Operators_Exp.Plus, intlit("y"))])
                  |> mk_OpSeq,
                ),
              ),
            ),
          ),
          [
            (Operators_Exp.Space, intlit("1")),
            (Operators_Exp.Space, intlit("2")),
          ],
        )
        |> mk_OpSeq,
      )
    ),
  caption: "First, the function is applied to argument 1, and evaluating the body of the function results in the function fun y {x + y}. Then, this resulting function is applied to argument 2, and evaluating the body gives 1 + 2 which equals 3.",
  rankz: (-1),
  result: DHExp.IntLit(3),
};

let curry_fun_app_ex_4 = {
  idz: "example_4",
  expressionz:
    UHExp.[
      letline(
        UHPat.(OpSeq.wrap(var("incr_or_decr"))),
        Block.wrap(
          lam(
            UHPat.(OpSeq.wrap(var("is_incr"))),
            Block.wrap(
              lam(
                UHPat.(OpSeq.wrap(var("val"))),
                Block.wrap(
                  case(
                    Block.wrap(UHExp.var("is_incr")),
                    [
                      Rule(
                        OpSeq.wrap(UHPat.boollit(true)),
                        Block.wrap'(
                          Seq.mk(
                            var("val"),
                            [(Operators_Exp.Plus, intlit("1"))],
                          )
                          |> mk_OpSeq,
                        ),
                      ),
                      Rule(
                        OpSeq.wrap(UHPat.boollit(false)),
                        Block.wrap'(
                          Seq.mk(
                            var("val"),
                            [(Operators_Exp.Minus, intlit("1"))],
                          )
                          |> mk_OpSeq,
                        ),
                      ),
                    ],
                  ),
                ),
              ),
            ),
          ),
        ),
      ),
      ExpLine(
        Seq.mk(
          var("incr_or_decr"),
          [
            (Operators_Exp.Space, boollit(true)),
            (Operators_Exp.Space, intlit("2")),
          ],
        )
        |> mk_OpSeq,
      ),
    ],
  caption: "First, the function incr_or_decr is applied to argument true, and evaluating the body of the function results in the function fun val {case true | true => val + 1 | false => val - 1}. Then, this resulting function is applied to argument 2, and evaluating the body gives case true | true => val + 1 | false => val - 1. This case expression is then evaluates to 2 + 1 which equals 3.",
  rankz: (-1),
  result: DHExp.IntLit(3),
};

let curry_fun_app_ex_5 = {
  idz: "example_5",
  expressionz:
    UHExp.(
      Block.wrap'(
        Seq.mk(
          lam(
            UHPat.(OpSeq.wrap(var("x"))),
            Block.wrap(
              lam(
                UHPat.(OpSeq.wrap(var("y"))),
                Block.wrap'(
                  Seq.mk(var("x"), [(Operators_Exp.Plus, intlit("y"))])
                  |> mk_OpSeq,
                ),
              ),
            ),
          ),
          [(Operators_Exp.Space, intlit("1"))],
        )
        |> mk_OpSeq,
      )
    ),
  caption: "First, the function is applied to argument 1, and evaluating the body of the function results in the function fun y {1 + y}.",
  rankz: (-1),
  result:
    DHExp.Lam(
      DHPat.Var("y"),
      HTyp.Hole,
      DHExp.BinIntOp(Plus, IntLit(1), BoundVar("y")),
    ),
};

let curry_fun_app_ex_6 = {
  idz: "example_6",
  expressionz:
    UHExp.[
      letline(
        UHPat.(OpSeq.wrap(var("add"))),
        Block.wrap(
          lam(
            UHPat.(OpSeq.wrap(var("x"))),
            Block.wrap(
              lam(
                UHPat.(OpSeq.wrap(var("y"))),
                Block.wrap(
                  lam(
                    UHPat.(OpSeq.wrap(var("z"))),
                    Block.wrap'(
                      Seq.mk(
                        var("x"),
                        [
                          (Operators_Exp.Plus, var("y")),
                          (Operators_Exp.Plus, var("z")),
                        ],
                      )
                      |> mk_OpSeq,
                    ),
                  ),
                ),
              ),
            ),
          ),
        ),
      ),
      ExpLine(
        Seq.mk(
          var("add"),
          [
            (Operators_Exp.Space, intlit("1")),
            (Operators_Exp.Space, intlit("2")),
            (Operators_Exp.Space, intlit("3")),
          ],
        )
        |> mk_OpSeq,
      ),
    ],
  caption: "First, the function add is applied to argument 1, and evaluating the body of the function results in function fun y { fun z {1 + y + z}}. Then the resulting function is applied to argument 2, and evaluating the body of this function results in function fun z {1 + 2 + z}. Then this resulting function is applied to argument 3, and evaluating the body of this function results in 1 + 2 + 3 which equals 6.",
  rankz: (-1),
  result: DHExp.IntLit(6),
};

let curry_fun_app_explanations = [
  curry_fun_app_expl_1,
  curry_fun_app_expl_2,
  curry_fun_app_expl_3,
  curry_fun_app_expl_4,
];
let curry_fun_app_examples = [
  curry_fun_app_ex_1,
  curry_fun_app_ex_2,
  curry_fun_app_ex_3,
  curry_fun_app_ex_4,
  curry_fun_app_ex_5,
  curry_fun_app_ex_6,
];

/****************** FUNCTION APPLICATION WITH TUPLE ***********************************************/
let tuple_fun_app_snippet =
  UHExp.[
    letline(OpSeq.wrap(UHPat.var("foo")), Block.wrap(UHExp.EmptyHole(1))),
    ExpLine(
      OpSeq.wrap(
        Parenthesized(
          Block.wrap'(
            Seq.mk(
              var("foo"),
              [
                (
                  Operators_Exp.Space,
                  Parenthesized(
                    Block.wrap'(
                      Seq.mk(
                        intlit("1"),
                        [(Operators_Exp.Comma, boollit(true))],
                      )
                      |> mk_OpSeq,
                    ),
                  ),
                ),
              ],
            )
            |> UHExp.mk_OpSeq,
          ),
        ),
      ),
    ),
  ];

let tuple_fun_app_expl_1 = {
  id: "explanation_1",
  expression: "Apply function [`foo`](0) to argument [`(1, true)`](1).",
  rank: (-1),
};

let tuple_fun_app_expl_2 = {
  id: "explanation_2",
  expression: "Apply function [`foo`](0) to argument [`(1, true)`](1).\n- The argument is a tuple with first element `1` and second element `true`.",
  rank: (-1),
};

let tuple_fun_app_expl_3 = {
  id: "explanation_3",
  expression: "Apply function [`foo`](0) to arguments [`1`](1 0) and [`true`](1 1).",
  rank: (-1),
};

let tuple_fun_app_ex_1 = {
  idz: "example_1",
  expressionz:
    UHExp.(
      Block.wrap'(
        Seq.mk(
          lam(
            UHPat.(OpSeq.wrap(var("x"))),
            Block.wrap'(
              Seq.mk(var("x"), [(Operators_Exp.Plus, intlit("1"))])
              |> mk_OpSeq,
            ),
          ),
          [(Operators_Exp.Space, intlit("2"))],
        )
        |> mk_OpSeq,
      )
    ),
  caption: "The function is applied to argument 2, and evaluating the body of the function gives 2 + 1 which equals 3.",
  rankz: (-1),
  result: DHExp.IntLit(3),
};

let tuple_fun_app_ex_2 = {
  idz: "example_2",
  expressionz:
    UHExp.[
      letline(
        UHPat.(OpSeq.wrap(var("add_one"))),
        Block.wrap(
          lam(
            UHPat.(OpSeq.wrap(var("x"))),
            Block.wrap'(
              Seq.mk(var("x"), [(Operators_Exp.Plus, intlit("1"))])
              |> mk_OpSeq,
            ),
          ),
        ),
      ),
      ExpLine(
        Seq.mk(var("add_one"), [(Operators_Exp.Space, intlit("2"))])
        |> mk_OpSeq,
      ),
    ],
  caption: "The function add_one is applied to argument 2, and evaluating the body of the function gives 2 + 1 which equals 3.",
  rankz: (-1),
  result: DHExp.IntLit(3),
};

let tuple_fun_app_ex_3 = {
  idz: "example_3",
  expressionz:
    UHExp.[
      letline(
        UHPat.(OpSeq.wrap(var("incr_or_decr"))),
        Block.wrap(
          lam(
            UHPat.(
              OpSeq.wrap(
                Parenthesized(
                  Seq.mk(
                    var("is_incr"),
                    [(Operators_Pat.Comma, var("val"))],
                  )
                  |> mk_OpSeq,
                ),
              )
            ),
            Block.wrap(
              case(
                Block.wrap(UHExp.var("is_incr")),
                [
                  Rule(
                    OpSeq.wrap(UHPat.boollit(true)),
                    Block.wrap'(
                      Seq.mk(
                        var("val"),
                        [(Operators_Exp.Plus, intlit("1"))],
                      )
                      |> mk_OpSeq,
                    ),
                  ),
                  Rule(
                    OpSeq.wrap(UHPat.boollit(false)),
                    Block.wrap'(
                      Seq.mk(
                        var("val"),
                        [(Operators_Exp.Minus, intlit("1"))],
                      )
                      |> mk_OpSeq,
                    ),
                  ),
                ],
              ),
            ),
          ),
        ),
      ),
      ExpLine(
        Seq.mk(
          var("incr_or_decr"),
          [
            (
              Operators_Exp.Space,
              Parenthesized(
                Block.wrap'(
                  Seq.mk(
                    boollit(true),
                    [(Operators_Exp.Comma, intlit("2"))],
                  )
                  |> mk_OpSeq,
                ),
              ),
            ),
          ],
        )
        |> mk_OpSeq,
      ),
    ],
  caption: "First, the function incr_or_decr is applied to argument (true, 2), and evaluating the body of the function results in case true | true => val + 1 | false => val - 1. This case expression evaluates to 2 + 1 which equals 6.",
  rankz: (-1),
  result: DHExp.IntLit(3),
};

let tuple_fun_app_ex_4 = {
  idz: "example_4",
  expressionz:
    UHExp.[
      letline(
        UHPat.(OpSeq.wrap(var("add"))),
        Block.wrap(
          lam(
            UHPat.(
              OpSeq.wrap(
                Parenthesized(
                  Seq.mk(
                    var("x"),
                    [
                      (Operators_Pat.Comma, var("y")),
                      (Operators_Pat.Comma, var("z")),
                    ],
                  )
                  |> mk_OpSeq,
                ),
              )
            ),
            Block.wrap'(
              Seq.mk(
                var("x"),
                [
                  (Operators_Exp.Plus, var("y")),
                  (Operators_Exp.Plus, var("z")),
                ],
              )
              |> mk_OpSeq,
            ),
          ),
        ),
      ),
      ExpLine(
        Seq.mk(
          var("add"),
          [
            (
              Operators_Exp.Space,
              Parenthesized(
                Block.wrap'(
                  Seq.mk(
                    intlit("1"),
                    [
                      (Operators_Exp.Comma, intlit("2")),
                      (Operators_Exp.Comma, intlit("3")),
                    ],
                  )
                  |> mk_OpSeq,
                ),
              ),
            ),
          ],
        )
        |> mk_OpSeq,
      ),
    ],
  caption: "First, the function add is applied to argument (1, 2, 3), and evaluating the body of the function results in 1 + 2 + 3 which equals 6.",
  rankz: (-1),
  result: DHExp.IntLit(6),
};

let tuple_fun_app_explanations = [
  tuple_fun_app_expl_1,
  tuple_fun_app_expl_2,
  tuple_fun_app_expl_3,
];
let tuple_fun_app_examples = [
  tuple_fun_app_ex_1,
  tuple_fun_app_ex_2,
  tuple_fun_app_ex_3,
  tuple_fun_app_ex_4,
];

/****************** CASE ***********************************************/
let case_snippet =
  UHExp.[
    letline(OpSeq.wrap(UHPat.var("x")), Block.wrap(UHExp.EmptyHole(1))),
    ExpLine(
      Seq.wrap(
        case(
          Block.wrap(UHExp.var("x")),
          [
            Rule(
              UHPat.(
                OpSeq.wrap(
                  Parenthesized(
                    Seq.mk(
                      intlit("1"),
                      [(Operators_Pat.Comma, boollit(true))],
                    )
                    |> mk_OpSeq,
                  ),
                )
              ),
              Block.wrap(floatlit("1.0")),
            ),
            Rule(
              UHPat.(
                OpSeq.wrap(
                  Parenthesized(
                    Seq.mk(wild(), [(Operators_Pat.Comma, boollit(false))])
                    |> mk_OpSeq,
                  ),
                )
              ),
              Block.wrap(floatlit("3.0")),
            ),
            Rule(OpSeq.wrap(UHPat.wild()), Block.wrap(floatlit("5.0"))),
          ],
        ),
      )
      |> mk_OpSeq,
    ),
  ];

let case_expl_1 = {
  id: "explanation_1",
  expression: "Consider by the cases of [`x`](0). If [`x`](0) matches: \n- the first pattern [`(1, true)`](1 0), evaluate to the first clause [`1.0`](1 1).\n- the second pattern [`(_, false)`](2 0), evaluate to the second clause [`3.0`](2 1).\n- otherwise, evaluate to the last clause [`5.0`](3 1).",
  rank: (-1),
};

let case_expl_2 = {
  id: "explanation_2",
  expression: "Consider by the cases of [`x`](0). If [`x`](0) matches: \n- the first pattern [`(1, true)`](1 0), evaluate to the first clause [`1.0`](1 1). The first pattern is matched if the first element of [`x`](0) is `1` and the second element is `true`.\n- the second pattern [`(_, false)`](2 0), evaluate to the second clause [`3.0`](2 1). The second pattern is matched for any first element of [`x`](0) if the second element is `false`.\n- otherwise, evaluate to the last clause [`5.0`](3 1). The last pattern is a [wildcard](3 0) pattern which matches any value.",
  rank: (-1),
};

let case_expl_3 = {
  id: "explanation_3",
  expression: "Consider by the cases of [`x`](0). For the first branch with a matching pattern to [`x`](0), evaluate to the clause of that branch.",
  rank: (-1),
};

let case_ex_1 = {
  idz: "example_1",
  expressionz:
    UHExp.[
      ExpLine(
        Seq.wrap(
          case(
            Block.wrap(UHExp.intlit("2")),
            [
              Rule(
                UHPat.(OpSeq.wrap(intlit("1"))),
                Block.wrap(floatlit("1.0")),
              ),
              Rule(
                UHPat.(OpSeq.wrap(intlit("2"))),
                Block.wrap(floatlit("2.0")),
              ),
              Rule(
                UHPat.(OpSeq.wrap(wild())),
                Block.wrap(floatlit("3.0")),
              ),
            ],
          ),
        )
        |> mk_OpSeq,
      ),
    ],
  caption: "The scrutinee 2 matches the 2 pattern, so the case expression evaluates to the second clause 2.0.",
  rankz: (-1),
  result: DHExp.FloatLit(2.0),
};

let case_ex_2 = {
  idz: "example_2",
  expressionz:
    UHExp.[
      letline(
        UHPat.(OpSeq.wrap(var("int_to_float"))),
        Block.wrap(
          lam(
            UHPat.(OpSeq.wrap(var("i"))),
            Block.wrap(
              case(
                Block.wrap(UHExp.var("i")),
                [
                  Rule(
                    OpSeq.wrap(UHPat.intlit("1")),
                    Block.wrap(UHExp.floatlit("1.0")),
                  ),
                  Rule(
                    OpSeq.wrap(UHPat.intlit("2")),
                    Block.wrap(UHExp.floatlit("2.0")),
                  ),
                  Rule(
                    OpSeq.wrap(UHPat.wild()),
                    Block.wrap(UHExp.floatlit("3.0")),
                  ),
                ],
              ),
            ),
          ),
        ),
      ),
      ExpLine(
        Seq.mk(var("int_to_float"), [(Operators_Exp.Space, intlit("2"))])
        |> mk_OpSeq,
      ),
    ],
  caption: "First, the function int_to_float takes 2 as an argument. The case expression pattern matches on this argument. The argument matches the pattern of the second branch, so the function evalautes to 2.0.",
  rankz: (-1),
  result: DHExp.FloatLit(2.0),
};

let case_ex_3 = {
  idz: "example_3",
  expressionz:
    UHExp.[
      ExpLine(
        Seq.wrap(
          case(
            Block.wrap(UHExp.intlit("2")),
            [
              Rule(
                UHPat.(
                  OpSeq.wrap(
                    Parenthesized(
                      Seq.mk(
                        intlit("1"),
                        [(Operators_Pat.Comma, intlit("3"))],
                      )
                      |> mk_OpSeq,
                    ),
                  )
                ),
                Block.wrap(floatlit("1.0")),
              ),
              Rule(
                UHPat.(
                  OpSeq.wrap(
                    Parenthesized(
                      Seq.mk(intlit("2"), [(Operators_Pat.Comma, wild())])
                      |> mk_OpSeq,
                    ),
                  )
                ),
                Block.wrap(floatlit("2.0")),
              ),
              Rule(
                UHPat.(OpSeq.wrap(wild())),
                Block.wrap(floatlit("3.0")),
              ),
            ],
          ),
        )
        |> mk_OpSeq,
      ),
    ],
  caption: "The scrutinee (2, 3) does not match the first pattern because the first element of the tuple 2 does not match 1. The scrutinee does match the second pattern (2, _) because 2 matches the 2 pattern and anything matches the wildcard pattern. Thus, the expression evaluates to the second clause 2.0.",
  rankz: (-1),
  result: DHExp.FloatLit(2.0),
};

let case_ex_4 = {
  idz: "example_4",
  expressionz:
    UHExp.[
      ExpLine(
        Seq.wrap(
          case(
            Block.wrap(
              Parenthesized(
                Block.wrap'(
                  Seq.mk(
                    intlit("2"),
                    [(Operators_Exp.Comma, boollit(true))],
                  )
                  |> mk_OpSeq,
                ),
              ),
            ),
            [
              Rule(
                UHPat.(
                  OpSeq.wrap(
                    Parenthesized(
                      Seq.mk(
                        intlit("1"),
                        [(Operators_Pat.Comma, boollit(true))],
                      )
                      |> mk_OpSeq,
                    ),
                  )
                ),
                Block.wrap(floatlit("1.0")),
              ),
              Rule(
                UHPat.(
                  OpSeq.wrap(
                    Parenthesized(
                      Seq.mk(
                        intlit("2"),
                        [(Operators_Pat.Comma, boollit(false))],
                      )
                      |> mk_OpSeq,
                    ),
                  )
                ),
                Block.wrap(floatlit("2.0")),
              ),
              Rule(
                UHPat.(OpSeq.wrap(wild())),
                Block.wrap(floatlit("3.0")),
              ),
            ],
          ),
        )
        |> mk_OpSeq,
      ),
    ],
  caption: "The scrutinee (2, true) does not match the first pattern because the first element of the tuple 2 does not match 1. The scrutinee does not match the second pattern because the second element of the tuple true does not match false. The scrutinee matches the last pattern because anything matches the wildcard pattern. Thus, the expression evaluates to the last clause 3.0.",
  rankz: (-1),
  result: DHExp.FloatLit(2.0),
};

let case_explanations = [case_expl_1, case_expl_2, case_expl_3];
let case_examples = [case_ex_1, case_ex_2, case_ex_3, case_ex_4];

/****************** LAMBDA WITH TUPLE ***********************************************/
let lambda_with_tuple_snippet =
  UHExp.[
    ExpLine(
      Seq.wrap(
        lam(
          UHPat.(
            OpSeq.wrap(
              Parenthesized(
                Seq.mk(var("x"), [(Operators_Pat.Comma, var("y"))])
                |> mk_OpSeq,
              ),
            )
          ),
          Block.wrap(
            Parenthesized(
              Block.wrap'(
                Seq.mk(var("x"), [(Operators_Exp.Times, var("y"))])
                |> mk_OpSeq,
              ),
            ),
          ),
        ),
      )
      |> mk_OpSeq,
    ),
  ];

let lambda_with_tuple_expl_1 = {
  id: "explanation_1",
  expression: "A function that takes in two arguments [`x`](0 0) and [`y`](0 1) and returns the result of computing the body [`x * y`](1).",
  rank: (-1),
};

let lambda_with_tuple_expl_2 = {
  id: "explanation_2",
  expression: "Function literal that returns the value of the body [`x * y`](1) when applied to an argument [`(x, y)`](0).",
  rank: (-1),
};

let lambda_with_tuple_expl_3 = {
  id: "explanation_3",
  expression: "Function literal that returns the value of the body [`x * y`](1) when applied to an argument tuple containing [`x`](0 0) and [`y`](0 1).",
  rank: (-1),
};

let lambda_with_tuple_ex_1 = {
  idz: "example_1",
  expressionz:
    UHExp.[
      letline(
        UHPat.(OpSeq.wrap(var("incr_or_decr"))),
        Block.wrap(
          lam(
            UHPat.(
              OpSeq.wrap(
                Parenthesized(
                  Seq.mk(
                    var("is_incr"),
                    [(Operators_Pat.Comma, var("val"))],
                  )
                  |> mk_OpSeq,
                ),
              )
            ),
            Block.wrap(
              case(
                Block.wrap(UHExp.var("is_incr")),
                [
                  Rule(
                    OpSeq.wrap(UHPat.boollit(true)),
                    Block.wrap'(
                      Seq.mk(
                        var("val"),
                        [(Operators_Exp.Plus, intlit("1"))],
                      )
                      |> mk_OpSeq,
                    ),
                  ),
                  Rule(
                    OpSeq.wrap(UHPat.boollit(false)),
                    Block.wrap'(
                      Seq.mk(
                        var("val"),
                        [(Operators_Exp.Minus, intlit("1"))],
                      )
                      |> mk_OpSeq,
                    ),
                  ),
                ],
              ),
            ),
          ),
        ),
      ),
      ExpLine(
        Seq.mk(
          var("incr_or_decr"),
          [
            (
              Operators_Exp.Space,
              Parenthesized(
                Block.wrap'(
                  Seq.mk(
                    boollit(true),
                    [(Operators_Exp.Comma, intlit("2"))],
                  )
                  |> mk_OpSeq,
                ),
              ),
            ),
          ],
        )
        |> mk_OpSeq,
      ),
    ],
  caption: "First, the function incr_or_decr is applied to argument (true, 2), and evaluating the body of the function results in case true | true => val + 1 | false => val - 1. This case expression evaluates to 2 + 1 which equals 6.",
  rankz: (-1),
  result: DHExp.IntLit(3),
};

let lambda_with_tuple_ex_2 = {
  idz: "example_2",
  expressionz:
    UHExp.[
      letline(
        UHPat.(OpSeq.wrap(var("add"))),
        Block.wrap(
          lam(
            UHPat.(
              OpSeq.wrap(
                Parenthesized(
                  Seq.mk(
                    var("x"),
                    [
                      (Operators_Pat.Comma, var("y")),
                      (Operators_Pat.Comma, var("z")),
                    ],
                  )
                  |> mk_OpSeq,
                ),
              )
            ),
            Block.wrap'(
              Seq.mk(
                var("x"),
                [
                  (Operators_Exp.Plus, var("y")),
                  (Operators_Exp.Plus, var("z")),
                ],
              )
              |> mk_OpSeq,
            ),
          ),
        ),
      ),
      ExpLine(
        Seq.mk(
          var("add"),
          [
            (
              Operators_Exp.Space,
              Parenthesized(
                Block.wrap'(
                  Seq.mk(
                    intlit("1"),
                    [
                      (Operators_Exp.Comma, intlit("2")),
                      (Operators_Exp.Comma, intlit("3")),
                    ],
                  )
                  |> mk_OpSeq,
                ),
              ),
            ),
          ],
        )
        |> mk_OpSeq,
      ),
    ],
  caption: "First, the function add is applied to argument (1, 2, 3), and evaluating the body of the function results in 1 + 2 + 3 which equals 6.",
  rankz: (-1),
  result: DHExp.IntLit(6),
};

let lambda_with_tuple_ex_3 = {
  idz: "example_3",
  expressionz:
    UHExp.(
      Block.wrap'(
        Seq.mk(
          lam(
            UHPat.(OpSeq.wrap(var("x"))),
            Block.wrap'(
              Seq.mk(var("x"), [(Operators_Exp.Plus, intlit("1"))])
              |> mk_OpSeq,
            ),
          ),
          [(Operators_Exp.Space, intlit("2"))],
        )
        |> mk_OpSeq,
      )
    ),
  caption: "The function is applied to argument 2, and evaluating the body of the function gives 2 + 1 which equals 3.",
  rankz: (-1),
  result: DHExp.IntLit(3),
};

let lambda_with_tuple_ex_4 = {
  idz: "example_4",
  expressionz:
    UHExp.[
      letline(
        UHPat.(OpSeq.wrap(var("add_one"))),
        Block.wrap(
          lam(
            UHPat.(OpSeq.wrap(var("x"))),
            Block.wrap'(
              Seq.mk(var("x"), [(Operators_Exp.Plus, intlit("1"))])
              |> mk_OpSeq,
            ),
          ),
        ),
      ),
      ExpLine(
        Seq.mk(var("add_one"), [(Operators_Exp.Space, intlit("2"))])
        |> mk_OpSeq,
      ),
    ],
  caption: "The function add_one is applied to argument 2, and evaluating the body of the function gives 2 + 1 which equals 3.",
  rankz: (-1),
  result: DHExp.IntLit(3),
};

let lambda_with_tuple_ex_5 = {
  idz: "example_5",
  expressionz:
    UHExp.(
      Block.wrap(
        lam(
          UHPat.(OpSeq.wrap(var("x"))),
          Block.wrap'(
            Seq.mk(var("x"), [(Operators_Exp.Plus, intlit("1"))])
            |> mk_OpSeq,
          ),
        ),
      )
    ),
  caption: "The function literal fun x {x + 1} is a value. The body is only evaluated when the function is applied to an argument.",
  rankz: (-1),
  result: DHExp.IntLit(3),
};

let lambda_with_tuple_explanations = [
  lambda_with_tuple_expl_1,
  lambda_with_tuple_expl_2,
  lambda_with_tuple_expl_3,
];
let lambda_with_tuple_examples = [
  lambda_with_tuple_ex_1,
  lambda_with_tuple_ex_2,
  lambda_with_tuple_ex_3,
  lambda_with_tuple_ex_4,
  lambda_with_tuple_ex_5,
];

/****************** LET WITH TUPLE ***********************************************/
let let_with_tuple_snippet =
  UHExp.[
    letline(
      UHPat.(
        OpSeq.wrap(
          Parenthesized(
            Seq.mk(var("x"), [(Operators_Pat.Comma, var("y"))])
            |> mk_OpSeq,
          ),
        )
      ),
      Block.wrap(
        Parenthesized(
          Block.wrap'(
            Seq.mk(intlit("1"), [(Operators_Exp.Comma, intlit("2"))])
            |> mk_OpSeq,
          ),
        ),
      ),
    ),
    ExpLine(
      Seq.mk(var("x"), [(Operators_Exp.Plus, var("y"))]) |> mk_OpSeq,
    ),
  ];

let let_with_tuple_expl_1 = {
  id: "explanation_1",
  expression: "Bind the definition [`(1, 2)`](1) to the pattern [`(x, y)`](0) and evaluate the body [`x + y`](2).",
  rank: (-1),
};

let let_with_tuple_expl_2 = {
  id: "explanation_2",
  expression: "In the body [`x + y`](2), the first pattern [`x`](0 0) will be bound to the first element [`1`](1 0) and the second pattern [`y`](0 1) will be bound to the second element [`2`](1 1) of the definition tuple.",
  rank: (-1),
};

let let_with_tuple_expl_3 = {
  id: "explanation_3",
  expression: "In the body [`x + y`](2), the first pattern [`x`](0 0) gets replaced by the first element [`1`](1 0) and the second pattern [`y`](0 1) gets replaced by the second element [`2`](1 1) of the definition tuple.",
  rank: (-1),
};

let let_with_tuple_ex_1 = {
  idz: "example_1",
  expressionz:
    UHExp.[
      letline(UHPat.(OpSeq.wrap(var("x"))), Block.wrap(intlit("1"))),
      ExpLine(
        Seq.mk(var("x"), [(Operators_Exp.Plus, intlit("2"))]) |> mk_OpSeq,
      ),
    ],
  caption: "x is bound to 1 in x + 2, which evaluates to 1 + 2 which equals 3",
  rankz: (-1),
  result: DHExp.IntLit(3),
};

let let_with_tuple_ex_2 = {
  idz: "example_2",
  expressionz:
    UHExp.[
      letline(
        UHPat.(
          OpSeq.wrap(
            Parenthesized(
              Seq.mk(var("x"), [(Operators_Pat.Comma, var("y"))])
              |> mk_OpSeq,
            ),
          )
        ),
        Block.wrap(
          Parenthesized(
            Block.wrap'(
              Seq.mk(intlit("2"), [(Operators_Exp.Comma, intlit("3"))])
              |> mk_OpSeq,
            ),
          ),
        ),
      ),
      ExpLine(
        Seq.mk(var("x"), [(Operators_Exp.Times, var("y"))]) |> mk_OpSeq,
      ),
    ],
  caption: "x is bound to 2 and y is bound to 3 in x + y which evaluates to 2 * 3 which equals 6.",
  rankz: (-1),
  result: DHExp.IntLit(6),
};

let let_with_tuple_ex_3 = {
  idz: "example_3",
  expressionz:
    UHExp.[
      letline(
        UHPat.(
          OpSeq.wrap(
            Parenthesized(
              Seq.mk(var("is_incr"), [(Operators_Pat.Comma, var("val"))])
              |> mk_OpSeq,
            ),
          )
        ),
        Block.wrap(
          Parenthesized(
            Block.wrap'(
              Seq.mk(boollit(true), [(Operators_Exp.Comma, intlit("2"))])
              |> mk_OpSeq,
            ),
          ),
        ),
      ),
      ExpLine(
        OpSeq.wrap(
          case(
            Block.wrap(UHExp.var("is_incr")),
            [
              Rule(
                OpSeq.wrap(UHPat.boollit(true)),
                Block.wrap'(
                  Seq.mk(var("val"), [(Operators_Exp.Plus, intlit("1"))])
                  |> mk_OpSeq,
                ),
              ),
              Rule(
                OpSeq.wrap(UHPat.boollit(false)),
                Block.wrap'(
                  Seq.mk(var("val"), [(Operators_Exp.Minus, intlit("1"))])
                  |> mk_OpSeq,
                ),
              ),
            ],
          ),
        ),
      ),
    ],
  caption: "is_incr is bound to true and val is bound to 2 in the case expression which evaluates to 2 + 1 which equals 3.",
  rankz: (-1),
  result: DHExp.IntLit(3),
};

let let_with_tuple_explanations = [
  let_with_tuple_expl_1,
  let_with_tuple_expl_2,
  let_with_tuple_expl_3,
];
let let_with_tuple_examples = [
  let_with_tuple_ex_1,
  let_with_tuple_ex_2,
  let_with_tuple_ex_3,
];

/************* PROMPTS ***************/
let prompt_msg = "Rank the code explanations and examples on the right for the selected code snippet below. Higher rankings correspond more useful explanations and examples. Use the indicated syntactic form as a proxy for what a programmer is trying to understand about the code snippet.";

let prompts: list(t) = [
  {
    key: "let with tuple - less specific",
    program: ZExp.place_before(let_with_tuple_snippet),
    prompt_message: prompt_msg,
    explanation: let_with_tuple_explanations,
    explanation_text_box: "",
    examples: let_with_tuple_examples,
    example_text_box: "example test",
    syntactic_form_level: 2,
  },
  {
    key: "let with tuple - more specific",
    program: ZExp.place_before(let_with_tuple_snippet),
    prompt_message: prompt_msg,
    explanation: let_with_tuple_explanations,
    explanation_text_box: "",
    examples: let_with_tuple_examples,
    example_text_box: "",
    syntactic_form_level: 4,
  },
  {
    key: "lambda with tuple - less specific",
    program: ZExp.place_before(lambda_with_tuple_snippet),
    prompt_message: prompt_msg,
    explanation: lambda_with_tuple_explanations,
    explanation_text_box: "",
    examples: lambda_with_tuple_examples,
    example_text_box: "",
    syntactic_form_level: 2,
  },
  {
    key: "lambda with tuple - more specific",
    program: ZExp.place_before(lambda_with_tuple_snippet),
    prompt_message: prompt_msg,
    explanation: lambda_with_tuple_explanations,
    explanation_text_box: "",
    examples: lambda_with_tuple_examples,
    example_text_box: "",
    syntactic_form_level: 4,
  },
  {
    key: "case",
    program:
      OptUtil.get(
        () => failwith("bad path"),
        CursorPath_Exp.follow(([1, 0], OnDelim(0, Before)), case_snippet),
      ),
    prompt_message: prompt_msg,
    explanation: case_explanations,
    explanation_text_box: "",
    examples: case_examples,
    example_text_box: "",
    syntactic_form_level: 1,
  },
  {
    key: "tuple function application - less specific",
    program:
      OptUtil.get(
        () => failwith("bad path"),
        CursorPath_Exp.follow(
          ([1, 0], OnDelim(0, Before)),
          tuple_fun_app_snippet,
        ),
      ),
    prompt_message: prompt_msg,
    explanation: tuple_fun_app_explanations,
    explanation_text_box: "",
    examples: tuple_fun_app_examples,
    example_text_box: "",
    syntactic_form_level: 1,
  },
  {
    key: "tuple function application - more specific",
    program:
      OptUtil.get(
        () => failwith("bad path"),
        CursorPath_Exp.follow(
          ([1, 0], OnDelim(0, Before)),
          tuple_fun_app_snippet,
        ),
      ),
    prompt_message: prompt_msg,
    explanation: tuple_fun_app_explanations,
    explanation_text_box: "",
    examples: tuple_fun_app_examples,
    example_text_box: "",
    syntactic_form_level: 2,
  },
  {
    key: "curried function application - less specific",
    program:
      OptUtil.get(
        () => failwith("bad path"),
        CursorPath_Exp.follow(
          ([1, 0], OnDelim(0, Before)),
          curry_fun_app_snippet,
        ),
      ),
    prompt_message: prompt_msg,
    explanation: curry_fun_app_explanations,
    explanation_text_box: "",
    examples: curry_fun_app_examples,
    example_text_box: "",
    syntactic_form_level: 1,
  },
  {
    key: "curried function application - more specific",
    program:
      OptUtil.get(
        () => failwith("bad path"),
        CursorPath_Exp.follow(
          ([1, 0], OnDelim(0, Before)),
          curry_fun_app_snippet,
        ),
      ),
    prompt_message: prompt_msg,
    explanation: curry_fun_app_explanations,
    explanation_text_box: "",
    examples: curry_fun_app_examples,
    example_text_box: "",
    syntactic_form_level: 2,
  },
];

// given a prompt and an explanation index, updates the rank
// Same function but for examples
let update_explanation_rank = (prompt, index, rank) => {
  let new_explanations =
    List.mapi(
      (idx, explanation) =>
        if (idx == index) {
          {...explanation, rank};
        } else {
          explanation;
        },
      prompt.explanation,
    );
  {...prompt, explanation: new_explanations};
};
let update_example_rank = (prompt, index, rank) => {
  let new_examples =
    List.mapi(
      (idx, example) =>
        if (idx == index) {
          {...example, rankz: rank};
        } else {
          example;
        },
      prompt.examples,
    );
  {...prompt, examples: new_examples};
};

let update_explanation_text = (prompt, text) => {
  {...prompt, explanation_text_box: text};
};

let update_example_text = (prompt, text) => {
  {...prompt, example_text_box: text};
};

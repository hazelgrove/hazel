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

let lambda_with_tuple =
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

let lambda_with_tuple_ex_1 =
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
    ExpLine(OpSeq.wrap(var("x"))),
  ];

let lambda_with_tuple_ex_2 =
  UHExp.[
    letline(
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
      Block.wrap(
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
    ),
    ExpLine(OpSeq.wrap(var("y"))),
  ];

let prompts: list(t) = [
  {
    key: "lambda_with_tuple_",
    program: ZExp.place_before(lambda_with_tuple),
    prompt_message: "Rank the code explanations and examples for the code snippet below, targetting a user who is having their first exposure to functional programming concepts. The user is only trying to make small adjustments to starter code they have been given.",
    explanation: [
      {
        id: "explanation_1",
        expression: "In the body `x + y`, the first pattern `x` will be bound to the first element `1` and the second pattern `y` will be bound to the second element `2` of the definition tuple.",
        rank: (-1),
      },
      {
        id: "explanation_2",
        expression: "Bind the definition `(1, 2)` to the pattern `(x, y)` and evaluate the body `x + y`.\n - The first pattern `x` will be bound to the first element `1` and the second pattern `y` will be bound to the second element `2`.",

        rank: (-1),
      },
    ],
    examples: [
      {
        idz: "example_1",
        expressionz: lambda_with_tuple_ex_1,
        caption: "test caption",
        rankz: (-1),
      },
      {
        idz: "example_2",
        expressionz: lambda_with_tuple_ex_2,
        caption: "test caption",
        rankz: (-1),
      },
    ],
  },
];

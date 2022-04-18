// TODO change this type to have the right kind of expression
module Memo = Core_kernel.Memo;

// General type for a single example
type quest = {
  idz: string,
  expressionz: UHExp.t,
  rankz: int,
};

type explain = {
  id: string,
  expression: string,
  rank: int,
};

/*
 TODO: The centralized model should keep track of all of the different prompts (so add to the type t at the top of Model.re something like prompt: list(Prompt.t)
  */

type t = {
  key: string,
  program: ZExp.t,
  prompt_message: string,
  explanation: list(explain),
  examples: list(quest),
};

let prompts: list(t) = [
  {
    key: "dummy-key",
    program: ZExp.place_before(UHExp.Block.wrap(EmptyHole(0))),
    prompt_message: "Pretend the user knows some things",
    explanation: [
      {id: "dummy-exp-key", expression: "Some sort of explanation", rank: 6},
    ],
    examples: [
      {
        idz: "dummy-ex-key",
        expressionz: UHExp.Block.wrap(EmptyHole(0)),
        rankz: 6,
      },
    ],
  },
];

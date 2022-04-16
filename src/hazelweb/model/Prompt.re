// TODO change this type to have the right kind of expression
module Memo = Core_kernel.Memo;

let elaborate = Elaborator_Exp.syn_elab(Contexts.empty, Delta.empty);
let get_elaboration = (program: UHExp.t): DHExp.t =>
  switch (program |> elaborate) {
  | DoesNotElaborate => raise(Program.DoesNotElaborate)
  | Elaborates(d, _, _) => d
  };
// TODO need to call DHCode.view() to display this

// General type for a single example
type quest = {
  id: string,
  expression: UHExp.t,
  rank: int,
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

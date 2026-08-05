open Haz3lcore;
open Example;
open ExplainThisForm;

let explored_exp = exp("e");

let explore_exp_coloring_ids = (~exp_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(explored_exp), exp_id),
];

let explore_exp: form = {
  let explanation = "Opens the [*expression*](%s) in an exploratory stepper without producing a program value. Free variables in the expression are treated symbolically using their assumed types.";
  {
    id: ExploreExp,
    syntactic_form: [mk_explore([[space(), explored_exp, space()]])],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let explores: group = {
  id: ExploreExp,
  forms: [explore_exp],
};

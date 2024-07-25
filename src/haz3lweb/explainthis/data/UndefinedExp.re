open ExplainThisForm;
open Example;

let undefined_ex_1 = {
  sub_id: Undefined2,
  term:
    mk_example(
      "let sgn = \nfun num ->\nif num == 0  \nthen undefined  \nelse\nif num > 0 \nthen \"+\"\nelse \"-\"\nin\n(sgn(-1), sgn(0), sgn(5))",
    ),
  message: "The undefined expression can be used in cases where a partial function is undefined.",
};

let undefined_ex_2 = {
  sub_id: Undefined1,
  term:
    mk_example(
      "let sum : [Int] -> Int =\nfun xs ->\ncase undefined\n| [] => 0\n| hd::tl => \nend\nin\nsum([1,2,3])",
    ),
  message: "The undefined expression behaves much like a hole during evaluation.",
};

let undefined_exp: form = {
  let explanation = "Represents an expression that lacks definition.";
  {
    id: UndefinedExp,
    syntactic_form: [exp("undefined")],
    expandable_id: None,
    explanation,
    examples: [undefined_ex_1, undefined_ex_2],
  };
};

let undefined_exps: group = {id: UndefinedExp, forms: [undefined_exp]};

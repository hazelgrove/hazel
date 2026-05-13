open Haz3lcore;
open ExplainThisForm;
open Example;

let typfunapp_exp_ex = {
  sub_id: TypFunAp,
  term:
    mk_example(
      "let id : \n poly a -> (a -> a) = \n typfun a -> \n fun x : a -> x \n in id@<Int>",
    ),
  message: "The polymorphic identity function is instantiated at Int. The type variable a is bound to Int in the type function body and the body evaluates to the identity function on integers.",
};
let exp_tfun = exp("e_tfun");
let typ = typ("ty");
let typfunapp_exp_coloring_ids =
    (~f_id: Id.t, ~typ_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(exp_tfun), f_id),
  (Piece.id(typ), typ_id),
];
let typfunapp_exp: form = {
  let explanation = "Applies the [*type function*](%s) to the [*type*](%s).";
  {
    id: TypFunApExp,
    syntactic_form: [exp_tfun, mk_ap_exp_typ([[typ]])],
    expandable_id: None,
    explanation,
    examples: [typfunapp_exp_ex],
  };
};

let typfunaps: group = {
  id: TypFunApExp,
  forms: [typfunapp_exp],
};

open Haz3lcore;
open ExplainThisForm;
open Example;

let poly_id_ex = {
  sub_id: TypFun(Basic),
  term:
    mk_example(
      "let id : \n forall X -> (X -> X) = \n typfun X -> \n fun x : X -> x \n in id",
    ),
  message: "The polymorphic identity function. It may be instantiated at any type X, after which the function acts as type (X -> X).",
};

let _tp = tpat("X");
let _exp = exp("e");
let typfun_var: form = {
  let explanation = "When applied to a type that which is bound to the [*type variable*](%s), evaluates to the type function [*body*](%s).";
  let form = [mk_fun([[space(), _tp, space()]]), space(), _exp];
  {
    id: TypFunctionExp,
    syntactic_form: form,
    expandable_id:
      Some((Piece.id(_tp), [Grout({id: Id.mk(), shape: Convex})])),
    explanation,
    examples: [poly_id_ex],
  };
};

let type_functions_basic = {id: TypFunctionExp, forms: [typfun_var]};

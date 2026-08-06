open Haz3lcore;
open ExplainThisForm;
open Example;

let poly_id_ex = {
  sub_id: TypFun(Basic),
  term:
    mk_example(
      "let id : \n poly a -> (a -> a) = \n typfun a -> \n fun x : a -> x \n in id",
    ),
  message: "The polymorphic identity function. It may be instantiated at any type a, after which the function acts as type (a -> a).",
};

let tp = tpat("a");
let e = exp("e");
let typfun_var_form = [mk_typfun([[space(), tp, space()]]), space(), e];
let typfun_var_expandable =
  Piece.Grout({
    id: Id.mk(),
    shape: Convex,
  });
let typfun_var = (~tpat_id: Id.t, ~body_id: Id.t): form => {
  id: TypFunctionExp,
  syntactic_form: typfun_var_form,
  colorings: [],
  expandable_id: Some((Piece.id(tp), [typfun_var_expandable])),
  explanation:
    Printf.sprintf(
      "When applied to a type that which is bound to the [*type variable*](%s), evaluates to the type function [*body*](%s).",
      Id.to_string(tpat_id),
      Id.to_string(body_id),
    ),
  examples: [poly_id_ex],
};

let type_functions_basic = (~tpat_id: Id.t, ~body_id: Id.t): group => {
  id: TypFunctionExp,
  forms: [typfun_var(~tpat_id, ~body_id)],
};

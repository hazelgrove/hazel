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
let typfunapp_exp_form = [exp_tfun, mk_ap_exp_typ([[typ]])];
let typfunapp_exp = (~f_id: Id.t, ~typ_id: Id.t): form => {
  id: TypFunApExp,
  syntactic_form: typfunapp_exp_form,
  colorings: typfunapp_exp_coloring_ids(~f_id, ~typ_id),
  expandable_id: None,
  explanation:
    Stdlib.Printf.sprintf(
      "Applies the [*type function*](%s) to the [*type*](%s).",
      Id.to_string(f_id),
      Id.to_string(typ_id),
    ),
  examples: [typfunapp_exp_ex],
};

let typfunaps = (~f_id: Id.t, ~typ_id: Id.t): group =>
  singleton(typfunapp_exp(~f_id, ~typ_id));

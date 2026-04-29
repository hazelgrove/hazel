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

let _tp = tpat("a");
let _exp = exp("e");
let typfun_var: form = {
  let explanation = "A value-level type abstraction. At a type application `@<X>` the [*type variable*](%s) is substituted by `X` in the [*body*](%s). Multi-binder forms `typfun a, b -> e` abstract over several type variables at once, consumed together by a single `@<X, Y>` type application.";
  let form = [mk_typfun([[space(), _tp, space()]]), space(), _exp];
  {
    id: TypFunctionExp,
    syntactic_form: form,
    expandable_id:
      Some((
        Piece.id(_tp),
        [
          Grout({
            id: Id.mk(),
            shape: Convex,
          }),
        ],
      )),
    explanation,
    examples: [poly_id_ex],
  };
};

let type_functions_basic = {
  id: TypFunctionExp,
  forms: [typfun_var],
};

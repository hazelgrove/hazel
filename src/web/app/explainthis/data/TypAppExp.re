open Haz3lcore;
open ExplainThisForm;
open Example;

let typabsap_exp_ex = {
  sub_id: TypAbsAp,
  term:
    mk_example(
      "let id : \n poly a -> (a -> a) = \n abs a -> \n fun x : a -> x \n in id@<Int>",
    ),
  message: "The polymorphic identity function is instantiated at Int. The type variable a is bound to Int in the type abstraction's body, which evaluates to the identity function on integers.",
};

let typabsap2_exp_ex = {
  sub_id: TypAbsAp2,
  term:
    mk_example(
      "let swap : \n poly a, b -> (a, b) -> (b, a) = \n abs a, b -> \n fun (x, y) -> (y, x) \n in swap@<Int, Bool>((1, true))",
    ),
  message: "swap binds two type variables, so one application supplies both type arguments.",
};

/* `e@<T>`, `e@<T1, T2>`: instantiating a polymorphic value. */
let typabsap_exp = (~f_id: Id.t, ~typ_ids: list(Id.t)): form => {
  let n = List.length(typ_ids);
  let exp_tabs = exp("e_poly");
  let (args_seg, arg_pieces) = typ_list("ty", n);
  {
    id: TypAbsApExp,
    syntactic_form: [exp_tabs, mk_ap_exp_typ([args_seg])],
    colorings:
      [(Piece.id(exp_tabs), f_id)] @ zip_colorings(arg_pieces, typ_ids),
    expandable_id: None,
    explanation:
      Printf.sprintf(
        "Instantiates the [*polymorphic value*](%s) at the %s %s: the type %s substituted for the type variable%s its `poly` type binds, which must be as many as are given here.",
        Id.to_string(f_id),
        n == 1 ? "type" : "types",
        links("type", typ_ids),
        n == 1 ? "is" : "are",
        n == 1 ? "" : "s",
      ),
    examples: n == 1 ? [typabsap_exp_ex] : [typabsap2_exp_ex],
  };
};

let typabsaps = (~f_id: Id.t, ~typ_ids: list(Id.t)): group =>
  singleton(typabsap_exp(~f_id, ~typ_ids));

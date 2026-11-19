open Haz3lcore;
open ExplainThisForm;
open Example;

let poly_id_ex = {
  sub_id: TypAbs(Basic),
  term:
    mk_example(
      "let id : \n poly a -> (a -> a) = \n abs a -> \n fun x : a -> x \n in id",
    ),
  message: "The polymorphic identity function. It may be instantiated at any type a, after which the function acts as type (a -> a).",
};

let poly_pair_ex = {
  sub_id: TypAbs(Multi),
  term:
    mk_example(
      "let swap : \n poly a, b -> (a, b) -> (b, a) = \n abs a, b -> \n fun (x, y) -> (y, x) \n in swap@<Int, Bool>",
    ),
  message: "A type abstraction over two type variables. One type application supplies both type arguments.",
};

/* `abs a -> e`, `abs a, b -> e`: a value-level type abstraction, the
   values of `poly` types. [tpat_ids] are the binders' terms. */
let typ_abs = (~binders: list((string, Id.t)), ~body_id: Id.t): form => {
  let tpat_ids = List.map(~f=snd, binders);
  let n = List.length(tpat_ids);
  let (binder_seg, binder_pieces) = tpat_list("a", n);
  let e = exp("e");
  {
    id: TypAbsExp,
    syntactic_form: [
      mk_typabs([[space()] @ binder_seg @ [space()]]),
      space(),
      e,
    ],
    colorings:
      zip_colorings(binder_pieces, tpat_ids) @ [(Piece.id(e), body_id)],
    expandable_id: None,
    explanation:
      Printf.sprintf(
        "A type abstraction: a value that takes %s as %s. Its type is a `poly` type. Apply it with `e@<%s>` to substitute types for %s in the [*body*](%s).",
        named_links(binders),
        n == 1 ? "a type argument" : "type arguments",
        n == 1
          ? "T"
          : String.concat(
              ~sep=", ",
              List.init(n, ~f=i => Printf.sprintf("T%d", i + 1)),
            ),
        n == 1 ? "it" : "them",
        Id.to_string(body_id),
      ),
    examples: n == 1 ? [poly_id_ex] : [poly_pair_ex],
  };
};

let typ_abss = (~binders: list((string, Id.t)), ~body_id: Id.t): group =>
  singleton(typ_abs(~binders, ~body_id));

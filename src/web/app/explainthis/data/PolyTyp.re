open Haz3lcore;
open Example;
open ExplainThisForm;

/* `poly a -> t`, `poly a, b -> t`: the type of polymorphic values. A
   proper type (kind `Type`), unlike the type function `typfun a -> t`. */
let poly_typ = (~binders: list((string, Id.t)), ~tbody_id: Id.t): form => {
  let tpat_ids = List.map(snd, binders);
  let n = List.length(tpat_ids);
  let (binder_seg, binder_pieces) = tpat_list("a", n);
  let typ_body = typ("ty");
  {
    id: PolyTyp,
    syntactic_form: [
      mk_poly([[space()] @ binder_seg @ [space()]]),
      space(),
      typ_body,
    ],
    colorings:
      zip_colorings(binder_pieces, tpat_ids)
      @ [(Piece.id(typ_body), tbody_id)],
    expandable_id: None,
    explanation:
      Printf.sprintf(
        "A polymorphic type: the type of values that work at every choice of %s, with type [*body*](%s). It is an ordinary type (kind `Type`) whose values are type abstractions (`abs`), used by applying them to types, `e@<T>`. Compare `typfun`, which is not the type of any value but a function from types to types.",
        named_links(binders),
        Id.to_string(tbody_id),
      ),
    examples: [],
  };
};

let poly = (~binders: list((string, Id.t)), ~tbody_id: Id.t): group =>
  singleton(poly_typ(~binders, ~tbody_id));

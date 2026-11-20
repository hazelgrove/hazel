open Haz3lcore;
open Example;
open ExplainThisForm;

/* `a, b` as the binder of `poly`, `abs` or `typfun`. */
let tuple_tpat = (~binder_ids: list(Id.t)): form => {
  let n = List.length(binder_ids);
  let (seg, pieces) = tpat_list("a", n);
  {
    id: TupleTPat,
    syntactic_form: seg,
    colorings: zip_colorings(pieces, binder_ids),
    expandable_id: None,
    explanation:
      Printf.sprintf(
        "Binds the %d type variables %s at once. The form they bind takes all %d type arguments together, in one application.",
        n,
        links("type variable", binder_ids),
        n,
      ),
    examples: [],
  };
};

let tuple_tpats = (~binder_ids: list(Id.t)): group =>
  singleton(tuple_tpat(~binder_ids));

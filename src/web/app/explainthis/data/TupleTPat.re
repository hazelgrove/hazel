open Haz3lcore;
open Example;
open ExplainThisForm;

/* `a, b, …` comma-separated type binders as the binder of
   `poly`, `typfun`, `typlam`, or `rec`. `Tuple` only appears in a
   binder position; each element must be a plain type-variable
   pattern. */

let _p1 = tpat("a");
let _p2 = tpat("b");

let tuple_tpat_coloring_ids =
    (~params_ids: list(Id.t)): list((Id.t, Id.t)) => {
  let exemplars = [_p1, _p2];
  List.mapi(
    (i, pid) =>
      if (i < List.length(exemplars)) {
        (Piece.id(List.nth(exemplars, i)), pid);
      } else {
        (pid, pid);
      },
    params_ids,
  );
};

let tuple_tpat_form: form = {
  let explanation = "A comma-separated list of type binders, e.g. [*%s*](%s), [*%s*](%s), … used as the binder of a multi-binder `poly` or `typfun`.";
  {
    id: TupleTPat,
    syntactic_form: [_p1, comma_tpat(), space(), _p2],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let tuple_tpats: group = {
  id: TupleTPat,
  forms: [tuple_tpat_form],
};

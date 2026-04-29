open Haz3lcore;
open Example;
open ExplainThisForm;

/* `T(a, b)` as a type pattern — used in the outermost head of a
   parameterized type alias `type T(a, b) = def in body`. The head
   `T` is the alias's name; the parameters `a`, `b` are type
   variables bound in `def`. */

let _head = tpat("T");
let _p1 = tpat("a");
let _p2 = tpat("b");

let param_tpat_coloring_ids =
    (~head_id: Id.t, ~params_ids: list(Id.t)): list((Id.t, Id.t)) => {
  let exemplars = [_p1, _p2];
  let head = (Piece.id(_head), head_id);
  let params =
    List.mapi(
      (i, pid) =>
        if (i < List.length(exemplars)) {
          (Piece.id(List.nth(exemplars, i)), pid);
        } else {
          /* ExplainThis displays only the exemplar parameters; extra
             params keep their own id for the no-op case. */
          (pid, pid);
        },
      params_ids,
    );
  [head, ...params];
};

let param_tpat_form: form = {
  let explanation = "`%s` is a parameterized type constructor bound in the body. Its parameters [*%s*](%s), [*%s*](%s), … are type variables available inside the definition.";
  {
    id: ParamTPat,
    syntactic_form: [
      _head,
      mk_ap_tpat([[_p1, comma_tpat(), space(), _p2]]),
    ],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let param_tpats: group = {
  id: ParamTPat,
  forms: [param_tpat_form],
};

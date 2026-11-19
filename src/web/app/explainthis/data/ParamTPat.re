open Haz3lcore;
open Example;
open ExplainThisForm;

/* `T(a, b)` at the head of a type alias. */
let param_tpat = (~head_id: Id.t, ~param_ids: list(Id.t)): form => {
  let n = List.length(param_ids);
  let head = tpat("T");
  let (params_seg, param_pieces) = tpat_list("a", n);
  {
    id: ParamTPat,
    syntactic_form: [head, mk_ap_tpat([params_seg])],
    colorings:
      [(Piece.id(head), head_id)] @ zip_colorings(param_pieces, param_ids),
    expandable_id: None,
    explanation:
      Printf.sprintf(
        "Names a parameterized type [*%s*](%s) with the type %s %s. The alias is a type function: `T(%s)` stands for the definition with the arguments substituted for the parameters.",
        "T",
        Id.to_string(head_id),
        n == 1 ? "parameter" : "parameters",
        links("parameter", param_ids),
        String.concat(", ", List.init(n, _ => "X")),
      ),
    examples: [],
  };
};

let param_tpats = (~head_id: Id.t, ~param_ids: list(Id.t)): group =>
  singleton(param_tpat(~head_id, ~param_ids));

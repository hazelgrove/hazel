open ExplainThisForm;
open Example;

let var_typ_pat = (n: string): form => {
  id: VarTPat,
  syntactic_form: [n |> abbreviate |> tpat],
  expandable_id: None,
  explanation: Printf.sprintf("`%s` binds a type variable.", n),
  examples: [],
};

let var_typ_pats = (n: string): group => {
  id: VarTPat,
  forms: [var_typ_pat(n)],
};

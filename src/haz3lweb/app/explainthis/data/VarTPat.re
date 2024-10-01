open ExplainThisForm;
open Example;

let var_typ_pat = (n: string): form => {
  let explanation = "`%s` binds a type variable.";
  {
    id: VarTPat,
    syntactic_form: [n |> abbreviate |> tpat],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let var_typ_pats = (n: string): group => {
  id: VarTPat,
  forms: [var_typ_pat(n)],
};

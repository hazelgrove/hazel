open ExplainThisForm;
open Example;

let var_typ_pat: form = {
  let explanation = "`%s` binds a type variable.";
  {
    id: VarTPat,
    syntactic_form: [typ_pat_var("T")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let var_typ_pats: group = {id: VarTPat, forms: [var_typ_pat]};

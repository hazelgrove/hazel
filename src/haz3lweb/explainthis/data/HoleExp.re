open ExplainThisForm;
open Example;

let empty_hole_exp: form =
  HoleTemplate.empty_hole_template(exp, "an expression", EmptyHoleExp);
let empty_hole_exps = {id: EmptyHoleExp, forms: [empty_hole_exp]};

let multi_hole_exp: form =
  HoleTemplate.multi_hole_template(exp, MultiHoleExp);

let multi_hole_exps = {id: MultiHoleExp, forms: [multi_hole_exp]};

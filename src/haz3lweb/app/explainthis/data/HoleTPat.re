open ExplainThisForm;
open Example;

let empty_hole_tpat: form =
  HoleTemplate.empty_hole_template(tpat, "a type pattern", EmptyHoleTPat);
let empty_hole_tpats = {id: EmptyHoleTPat, forms: [empty_hole_tpat]};

let multi_hole_tpat: form =
  HoleTemplate.multi_hole_template(tpat, MultiHoleTPat);

let multi_hole_tpats = {id: MultiHoleTPat, forms: [multi_hole_tpat]};

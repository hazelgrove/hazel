let matched_arrow:
  HTyp.t => (option((HTyp.t, HTyp.t)), list(InfTyp.inf_constraint));

let matched_sum:
  HTyp.t => (option((HTyp.t, HTyp.t)), list(InfTyp.inf_constraint));

let matched_list: HTyp.t => (option(HTyp.t), list(inf_constraint));

let matched_arrow_inf:
  HTyp.t => (option((HTyp.t, HTyp.t)), list(InfTyp.inf_constraint));

let matched_prod_inf:
  HTyp.t => ((HTyp.t, HTyp.t), list(InfTyp.inf_constraint));

let matched_sum_inf:
  HTyp.t => (option((HTyp.t, HTyp.t)), list(InfTyp.inf_constraint));

let matched_list_inf:
  HTyp.t => (option(HTyp.t), list(InfTyp.inf_constraint));

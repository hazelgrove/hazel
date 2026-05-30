type prover_hint = {
  prover: string,
  tactic: string,
};

type rewrite_rule = {
  id: string,
  label: string,
  prover_hints: list(prover_hint),
};

type rewrite_group = {
  name: string,
  label: string,
  rules: list(rewrite_rule),
};

let lean = tactic => {
  prover: "lean",
  tactic,
};

let arithmetic_rewrite_group = {
  name: "arithmetic",
  label: "arithmetic",
  rules: [
    {
      id: "arith.add_comm",
      label: "commute addition",
      prover_hints: [lean("rw [add_comm]")],
    },
    {
      id: "arith.add_assoc",
      label: "associate addition",
      prover_hints: [lean("rw [add_assoc]")],
    },
    {
      id: "arith.add_zero",
      label: "remove additive identity",
      prover_hints: [lean("rw [add_zero, zero_add]")],
    },
    {
      id: "arith.add_neg",
      label: "cancel additive inverses",
      prover_hints: [lean("ring_nf")],
    },
    {
      id: "arith.const_fold",
      label: "fold constants",
      prover_hints: [lean("norm_num")],
    },
    {
      id: "arith.mul_const",
      label: "scale term by constant",
      prover_hints: [lean("ring_nf")],
    },
    {
      id: "arith.collect_like_terms",
      label: "collect like terms",
      prover_hints: [lean("ring_nf")],
    },
  ],
};

let rewrite_groups = [arithmetic_rewrite_group];

let rewrite_group_by_name = name =>
  rewrite_groups |> List.find_opt(group => group.name == name);

let rewrite_rule_by_id = (group, id) =>
  group.rules |> List.find_opt(rule => rule.id == id);

let v: ProofCtx.t =
  []
  |> ProofCtx.add_exp(
       "Reflexive(==)",
       Forall(
         Var("x") |> Pat.fresh,
         BinOp(
           Poly(Equals),
           BinOp(
             Poly(Equals),
             Var("x") |> Exp.fresh,
             Var("x") |> Exp.fresh,
           )
           |> Exp.fresh,
           Atom(Bool(true)) |> Exp.fresh,
         )
         |> Exp.fresh,
       )
       |> Exp.fresh,
     );

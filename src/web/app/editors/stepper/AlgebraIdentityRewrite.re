open TrigRewrite;

let specs: list(spec) = [
  {
    rule_id: "alg.difference_of_squares",
    label: "difference of squares",
    left: sub(pow(m("a"), i(2)), pow(m("b"), i(2))),
    right: mul(add(m("a"), m("b")), sub(m("a"), m("b"))),
  },
  {
    rule_id: "alg.square_of_sum",
    label: "square of a sum",
    left: pow(add(m("a"), m("b")), i(2)),
    right:
      add(
        add(pow(m("a"), i(2)), mul(mul(i(2), m("a")), m("b"))),
        pow(m("b"), i(2)),
      ),
  },
  {
    rule_id: "alg.square_of_difference",
    label: "square of a difference",
    left: pow(sub(m("a"), m("b")), i(2)),
    right:
      add(
        sub(pow(m("a"), i(2)), mul(mul(i(2), m("a")), m("b"))),
        pow(m("b"), i(2)),
      ),
  },
  {
    rule_id: "alg.difference_of_cubes",
    label: "difference of cubes",
    left: sub(pow(m("a"), i(3)), pow(m("b"), i(3))),
    right:
      mul(
        sub(m("a"), m("b")),
        add(
          add(pow(m("a"), i(2)), mul(m("a"), m("b"))),
          pow(m("b"), i(2)),
        ),
      ),
  },
  {
    rule_id: "alg.sum_of_cubes",
    label: "sum of cubes",
    left: add(pow(m("a"), i(3)), pow(m("b"), i(3))),
    right:
      mul(
        add(m("a"), m("b")),
        add(
          sub(pow(m("a"), i(2)), mul(m("a"), m("b"))),
          pow(m("b"), i(2)),
        ),
      ),
  },
  {
    rule_id: "alg.cube_of_sum",
    label: "cube of a sum",
    left: pow(add(m("a"), m("b")), i(3)),
    right:
      add(
        add(
          add(
            pow(m("a"), i(3)),
            mul(mul(i(3), pow(m("a"), i(2))), m("b")),
          ),
          mul(mul(i(3), m("a")), pow(m("b"), i(2))),
        ),
        pow(m("b"), i(3)),
      ),
  },
  {
    rule_id: "alg.cube_of_difference",
    label: "cube of a difference",
    left: pow(sub(m("a"), m("b")), i(3)),
    right:
      sub(
        add(
          sub(
            pow(m("a"), i(3)),
            mul(mul(i(3), pow(m("a"), i(2))), m("b")),
          ),
          mul(mul(i(3), m("a")), pow(m("b"), i(2))),
        ),
        pow(m("b"), i(3)),
      ),
  },
];

let rule_ids = specs |> List.map((spec: spec) => spec.rule_id);

let is_rule_id = rule_id => List.mem(rule_id, rule_ids);

let apply_rule_at_root = (rule_id, exp) =>
  specs
  |> List.filter((spec: spec) => spec.rule_id == rule_id)
  |> List.concat_map(spec => apply_spec(spec, exp));

let applicable_at_root = exp =>
  specs |> List.concat_map(spec => apply_spec(spec, exp));

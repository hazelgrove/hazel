open Haz3lcore;
open ExplainThisForm;
open Example;

let theorem_base_ex = {
  sub_id: Theorem(Basic),
  term: mk_example("theorem x proof 1 in \nx"),
  message: "The variable x is bound to 1, so the expression evaluates to 1",
};
let theorem_wild_ex = {
  sub_id: Theorem(Wild),
  term: mk_example("theorem _ proof 1 in \n2"),
  message: "The 1 is thrown away, so the expression evaluates to 2.",
};
let theorem_int_ex = {
  sub_id: Theorem(IntLit),
  term: mk_example("theorem 1 proof 1 in \n2"),
  message: "The 1 is thrown away, so the expression evaluates to 2.",
};
let theorem_float_ex = {
  sub_id: Theorem(FloatLit),
  term: mk_example("theorem 1.1 proof 1.1 in \n2"),
  message: "The 1.1 is thrown away, so the expression evaluates to 2.",
};
let theorem_bool_ex = {
  sub_id: Theorem(BoolLit),
  term: mk_example("theorem true proof true in \n2"),
  message: "The true is thrown away, so the expression evaluates to 2.",
};
let theorem_str_ex = {
  sub_id: Theorem(StrLit),
  term: mk_example("theorem \"abc\" proof \"abc\" in \n2"),
  message: "The true is thrown away, so the expression evaluates to 2.",
};
let theorem_triv_ex = {
  sub_id: Theorem(Triv),
  term: mk_example("theorem () proof () in \n2"),
  message: "The () is thrown away, so the expression evaluates to 2.",
};
let theorem_listlit_ex = {
  sub_id: Theorem(ListLit),
  term: mk_example("theorem [x, y] proof [1, 2] in \nx"),
  message: "The variable x is bound to 1 and the y is bound to 2, so the expression evaluates to 1.",
};
let theorem_listnil_ex = {
  sub_id: Theorem(ListNil),
  term: mk_example("theorem [] proof [] in \n2"),
  message: "The empty list is thrown away, so the expression evaluates to 2.",
};
let theorem_cons_hd_ex = {
  sub_id: Theorem(ConsHd),
  term: mk_example("theorem hd::tl proof 1::[] in \nhd"),
  message: "The hd is bound to 1 and the tl is bound to the empty list, so the expression evaluates to 1.",
};
let theorem_cons_snd_ex = {
  sub_id: Theorem(ConsSnd),
  term: mk_example("theorem fst::snd::tl proof true::false::[] in \nsnd"),
  message: "The fst is bound to true, the snd is bound to false, and the tl is bound to the empty list, so the expression evaluates to false.",
};
let theorem_var_ex = {
  sub_id: Theorem(Var),
  term: mk_example("theorem x proof 1 in \nx + 2"),
  message: "The variable x is bound to 1, so the expression evaluates to 1 + 2, which is 3.",
};
let theorem_tuple2_ex = {
  sub_id: Theorem(Tuple2),
  term: mk_example("theorem (x, y) proof (1, 2) in \nx + y"),
  message: "The variable x is bound to 1 and the y is bound to 2, so the expression evaluates to 1 + 2, which is 3.",
};
let theorem_tuple3_ex = {
  sub_id: Theorem(Tuple3),
  term: mk_example("theorem (x, y, z) proof (1, 2, 3)\nin x + y + z"),
  message: "The variable x is bound to 1, the y is bound to 2, and the z is bound to 3, so the expression evaluates to 1 + 2 + 3, which is 6.",
};
let theorem_ctr_ex = {
  sub_id: Theorem(Ctr),
  term:
    mk_example(
      "type T = None + Some(Int)\n in theorem None proof None\nin 2",
    ),
  message: "The None is thrown away, so the expression evaluates to 2.",
};
let theorem_ap_ex = {
  sub_id: Theorem(Ap),
  term:
    mk_example(
      "type T = None + Some(Int)\n in theorem Some(a) proof Some(2)\nin a",
    ),
  message: "The a is bound to 2, so the expression evaluates to 2.",
};
let _pat_def_body_theorem_exp_coloring_ids =
    (
      sf_pat_id: Id.t,
      sf_def_id: Id.t,
      sf_body_id: Id.t,
      ~pat_id: Id.t,
      ~def_id: Id.t,
      ~body_id: Id.t,
    )
    : list((Id.t, Id.t)) => {
  [(sf_pat_id, pat_id), (sf_def_id, def_id), (sf_body_id, body_id)];
};
let _pat_def_theorem_exp_coloring_ids =
    (sf_pat_id: Id.t, sf_def_id: Id.t, ~pat_id: Id.t, ~def_id: Id.t)
    : list((Id.t, Id.t)) => {
  [(sf_pat_id, pat_id), (sf_def_id, def_id)];
};
let _pat = pat("p");
let _exp_def = exp("e_def");
let theorem_base_exp_coloring_ids =
  _pat_def_theorem_exp_coloring_ids(Piece.id(_pat), Piece.id(_exp_def));
let theorem_base_exp: form = {
  let explanation = "The [*definition*](%s) is matched against the [*pattern*](%s).";
  let form = [
    mk_theorem([[space(), _pat, space()], [space(), _exp_def, space()]]),
    linebreak(),
    exp("e_body"),
  ];
  {
    id: TheoremExp(Base),
    syntactic_form: form,
    expandable_id: Some((Piece.id(_pat), [pat("p")])),
    explanation,
    examples: [theorem_base_ex],
  };
};
let _pat = Piece.Grout({id: Id.mk(), shape: Convex});
let _exp_def = exp("e_def");
let theorem_empty_hole_exp_coloring_ids =
  _pat_def_theorem_exp_coloring_ids(Piece.id(_pat), Piece.id(_exp_def));
let theorem_empty_hole_exp: form = {
  let explanation = "After the [*empty hole pattern*](%s) is filled, the [*definition*](%s) is matched against the [*pattern*](%s).";
  let form = [
    mk_theorem([[space(), _pat, space()], [space(), _exp_def, space()]]),
    linebreak(),
    exp("e_body"),
  ];
  {
    id: TheoremExp(EmptyHole),
    syntactic_form: form,
    expandable_id:
      Some((Piece.id(_pat), [Grout({id: Id.mk(), shape: Convex})])),
    explanation,
    examples: [theorem_base_ex],
  };
};
let _pat = pat("INVALID");
let _exp_def = exp("e_def");
let theorem_multi_hole_exp_coloring_ids =
  _pat_def_theorem_exp_coloring_ids(Piece.id(_pat), Piece.id(_exp_def));
let theorem_multi_hole_exp: form = {
  let explanation = "After the [invalid pattern](%s) is corrected, the [*definition*](%s) is matched against the [*pattern*](%s).";
  let form = [
    mk_theorem([[space(), _pat, space()], [space(), _exp_def, space()]]),
    linebreak(),
    exp("e_body"),
  ];
  {
    id: TheoremExp(MultiHole),
    syntactic_form: form,
    expandable_id: Some((Piece.id(_pat), [pat("INVALID")])),
    explanation,
    examples: [theorem_base_ex],
  };
};
let _exp_def = exp("e_def");
let _exp_body = exp("e_body");
let theorem_wild_exp_coloring_ids =
    (~def_id: Id.t, ~body_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_exp_def), def_id),
  (Piece.id(_exp_body), body_id),
];
let theorem_wild_exp: form = {
  let explanation = "The [*definition*](%s) is evaluated and ignored. The [*definition*](%s) can't be referenced in the [*body*](%s).";
  let pat_ = pat("_");
  let form = [
    mk_theorem([[space(), pat_, space()], [space(), _exp_def, space()]]),
    linebreak(),
    _exp_body,
  ];
  {
    id: TheoremExp(Wild),
    syntactic_form: form,
    expandable_id: Some((Piece.id(pat_), [pat("_")])),
    explanation,
    examples: [theorem_wild_ex],
  };
};
let _pat = pat("IntLit");
let _exp_def = exp("e_def");
let _exp_body = exp("e_body");
let theorem_int_exp_coloring_ids =
  _pat_def_body_theorem_exp_coloring_ids(
    Piece.id(_pat),
    Piece.id(_exp_def),
    Piece.id(_exp_body),
  );
let theorem_int_exp: form = {
  let explanation = "The only value for the [*definition*](%s) that matches the [*pattern*](%s) is `%s`. The [*definition*](%s) can't be referenced in the [*body*](%s).";
  let form = [
    mk_theorem([[space(), _pat, space()], [space(), _exp_def, space()]]),
    linebreak(),
    _exp_body,
  ];
  {
    id: TheoremExp(Int),
    syntactic_form: form,
    expandable_id: Some((Piece.id(_pat), [pat("IntLit")])),
    explanation,
    examples: [theorem_int_ex],
  };
};
let _pat = pat("FloatLit");
let _exp_def = exp("e_def");
let _exp_body = exp("e_body");
let theorem_float_exp_coloring_ids =
  _pat_def_body_theorem_exp_coloring_ids(
    Piece.id(_pat),
    Piece.id(_exp_def),
    Piece.id(_exp_body),
  );
let theorem_float_exp: form = {
  let explanation = "The only value for the [*definition*](%s) that matches the [*pattern*](%s) is `%f`. The [*definition*](%s) can't be referenced in the [*body*](%s).";
  let form = [
    mk_theorem([[space(), _pat, space()], [space(), _exp_def, space()]]),
    linebreak(),
    _exp_body,
  ];
  {
    id: TheoremExp(Float),
    syntactic_form: form,
    expandable_id: Some((Piece.id(_pat), [pat("FloatLit")])),
    explanation,
    examples: [theorem_float_ex],
  };
};
let _pat = pat("BoolLit");
let _exp_def = exp("e_def");
let _exp_body = exp("e_body");
let theorem_bool_exp_coloring_ids =
  _pat_def_body_theorem_exp_coloring_ids(
    Piece.id(_pat),
    Piece.id(_exp_def),
    Piece.id(_exp_body),
  );
let theorem_bool_exp: form = {
  let explanation = "The only value for the [*definition*](%s) that matches the [*pattern*](%s) is `%b`. The [*definition*](%s) can't be referenced in the [*body*](%s).";
  let form = [
    mk_theorem([[space(), _pat, space()], [space(), _exp_def, space()]]),
    linebreak(),
    _exp_body,
  ];
  {
    id: TheoremExp(Bool),
    syntactic_form: form,
    expandable_id: Some((Piece.id(_pat), [pat("BoolLit")])),
    explanation,
    examples: [theorem_bool_ex],
  };
};
let _pat = pat("StringLit");
let _exp_def = exp("e_def");
let _exp_body = exp("e_body");
let theorem_str_exp_coloring_ids =
  _pat_def_body_theorem_exp_coloring_ids(
    Piece.id(_pat),
    Piece.id(_exp_def),
    Piece.id(_exp_body),
  );
let theorem_str_exp: form = {
  let explanation = "The only value for the [*definition*](%s) that matches the [*pattern*](%s) is `%s`. The [*definition*](%s) can't be referenced in the [*body*](%s).";
  let form = [
    mk_theorem([[space(), _pat, space()], [space(), _exp_def, space()]]),
    linebreak(),
    _exp_body,
  ];
  {
    id: TheoremExp(String),
    syntactic_form: form,
    expandable_id: Some((Piece.id(_pat), [pat("StringLit")])),
    explanation,
    examples: [theorem_str_ex],
  };
};
let _pat = pat("()");
let _exp_def = exp("e_def");
let _exp_body = exp("e_body");
let theorem_triv_exp_coloring_ids =
  _pat_def_body_theorem_exp_coloring_ids(
    Piece.id(_pat),
    Piece.id(_exp_def),
    Piece.id(_exp_body),
  );
let theorem_triv_exp: form = {
  let explanation = "The only value for the [*definition*](%s) that matches the [*pattern*](%s) is the trivial value `()`. The [*definition*](%s) can't be referenced in the [*body*](%s).";
  let form = [
    mk_theorem([[space(), _pat, space()], [space(), _exp_def, space()]]),
    linebreak(),
    _exp_body,
  ];
  {
    id: TheoremExp(Triv),
    syntactic_form: form,
    expandable_id: Some((Piece.id(_pat), [pat("()")])),
    explanation,
    examples: [theorem_triv_ex],
  };
};
let _pat = mk_list_pat([[pat("p1"), comma_pat(), space(), pat("...")]]);
let _exp_def = exp("e_def");
let theorem_listlit_exp_coloring_ids =
  _pat_def_theorem_exp_coloring_ids(Piece.id(_pat), Piece.id(_exp_def));
let theorem_listlit_exp: form = {
  let explanation = "The only values for the [*definition*](%s) that match the [*pattern*](%s) are lists with %s-elements, where each element matches the corresponding element pattern.";
  let form = [
    mk_theorem([[space(), _pat, space()], [space(), _exp_def, space()]]),
    linebreak(),
    exp("e_body"),
  ];
  {
    id: TheoremExp(ListLit),
    syntactic_form: form,
    expandable_id:
      Some((Piece.id(_pat), [pat("p1"), comma_pat(), pat("...")])),
    explanation,
    examples: [theorem_listlit_ex],
  };
};
let _pat = pat("[]");
let _exp_def = exp("e_def");
let _exp_body = exp("e_body");
let theorem_listnil_exp_coloring_ids =
  _pat_def_body_theorem_exp_coloring_ids(
    Piece.id(_pat),
    Piece.id(_exp_def),
    Piece.id(_exp_body),
  );
let theorem_listnil_exp: form = {
  let explanation = "The only value for the [*definition*](%s) that matches the [*pattern*](%s) is the empty list `[]`. The [*definition*](%s) can't be referenced in the [*body*](%s).";
  let form = [
    mk_theorem([[space(), _pat, space()], [space(), _exp_def, space()]]),
    linebreak(),
    _exp_body,
  ];
  {
    id: TheoremExp(ListNil),
    syntactic_form: form,
    expandable_id: Some((Piece.id(_pat), [pat("[]")])),
    explanation,
    examples: [theorem_listnil_ex],
  };
};
let _pat_hd = pat("p_hd");
let _pat_tl = pat("p_tl");
let _exp_def = exp("e_def");
let theorem_cons_exp_coloring_ids =
    (~hd_id: Id.t, ~tl_id: Id.t, ~def_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_pat_hd), hd_id),
  (Piece.id(_pat_tl), tl_id),
  (Piece.id(_exp_def), def_id),
];
let theorem_cons_exp: form = {
  let explanation = "The only values for the [*definition*](%s) that match the *pattern* are non-empty lists that match the [*head*](%s) and [*tail*](%s) patterns.";
  let cons = cons_pat();
  let form = [
    mk_theorem([
      [space(), _pat_hd, cons, _pat_tl, space()],
      [space(), _exp_def, space()],
    ]),
    linebreak(),
    exp("e_body"),
  ];
  {
    id: TheoremExp(ListCons),
    syntactic_form: form,
    expandable_id:
      Some((Piece.id(cons), [pat("p_hd"), cons_pat(), pat("p_tl")])),
    explanation,
    examples: [theorem_cons_hd_ex, theorem_cons_snd_ex],
  };
};
let _pat = pat("x");
let _exp_def = exp("e_def");
let _exp_body = exp("e_body");
let theorem_var_exp_coloring_ids =
  _pat_def_body_theorem_exp_coloring_ids(
    Piece.id(_pat),
    Piece.id(_exp_def),
    Piece.id(_exp_body),
  );
let theorem_var_exp: form = {
  let explanation = "The [*definition*](%s) is bound to the [*variable*](%s) `%s` in the [*body*](%s).";
  let form = [
    mk_theorem([[space(), _pat, space()], [space(), _exp_def, space()]]),
    linebreak(),
    _exp_body,
  ];
  {
    id: TheoremExp(Var),
    syntactic_form: form,
    expandable_id: Some((Piece.id(_pat), [pat("x")])),
    explanation,
    examples: [theorem_var_ex],
    // TODO Does this example being slightly different actually add anything?
  };
};
let _comma = comma_pat();
let _exp_def = exp("e_def");
let theorem_tuple_exp_coloring_ids =
  _pat_def_theorem_exp_coloring_ids(Piece.id(_comma), Piece.id(_exp_def));
let theorem_tuple_exp: form = {
  let explanation = "The only values for the [*definition*](%s) that match the [*pattern*](%s) are %s-tuples where each element matches the corresponding element pattern.";
  let form = [
    mk_theorem([
      [space(), pat("p1"), _comma, space(), pat("..."), space()],
      [space(), _exp_def, space()],
    ]),
    linebreak(),
    exp("e_body"),
  ];
  {
    id: TheoremExp(Tuple),
    syntactic_form: form,
    expandable_id:
      Some((Piece.id(_comma), [pat("p1"), comma_pat(), pat("...")])),
    explanation,
    examples: [theorem_tuple2_ex, theorem_tuple3_ex],
  };
};
let _pat1 = pat("p1");
let _pat2 = pat("p2");
let _exp_def = exp("e_def");
let theorem_tuple2_exp_coloring_ids =
    (~pat1_id: Id.t, ~pat2_id: Id.t, ~def_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_pat1), pat1_id),
  (Piece.id(_pat2), pat2_id),
  (Piece.id(_exp_def), def_id),
];
let theorem_tuple2_exp: form = {
  let explanation = "The only values for the [*definition*](%s) that match the *pattern* are 2-tuples where the first element matches the [*first element pattern*](%s) and the second element matches the [*second element pattern*](%s).";
  let comma = comma_pat();
  let form = [
    mk_theorem([
      [space(), _pat1, comma, space(), _pat2, space()],
      [space(), _exp_def, space()],
    ]),
    linebreak(),
    exp("e_body"),
  ];
  {
    id: TheoremExp(Tuple2),
    syntactic_form: form,
    expandable_id:
      Some((Piece.id(comma), [pat("p1"), comma_pat(), pat("p2")])),
    explanation,
    examples: [theorem_tuple2_ex],
  };
};
let _pat1 = pat("p1");
let _pat2 = pat("p2");
let _pat3 = pat("p3");
let _exp_def = exp("e_def");
let theorem_tuple3_exp_coloring_ids =
    (~pat1_id: Id.t, ~pat2_id: Id.t, ~pat3_id: Id.t, ~def_id: Id.t)
    : list((Id.t, Id.t)) => [
  (Piece.id(_pat1), pat1_id),
  (Piece.id(_pat2), pat2_id),
  (Piece.id(_pat3), pat3_id),
  (Piece.id(_exp_def), def_id),
];
let theorem_tuple3_exp: form = {
  let explanation = "The only values for the [*definition*](%s) that match the *pattern* are 3-tuples where the first element matches the [*first element pattern*](%s), the second element matches the [*second element pattern*](%s), and the third element matches the [*third element pattern*](%s).";
  let comma = comma_pat();
  let form = [
    mk_theorem([
      [
        space(),
        _pat1,
        comma_pat(),
        space(),
        _pat2,
        comma,
        space(),
        _pat3,
        space(),
      ],
      [space(), _exp_def, space()],
    ]),
    linebreak(),
    exp("e_body"),
  ];
  {
    id: TheoremExp(Tuple3),
    syntactic_form: form,
    expandable_id:
      Some((
        Piece.id(comma),
        [pat("p1"), comma_pat(), pat("p2"), comma_pat(), pat("p3")],
      )),
    explanation,
    examples: [theorem_tuple3_ex],
  };
};
let _pat = pat("C");
let _exp_def = exp("e_def");
let _exp_body = exp("e_body");
let theorem_ctr_exp_coloring_ids =
  _pat_def_body_theorem_exp_coloring_ids(
    Piece.id(_pat),
    Piece.id(_exp_def),
    Piece.id(_exp_body),
  );
let theorem_ctr_exp: form = {
  let explanation = "The only value for the [*definition*](%s) that matches the [*pattern*](%s) is the *`%s` constructor*. The [*definition*](%s) can't be referenced in the [*body*](%s).";
  let form = [
    mk_theorem([[space(), _pat, space()], [space(), _exp_def, space()]]),
    linebreak(),
    _exp_body,
  ];
  {
    id: TheoremExp(Ctr),
    syntactic_form: form,
    expandable_id: Some((Piece.id(_pat), [pat("C")])),
    explanation,
    examples: [theorem_ctr_ex],
  };
};
let _pat_con = pat("p_con");
let _pat_arg = pat("p_arg");
let _exp_def = exp("e_def");
let theorem_ap_exp_coloring_ids =
    (~con_id: Id.t, ~arg_id: Id.t, ~def_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_pat_con), con_id),
  (Piece.id(_pat_arg), arg_id),
  (Piece.id(_exp_def), def_id),
];
let theorem_ap_exp: form = {
  let explanation = "The only values for the [*definition*](%s) that match the *pattern* are the [*constructor*](%s) where the *argument* matches the [*argument pattern*](%s).";
  let ap = mk_ap_pat([[_pat_arg]]);
  let form = [
    mk_theorem([
      [space(), _pat_con, ap, space()],
      [space(), _exp_def, space()],
    ]),
    linebreak(),
    exp("e_body"),
  ];
  {
    id: TheoremExp(Ap),
    syntactic_form: form,
    expandable_id:
      Some((Piece.id(ap), [pat("p_con"), mk_ap_pat([[pat("p_arg")]])])),
    explanation,
    examples: [theorem_ap_ex],
  };
};

let theorems_emptyhole: group = {
  id: TheoremExp(EmptyHole),
  forms: [theorem_empty_hole_exp, theorem_base_exp],
};

let theorems_mutlihole: group = {
  id: TheoremExp(MultiHole),
  forms: [theorem_multi_hole_exp, theorem_base_exp],
};

let theorems_wild: group = {
  id: TheoremExp(Wild),
  forms: [theorem_wild_exp, theorem_base_exp],
};

let theorems_int: group = {
  id: TheoremExp(Int),
  forms: [theorem_int_exp, theorem_base_exp],
};

let theorems_float: group = {
  id: TheoremExp(Float),
  forms: [theorem_float_exp, theorem_base_exp],
};

let theorems_bool: group = {
  id: TheoremExp(Bool),
  forms: [theorem_bool_exp, theorem_base_exp],
};

let theorems_str: group = {
  id: TheoremExp(String),
  forms: [theorem_str_exp, theorem_base_exp],
};

let theorems_triv: group = {
  id: TheoremExp(Triv),
  forms: [theorem_triv_exp, theorem_base_exp],
};

let theorems_listlit: group = {
  id: TheoremExp(ListLit),
  forms: [theorem_listlit_exp, theorem_base_exp],
};

let theorems_listnil: group = {
  id: TheoremExp(ListNil),
  forms: [theorem_listnil_exp, theorem_base_exp],
};

let theorems_cons: group = {
  id: TheoremExp(ListCons),
  forms: [theorem_cons_exp, theorem_base_exp],
};

let theorems_var: group = {
  id: TheoremExp(Var),
  forms: [theorem_var_exp, theorem_base_exp],
};

let theorems_tuple: group = {
  id: TheoremExp(Tuple),
  forms: [theorem_tuple_exp, theorem_base_exp],
};

let theorems_tuple2: group = {
  id: TheoremExp(Tuple2),
  forms: [theorem_tuple2_exp, theorem_tuple_exp, theorem_base_exp],
};

let theorems_tuple3: group = {
  id: TheoremExp(Tuple3),
  forms: [theorem_tuple3_exp, theorem_tuple_exp, theorem_base_exp],
};

let theorems_ctr: group = {
  id: TheoremExp(Ctr),
  forms: [theorem_ctr_exp, theorem_base_exp],
};

let theorems_ap: group = {
  id: TheoremExp(Ap),
  forms: [theorem_ap_exp, theorem_base_exp],
};

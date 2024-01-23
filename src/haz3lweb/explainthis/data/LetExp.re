open Haz3lcore;
open ExplainThisForm;
open Example;

let let_base_ex = {
  sub_id: Let(Basic),
  term: mk_example("let x = 1 in \nx"),
  message: "The variable x is bound to 1, so the expression evaluates to 1",
};
let let_wild_ex = {
  sub_id: Let(Wild),
  term: mk_example("let _ = 1 in \n2"),
  message: "The 1 is thrown away, so the expression evaluates to 2.",
};
let let_int_ex = {
  sub_id: Let(IntLit),
  term: mk_example("let 1 = 1 in \n2"),
  message: "The 1 is thrown away, so the expression evaluates to 2.",
};
let let_float_ex = {
  sub_id: Let(FloatLit),
  term: mk_example("let 1.1 = 1.1 in \n2"),
  message: "The 1.1 is thrown away, so the expression evaluates to 2.",
};
let let_bool_ex = {
  sub_id: Let(BoolLit),
  term: mk_example("let true = true in \n2"),
  message: "The true is thrown away, so the expression evaluates to 2.",
};
let let_str_ex = {
  sub_id: Let(StrLit),
  term: mk_example("let \"abc\" = \"abc\" in \n2"),
  message: "The true is thrown away, so the expression evaluates to 2.",
};
let let_triv_ex = {
  sub_id: Let(Triv),
  term: mk_example("let () = () in \n2"),
  message: "The () is thrown away, so the expression evaluates to 2.",
};
let let_listlit_ex = {
  sub_id: Let(ListLit),
  term: mk_example("let [x, y] = [1, 2] in \nx"),
  message: "The variable x is bound to 1 and the y is bound to 2, so the expression evaluates to 1.",
};
let let_listnil_ex = {
  sub_id: Let(ListNil),
  term: mk_example("let [] = [] in \n2"),
  message: "The empty list is thrown away, so the expression evaluates to 2.",
};
let let_cons_hd_ex = {
  sub_id: Let(ConsHd),
  term: mk_example("let hd::tl = 1::[] in \nhd"),
  message: "The hd is bound to 1 and the tl is bound to the empty list, so the expression evaluates to 1.",
};
let let_cons_snd_ex = {
  sub_id: Let(ConsSnd),
  term: mk_example("let fst::snd::tl = true::false::[] in \nsnd"),
  message: "The fst is bound to true, the snd is bound to false, and the tl is bound to the empty list, so the expression evaluates to false.",
};
let let_var_ex = {
  sub_id: Let(Var),
  term: mk_example("let x = 1 in \nx + 2"),
  message: "The variable x is bound to 1, so the expression evaluates to 1 + 2, which is 3.",
};
let let_tuple2_ex = {
  sub_id: Let(Tuple2),
  term: mk_example("let (x, y) = (1, 2) in \nx + y"),
  message: "The variable x is bound to 1 and the y is bound to 2, so the expression evaluates to 1 + 2, which is 3.",
};
let let_tuple3_ex = {
  sub_id: Let(Tuple3),
  term: mk_example("let (x, y, z) = (1, 2, 3) in \nx + y + z"),
  message: "The variable x is bound to 1, the y is bound to 2, and the z is bound to 3, so the expression evaluates to 1 + 2 + 3, which is 6.",
};
let let_ctr_ex = {
  sub_id: Let(Ctr),
  term: mk_example("type T = None + Some(Int)\n in let None = None\nin 2"),
  message: "The None is thrown away, so the expression evaluates to 2.",
};
let let_ap_ex = {
  sub_id: Let(Ap),
  term:
    mk_example("type T = None + Some(Int)\n in let Some(a) = Some(2)\nin a"),
  message: "The a is bound to 2, so the expression evaluates to 2.",
};
let _pat_def_body_let_exp_coloring_ids =
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
let _pat_def_let_exp_coloring_ids =
    (sf_pat_id: Id.t, sf_def_id: Id.t, ~pat_id: Id.t, ~def_id: Id.t)
    : list((Id.t, Id.t)) => {
  [(sf_pat_id, pat_id), (sf_def_id, def_id)];
};
let _pat = pat("p");
let _exp_def = exp("e_def");
let let_base_exp_coloring_ids =
  _pat_def_let_exp_coloring_ids(Piece.id(_pat), Piece.id(_exp_def));
let let_base_exp: form = {
  let explanation = "The [*definition*](%s) is matched against the [*pattern*](%s).";
  let form = [
    mk_let([[space(), _pat, space()], [space(), _exp_def, space()]]),
    linebreak(),
    exp("e_body"),
  ];
  {
    id: LetExp(Base),
    syntactic_form: form,
    expandable_id: Some((Piece.id(_pat), [pat("p")])),
    explanation,
    examples: [let_base_ex],
  };
};
let _pat = Piece.Grout({id: Id.mk(), shape: Convex});
let _exp_def = exp("e_def");
let let_empty_hole_exp_coloring_ids =
  _pat_def_let_exp_coloring_ids(Piece.id(_pat), Piece.id(_exp_def));
let let_empty_hole_exp: form = {
  let explanation = "After the [*empty hole pattern*](%s) is filled, the [*definition*](%s) is matched against the [*pattern*](%s).";
  let form = [
    mk_let([[space(), _pat, space()], [space(), _exp_def, space()]]),
    linebreak(),
    exp("e_body"),
  ];
  {
    id: LetExp(EmptyHole),
    syntactic_form: form,
    expandable_id:
      Some((Piece.id(_pat), [Grout({id: Id.mk(), shape: Convex})])),
    explanation,
    examples: [let_base_ex],
  };
};
let _pat = pat("INVALID");
let _exp_def = exp("e_def");
let let_multi_hole_exp_coloring_ids =
  _pat_def_let_exp_coloring_ids(Piece.id(_pat), Piece.id(_exp_def));
let let_multi_hole_exp: form = {
  let explanation = "After the [invalid pattern](%s) is corrected, the [*definition*](%s) is matched against the [*pattern*](%s).";
  let form = [
    mk_let([[space(), _pat, space()], [space(), _exp_def, space()]]),
    linebreak(),
    exp("e_body"),
  ];
  {
    id: LetExp(MultiHole),
    syntactic_form: form,
    expandable_id: Some((Piece.id(_pat), [pat("INVALID")])),
    explanation,
    examples: [let_base_ex],
  };
};
let _exp_def = exp("e_def");
let _exp_body = exp("e_body");
let let_wild_exp_coloring_ids =
    (~def_id: Id.t, ~body_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_exp_def), def_id),
  (Piece.id(_exp_body), body_id),
];
let let_wild_exp: form = {
  let explanation = "The [*definition*](%s) is evaluated and ignored. The [*definition*](%s) can't be referenced in the [*body*](%s).";
  let pat_ = pat("_");
  let form = [
    mk_let([[space(), pat_, space()], [space(), _exp_def, space()]]),
    linebreak(),
    _exp_body,
  ];
  {
    id: LetExp(Wild),
    syntactic_form: form,
    expandable_id: Some((Piece.id(pat_), [pat("_")])),
    explanation,
    examples: [let_wild_ex],
  };
};
let _pat = pat("IntLit");
let _exp_def = exp("e_def");
let _exp_body = exp("e_body");
let let_int_exp_coloring_ids =
  _pat_def_body_let_exp_coloring_ids(
    Piece.id(_pat),
    Piece.id(_exp_def),
    Piece.id(_exp_body),
  );
let let_int_exp: form = {
  let explanation = "The only value for the [*definition*](%s) that matches the [*pattern*](%s) is `%s`. The [*definition*](%s) can't be referenced in the [*body*](%s).";
  let form = [
    mk_let([[space(), _pat, space()], [space(), _exp_def, space()]]),
    linebreak(),
    _exp_body,
  ];
  {
    id: LetExp(Int),
    syntactic_form: form,
    expandable_id: Some((Piece.id(_pat), [pat("IntLit")])),
    explanation,
    examples: [let_int_ex],
  };
};
let _pat = pat("FloatLit");
let _exp_def = exp("e_def");
let _exp_body = exp("e_body");
let let_float_exp_coloring_ids =
  _pat_def_body_let_exp_coloring_ids(
    Piece.id(_pat),
    Piece.id(_exp_def),
    Piece.id(_exp_body),
  );
let let_float_exp: form = {
  let explanation = "The only value for the [*definition*](%s) that matches the [*pattern*](%s) is `%f`. The [*definition*](%s) can't be referenced in the [*body*](%s).";
  let form = [
    mk_let([[space(), _pat, space()], [space(), _exp_def, space()]]),
    linebreak(),
    _exp_body,
  ];
  {
    id: LetExp(Float),
    syntactic_form: form,
    expandable_id: Some((Piece.id(_pat), [pat("FloatLit")])),
    explanation,
    examples: [let_float_ex],
  };
};
let _pat = pat("BoolLit");
let _exp_def = exp("e_def");
let _exp_body = exp("e_body");
let let_bool_exp_coloring_ids =
  _pat_def_body_let_exp_coloring_ids(
    Piece.id(_pat),
    Piece.id(_exp_def),
    Piece.id(_exp_body),
  );
let let_bool_exp: form = {
  let explanation = "The only value for the [*definition*](%s) that matches the [*pattern*](%s) is `%b`. The [*definition*](%s) can't be referenced in the [*body*](%s).";
  let form = [
    mk_let([[space(), _pat, space()], [space(), _exp_def, space()]]),
    linebreak(),
    _exp_body,
  ];
  {
    id: LetExp(Bool),
    syntactic_form: form,
    expandable_id: Some((Piece.id(_pat), [pat("BoolLit")])),
    explanation,
    examples: [let_bool_ex],
  };
};
let _pat = pat("StringLit");
let _exp_def = exp("e_def");
let _exp_body = exp("e_body");
let let_str_exp_coloring_ids =
  _pat_def_body_let_exp_coloring_ids(
    Piece.id(_pat),
    Piece.id(_exp_def),
    Piece.id(_exp_body),
  );
let let_str_exp: form = {
  let explanation = "The only value for the [*definition*](%s) that matches the [*pattern*](%s) is `%s`. The [*definition*](%s) can't be referenced in the [*body*](%s).";
  let form = [
    mk_let([[space(), _pat, space()], [space(), _exp_def, space()]]),
    linebreak(),
    _exp_body,
  ];
  {
    id: LetExp(String),
    syntactic_form: form,
    expandable_id: Some((Piece.id(_pat), [pat("StringLit")])),
    explanation,
    examples: [let_str_ex],
  };
};
let _pat = pat("()");
let _exp_def = exp("e_def");
let _exp_body = exp("e_body");
let let_triv_exp_coloring_ids =
  _pat_def_body_let_exp_coloring_ids(
    Piece.id(_pat),
    Piece.id(_exp_def),
    Piece.id(_exp_body),
  );
let let_triv_exp: form = {
  let explanation = "The only value for the [*definition*](%s) that matches the [*pattern*](%s) is the trivial value `()`. The [*definition*](%s) can't be referenced in the [*body*](%s).";
  let form = [
    mk_let([[space(), _pat, space()], [space(), _exp_def, space()]]),
    linebreak(),
    _exp_body,
  ];
  {
    id: LetExp(Triv),
    syntactic_form: form,
    expandable_id: Some((Piece.id(_pat), [pat("()")])),
    explanation,
    examples: [let_triv_ex],
  };
};
let _pat = mk_list_pat([[pat("p1"), comma_pat(), space(), pat("...")]]);
let _exp_def = exp("e_def");
let let_listlit_exp_coloring_ids =
  _pat_def_let_exp_coloring_ids(Piece.id(_pat), Piece.id(_exp_def));
let let_listlit_exp: form = {
  let explanation = "The only values for the [*definition*](%s) that match the [*pattern*](%s) are lists with %s-elements, where each element matches the corresponding element pattern.";
  let form = [
    mk_let([[space(), _pat, space()], [space(), _exp_def, space()]]),
    linebreak(),
    exp("e_body"),
  ];
  {
    id: LetExp(ListLit),
    syntactic_form: form,
    expandable_id:
      Some((Piece.id(_pat), [pat("p1"), comma_pat(), pat("...")])),
    explanation,
    examples: [let_listlit_ex],
  };
};
let _pat = pat("[]");
let _exp_def = exp("e_def");
let _exp_body = exp("e_body");
let let_listnil_exp_coloring_ids =
  _pat_def_body_let_exp_coloring_ids(
    Piece.id(_pat),
    Piece.id(_exp_def),
    Piece.id(_exp_body),
  );
let let_listnil_exp: form = {
  let explanation = "The only value for the [*definition*](%s) that matches the [*pattern*](%s) is the empty list `[]`. The [*definition*](%s) can't be referenced in the [*body*](%s).";
  let form = [
    mk_let([[space(), _pat, space()], [space(), _exp_def, space()]]),
    linebreak(),
    _exp_body,
  ];
  {
    id: LetExp(ListNil),
    syntactic_form: form,
    expandable_id: Some((Piece.id(_pat), [pat("[]")])),
    explanation,
    examples: [let_listnil_ex],
  };
};
let _pat_hd = pat("p_hd");
let _pat_tl = pat("p_tl");
let _exp_def = exp("e_def");
let let_cons_exp_coloring_ids =
    (~hd_id: Id.t, ~tl_id: Id.t, ~def_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_pat_hd), hd_id),
  (Piece.id(_pat_tl), tl_id),
  (Piece.id(_exp_def), def_id),
];
let let_cons_exp: form = {
  let explanation = "The only values for the [*definition*](%s) that match the *pattern* are non-empty lists that match the [*head*](%s) and [*tail*](%s) patterns.";
  let cons = cons_pat();
  let form = [
    mk_let([
      [space(), _pat_hd, cons, _pat_tl, space()],
      [space(), _exp_def, space()],
    ]),
    linebreak(),
    exp("e_body"),
  ];
  {
    id: LetExp(ListCons),
    syntactic_form: form,
    expandable_id:
      Some((Piece.id(cons), [pat("p_hd"), cons_pat(), pat("p_tl")])),
    explanation,
    examples: [let_cons_hd_ex, let_cons_snd_ex],
  };
};
let _pat = pat("x");
let _exp_def = exp("e_def");
let _exp_body = exp("e_body");
let let_var_exp_coloring_ids =
  _pat_def_body_let_exp_coloring_ids(
    Piece.id(_pat),
    Piece.id(_exp_def),
    Piece.id(_exp_body),
  );
let let_var_exp: form = {
  let explanation = "The [*definition*](%s) is bound to the [*variable*](%s) `%s` in the [*body*](%s).";
  let form = [
    mk_let([[space(), _pat, space()], [space(), _exp_def, space()]]),
    linebreak(),
    _exp_body,
  ];
  {
    id: LetExp(Var),
    syntactic_form: form,
    expandable_id: Some((Piece.id(_pat), [pat("x")])),
    explanation,
    examples: [let_var_ex],
    // TODO Does this example being slightly different actually add anything?
  };
};
let _comma = comma_pat();
let _exp_def = exp("e_def");
let let_tuple_exp_coloring_ids =
  _pat_def_let_exp_coloring_ids(Piece.id(_comma), Piece.id(_exp_def));
let let_tuple_exp: form = {
  let explanation = "The only values for the [*definition*](%s) that match the [*pattern*](%s) are %s-tuples where each element matches the corresponding element pattern.";
  let form = [
    mk_let([
      [space(), pat("p1"), _comma, space(), pat("..."), space()],
      [space(), _exp_def, space()],
    ]),
    linebreak(),
    exp("e_body"),
  ];
  {
    id: LetExp(Tuple),
    syntactic_form: form,
    expandable_id:
      Some((Piece.id(_comma), [pat("p1"), comma_pat(), pat("...")])),
    explanation,
    examples: [let_tuple2_ex, let_tuple3_ex],
  };
};
let _pat1 = pat("p1");
let _pat2 = pat("p2");
let _exp_def = exp("e_def");
let let_tuple2_exp_coloring_ids =
    (~pat1_id: Id.t, ~pat2_id: Id.t, ~def_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_pat1), pat1_id),
  (Piece.id(_pat2), pat2_id),
  (Piece.id(_exp_def), def_id),
];
let let_tuple2_exp: form = {
  let explanation = "The only values for the [*definition*](%s) that match the *pattern* are 2-tuples where the first element matches the [*first element pattern*](%s) and the second element matches the [*second element pattern*](%s).";
  let comma = comma_pat();
  let form = [
    mk_let([
      [space(), _pat1, comma, space(), _pat2, space()],
      [space(), _exp_def, space()],
    ]),
    linebreak(),
    exp("e_body"),
  ];
  {
    id: LetExp(Tuple2),
    syntactic_form: form,
    expandable_id:
      Some((Piece.id(comma), [pat("p1"), comma_pat(), pat("p2")])),
    explanation,
    examples: [let_tuple2_ex],
  };
};
let _pat1 = pat("p1");
let _pat2 = pat("p2");
let _pat3 = pat("p3");
let _exp_def = exp("e_def");
let let_tuple3_exp_coloring_ids =
    (~pat1_id: Id.t, ~pat2_id: Id.t, ~pat3_id: Id.t, ~def_id: Id.t)
    : list((Id.t, Id.t)) => [
  (Piece.id(_pat1), pat1_id),
  (Piece.id(_pat2), pat2_id),
  (Piece.id(_pat3), pat3_id),
  (Piece.id(_exp_def), def_id),
];
let let_tuple3_exp: form = {
  let explanation = "The only values for the [*definition*](%s) that match the *pattern* are 3-tuples where the first element matches the [*first element pattern*](%s), the second element matches the [*second element pattern*](%s), and the third element matches the [*third element pattern*](%s).";
  let comma = comma_pat();
  let form = [
    mk_let([
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
    id: LetExp(Tuple3),
    syntactic_form: form,
    expandable_id:
      Some((
        Piece.id(comma),
        [pat("p1"), comma_pat(), pat("p2"), comma_pat(), pat("p3")],
      )),
    explanation,
    examples: [let_tuple3_ex],
  };
};
let _pat = pat("C");
let _exp_def = exp("e_def");
let _exp_body = exp("e_body");
let let_ctr_exp_coloring_ids =
  _pat_def_body_let_exp_coloring_ids(
    Piece.id(_pat),
    Piece.id(_exp_def),
    Piece.id(_exp_body),
  );
let let_ctr_exp: form = {
  let explanation = "The only value for the [*definition*](%s) that matches the [*pattern*](%s) is the *`%s` constructor*. The [*definition*](%s) can't be referenced in the [*body*](%s).";
  let form = [
    mk_let([[space(), _pat, space()], [space(), _exp_def, space()]]),
    linebreak(),
    _exp_body,
  ];
  {
    id: LetExp(Ctr),
    syntactic_form: form,
    expandable_id: Some((Piece.id(_pat), [pat("C")])),
    explanation,
    examples: [let_ctr_ex],
  };
};
let _pat_con = pat("p_con");
let _pat_arg = pat("p_arg");
let _exp_def = exp("e_def");
let let_ap_exp_coloring_ids =
    (~con_id: Id.t, ~arg_id: Id.t, ~def_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_pat_con), con_id),
  (Piece.id(_pat_arg), arg_id),
  (Piece.id(_exp_def), def_id),
];
let let_ap_exp: form = {
  let explanation = "The only values for the [*definition*](%s) that match the *pattern* are the [*constructor*](%s) where the *argument* matches the [*argument pattern*](%s).";
  let ap = mk_ap_pat([[_pat_arg]]);
  let form = [
    mk_let([
      [space(), _pat_con, ap, space()],
      [space(), _exp_def, space()],
    ]),
    linebreak(),
    exp("e_body"),
  ];
  {
    id: LetExp(Ap),
    syntactic_form: form,
    expandable_id:
      Some((Piece.id(ap), [pat("p_con"), mk_ap_pat([[pat("p_arg")]])])),
    explanation,
    examples: [let_ap_ex],
  };
};

let lets_emptyhole: group = {
  id: LetExp(EmptyHole),
  forms: [let_empty_hole_exp, let_base_exp],
};

let lets_mutlihole: group = {
  id: LetExp(MultiHole),
  forms: [let_multi_hole_exp, let_base_exp],
};

let lets_wild: group = {
  id: LetExp(Wild),
  forms: [let_wild_exp, let_base_exp],
};

let lets_int: group = {id: LetExp(Int), forms: [let_int_exp, let_base_exp]};

let lets_float: group = {
  id: LetExp(Float),
  forms: [let_float_exp, let_base_exp],
};

let lets_bool: group = {
  id: LetExp(Bool),
  forms: [let_bool_exp, let_base_exp],
};

let lets_str: group = {
  id: LetExp(String),
  forms: [let_str_exp, let_base_exp],
};

let lets_triv: group = {
  id: LetExp(Triv),
  forms: [let_triv_exp, let_base_exp],
};

let lets_listlit: group = {
  id: LetExp(ListLit),
  forms: [let_listlit_exp, let_base_exp],
};

let lets_listnil: group = {
  id: LetExp(ListNil),
  forms: [let_listnil_exp, let_base_exp],
};

let lets_cons: group = {
  id: LetExp(ListCons),
  forms: [let_cons_exp, let_base_exp],
};

let lets_var: group = {id: LetExp(Var), forms: [let_var_exp, let_base_exp]};

let lets_tuple: group = {
  id: LetExp(Tuple),
  forms: [let_tuple_exp, let_base_exp],
};

let lets_tuple2: group = {
  id: LetExp(Tuple2),
  forms: [let_tuple2_exp, let_tuple_exp, let_base_exp],
};

let lets_tuple3: group = {
  id: LetExp(Tuple3),
  forms: [let_tuple3_exp, let_tuple_exp, let_base_exp],
};

let lets_ctr: group = {id: LetExp(Ctr), forms: [let_ctr_exp, let_base_exp]};

let lets_ap: group = {id: LetExp(Ap), forms: [let_ap_exp, let_base_exp]};

open Haz3lcore;
open ExplainThisForm;
open ExampleUtil;

let let_base_exp_group = "let_base_exp_group";
let let_empty_hole_exp_group = "let_empty_hole_exp_group";
let let_multi_hole_exp_group = "let_multi_hole_exp_group";
let let_wild_exp_group = "let_wild_hole_exp_group";
let let_int_exp_group = "let_int_exp_group";
let let_float_exp_group = "let_float_exp_group";
let let_bool_exp_group = "let_bool_exp_group";
let let_str_exp_group = "let_str_exp_group";
let let_triv_exp_group = "let_triv_exp_group";
let let_listlit_exp_group = "let_listlit_exp_group";
let let_listnil_exp_group = "let_listnil_exp_group";
let let_cons_exp_group = "let_cons_exp_group";
let let_var_exp_group = "let_var_exp_group";
let let_tuple_base_exp_group = "let_tuple_base_exp_group";
let let_tuple2_exp_group = "let_tuple2_exp_group";
let let_tuple3_exp_group = "let_tuple3_exp_group";
let let_tag_exp_group = "let_tag_exp_group";
let let_ap_exp_group = "let_ap_exp_group";
let let_base_ex = {
  sub_id: "let_base_ex",
  term: mk_example("let x = 1 in \nx"),
  message: "The variable x is bound to 1, so the expression evaluates to 1",
  feedback: Unselected,
};
let let_wild_ex = {
  sub_id: "let_wild_ex",
  term: mk_example("let _ = 1 in \n2"),
  message: "The 1 is thrown away, so the expression evaluates to 2.",
  feedback: Unselected,
};
let let_int_ex = {
  sub_id: "let_int_ex",
  term: mk_example("let 1 = 1 in \n2"),
  message: "The 1 is thrown away, so the expression evaluates to 2.",
  feedback: Unselected,
};
let let_float_ex = {
  sub_id: "let_float_ex",
  term: mk_example("let 1.1 = 1.1 in \n2"),
  message: "The 1.1 is thrown away, so the expression evaluates to 2.",
  feedback: Unselected,
};
let let_bool_ex = {
  sub_id: "let_bool_ex",
  term: mk_example("let true = true in \n2"),
  message: "The true is thrown away, so the expression evaluates to 2.",
  feedback: Unselected,
};
let let_str_ex = {
  sub_id: "let_str_ex",
  term: mk_example("let \"abc\" = \"abc\" in \n2"),
  message: "The true is thrown away, so the expression evaluates to 2.",
  feedback: Unselected,
};
let let_triv_ex = {
  sub_id: "let_triv_ex",
  term: mk_example("let triv = triv in \n2"),
  message: "The triv is thrown away, so the expression evaluates to 2.",
  feedback: Unselected,
};
let let_listlit_ex = {
  sub_id: "let_listlit_ex",
  term: mk_example("let [x, y] = [1, 2] in \nx"),
  message: "The x is bound to 1 and the y is bound to 2, so the expression evaluates to 1.",
  feedback: Unselected,
};
let let_listnil_ex = {
  sub_id: "let_listnil_ex",
  term: mk_example("let nil = nil in \n2"),
  message: "The empty list is thrown away, so the expression evaluates to 2.",
  feedback: Unselected,
};
let let_cons_hd_ex = {
  sub_id: "let_cons_hd_ex",
  term: mk_example("let hd::tl = 1::nil in \nhd"),
  message: "The hd is bound to 1 and the tl is bound to the empty list, so the expression evaluates to 1.",
  feedback: Unselected,
};
let let_cons_snd_ex = {
  sub_id: "let_cons_snd_ex",
  term: mk_example("let fst::snd::tl = true::false::nil in \nsnd"),
  message: "The fst is bound to true, the snd is bound to false, and the tl is bound to the empty list, so the expression evaluates to false.",
  feedback: Unselected,
};
let let_var_ex = {
  sub_id: "let_var_ex",
  term: mk_example("let x = 1 in \nx + 2"),
  message: "The x is bound to 1, so the expression evaluates to 1 + 2, which is 3.",
  feedback: Unselected,
};
let let_tuple2_ex = {
  sub_id: "let_tuple2_ex",
  term: mk_example("let (x, y) = (1, 2) in \nx + y"),
  message: "The x is bound to 1 and the y is bound to 2, so the expression evaluates to 1 + 2, which is 3.",
  feedback: Unselected,
};
let let_tuple3_ex = {
  sub_id: "let_tuple3_ex",
  term: mk_example("let (x, y, z) = (1, 2, 3) in \nx + y + z"),
  message: "The x is bound to 1, the y is bound to 2, and the z is bound to 3, so the expression evaluates to 1 + 2 + 3, which is 6.",
  feedback: Unselected,
};
let let_tag_ex = {
  sub_id: "let_tag_ex",
  term: mk_example("let None = None in \n2"),
  message: "The None is thrown away, so the expression evaluates to 2.",
  feedback: Unselected,
};
let let_ap_ex = {
  sub_id: "let_ap_ex",
  term: mk_example("let Some(a) = Some(2) in \na"),
  message: "The a is bound to 2, so the expression evaluates to 2.",
  feedback: Unselected,
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
  let explanation = {
    message: "Let expression. The [*definition*](%i) is matched against the [*pattern*](%i).",
    feedback: Unselected,
  };
  let form = [
    mk_let([[space(), _pat, space()], [space(), _exp_def, space()]]),
    linebreak(),
    exp("e_body"),
  ];
  {
    id: "let_base_exp",
    syntactic_form: form,
    expandable_id: Some(Piece.id(_pat)),
    explanation,
    examples: [let_base_ex],
  };
};
let _pat = pat("EmptyHole");
let _exp_def = exp("e_def");
let let_empty_hole_exp_coloring_ids =
  _pat_def_let_exp_coloring_ids(Piece.id(_pat), Piece.id(_exp_def));
let let_empty_hole_exp: form = {
  let explanation = {
    message: "Let expression. After the [*empty hole pattern*](%i) is filled, the [*definition*](%i) is matched against the [*pattern*](%i).",
    feedback: Unselected,
  };
  let form = [
    mk_let([[space(), _pat, space()], [space(), _exp_def, space()]]),
    linebreak(),
    exp("e_body"),
  ];
  {
    id: "let_empty_hole_exp",
    syntactic_form: form,
    expandable_id: Some(Piece.id(_pat)),
    explanation,
    examples: [let_base_ex],
  };
};
let _pat = pat("INVALID");
let _exp_def = exp("e_def");
let let_multi_hole_exp_coloring_ids =
  _pat_def_let_exp_coloring_ids(Piece.id(_pat), Piece.id(_exp_def));
let let_multi_hole_exp: form = {
  let explanation = {
    message: "Let expression. After the [invalid pattern](%i) is corrected, the [*definition*](%i) is matched against the [*pattern*](%i).",
    feedback: Unselected,
  };
  let form = [
    mk_let([[space(), _pat, space()], [space(), _exp_def, space()]]),
    linebreak(),
    exp("e_body"),
  ];
  {
    id: "let_multi_hole_exp",
    syntactic_form: form,
    expandable_id: Some(Piece.id(_pat)),
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
  let explanation = {
    message: "Let expression. The [*definition*](%i) is evaluated and ignored. The [*definition*](%i) can't be referenced in the [*body*](%i).",
    feedback: Unselected,
  };
  let pat = pat("_");
  let form = [
    mk_let([[space(), pat, space()], [space(), _exp_def, space()]]),
    linebreak(),
    _exp_body,
  ];
  {
    id: "let_wild_exp",
    syntactic_form: form,
    expandable_id: Some(Piece.id(pat)),
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
  let explanation = {
    message: "Let expression. The only value for the [*definition*](%i) that matches the [*pattern*](%i) is `%i`. The [*definition*](%i) can't be referenced in the [*body*](%i).",
    feedback: Unselected,
  };
  let form = [
    mk_let([[space(), _pat, space()], [space(), _exp_def, space()]]),
    linebreak(),
    _exp_body,
  ];
  {
    id: "let_int_exp",
    syntactic_form: form,
    expandable_id: Some(Piece.id(_pat)),
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
  let explanation = {
    message: "Let expression. The only value for the [*definition*](%i) that matches the [*pattern*](%i) is `%f`. The [*definition*](%i) can't be referenced in the [*body*](%i).",
    feedback: Unselected,
  };
  let form = [
    mk_let([[space(), _pat, space()], [space(), _exp_def, space()]]),
    linebreak(),
    _exp_body,
  ];
  {
    id: "let_float_exp",
    syntactic_form: form,
    expandable_id: Some(Piece.id(_pat)),
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
  let explanation = {
    message: "Let expression. The only value for the [*definition*](%i) that matches the [*pattern*](%i) is `%b`. The [*definition*](%i) can't be referenced in the [*body*](%i).",
    feedback: Unselected,
  };
  let form = [
    mk_let([[space(), _pat, space()], [space(), _exp_def, space()]]),
    linebreak(),
    _exp_body,
  ];
  {
    id: "let_bool_exp",
    syntactic_form: form,
    expandable_id: Some(Piece.id(_pat)),
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
  let explanation = {
    message: "Let expression. The only value for the [*definition*](%i) that matches the [*pattern*](%i) is `%s`. The [*definition*](%i) can't be referenced in the [*body*](%i).",
    feedback: Unselected,
  };
  let form = [
    mk_let([[space(), _pat, space()], [space(), _exp_def, space()]]),
    linebreak(),
    _exp_body,
  ];
  {
    id: "let_str_exp",
    syntactic_form: form,
    expandable_id: Some(Piece.id(_pat)),
    explanation,
    examples: [let_str_ex],
  };
};
let _pat = pat("triv");
let _exp_def = exp("e_def");
let _exp_body = exp("e_body");
let let_triv_exp_coloring_ids =
  _pat_def_body_let_exp_coloring_ids(
    Piece.id(_pat),
    Piece.id(_exp_def),
    Piece.id(_exp_body),
  );
let let_triv_exp: form = {
  let explanation = {
    message: "Let expression. The only value for the [*definition*](%i) that matches the [*pattern*](%i) is the trivial value `triv`. The [*definition*](%i) can't be referenced in the [*body*](%i).",
    feedback: Unselected,
  };
  let form = [
    mk_let([[space(), _pat, space()], [space(), _exp_def, space()]]),
    linebreak(),
    _exp_body,
  ];
  {
    id: "let_triv_exp",
    syntactic_form: form,
    expandable_id: Some(Piece.id(_pat)),
    explanation,
    examples: [let_triv_ex],
  };
};
let _pat = mk_list_pat([[pat("p1"), comma_pat(), space(), pat("...")]]);
let _exp_def = exp("e_def");
let let_listlit_exp_coloring_ids =
  _pat_def_let_exp_coloring_ids(Piece.id(_pat), Piece.id(_exp_def));
let let_listlit_exp: form = {
  let explanation = {
    message: "Let expression. The only values for the [*definition*](%i) that match the [*pattern*](%i) are lists with %i-elements, where each element matches the corresponding element pattern.",
    feedback: Unselected,
  };
  let form = [
    mk_let([[space(), _pat, space()], [space(), _exp_def, space()]]),
    linebreak(),
    exp("e_body"),
  ];
  {
    id: "let_listlit_exp",
    syntactic_form: form,
    expandable_id: Some(Piece.id(_pat)),
    explanation,
    examples: [let_listlit_ex],
  };
};
let _pat = pat("nil");
let _exp_def = exp("e_def");
let _exp_body = exp("e_body");
let let_listnil_exp_coloring_ids =
  _pat_def_body_let_exp_coloring_ids(
    Piece.id(_pat),
    Piece.id(_exp_def),
    Piece.id(_exp_body),
  );
let let_listnil_exp: form = {
  let explanation = {
    message: "Let expression. The only value for the [*definition*](%i) that matches the [*pattern*](%i) is the empty list `nil`. The [*definition*](%i) can't be referenced in the [*body*](%i).",
    feedback: Unselected,
  };
  let form = [
    mk_let([[space(), _pat, space()], [space(), _exp_def, space()]]),
    linebreak(),
    _exp_body,
  ];
  {
    id: "let_listnil_exp",
    syntactic_form: form,
    expandable_id: Some(Piece.id(_pat)),
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
  let explanation = {
    message: "Let expression. The only values for the [*definition*](%i) that match the *pattern* are non-empty lists that match the [*head*](%i) and [*tail*](%i) patterns.",
    feedback: Unselected,
  };
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
    id: "let_cons_exp",
    syntactic_form: form,
    expandable_id: Some(Piece.id(cons)),
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
  let explanation = {
    message: "Let expression. The [*definition*](%i) is bound to the [*variable*](%i) `%s` in the [*body*](%i).",
    feedback: Unselected,
  };
  let form = [
    mk_let([[space(), _pat, space()], [space(), _exp_def, space()]]),
    linebreak(),
    _exp_body,
  ];
  {
    id: "let_var_exp",
    syntactic_form: form,
    expandable_id: Some(Piece.id(_pat)),
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
  let explanation = {
    message: "Let expression. The only values for the [*definition*](%i) that match the [*pattern*](%i) are %i-tuples where each element matches the corresponding element pattern.",
    feedback: Unselected,
  };
  let form = [
    mk_let([
      [space(), pat("p1"), _comma, space(), pat("..."), space()],
      [space(), _exp_def, space()],
    ]),
    linebreak(),
    exp("e_body"),
  ];
  {
    id: "let_tuple_exp",
    syntactic_form: form,
    expandable_id: Some(Piece.id(_comma)),
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
  let explanation = {
    message: "Let expression. The only values for the [*definition*](%i) that match the *pattern* are 2-tuples where the first element matches the [*first element pattern*](%i) and the second element matches the [*second element pattern*](%i).",
    feedback: Unselected,
  };
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
    id: "let_tuple2_exp",
    syntactic_form: form,
    expandable_id: Some(Piece.id(comma)),
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
  let explanation = {
    message: "Let expression. The only values for the [*definition*](%i) that match the *pattern* are 3-tuples where the first element matches the [*first element pattern*](%i), the second element matches the [*second element pattern*](%i), and the third element matches the [*third element pattern*](%i).",
    feedback: Unselected,
  };
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
    id: "let_tuple3_exp",
    syntactic_form: form,
    expandable_id: Some(Piece.id(comma)),
    explanation,
    examples: [let_tuple3_ex],
  };
};
let _pat = pat("C");
let _exp_def = exp("e_def");
let _exp_body = exp("e_body");
let let_tag_exp_coloring_ids =
  _pat_def_body_let_exp_coloring_ids(
    Piece.id(_pat),
    Piece.id(_exp_def),
    Piece.id(_exp_body),
  );
let let_tag_exp: form = {
  let explanation = {
    message: "Let expression. The only value for the [*definition*](%i) that matches the [*pattern*](%i) is the *`%s` constructor*. The [*definition*](%i) can't be referenced in the [*body*](%i).",
    feedback: Unselected,
  };
  let form = [
    mk_let([[space(), _pat, space()], [space(), _exp_def, space()]]),
    linebreak(),
    _exp_body,
  ];
  {
    id: "let_tag_exp",
    syntactic_form: form,
    expandable_id: Some(Piece.id(_pat)),
    explanation,
    examples: [let_tag_ex],
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
  let explanation = {
    message: "Let expression. The only values for the [*definition*](%i) that match the *pattern* are the [*constructor*](%i) where the *argument* matches the [*argument pattern*](%i).",
    feedback: Unselected,
  };
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
    id: "let_ap_exp",
    syntactic_form: form,
    expandable_id: Some(Piece.id(ap)),
    explanation,
    examples: [let_ap_ex],
  };
};

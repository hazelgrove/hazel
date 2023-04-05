open Haz3lcore;
open ExplainThisForm;
open Example;

[@deriving (show({with_path: false}), sexp, yojson)]
type function_group = {
  id: group_id,
  function_exp: form,
};
[@deriving (show({with_path: false}), sexp, yojson)]
type function_empty_hole_group = {
  id: group_id,
  function_exp: form_option,
  function_empty_hole_exp: form_option,
};
[@deriving (show({with_path: false}), sexp, yojson)]
type function_multi_hole_group = {
  id: group_id,
  function_exp: form_option,
  function_multi_hole_exp: form_option,
};
[@deriving (show({with_path: false}), sexp, yojson)]
type function_wild_group = {
  id: group_id,
  function_exp: form_option,
  function_wild_exp: form_option,
};
[@deriving (show({with_path: false}), sexp, yojson)]
type function_int_group = {
  id: group_id,
  function_exp: form_option,
  function_intlit_exp: form_option,
};
[@deriving (show({with_path: false}), sexp, yojson)]
type function_float_group = {
  id: group_id,
  function_exp: form_option,
  function_floatlit_exp: form_option,
};
[@deriving (show({with_path: false}), sexp, yojson)]
type function_bool_group = {
  id: group_id,
  function_exp: form_option,
  function_boollit_exp: form_option,
};
let function_str_group = "function_str_group";
let function_triv_group = "function_triv_group";
let function_listnil_group = "function_listnil_group";
let function_listlit_group = "function_listlit_group";
let function_cons_group = "function_cons_group";
let function_var_group = "function_var_group";
let function_tuple_group = "function_tuple_group";
let function_tuple_2_group = "function_tuple_2_group";
let function_tuple_3_group = "function_tuple_3_group";
let function_tag_group = "function_tag_group";
let function_ap_group = "function_ap_group";
let basic_fun_ex = {
  sub_id: BasicFun,
  term: mk_example("fun x -> x"),
  message: "The identity function. When given an argument, the function evaluates to that argument.",
  feedback: Unselected,
};
let wild_fun_ex = {
  sub_id: WildFun,
  term: mk_example("fun _ -> 3"),
  message: "When given an argument, the function throws away the supplied argument and always evaluates to 3.",
  feedback: Unselected,
};
let intlit_fun_ex = {
  sub_id: IntLitFun,
  term: mk_example("fun 1 -> 2"),
  message: "When given an argument with value 1, the function throws away the supplied argument and always evaluates to 2.",
  feedback: Unselected,
};
let floatlit_fun_ex = {
  sub_id: FloatLitFun,
  term: mk_example("fun 1.1 -> 2"),
  message: "When given an argument with value 1.1, the function throws away the supplied argument and always evaluates to 2.",
  feedback: Unselected,
};
let boollit_fun_ex = {
  sub_id: BoolLitFun,
  term: mk_example("fun true -> 2"),
  message: "When given an argument with value true, the function throws away the supplied argument and always evaluates to 2.",
  feedback: Unselected,
};
let strlit_fun_ex = {
  sub_id: StrLitFun,
  term: mk_example("fun \"abc\" -> 2"),
  message: "When given an argument with value \"abc\", the function throws away the supplied argument and always evaluates to 2.",
  feedback: Unselected,
};
let triv_fun_ex = {
  sub_id: TrivFun,
  term: mk_example("fun triv -> 2"),
  message: "When given an argument with the triv value, the function throws away the supplied argument and always evaluates to 2.",
  feedback: Unselected,
};
let listnil_fun_ex = {
  sub_id: ListNilFun,
  term: mk_example("fun nil -> 2"),
  message: "When given an argument with the empty list value, the function throws away the supplied argument and always evaluates to 2.",
  feedback: Unselected,
};
let listlist_fun_ex = {
  sub_id: ListListFun,
  term: mk_example("fun [x, y] -> x"),
  message: "When given an argument that is a list of two elements, the function evaluates to the first element of that list.",
  feedback: Unselected,
};
let cons_hd_fun_ex = {
  sub_id: ConsHdFun,
  term: mk_example("fun hd::tl -> hd"),
  message: "When given an argument that is a non-empty list, the function evaluates to the head of that list.",
  feedback: Unselected,
};
let cons_snd_fun_ex = {
  sub_id: ConsSndFun,
  term: mk_example("fun fst::snd::tl -> snd"),
  message: "When given an argument that is a list with at least two elements, the function evaluates to the second element of that list.",
  feedback: Unselected,
};
let var_incr_fun_ex = {
  sub_id: VarIncrFun,
  term: mk_example("fun x -> x + 1"),
  message: "When given an integer argument, the function evaluates to the argument plus 1.",
  feedback: Unselected,
};
let var_and_fun_ex = {
  sub_id: VarAndFun,
  term: mk_example("fun b -> b && true"),
  message: "When given a boolean argument, the function evaluates to the logical-and of the argument and true, which evaluates to the truth value of the argument.",
  feedback: Unselected,
};
let tuple2_fun_ex = {
  sub_id: Tuple2Fun,
  term: mk_example("fun (x, y) -> x + y"),
  message: "When given a 2-tuple of integers, the function evaluates to the sum of the two integers.",
  feedback: Unselected,
};
let tuple3_fun_ex = {
  sub_id: Tuple3Fun,
  term: mk_example("fun (a, b, c) -> a && b && c"),
  message: "When given a 3-tuple of booleans, the function evaluates to the logical-and of the three booleans.",
  feedback: Unselected,
};
let tag_fun_ex = {
  sub_id: TagFun,
  term: mk_example("fun None -> 1"),
  message: "When given a None constructor argument, the function evaluates 1.",
  feedback: Unselected,
};
let ap_fun_ex = {
  sub_id: ApFun,
  term: mk_example("fun Some(a) -> a"),
  message: "When given a Some constructor argument, the function evaluates to the constructor's argument.",
  feedback: Unselected,
};
// TODO for shared examples, should the feedback be stored separately for each "instance"?
let _pat_body_function_exp_coloring_ids =
    (sf_pat_id: Id.t, sf_body_id: Id.t, ~pat_id: Id.t, ~body_id: Id.t)
    : list((Id.t, Id.t)) => {
  [(sf_pat_id, pat_id), (sf_body_id, body_id)];
};
let _pat = pat("p");
let _exp = exp("e");
let function_exp_coloring_ids =
  _pat_body_function_exp_coloring_ids(Piece.id(_pat), Piece.id(_exp));
let function_exp: form = {
  let explanation = {
    message: "Function literal. When applied to an argument that matches the [*argument pattern*](%i), evaluates to the function [*body*](%i).",
    feedback: Unselected,
  };
  let form = [mk_fun([[space(), _pat, space()]]), space(), _exp];
  {
    id: FunctionExp,
    syntactic_form: form,
    expandable_id: Some(Piece.id(_pat)),
    explanation,
    examples: [basic_fun_ex] // TODO What other examples should be here
  };
};

let _pat = pat("EmptyHole");
let _exp = exp("e");
let function_empty_hole_exp_coloring_ids =
  _pat_body_function_exp_coloring_ids(Piece.id(_pat), Piece.id(_exp));
let function_empty_hole_exp: form = {
  let explanation = {
    message: "Function literal. When applied to an argument that matches the [*argument pattern*](%i), evaluates to the function [*body*](%i), after the [empty hole pattern](%i) is filled.",
    feedback: Unselected,
  };
  let form = [mk_fun([[space(), _pat, space()]]), space(), _exp];
  {
    id: FunctionEmptyHole,
    syntactic_form: form,
    expandable_id: Some(Piece.id(_pat)),
    explanation,
    examples: [basic_fun_ex],
  };
};
let _pat = pat("INVALID");
let _exp = exp("e");
let function_multi_hole_exp_coloring_ids =
  _pat_body_function_exp_coloring_ids(Piece.id(_pat), Piece.id(_exp));
let function_multi_hole_exp: form = {
  let explanation = {
    message: "Function literal. When applied to an argument that matches the [*argument pattern*](%i), evaluates to the function [*body*](%i), after the [invalid argument pattern](%i) is corrected.",
    feedback: Unselected,
  };
  let form = [mk_fun([[space(), _pat, space()]]), space(), _exp];
  {
    id: FunctionMultiHole,
    syntactic_form: form,
    expandable_id: Some(Piece.id(_pat)),
    explanation,
    examples: [basic_fun_ex],
  };
};
let _exp = exp("e");
let function_wild_exp_coloring_ids = (~body_id: Id.t): list((Id.t, Id.t)) => {
  [(Piece.id(_exp), body_id)];
};
let function_wild_exp: form = {
  let explanation = {
    message: "Function literal. When applied to an argument that is ignored, evaluates to the function [*body*](%i).",
    feedback: Unselected,
  };
  let pat = pat("_");
  let form = [mk_fun([[space(), pat, space()]]), space(), _exp];
  {
    id: FunctionWild,
    syntactic_form: form,
    expandable_id: Some(Piece.id(pat)),
    explanation,
    examples: [wild_fun_ex],
  };
};
let _pat = pat("IntLit");
let _exp = exp("e");
let function_intlit_exp_coloring_ids =
  _pat_body_function_exp_coloring_ids(Piece.id(_pat), Piece.id(_exp));
let function_intlit_exp: form = {
  let explanation = {
    message: "Function literal. The only value that matches the [*argument pattern*](%i) is `%i`. When applied to an argument which matches the [*argument pattern*](%i), evaluates to the function [*body*](%i).",
    feedback: Unselected,
  };
  let form = [mk_fun([[space(), _pat, space()]]), space(), _exp];
  {
    id: FunctionInt,
    syntactic_form: form,
    expandable_id: Some(Piece.id(_pat)),
    explanation,
    examples: [intlit_fun_ex],
  };
};
let _pat = pat("FloatLit");
let _exp = exp("e");
let function_floatlit_exp_coloring_ids =
  _pat_body_function_exp_coloring_ids(Piece.id(_pat), Piece.id(_exp));
let function_floatlit_exp: form = {
  let explanation = {
    message: "Function literal. The only value that matches the [*argument pattern*](%i) is `%f`. When applied to an argument which matches the [*argument pattern*](%i), evaluates to the function [*body*](%i).",
    feedback: Unselected,
  };
  // TODO print out the float literal nicer
  let form = [mk_fun([[space(), _pat, space()]]), space(), _exp];
  {
    id: FunctionFloat,
    syntactic_form: form,
    expandable_id: Some(Piece.id(_pat)),
    explanation,
    examples: [floatlit_fun_ex],
  };
};
let _pat = pat("BoolLit");
let _exp = exp("e");
let function_boollit_exp_coloring_ids =
  _pat_body_function_exp_coloring_ids(Piece.id(_pat), Piece.id(_exp));
let function_boollit_exp: form = {
  let explanation = {
    message: "Function literal. The only value that matches the [*argument pattern*](%i) is `%b`. When applied to an argument which matches the [*argument pattern*](%i), evaluates to the function [*body*](%i).",
    feedback: Unselected,
  };
  let form = [mk_fun([[space(), _pat, space()]]), space(), _exp];
  {
    id: FunctionBool,
    syntactic_form: form,
    expandable_id: Some(Piece.id(_pat)),
    explanation,
    examples: [boollit_fun_ex],
  };
};
/*let _pat = pat("StringLit");
  let _exp = exp("e");
  let function_strlit_exp_coloring_ids =
    _pat_body_function_exp_coloring_ids(Piece.id(_pat), Piece.id(_exp));
  let function_strlit_exp: form = {
    let explanation = {
      message: "Function literal. The only value that matches the [*argument pattern*](%i) is `%s`. When applied to an argument which matches the [*argument pattern*](%i), evaluates to the function [*body*](%i).",
      feedback: Unselected,
    };
    let form = [mk_fun([[space(), _pat, space()]]), space(), _exp];
    {
      id: "function_strlit_exp",
      syntactic_form: form,
      expandable_id: Some(Piece.id(_pat)),
      explanation,
      examples: [strlit_fun_ex],
    };
  };
  let _pat = pat("triv");
  let _exp = exp("e");
  let function_triv_exp_coloring_ids =
    _pat_body_function_exp_coloring_ids(Piece.id(_pat), Piece.id(_exp));
  let function_triv_exp: form = {
    let explanation = {
      message: "Function literal. The only value that matches the [*argument pattern*](%i) is the trivial value `triv`. When applied to an argument which matches the [*argument pattern*](%i), evaluates to the function [*body*](%i). This if functionally equivalent to a zero argument function.",
      feedback: Unselected,
    };
    let form = [mk_fun([[space(), _pat, space()]]), space(), _exp];
    {
      id: "function_triv_exp",
      syntactic_form: form,
      expandable_id: Some(Piece.id(_pat)),
      explanation,
      examples: [triv_fun_ex],
    };
  };
  let _pat = pat("nil");
  let _exp = exp("e");
  let function_listnil_exp_coloring_ids =
    _pat_body_function_exp_coloring_ids(Piece.id(_pat), Piece.id(_exp));
  let function_listnil_exp: form = {
    let explanation = {
      message: "Function literal. The only value that matches the [*argument pattern*](%i) is the empty list `nil`. When applied to an argument which matches the [*argument pattern*](%i), evaluates to the function [*body*](%i).",
      feedback: Unselected,
    };
    let form = [mk_fun([[space(), _pat, space()]]), space(), _exp];
    {
      id: "function_listnil_exp",
      syntactic_form: form,
      expandable_id: Some(Piece.id(_pat)),
      explanation,
      examples: [listnil_fun_ex],
    };
  };
  let _pat = mk_list_pat([[pat("p1"), comma_pat(), space(), pat("...")]]);
  let _exp = exp("e");
  let function_listlit_exp_coloring_ids =
    _pat_body_function_exp_coloring_ids(Piece.id(_pat), Piece.id(_exp));
  let function_listlit_exp: form = {
    let explanation = {
      message: "Function literal. The only values that match the [*argument pattern*](%i) are lists with %n-elements, each matching the corresponding element pattern. When applied to an argument which matches the [*argument pattern*](%i), evaluates to the function [*body*](%i).",
      feedback: Unselected,
    };
    let form = [mk_fun([[space(), _pat, space()]]), space(), _exp];
    {
      id: "function_listlit_exp",
      syntactic_form: form,
      expandable_id: Some(Piece.id(_pat)),
      explanation,
      examples: [listnil_fun_ex, listlist_fun_ex],
    };
  };
  let _pat_hd = pat("p_hd");
  let _pat_tl = pat("p_tl");
  let _exp = exp("e");
  let function_cons_exp_coloring_ids =
      (~hd_id: Id.t, ~tl_id: Id.t, ~body_id: Id.t): list((Id.t, Id.t)) => {
    [
      (Piece.id(_pat_hd), hd_id),
      (Piece.id(_pat_tl), tl_id),
      (Piece.id(_exp), body_id),
    ];
  };
  let function_cons_exp: form = {
    let explanation = {
      message: "Function literal. The only values that match the *argument pattern* are non-empty lists that match the [*head pattern*](%i) and [*tail pattern*](%i). When applied to an argument which matches the *argument pattern*, evaluates to the function [*body*](%i).",
      feedback: Unselected,
    };
    let cons = cons_pat();
    let form = [
      mk_fun([[space(), _pat_hd, cons, _pat_tl, space()]]),
      space(),
      _exp,
    ];
    {
      id: "function_cons_exp",
      syntactic_form: form,
      expandable_id: Some(Piece.id(cons)),
      explanation,
      examples: [cons_hd_fun_ex, cons_snd_fun_ex],
    };
  };
  let _pat = pat("x");
  let _exp = exp("e");
  let function_var_exp_coloring_ids =
    _pat_body_function_exp_coloring_ids(Piece.id(_pat), Piece.id(_exp));
  let function_var_exp: form = {
    let explanation = {
      message: "Function literal. When applied to an argument which is bound to the [*variable*](%i) `%s`, evaluates to the function [*body*](%i).",
      feedback: Unselected,
    };
    let form = [mk_fun([[space(), _pat, space()]]), space(), _exp];
    {
      id: "function_var_exp",
      syntactic_form: form,
      expandable_id: Some(Piece.id(_pat)),
      explanation,
      examples: [basic_fun_ex, var_incr_fun_ex, var_and_fun_ex],
    };
  };
  let _comma = comma_pat();
  let _exp = exp("e");
  let function_tuple_exp_coloring_ids =
    _pat_body_function_exp_coloring_ids(Piece.id(_comma), Piece.id(_exp));
  let function_tuple_exp: form = {
    let explanation = {
      message: "Function literal. The only values that match the [*argument pattern*](%i) are %i-tuples where each element matches the corresponding argument element pattern. When applied to an argument which matches the [*argument pattern*](%i), evaluates to the function [*body*](%i).",
      feedback: Unselected,
    };
    let form = [
      mk_fun([[space(), pat("p1"), _comma, space(), pat("..."), space()]]),
      space(),
      _exp,
    ];
    {
      id: "function_tuple_exp",
      syntactic_form: form,
      expandable_id: Some(Piece.id(_comma)),
      explanation,
      examples: [tuple2_fun_ex, tuple3_fun_ex],
    };
  };
  let _pat1 = pat("p1");
  let _pat2 = pat("p2");
  let _exp = exp("e");
  let function_tuple2_exp_coloring_ids =
      (~pat1_id: Id.t, ~pat2_id: Id.t, ~body_id: Id.t): list((Id.t, Id.t)) => {
    [
      (Piece.id(_pat1), pat1_id),
      (Piece.id(_pat2), pat2_id),
      (Piece.id(_exp), body_id),
    ];
  };
  let function_tuple2_exp: form = {
    let explanation = {
      message: "Function literal. The only values that match the *argument pattern* are 2-tuples where the first element matches the [*first element pattern*](%i) and the second element matches the [*second element pattern*](%i). When applied to an argument which matches the *argument pattern*, evaluates to the function [*body*](%i).",
      feedback: Unselected,
    };
    let comma = comma_pat();
    let form = [
      mk_fun([[space(), _pat1, comma, space(), _pat2, space()]]),
      space(),
      _exp,
    ];
    {
      id: "function_tuple2_exp",
      syntactic_form: form,
      expandable_id: Some(Piece.id(comma)),
      explanation,
      examples: [tuple2_fun_ex],
    };
  };
  let _pat1 = pat("p1");
  let _pat2 = pat("p2");
  let _pat3 = pat("p3");
  let _exp = exp("e");
  let function_tuple3_exp_coloring_ids =
      (~pat1_id: Id.t, ~pat2_id: Id.t, ~pat3_id: Id.t, ~body_id: Id.t)
      : list((Id.t, Id.t)) => {
    [
      (Piece.id(_pat1), pat1_id),
      (Piece.id(_pat2), pat2_id),
      (Piece.id(_pat3), pat3_id),
      (Piece.id(_exp), body_id),
    ];
  };
  let function_tuple3_exp: form = {
    let explanation = {
      message: "Function literal. The only values that match the *argument pattern* are 2-tuples where the first element matches the [*first element pattern*](%i), the second element matches the [*second element pattern*](%i), and the third element matches the [*third element pattern*](%i). When applied to an argument which matches the *argument pattern*, evaluates to the function [*body*](%i).",
      feedback: Unselected,
    };
    let comma = comma_pat();
    let form = [
      mk_fun([
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
      ]),
      space(),
      _exp,
    ];
    {
      id: "function_tuple3_exp",
      syntactic_form: form,
      expandable_id: Some(Piece.id(comma)),
      explanation,
      examples: [tuple3_fun_ex],
    };
  };
  let _pat = pat("C");
  let _exp = exp("e");
  let function_tag_exp_coloring_ids =
    _pat_body_function_exp_coloring_ids(Piece.id(_pat), Piece.id(_exp));
  let function_tag_exp: form = {
    let explanation = {
      message: "Function literal. The only value that matches the [*argument pattern*](%i) is the *`%s` constructor*. When applied to an argument which matches the [*argument pattern*](%i), evaluates to the function [*body*](%i).",
      feedback: Unselected,
    };
    let form = [mk_fun([[space(), _pat, space()]]), space(), _exp];
    {
      id: "function_tag_exp",
      syntactic_form: form,
      expandable_id: Some(Piece.id(_pat)),
      explanation,
      examples: [tag_fun_ex],
    };
  };
  let _pat_con = pat("p_con");
  let _pat_arg = pat("p_arg");
  let _exp = exp("e");
  let function_ap_exp_coloring_ids =
      (~con_id: Id.t, ~arg_id: Id.t, ~body_id: Id.t): list((Id.t, Id.t)) => {
    [
      (Piece.id(_pat_con), con_id),
      (Piece.id(_pat_arg), arg_id),
      (Piece.id(_exp), body_id),
    ];
  };
  let function_ap_exp: form = {
    let explanation = {
      message: "Function literal. The only values that match the *argument pattern* are the [*constructor*](%i) where the *constructor argument* matches the [*constructor argument pattern*](%i). When applied to an argument which matches the *argument pattern*, evaluates to the function [*body*](%i).",
      feedback: Unselected,
    };
    let ap = mk_ap_pat([[_pat_arg]]);
    let form = [mk_fun([[space(), _pat_con, ap, space()]]), space(), _exp];
    {
      id: "function_ap_exp",
      syntactic_form: form,
      expandable_id: Some(Piece.id(ap)),
      explanation,
      examples: [ap_fun_ex],
    };
  };*/

let function_group = {id: FunctionExp, function_exp};
let function_empty_hole_group = {
  id: FunctionEmptyHole,
  function_exp: {
    form: function_exp,
    expansion_label: [pat("p")],
    selected: false,
  },
  function_empty_hole_exp: {
    form: function_empty_hole_exp,
    expansion_label: [pat("EMPTYHOLE")],
    selected: true,
  },
};
let function_multi_hole_group = {
  id: FunctionMultiHole,
  function_exp: {
    form: function_exp,
    expansion_label: [pat("p")],
    selected: false,
  },
  function_multi_hole_exp: {
    form: function_multi_hole_exp,
    expansion_label: [pat("INVALID")],
    selected: true,
  },
};
let function_wild_group = {
  id: FunctionWild,
  function_exp: {
    form: function_exp,
    expansion_label: [pat("p")],
    selected: false,
  },
  function_wild_exp: {
    form: function_wild_exp,
    expansion_label: [pat("_")],
    selected: true,
  },
};
let function_int_group = {
  id: FunctionInt,
  function_exp: {
    form: function_exp,
    expansion_label: [pat("p")],
    selected: false,
  },
  function_intlit_exp: {
    form: function_intlit_exp,
    expansion_label: [pat("IntLit")],
    selected: true,
  },
};
let function_float_group = {
  id: FunctionFloat,
  function_exp: {
    form: function_exp,
    expansion_label: [pat("p")],
    selected: false,
  },
  function_floatlit_exp: {
    form: function_floatlit_exp,
    expansion_label: [pat("FloatLit")],
    selected: true,
  },
};
let function_bool_group = {
  id: FunctionBool,
  function_exp: {
    form: function_exp,
    expansion_label: [pat("p")],
    selected: false,
  },
  function_boollit_exp: {
    form: function_boollit_exp,
    expansion_label: [pat("BoolLit")],
    selected: true,
  },
};

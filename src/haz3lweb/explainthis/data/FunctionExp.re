open Haz3lcore;
open ExplainThisForm;
open Example;

let basic_fun_ex = {
  sub_id: Fun(Basic),
  term: mk_example("fun x -> x"),
  message: "The identity function. When given an argument, the function evaluates to that argument.",
};
let wild_fun_ex = {
  sub_id: Fun(Wild),
  term: mk_example("fun _ -> 3"),
  message: "When given an argument, the function throws away the supplied argument and always evaluates to 3.",
};
let intlit_fun_ex = {
  sub_id: Fun(IntLit),
  term: mk_example("fun 1 -> 2"),
  message: "When given an argument with value 1, the function throws away the supplied argument and always evaluates to 2.",
};
let floatlit_fun_ex = {
  sub_id: Fun(FloatLit),
  term: mk_example("fun 1.1 -> 2"),
  message: "When given an argument with value 1.1, the function throws away the supplied argument and always evaluates to 2.",
};
let boollit_fun_ex = {
  sub_id: Fun(BoolLit),
  term: mk_example("fun true -> 2"),
  message: "When given an argument with value true, the function throws away the supplied argument and always evaluates to 2.",
};
let strlit_fun_ex = {
  sub_id: Fun(StrLit),
  term: mk_example("fun \"abc\" -> 2"),
  message: "When given an argument with value \"abc\", the function throws away the supplied argument and always evaluates to 2.",
};
let triv_fun_ex = {
  sub_id: Fun(Triv),
  term: mk_example("fun () -> 2"),
  message: "When given an argument with the () value, the function throws away the supplied argument and always evaluates to 2.",
};
let listnil_fun_ex = {
  sub_id: Fun(ListNil),
  term: mk_example("fun [] -> 2"),
  message: "When given an argument with the empty list value, the function throws away the supplied argument and always evaluates to 2.",
};
let listlit_fun_ex = {
  sub_id: Fun(ListLit),
  term: mk_example("fun [x, y] -> x"),
  message: "When given an argument that is a list of two elements, the function evaluates to the first element of that list.",
};
let cons_hd_fun_ex = {
  sub_id: Fun(ConsHd),
  term: mk_example("fun hd::tl -> hd"),
  message: "When given an argument that is a non-empty list, the function evaluates to the head of that list.",
};
let cons_snd_fun_ex = {
  sub_id: Fun(ConsSnd),
  term: mk_example("fun fst::snd::tl -> snd"),
  message: "When given an argument that is a list with at least two elements, the function evaluates to the second element of that list.",
};
let var_incr_fun_ex = {
  sub_id: Fun(VarIncr),
  term: mk_example("fun x -> x + 1"),
  message: "When given an integer argument, the function evaluates to the argument plus 1.",
};
let var_and_fun_ex = {
  sub_id: Fun(VarAnd),
  term: mk_example("fun b -> b && true"),
  message: "When given a boolean argument, the function evaluates to the logical-and of the argument and true, which evaluates to the truth value of the argument.",
};
let tuple2_fun_ex = {
  sub_id: Fun(Tuple2),
  term: mk_example("fun (x, y) -> x + y"),
  message: "When given a 2-tuple of integers, the function evaluates to the sum of the two integers.",
};
let tuple3_fun_ex = {
  sub_id: Fun(Tuple3),
  term: mk_example("fun (a, b, c) ->\na && b && c"),
  message: "When given a 3-tuple of booleans, the function evaluates to the logical-and of the three booleans.",
};
let label_fun_ex = {
  sub_id: Fun(TupLabel),
  term: mk_example("fun x=y, y=z ->\ny"),
  message: "When given a 2-tuple of elements, the function evaluates to the first element (not the second).",
};
let ctr_fun_ex = {
  sub_id: Fun(Ctr),
  term: mk_example("fun None -> 1"),
  message: "When given a None constructor argument, the function evaluates 1.",
};
let ap_fun_ex = {
  sub_id: Fun(Ap),
  term: mk_example("fun Some(a) -> a"),
  message: "When given a Some constructor argument, the function evaluates to the constructor's argument.",
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
  let explanation = "When applied to an argument that matches the [*argument pattern*](%s), evaluates to the function [*body*](%s).";
  let form = [mk_fun([[space(), _pat, space()]]), space(), _exp];
  {
    id: FunctionExp(Base),
    syntactic_form: form,
    expandable_id: Some((Piece.id(_pat), [pat("p")])),
    explanation,
    examples: [basic_fun_ex] // TODO What other examples should be here
  };
};

let _pat = Piece.Grout({id: Id.mk(), shape: Convex});
let _exp = exp("e");
let function_empty_hole_exp_coloring_ids =
  _pat_body_function_exp_coloring_ids(Piece.id(_pat), Piece.id(_exp));
let function_empty_hole_exp: form = {
  let explanation = "When applied to an argument that matches the [*argument pattern*](%s), evaluates to the function [*body*](%s), after the [empty hole pattern](%s) is filled.";
  let form = [mk_fun([[space(), _pat, space()]]), space(), _exp];
  {
    id: FunctionExp(EmptyHole),
    syntactic_form: form,
    expandable_id:
      Some((Piece.id(_pat), [Grout({id: Id.mk(), shape: Convex})])),
    explanation,
    examples: [basic_fun_ex],
  };
};
let _pat = pat("INVALID");
let _exp = exp("e");
let function_multi_hole_exp_coloring_ids =
  _pat_body_function_exp_coloring_ids(Piece.id(_pat), Piece.id(_exp));
let function_multi_hole_exp: form = {
  let explanation = "When applied to an argument that matches the [*argument pattern*](%s), evaluates to the function [*body*](%s), after the [invalid argument pattern](%s) is corrected.";
  let form = [mk_fun([[space(), _pat, space()]]), space(), _exp];
  {
    id: FunctionExp(MultiHole),
    syntactic_form: form,
    expandable_id: Some((Piece.id(_pat), [pat("INVALID")])),
    explanation,
    examples: [basic_fun_ex],
  };
};
let _exp = exp("e");
let function_wild_exp_coloring_ids = (~body_id: Id.t): list((Id.t, Id.t)) => {
  [(Piece.id(_exp), body_id)];
};
let function_wild_exp: form = {
  let explanation = "When applied to an argument that is ignored, evaluates to the function [*body*](%s).";
  let _pat = pat("_");
  let form = [mk_fun([[space(), _pat, space()]]), space(), _exp];
  {
    id: FunctionExp(Wild),
    syntactic_form: form,
    expandable_id: Some((Piece.id(_pat), [pat("_")])),
    explanation,
    examples: [wild_fun_ex],
  };
};
let _pat = pat("IntLit");
let _exp = exp("e");
let function_intlit_exp_coloring_ids =
  _pat_body_function_exp_coloring_ids(Piece.id(_pat), Piece.id(_exp));
let function_intlit_exp: form = {
  let explanation = "The only value that matches the [*argument pattern*](%s) is `%s`. When applied to an argument which matches the [*argument pattern*](%s), evaluates to the function [*body*](%s).";
  let form = [mk_fun([[space(), _pat, space()]]), space(), _exp];
  {
    id: FunctionExp(Int),
    syntactic_form: form,
    expandable_id: Some((Piece.id(_pat), [pat("IntLit")])),
    explanation,
    examples: [intlit_fun_ex],
  };
};
let _pat = pat("FloatLit");
let _exp = exp("e");
let function_floatlit_exp_coloring_ids =
  _pat_body_function_exp_coloring_ids(Piece.id(_pat), Piece.id(_exp));
let function_floatlit_exp: form = {
  let explanation = "The only value that matches the [*argument pattern*](%s) is `%f`. When applied to an argument which matches the [*argument pattern*](%s), evaluates to the function [*body*](%s).";
  // TODO print out the float literal nicer
  let form = [mk_fun([[space(), _pat, space()]]), space(), _exp];
  {
    id: FunctionExp(Float),
    syntactic_form: form,
    expandable_id: Some((Piece.id(_pat), [pat("FloatLit")])),
    explanation,
    examples: [floatlit_fun_ex],
  };
};
let _pat = pat("BoolLit");
let _exp = exp("e");
let function_boollit_exp_coloring_ids =
  _pat_body_function_exp_coloring_ids(Piece.id(_pat), Piece.id(_exp));
let function_boollit_exp: form = {
  let explanation = "The only value that matches the [*argument pattern*](%s) is `%b`. When applied to an argument which matches the [*argument pattern*](%s), evaluates to the function [*body*](%s).";
  let form = [mk_fun([[space(), _pat, space()]]), space(), _exp];
  {
    id: FunctionExp(Bool),
    syntactic_form: form,
    expandable_id: Some((Piece.id(_pat), [pat("BoolLit")])),
    explanation,
    examples: [boollit_fun_ex],
  };
};

let _pat = pat("StringLit");
let _exp = exp("e");
let function_strlit_exp_coloring_ids =
  _pat_body_function_exp_coloring_ids(Piece.id(_pat), Piece.id(_exp));
let function_strlit_exp: form = {
  let explanation = "The only value that matches the [*argument pattern*](%s) is `%s`. When applied to an argument which matches the [*argument pattern*](%s), evaluates to the function [*body*](%s).";

  let form = [mk_fun([[space(), _pat, space()]]), space(), _exp];
  {
    id: FunctionExp(String),
    syntactic_form: form,
    expandable_id: Some((Piece.id(_pat), [pat("StringLit")])),
    explanation,
    examples: [strlit_fun_ex],
  };
};
let _pat = pat("()");
let _exp = exp("e");
let function_triv_exp_coloring_ids =
  _pat_body_function_exp_coloring_ids(Piece.id(_pat), Piece.id(_exp));
let function_triv_exp: form = {
  let explanation = "The only value that matches the [*argument pattern*](%s) is the trivial value `()`. When applied to an argument which matches the [*argument pattern*](%s), evaluates to the function [*body*](%s). This if functionally equivalent to a zero argument function.";
  let form = [mk_fun([[space(), _pat, space()]]), space(), _exp];
  {
    id: FunctionExp(Triv),
    syntactic_form: form,
    expandable_id: Some((Piece.id(_pat), [pat("()")])),
    explanation,
    examples: [triv_fun_ex],
  };
};
let _pat = pat("[]");
let _exp = exp("e");
let function_listnil_exp_coloring_ids =
  _pat_body_function_exp_coloring_ids(Piece.id(_pat), Piece.id(_exp));
let function_listnil_exp: form = {
  let explanation = "The only value that matches the [*argument pattern*](%s) is the empty list `[]`. When applied to an argument which matches the [*argument pattern*](%s), evaluates to the function [*body*](%s).";
  let form = [mk_fun([[space(), _pat, space()]]), space(), _exp];
  {
    id: FunctionExp(ListNil),
    syntactic_form: form,
    expandable_id: Some((Piece.id(_pat), [pat("[]")])),
    explanation,
    examples: [listnil_fun_ex],
  };
};
let _pat = mk_list_pat([[pat("p1"), comma_pat(), space(), pat("...")]]);
let _exp = exp("e");
let function_listlit_exp_coloring_ids =
  _pat_body_function_exp_coloring_ids(Piece.id(_pat), Piece.id(_exp));
let function_listlit_exp: form = {
  let explanation = "The only values that match the [*argument pattern*](%s) are lists with %s-elements, each matching the corresponding element pattern. When applied to an argument which matches the [*argument pattern*](%s), evaluates to the function [*body*](%s).";
  let form = [mk_fun([[space(), _pat, space()]]), space(), _exp];
  {
    id: FunctionExp(ListLit),
    syntactic_form: form,
    expandable_id:
      Some((
        Piece.id(_pat),
        [mk_list_pat([[pat("p1"), comma_pat(), pat("...")]])],
      )),
    explanation,
    examples: [listnil_fun_ex, listlit_fun_ex],
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
  let explanation = "The only values that match the *argument pattern* are non-empty lists that match the [*head pattern*](%s) and [*tail pattern*](%s). When applied to an argument which matches the *argument pattern*, evaluates to the function [*body*](%s).";
  let cons = cons_pat();
  let form = [
    mk_fun([[space(), _pat_hd, cons, _pat_tl, space()]]),
    space(),
    _exp,
  ];
  {
    id: FunctionExp(ListCons),
    syntactic_form: form,
    expandable_id:
      Some((Piece.id(cons), [pat("p_hd"), cons_pat(), pat("p_tl")])),
    explanation,
    examples: [cons_hd_fun_ex, cons_snd_fun_ex],
  };
};
let _pat = pat("x");
let _exp = exp("e");
let function_var_exp_coloring_ids =
  _pat_body_function_exp_coloring_ids(Piece.id(_pat), Piece.id(_exp));
let function_var_exp: form = {
  let explanation = "When applied to an argument which is bound to the [*variable*](%s) `%s`, evaluates to the function [*body*](%s).";
  let form = [mk_fun([[space(), _pat, space()]]), space(), _exp];
  {
    id: FunctionExp(Var),
    syntactic_form: form,
    expandable_id: Some((Piece.id(_pat), [pat("x")])),
    explanation,
    examples: [basic_fun_ex, var_incr_fun_ex, var_and_fun_ex],
  };
};

let _label_pat = label_pat();
let _exp = exp("e");
let function_label_exp_coloring_ids =
  _pat_body_function_exp_coloring_ids(Piece.id(_label_pat), Piece.id(_exp));
let function_label_exp: form = {
  let explanation = "Any unlabeled value matches with the [*argument*]. Only labeled elements that match the [*name*](%s) 'x' are accepted, and evaluate using the [*value*](%s) 'y' to the function [*body*](%s).";
  let form = [
    mk_fun([[space(), pat("x"), _label_pat, pat("y"), space()]]),
    space(),
    _exp,
  ];
  {
    id: FunctionExp(TupLabel),
    syntactic_form: form,
    expandable_id:
      Some((Piece.id(_label_pat), [pat("x"), label_pat(), pat("y")])),
    explanation,
    examples: [label_fun_ex],
  };
};
let _comma = comma_pat();
let _exp = exp("e");
let function_tuple_exp_coloring_ids =
  _pat_body_function_exp_coloring_ids(Piece.id(_comma), Piece.id(_exp));
let function_tuple_exp: form = {
  let explanation = "The only values that match the [*argument pattern*](%s) are %s-tuples where each element matches the corresponding argument element pattern. When applied to an argument which matches the [*argument pattern*](%s), evaluates to the function [*body*](%s).";
  let form = [
    mk_fun([[space(), pat("p1"), _comma, space(), pat("..."), space()]]),
    space(),
    _exp,
  ];
  {
    id: FunctionExp(Tuple),
    syntactic_form: form,
    expandable_id:
      Some((Piece.id(_comma), [pat("p1"), comma_pat(), pat("...")])),
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
  let explanation = "The only values that match the *argument pattern* are 2-tuples where the first element matches the [*first element pattern*](%s) and the second element matches the [*second element pattern*](%s). When applied to an argument which matches the *argument pattern*, evaluates to the function [*body*](%s).";
  let comma = comma_pat();
  let form = [
    mk_fun([[space(), _pat1, comma, space(), _pat2, space()]]),
    space(),
    _exp,
  ];
  {
    id: FunctionExp(Tuple2),
    syntactic_form: form,
    expandable_id:
      Some((Piece.id(comma), [pat("p1"), comma_pat(), pat("p2")])),
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
  let explanation = "The only values that match the *argument pattern* are 3-tuples where the first element matches the [*first element pattern*](%s), the second element matches the [*second element pattern*](%s), and the third element matches the [*third element pattern*](%s). When applied to an argument which matches the *argument pattern*, evaluates to the function [*body*](%s).";
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
    id: FunctionExp(Tuple3),
    syntactic_form: form,
    expandable_id:
      Some((
        Piece.id(comma),
        [pat("p1"), comma_pat(), pat("p2"), comma_pat(), pat("p3")],
      )),
    explanation,
    examples: [tuple3_fun_ex],
  };
};
let _pat = pat("C");
let _exp = exp("e");
let function_ctr_exp_coloring_ids =
  _pat_body_function_exp_coloring_ids(Piece.id(_pat), Piece.id(_exp));
let function_ctr_exp: form = {
  let explanation = "The only value that matches the [*argument pattern*](%s) is the *`%s` constructor*. When applied to an argument which matches the [*argument pattern*](%s), evaluates to the function [*body*](%s).";
  let form = [mk_fun([[space(), _pat, space()]]), space(), _exp];
  {
    id: FunctionExp(Ctr),
    syntactic_form: form,
    expandable_id: Some((Piece.id(_pat), [pat("C")])),
    explanation,
    examples: [ctr_fun_ex],
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
  let explanation = "The only values that match the *argument pattern* are the [*constructor*](%s) where the *constructor argument* matches the [*constructor argument pattern*](%s). When applied to an argument which matches the *argument pattern*, evaluates to the function [*body*](%s).";
  let ap = mk_ap_pat([[_pat_arg]]);
  let form = [mk_fun([[space(), _pat_con, ap, space()]]), space(), _exp];
  {
    id: FunctionExp(Ap),
    syntactic_form: form,
    expandable_id:
      Some((Piece.id(ap), [pat("p_con"), mk_ap_pat([[pat("p_arg")]])])),
    explanation,
    examples: [ap_fun_ex],
  };
};

let functions: group = {id: FunctionExp(Base), forms: [function_exp]};
let functions_empty_hole = {
  id: FunctionExp(EmptyHole),
  forms: [function_empty_hole_exp, function_exp],
};
let functions_multi_hole = {
  id: FunctionExp(MultiHole),
  forms: [function_multi_hole_exp, function_exp],
};
let functions_wild = {
  id: FunctionExp(Wild),
  forms: [function_wild_exp, function_exp],
};
let functions_int = {
  id: FunctionExp(Int),
  forms: [function_intlit_exp, function_exp],
};
let functions_float = {
  id: FunctionExp(Float),
  forms: [function_floatlit_exp, function_exp],
};
let functions_bool = {
  id: FunctionExp(Bool),
  forms: [function_boollit_exp, function_exp],
};

let functions_str = {
  id: FunctionExp(String),
  forms: [function_strlit_exp, function_exp],
};

let functions_triv = {
  id: FunctionExp(Triv),
  forms: [function_triv_exp, function_exp],
};

let functions_listnil = {
  id: FunctionExp(ListNil),
  forms: [function_listnil_exp, function_exp],
};

let functions_listlit = {
  id: FunctionExp(ListLit),
  forms: [function_listlit_exp, function_exp],
};

let functions_cons = {
  id: FunctionExp(ListCons),
  forms: [function_cons_exp, function_exp],
};

let functions_var = {
  id: FunctionExp(Var),
  forms: [function_var_exp, function_exp],
};

let functions_tuplabel = {
  id: FunctionExp(TupLabel),
  forms: [function_label_exp, function_exp],
};

let functions_tuple = {
  id: FunctionExp(Tuple),
  forms: [function_tuple_exp, function_exp],
};

let functions_tuple2 = {
  id: FunctionExp(Tuple2),
  forms: [function_tuple2_exp, function_tuple_exp, function_exp],
};

let functions_tuple3 = {
  id: FunctionExp(Tuple3),
  forms: [function_tuple3_exp, function_tuple_exp, function_exp],
};

let functions_ctr = {
  id: FunctionExp(Ctr),
  forms: [function_ctr_exp, function_exp],
};

let functions_ap = {
  id: FunctionExp(Ap),
  forms: [function_ap_exp, function_exp],
};

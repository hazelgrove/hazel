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
let sintlit_fun_ex = {
  sub_id: Fun(SIntLit),
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
let tuplabel_fun_ex = {
  sub_id: Fun(TupLabel),
  term: mk_example("(fun x=y, y=z -> y)\n(1, 2)"),
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
let pat_body_function_exp_coloring_ids =
    (sf_pat_id: Id.t, sf_body_id: Id.t, ~pat_id: Id.t, ~body_id: Id.t)
    : list((Id.t, Id.t)) => {
  [(sf_pat_id, pat_id), (sf_body_id, body_id)];
};
let p = pat("p");
let e = exp("e");
let function_exp_coloring_ids =
  pat_body_function_exp_coloring_ids(Piece.id(p), Piece.id(e));
let function_exp_id: form_id = FunctionExp(Base);
let function_exp_form = [mk_fun([[space(), p, space()]]), space(), e];
let function_exp_explanation = (~pat_id: Id.t, ~body_id: Id.t): string =>
  Printf.sprintf(
    "When applied to an argument that matches the [*argument pattern*](%s), evaluates to the function [*body*](%s).",
    Id.to_string(pat_id),
    Id.to_string(body_id),
  );
let function_exp = (~pat_id: Id.t, ~body_id: Id.t): form => {
  id: function_exp_id,
  syntactic_form: function_exp_form,
  colorings: function_exp_coloring_ids(~pat_id, ~body_id),
  expandable_id: Some((Piece.id(p), [pat("p")])),
  explanation: function_exp_explanation(~pat_id, ~body_id),
  examples: [basic_fun_ex] // TODO What other examples should be here
};

let p =
  Piece.Grout({
    id: Id.mk(),
    shape: Convex,
  });
let e = exp("e");
let function_empty_hole_exp_coloring_ids =
  pat_body_function_exp_coloring_ids(Piece.id(p), Piece.id(e));
let function_empty_hole_exp_id: form_id = FunctionExp(EmptyHole);
let function_empty_hole_exp_form = [
  mk_fun([[space(), p, space()]]),
  space(),
  e,
];
let function_empty_hole_exp_expandable =
  Piece.Grout({
    id: Id.mk(),
    shape: Convex,
  });
let function_empty_hole_exp = (~pat_id: Id.t, ~body_id: Id.t): form => {
  id: function_empty_hole_exp_id,
  syntactic_form: function_empty_hole_exp_form,
  colorings: function_empty_hole_exp_coloring_ids(~pat_id, ~body_id),
  expandable_id: Some((Piece.id(p), [function_empty_hole_exp_expandable])),
  explanation:
    Printf.sprintf(
      "When applied to an argument that matches the [*argument pattern*](%s), evaluates to the function [*body*](%s), after the [empty hole pattern](%s) is filled.",
      Id.to_string(pat_id),
      Id.to_string(body_id),
      Id.to_string(pat_id),
    ),
  examples: [basic_fun_ex],
};
let p = pat("INVALID");
let e = exp("e");
let function_multi_hole_exp_coloring_ids =
  pat_body_function_exp_coloring_ids(Piece.id(p), Piece.id(e));
let function_multi_hole_exp_id: form_id = FunctionExp(MultiHole);
let function_multi_hole_exp_form = [
  mk_fun([[space(), p, space()]]),
  space(),
  e,
];
let function_multi_hole_exp = (~pat_id: Id.t, ~body_id: Id.t): form => {
  id: function_multi_hole_exp_id,
  syntactic_form: function_multi_hole_exp_form,
  colorings: function_multi_hole_exp_coloring_ids(~pat_id, ~body_id),
  expandable_id: Some((Piece.id(p), [pat("INVALID")])),
  explanation:
    Printf.sprintf(
      "When applied to an argument that matches the [*argument pattern*](%s), evaluates to the function [*body*](%s), after the [invalid argument pattern](%s) is corrected.",
      Id.to_string(pat_id),
      Id.to_string(body_id),
      Id.to_string(pat_id),
    ),
  examples: [basic_fun_ex],
};
let e = exp("e");
let function_wild_exp_coloring_ids = (~body_id: Id.t): list((Id.t, Id.t)) => {
  [(Piece.id(e), body_id)];
};
let function_wild_exp_id: form_id = FunctionExp(Wild);
let p = pat("_");
let function_wild_exp_form = [mk_fun([[space(), p, space()]]), space(), e];
let function_wild_exp = (~body_id: Id.t): form => {
  id: function_wild_exp_id,
  syntactic_form: function_wild_exp_form,
  colorings: function_wild_exp_coloring_ids(~body_id),
  expandable_id: Some((Piece.id(p), [pat("_")])),
  explanation:
    Printf.sprintf(
      "When applied to an argument that is ignored, evaluates to the function [*body*](%s).",
      Id.to_string(body_id),
    ),
  examples: [wild_fun_ex],
};
let p = pat("IntLit");
let e = exp("e");
let function_intlit_exp_coloring_ids =
  pat_body_function_exp_coloring_ids(Piece.id(p), Piece.id(e));
let function_intlit_exp_id: form_id = FunctionExp(Int);
let function_intlit_exp_form = [
  mk_fun([[space(), p, space()]]),
  space(),
  e,
];
let function_intlit_exp = (~pat_id: Id.t, ~body_id: Id.t, ~i: Bigint.t): form => {
  id: function_intlit_exp_id,
  syntactic_form: function_intlit_exp_form,
  colorings: function_intlit_exp_coloring_ids(~pat_id, ~body_id),
  expandable_id: Some((Piece.id(p), [pat("IntLit")])),
  explanation:
    Printf.sprintf(
      "The only value that matches the [*argument pattern*](%s) is `%s`. When applied to an argument which matches the [*argument pattern*](%s), evaluates to the function [*body*](%s).",
      Id.to_string(pat_id),
      Bigint.to_string(i),
      Id.to_string(pat_id),
      Id.to_string(body_id),
    ),
  examples: [intlit_fun_ex],
};
let p = pat("SIntLit");
let e = exp("e");
let function_sintlit_exp_coloring_ids =
  pat_body_function_exp_coloring_ids(Piece.id(p), Piece.id(e));
let function_sintlit_exp_id: form_id = FunctionExp(SInt);
let function_sintlit_exp_form = [
  mk_fun([[space(), p, space()]]),
  space(),
  e,
];
let function_sintlit_exp = (~pat_id: Id.t, ~body_id: Id.t, ~i: int): form => {
  id: function_sintlit_exp_id,
  syntactic_form: function_sintlit_exp_form,
  colorings: function_sintlit_exp_coloring_ids(~pat_id, ~body_id),
  expandable_id: Some((Piece.id(p), [pat("SIntLit")])),
  explanation:
    Printf.sprintf(
      "The only value that matches the [*argument pattern*](%s) is `%d`. When applied to an argument which matches the [*argument pattern*](%s), evaluates to the function [*body*](%s).",
      Id.to_string(pat_id),
      i,
      Id.to_string(pat_id),
      Id.to_string(body_id),
    ),
  examples: [sintlit_fun_ex],
};
let p = pat("FloatLit");
let e = exp("e");
let function_floatlit_exp_coloring_ids =
  pat_body_function_exp_coloring_ids(Piece.id(p), Piece.id(e));
let function_floatlit_exp_id: form_id = FunctionExp(Float);
// TODO print out the float literal nicer
let function_floatlit_exp_form = [
  mk_fun([[space(), p, space()]]),
  space(),
  e,
];
let function_floatlit_exp = (~pat_id: Id.t, ~body_id: Id.t, ~f: float): form => {
  id: function_floatlit_exp_id,
  syntactic_form: function_floatlit_exp_form,
  colorings: function_floatlit_exp_coloring_ids(~pat_id, ~body_id),
  expandable_id: Some((Piece.id(p), [pat("FloatLit")])),
  explanation:
    Printf.sprintf(
      "The only value that matches the [*argument pattern*](%s) is `%f`. When applied to an argument which matches the [*argument pattern*](%s), evaluates to the function [*body*](%s).",
      Id.to_string(pat_id),
      f,
      Id.to_string(pat_id),
      Id.to_string(body_id),
    ),
  examples: [floatlit_fun_ex],
};
let p = pat("BoolLit");
let e = exp("e");
let function_boollit_exp_coloring_ids =
  pat_body_function_exp_coloring_ids(Piece.id(p), Piece.id(e));
let function_boollit_exp_id: form_id = FunctionExp(Bool);
let function_boollit_exp_form = [
  mk_fun([[space(), p, space()]]),
  space(),
  e,
];
let function_boollit_exp = (~pat_id: Id.t, ~body_id: Id.t, ~b: bool): form => {
  id: function_boollit_exp_id,
  syntactic_form: function_boollit_exp_form,
  colorings: function_boollit_exp_coloring_ids(~pat_id, ~body_id),
  expandable_id: Some((Piece.id(p), [pat("BoolLit")])),
  explanation:
    Printf.sprintf(
      "The only value that matches the [*argument pattern*](%s) is `%b`. When applied to an argument which matches the [*argument pattern*](%s), evaluates to the function [*body*](%s).",
      Id.to_string(pat_id),
      b,
      Id.to_string(pat_id),
      Id.to_string(body_id),
    ),
  examples: [boollit_fun_ex],
};

let p = pat("StringLit");
let e = exp("e");
let function_strlit_exp_coloring_ids =
  pat_body_function_exp_coloring_ids(Piece.id(p), Piece.id(e));
let function_strlit_exp_id: form_id = FunctionExp(String);
let function_strlit_exp_form = [
  mk_fun([[space(), p, space()]]),
  space(),
  e,
];
let function_strlit_exp = (~pat_id: Id.t, ~body_id: Id.t, ~s: string): form => {
  id: function_strlit_exp_id,
  syntactic_form: function_strlit_exp_form,
  colorings: function_strlit_exp_coloring_ids(~pat_id, ~body_id),
  expandable_id: Some((Piece.id(p), [pat("StringLit")])),
  explanation:
    Printf.sprintf(
      "The only value that matches the [*argument pattern*](%s) is `%s`. When applied to an argument which matches the [*argument pattern*](%s), evaluates to the function [*body*](%s).",
      Id.to_string(pat_id),
      s,
      Id.to_string(pat_id),
      Id.to_string(body_id),
    ),
  examples: [strlit_fun_ex],
};
let p = pat("()");
let e = exp("e");
let function_triv_exp_coloring_ids =
  pat_body_function_exp_coloring_ids(Piece.id(p), Piece.id(e));
let function_triv_exp_id: form_id = FunctionExp(Triv);
let function_triv_exp_form = [mk_fun([[space(), p, space()]]), space(), e];
let function_triv_exp = (~pat_id: Id.t, ~body_id: Id.t): form => {
  id: function_triv_exp_id,
  syntactic_form: function_triv_exp_form,
  colorings: function_triv_exp_coloring_ids(~pat_id, ~body_id),
  expandable_id: Some((Piece.id(p), [pat("()")])),
  explanation:
    Printf.sprintf(
      "The only value that matches the [*argument pattern*](%s) is the trivial value `()`. When applied to an argument which matches the [*argument pattern*](%s), evaluates to the function [*body*](%s). This is functionally equivalent to a zero argument function.",
      Id.to_string(pat_id),
      Id.to_string(pat_id),
      Id.to_string(body_id),
    ),
  examples: [triv_fun_ex],
};
let p = pat("[]");
let e = exp("e");
let function_listnil_exp_coloring_ids =
  pat_body_function_exp_coloring_ids(Piece.id(p), Piece.id(e));
let function_listnil_exp_id: form_id = FunctionExp(ListNil);
let function_listnil_exp_form = [
  mk_fun([[space(), p, space()]]),
  space(),
  e,
];
let function_listnil_exp = (~pat_id: Id.t, ~body_id: Id.t): form => {
  id: function_listnil_exp_id,
  syntactic_form: function_listnil_exp_form,
  colorings: function_listnil_exp_coloring_ids(~pat_id, ~body_id),
  expandable_id: Some((Piece.id(p), [pat("[]")])),
  explanation:
    Printf.sprintf(
      "The only value that matches the [*argument pattern*](%s) is the empty list `[]`. When applied to an argument which matches the [*argument pattern*](%s), evaluates to the function [*body*](%s).",
      Id.to_string(pat_id),
      Id.to_string(pat_id),
      Id.to_string(body_id),
    ),
  examples: [listnil_fun_ex],
};
let p = mk_list_pat([[pat("p1"), comma_pat(), space(), pat("...")]]);
let e = exp("e");
let function_listlit_exp_coloring_ids =
  pat_body_function_exp_coloring_ids(Piece.id(p), Piece.id(e));
let function_listlit_exp_id: form_id = FunctionExp(ListLit);
let function_listlit_exp_form = [
  mk_fun([[space(), p, space()]]),
  space(),
  e,
];
let function_listlit_exp_expandable =
  mk_list_pat([[pat("p1"), comma_pat(), pat("...")]]);
let function_listlit_exp = (~pat_id: Id.t, ~body_id: Id.t, ~n: int): form => {
  id: function_listlit_exp_id,
  syntactic_form: function_listlit_exp_form,
  colorings: function_listlit_exp_coloring_ids(~pat_id, ~body_id),
  expandable_id: Some((Piece.id(p), [function_listlit_exp_expandable])),
  explanation:
    Printf.sprintf(
      "The only values that match the [*argument pattern*](%s) are lists with %d-elements, each matching the corresponding element pattern. When applied to an argument which matches the [*argument pattern*](%s), evaluates to the function [*body*](%s).",
      Id.to_string(pat_id),
      n,
      Id.to_string(pat_id),
      Id.to_string(body_id),
    ),
  examples: [listnil_fun_ex, listlit_fun_ex],
};
let pat_hd = pat("p_hd");
let pat_tl = pat("p_tl");
let e = exp("e");
let function_cons_exp_coloring_ids =
    (~hd_id: Id.t, ~tl_id: Id.t, ~body_id: Id.t): list((Id.t, Id.t)) => {
  [
    (Piece.id(pat_hd), hd_id),
    (Piece.id(pat_tl), tl_id),
    (Piece.id(e), body_id),
  ];
};
let function_cons_exp_id: form_id = FunctionExp(ListCons);
let function_cons_exp_cons = cons_pat();
let function_cons_exp_form = [
  mk_fun([[space(), pat_hd, function_cons_exp_cons, pat_tl, space()]]),
  space(),
  e,
];
let function_cons_exp = (~hd_id: Id.t, ~tl_id: Id.t, ~body_id: Id.t): form => {
  id: function_cons_exp_id,
  syntactic_form: function_cons_exp_form,
  colorings: function_cons_exp_coloring_ids(~hd_id, ~tl_id, ~body_id),
  expandable_id:
    Some((
      Piece.id(function_cons_exp_cons),
      [pat("p_hd"), cons_pat(), pat("p_tl")],
    )),
  explanation:
    Printf.sprintf(
      "The only values that match the *argument pattern* are non-empty lists that match the [*head pattern*](%s) and [*tail pattern*](%s). When applied to an argument which matches the *argument pattern*, evaluates to the function [*body*](%s).",
      Id.to_string(hd_id),
      Id.to_string(tl_id),
      Id.to_string(body_id),
    ),
  examples: [cons_hd_fun_ex, cons_snd_fun_ex],
};
let p = pat("x");
let e = exp("e");
let function_var_exp_coloring_ids =
  pat_body_function_exp_coloring_ids(Piece.id(p), Piece.id(e));
let function_var_exp_id: form_id = FunctionExp(Var);
let function_var_exp_form = [mk_fun([[space(), p, space()]]), space(), e];
let function_var_exp = (~pat_id: Id.t, ~body_id: Id.t, ~name: string): form => {
  id: function_var_exp_id,
  syntactic_form: function_var_exp_form,
  colorings: function_var_exp_coloring_ids(~pat_id, ~body_id),
  expandable_id: Some((Piece.id(p), [pat("x")])),
  explanation:
    Printf.sprintf(
      "When applied to an argument which is bound to the [*variable*](%s) `%s`, evaluates to the function [*body*](%s).",
      Id.to_string(pat_id),
      name,
      Id.to_string(body_id),
    ),
  examples: [basic_fun_ex, var_incr_fun_ex, var_and_fun_ex],
};

let lp' = labeled_pat();
let exp' = exp("e");
let label = pat("x");
let pat' = pat("y");
let function_labeled_exp_coloring_ids =
    (~label_id: Id.t, ~pat_id: Id.t, ~body_id: Id.t): list((Id.t, Id.t)) => {
  [
    (Piece.id(label), label_id),
    (Piece.id(pat'), pat_id),
    (Piece.id(exp'), body_id),
  ];
};

let function_labeled_exp_id: form_id = FunctionExp(TupLabel);
let function_labeled_exp_form = [
  mk_fun([[space(), label, lp', pat', space()]]),
  space(),
  exp',
];
let function_labeled_exp =
    (~label_id: Id.t, ~pat_id: Id.t, ~body_id: Id.t): form => {
  id: function_labeled_exp_id,
  syntactic_form: function_labeled_exp_form,
  colorings: function_labeled_exp_coloring_ids(~label_id, ~pat_id, ~body_id),
  expandable_id:
    Some((Piece.id(lp'), [pat("x"), labeled_pat(), pat("y")])),
  explanation:
    Printf.sprintf(
      "A function with one [*labeled argument*]. Only labeled arguments that match the [*label*](%s) 'x' are accepted, and are bound to the [*parameter*](%s) 'y' in the function [*body*](%s).",
      Id.to_string(label_id),
      Id.to_string(pat_id),
      Id.to_string(body_id),
    ),
  examples: [tuplabel_fun_ex],
};
let comma = comma_pat();
let e = exp("e");
let function_tuple_exp_coloring_ids =
  pat_body_function_exp_coloring_ids(Piece.id(comma), Piece.id(e));
let function_tuple_exp_id: form_id = FunctionExp(Tuple);
let function_tuple_exp_form = [
  mk_fun([[space(), pat("p1"), comma, space(), pat("..."), space()]]),
  space(),
  e,
];
let function_tuple_exp = (~pat_id: Id.t, ~body_id: Id.t, ~n: int): form => {
  id: function_tuple_exp_id,
  syntactic_form: function_tuple_exp_form,
  colorings: function_tuple_exp_coloring_ids(~pat_id, ~body_id),
  expandable_id:
    Some((Piece.id(comma), [pat("p1"), comma_pat(), pat("...")])),
  explanation:
    Printf.sprintf(
      "The only values that match the [*argument pattern*](%s) are %d-tuples where each element matches the corresponding argument element pattern. When applied to an argument which matches the [*argument pattern*](%s), evaluates to the function [*body*](%s).",
      Id.to_string(pat_id),
      n,
      Id.to_string(pat_id),
      Id.to_string(body_id),
    ),
  examples: [tuple2_fun_ex, tuple3_fun_ex],
};
let pat1 = pat("p1");
let pat2 = pat("p2");
let e = exp("e");
let function_tuple2_exp_coloring_ids =
    (~pat1_id: Id.t, ~pat2_id: Id.t, ~body_id: Id.t): list((Id.t, Id.t)) => {
  [
    (Piece.id(pat1), pat1_id),
    (Piece.id(pat2), pat2_id),
    (Piece.id(e), body_id),
  ];
};
let function_tuple2_exp_id: form_id = FunctionExp(Tuple2);
let function_tuple2_exp_comma = comma_pat();
let function_tuple2_exp_form = [
  mk_fun([
    [space(), pat1, function_tuple2_exp_comma, space(), pat2, space()],
  ]),
  space(),
  e,
];
let function_tuple2_exp =
    (~pat1_id: Id.t, ~pat2_id: Id.t, ~body_id: Id.t): form => {
  id: function_tuple2_exp_id,
  syntactic_form: function_tuple2_exp_form,
  colorings: function_tuple2_exp_coloring_ids(~pat1_id, ~pat2_id, ~body_id),
  expandable_id:
    Some((
      Piece.id(function_tuple2_exp_comma),
      [pat("p1"), comma_pat(), pat("p2")],
    )),
  explanation:
    Printf.sprintf(
      "The only values that match the *argument pattern* are 2-tuples where the first element matches the [*first element pattern*](%s) and the second element matches the [*second element pattern*](%s). When applied to an argument which matches the *argument pattern*, evaluates to the function [*body*](%s).",
      Id.to_string(pat1_id),
      Id.to_string(pat2_id),
      Id.to_string(body_id),
    ),
  examples: [tuple2_fun_ex],
};
let pat1 = pat("p1");
let pat2 = pat("p2");
let pat3 = pat("p3");
let e = exp("e");
let function_tuple3_exp_coloring_ids =
    (~pat1_id: Id.t, ~pat2_id: Id.t, ~pat3_id: Id.t, ~body_id: Id.t)
    : list((Id.t, Id.t)) => {
  [
    (Piece.id(pat1), pat1_id),
    (Piece.id(pat2), pat2_id),
    (Piece.id(pat3), pat3_id),
    (Piece.id(e), body_id),
  ];
};
let function_tuple3_exp_id: form_id = FunctionExp(Tuple3);
let function_tuple3_exp_comma = comma_pat();
let function_tuple3_exp_form = [
  mk_fun([
    [
      space(),
      pat1,
      comma_pat(),
      space(),
      pat2,
      function_tuple3_exp_comma,
      space(),
      pat3,
      space(),
    ],
  ]),
  space(),
  e,
];
let function_tuple3_exp =
    (~pat1_id: Id.t, ~pat2_id: Id.t, ~pat3_id: Id.t, ~body_id: Id.t): form => {
  id: function_tuple3_exp_id,
  syntactic_form: function_tuple3_exp_form,
  colorings:
    function_tuple3_exp_coloring_ids(~pat1_id, ~pat2_id, ~pat3_id, ~body_id),
  expandable_id:
    Some((
      Piece.id(function_tuple3_exp_comma),
      [pat("p1"), comma_pat(), pat("p2"), comma_pat(), pat("p3")],
    )),
  explanation:
    Printf.sprintf(
      "The only values that match the *argument pattern* are 3-tuples where the first element matches the [*first element pattern*](%s), the second element matches the [*second element pattern*](%s), and the third element matches the [*third element pattern*](%s). When applied to an argument which matches the *argument pattern*, evaluates to the function [*body*](%s).",
      Id.to_string(pat1_id),
      Id.to_string(pat2_id),
      Id.to_string(pat3_id),
      Id.to_string(body_id),
    ),
  examples: [tuple3_fun_ex],
};
let p = pat("C");
let e = exp("e");
let function_ctr_exp_coloring_ids =
  pat_body_function_exp_coloring_ids(Piece.id(p), Piece.id(e));
let function_ctr_exp_id: form_id = FunctionExp(Ctr);
let function_ctr_exp_form = [mk_fun([[space(), p, space()]]), space(), e];
let function_ctr_exp = (~pat_id: Id.t, ~body_id: Id.t, ~name: string): form => {
  id: function_ctr_exp_id,
  syntactic_form: function_ctr_exp_form,
  colorings: function_ctr_exp_coloring_ids(~pat_id, ~body_id),
  expandable_id: Some((Piece.id(p), [pat("C")])),
  explanation:
    Printf.sprintf(
      "The only value that matches the [*argument pattern*](%s) is the *`%s` constructor*. When applied to an argument which matches the [*argument pattern*](%s), evaluates to the function [*body*](%s).",
      Id.to_string(pat_id),
      name,
      Id.to_string(pat_id),
      Id.to_string(body_id),
    ),
  examples: [ctr_fun_ex],
};
let pat_con = pat("p_con");
let pat_arg = pat("p_arg");
let e = exp("e");
let function_ap_exp_coloring_ids =
    (~con_id: Id.t, ~arg_id: Id.t, ~body_id: Id.t): list((Id.t, Id.t)) => {
  [
    (Piece.id(pat_con), con_id),
    (Piece.id(pat_arg), arg_id),
    (Piece.id(e), body_id),
  ];
};
let function_ap_exp_id: form_id = FunctionExp(ApFunc);
let function_ap_exp_ap = mk_ap_pat([[pat_arg]]);
let function_ap_exp_form = [
  mk_fun([[space(), pat_con, function_ap_exp_ap, space()]]),
  space(),
  e,
];
let function_ap_exp = (~con_id: Id.t, ~arg_id: Id.t, ~body_id: Id.t): form => {
  id: function_ap_exp_id,
  syntactic_form: function_ap_exp_form,
  colorings: function_ap_exp_coloring_ids(~con_id, ~arg_id, ~body_id),
  expandable_id:
    Some((
      Piece.id(function_ap_exp_ap),
      [pat("p_con"), mk_ap_pat([[pat("p_arg")]])],
    )),
  explanation:
    Printf.sprintf(
      "The only values that match the *argument pattern* are the [*constructor*](%s) where the *constructor argument* matches the [*constructor argument pattern*](%s). When applied to an argument which matches the *argument pattern*, evaluates to the function [*body*](%s).",
      Id.to_string(con_id),
      Id.to_string(arg_id),
      Id.to_string(body_id),
    ),
  examples: [ap_fun_ex],
};

let functions = (~pat_id: Id.t, ~body_id: Id.t): group => {
  id: FunctionExp(Base),
  forms: [function_exp(~pat_id, ~body_id)],
};
let functions_empty_hole = (~pat_id: Id.t, ~body_id: Id.t): group => {
  id: FunctionExp(EmptyHole),
  forms: [
    function_empty_hole_exp(~pat_id, ~body_id),
    function_exp(~pat_id, ~body_id),
  ],
};
let functions_multi_hole = (~pat_id: Id.t, ~body_id: Id.t): group => {
  id: FunctionExp(MultiHole),
  forms: [
    function_multi_hole_exp(~pat_id, ~body_id),
    function_exp(~pat_id, ~body_id),
  ],
};
let functions_wild = (~pat_id: Id.t, ~body_id: Id.t): group => {
  id: FunctionExp(Wild),
  forms: [function_wild_exp(~body_id), function_exp(~pat_id, ~body_id)],
};
let functions_int = (~pat_id: Id.t, ~body_id: Id.t, ~i: Bigint.t): group => {
  id: FunctionExp(Int),
  forms: [
    function_intlit_exp(~pat_id, ~body_id, ~i),
    function_exp(~pat_id, ~body_id),
  ],
};
let functions_sint = (~pat_id: Id.t, ~body_id: Id.t, ~i: int): group => {
  id: FunctionExp(SInt),
  forms: [
    function_sintlit_exp(~pat_id, ~body_id, ~i),
    function_exp(~pat_id, ~body_id),
  ],
};
let functions_float = (~pat_id: Id.t, ~body_id: Id.t, ~f: float): group => {
  id: FunctionExp(Float),
  forms: [
    function_floatlit_exp(~pat_id, ~body_id, ~f),
    function_exp(~pat_id, ~body_id),
  ],
};
let functions_bool = (~pat_id: Id.t, ~body_id: Id.t, ~b: bool): group => {
  id: FunctionExp(Bool),
  forms: [
    function_boollit_exp(~pat_id, ~body_id, ~b),
    function_exp(~pat_id, ~body_id),
  ],
};

let functions_str = (~pat_id: Id.t, ~body_id: Id.t, ~s: string): group => {
  id: FunctionExp(String),
  forms: [
    function_strlit_exp(~pat_id, ~body_id, ~s),
    function_exp(~pat_id, ~body_id),
  ],
};

let functions_triv = (~pat_id: Id.t, ~body_id: Id.t): group => {
  id: FunctionExp(Triv),
  forms: [
    function_triv_exp(~pat_id, ~body_id),
    function_exp(~pat_id, ~body_id),
  ],
};

let functions_listnil = (~pat_id: Id.t, ~body_id: Id.t): group => {
  id: FunctionExp(ListNil),
  forms: [
    function_listnil_exp(~pat_id, ~body_id),
    function_exp(~pat_id, ~body_id),
  ],
};

let functions_listlit = (~pat_id: Id.t, ~body_id: Id.t, ~n: int): group => {
  id: FunctionExp(ListLit),
  forms: [
    function_listlit_exp(~pat_id, ~body_id, ~n),
    function_exp(~pat_id, ~body_id),
  ],
};

let functions_cons =
    (~hd_id: Id.t, ~tl_id: Id.t, ~pat_id: Id.t, ~body_id: Id.t): group => {
  id: FunctionExp(ListCons),
  forms: [
    function_cons_exp(~hd_id, ~tl_id, ~body_id),
    function_exp(~pat_id, ~body_id),
  ],
};

let functions_var = (~pat_id: Id.t, ~body_id: Id.t, ~name: string): group => {
  id: FunctionExp(Var),
  forms: [
    function_var_exp(~pat_id, ~body_id, ~name),
    function_exp(~pat_id, ~body_id),
  ],
};

let functions_tuplabel =
    (~label_id: Id.t, ~label_pat_id: Id.t, ~pat_id: Id.t, ~body_id: Id.t)
    : group => {
  id: FunctionExp(TupLabel),
  forms: [
    function_labeled_exp(~label_id, ~pat_id=label_pat_id, ~body_id),
    function_exp(~pat_id, ~body_id),
  ],
};

let functions_tuple = (~pat_id: Id.t, ~body_id: Id.t, ~n: int): group => {
  id: FunctionExp(Tuple),
  forms: [
    function_tuple_exp(~pat_id, ~body_id, ~n),
    function_exp(~pat_id, ~body_id),
  ],
};

let functions_tuple2 =
    (~pat1_id: Id.t, ~pat2_id: Id.t, ~pat_id: Id.t, ~body_id: Id.t, ~n: int)
    : group => {
  id: FunctionExp(Tuple2),
  forms: [
    function_tuple2_exp(~pat1_id, ~pat2_id, ~body_id),
    function_tuple_exp(~pat_id, ~body_id, ~n),
    function_exp(~pat_id, ~body_id),
  ],
};

let functions_tuple3 =
    (
      ~pat1_id: Id.t,
      ~pat2_id: Id.t,
      ~pat3_id: Id.t,
      ~pat_id: Id.t,
      ~body_id: Id.t,
      ~n: int,
    )
    : group => {
  id: FunctionExp(Tuple3),
  forms: [
    function_tuple3_exp(~pat1_id, ~pat2_id, ~pat3_id, ~body_id),
    function_tuple_exp(~pat_id, ~body_id, ~n),
    function_exp(~pat_id, ~body_id),
  ],
};

let functions_ctr = (~pat_id: Id.t, ~body_id: Id.t, ~name: string): group => {
  id: FunctionExp(Ctr),
  forms: [
    function_ctr_exp(~pat_id, ~body_id, ~name),
    function_exp(~pat_id, ~body_id),
  ],
};

let functions_ap =
    (~con_id: Id.t, ~arg_id: Id.t, ~pat_id: Id.t, ~body_id: Id.t): group => {
  id: FunctionExp(ApFunc),
  forms: [
    function_ap_exp(~con_id, ~arg_id, ~body_id),
    function_exp(~pat_id, ~body_id),
  ],
};

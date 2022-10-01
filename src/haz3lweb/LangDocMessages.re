open Sexplib.Std;
open Haz3lcore;

[@deriving (show({with_path: false}), sexp, yojson)]
type feedback_option =
  | ThumbsUp
  | ThumbsDown
  | Unselected;

[@deriving (show({with_path: false}), sexp, yojson)]
type example = {
  sub_id: string,
  term: Segment.t,
  message: string,
  feedback: feedback_option,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type explanation = {
  message: string,
  feedback: feedback_option,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type form = {
  id: string,
  syntactic_form: Segment.t,
  expandable_id: option(Id.t),
  explanation,
  examples: list(example),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type form_group = {
  options: list((string, Segment.t)),
  current_selection: int,
};

// TODO Make sure using this for all the forms that should
let cons_exp = () => Example.mk_monotile(Form.get("cons_exp"));
let cons_pat = () => Example.mk_monotile(Form.get("cons_pat"));
let seq = () => Example.mk_monotile(Form.get("cell-join"));
let exp = v =>
  Example.mk_monotile(Form.mk(Form.ss, [v], Mold.(mk_op(Exp, []))));
let pat = v =>
  Example.mk_monotile(Form.mk(Form.ss, [v], Mold.(mk_op(Pat, []))));
let typ = t =>
  Example.mk_monotile(Form.mk(Form.ss, [t], Mold.(mk_op(Typ, []))));
let int = n => Example.mk_monotile(Form.mk_atomic(Exp, n));
let bool = b => Example.mk_monotile(Form.mk_atomic(Exp, b));
let mk_parens_exp = Example.mk_tile(Form.get("parens_exp"));
let mk_parens_pat = Example.mk_tile(Form.get("parens_pat"));
let mk_parens_typ = Example.mk_tile(Form.get("parens_typ"));
let mk_list_exp = Example.mk_tile(Form.get("list_lit_exp"));
let mk_list_pat = Example.mk_tile(Form.get("list_lit_pat"));
let mk_list_typ = Example.mk_tile(Form.get("list_typ"));
let arrow = () => Example.mk_monotile(Form.get("type-arrow"));
let unary_minus = () => Example.mk_monotile(Form.get("unary_minus"));
let plus = () => Example.mk_monotile(Form.get("plus"));
let minus = () => Example.mk_monotile(Form.get("minus"));
let times = () => Example.mk_monotile(Form.get("times"));
let divide = () => Example.mk_monotile(Form.get("divide"));
let equals = () => Example.mk_monotile(Form.get("equals"));
let lt = () => Example.mk_monotile(Form.get("lt"));
let lte = () => Example.mk_monotile(Form.get("lte"));
let gt = () => Example.mk_monotile(Form.get("gt"));
let gte = () => Example.mk_monotile(Form.get("gte"));
let fplus = () => Example.mk_monotile(Form.get("fplus"));
let fminus = () => Example.mk_monotile(Form.get("fminus"));
let ftimes = () => Example.mk_monotile(Form.get("ftimes"));
let fdivide = () => Example.mk_monotile(Form.get("fdivide"));
let fequals = () => Example.mk_monotile(Form.get("fequals"));
let flt = () => Example.mk_monotile(Form.get("flt"));
let flte = () => Example.mk_monotile(Form.get("flte"));
let fgt = () => Example.mk_monotile(Form.get("fgt"));
let fgte = () => Example.mk_monotile(Form.get("fgte"));
let sequals = () => Example.mk_monotile(Form.get("string_equals"));
let logical_and = () => Example.mk_monotile(Form.get("logical_and"));
let logical_or = () => Example.mk_monotile(Form.get("logical_or"));
let comma_exp = () => Example.mk_monotile(Form.get("comma_exp"));
let comma_pat = () => Example.mk_monotile(Form.get("comma_pat"));
let comma_typ = () => Example.mk_monotile(Form.get("comma_typ"));
let nil = () => exp("nil");
let mk_fun = Example.mk_tile(Form.get("fun_"));
let mk_ap = Example.mk_tile(Form.get("ap_exp"));
let mk_let = Example.mk_tile(Form.get("let_"));
let mk_if = Example.mk_tile(Form.get("if_"));
let mk_test = Example.mk_tile(Form.get("test"));
let mk_case = Example.mk_tile(Form.get("case"));
let mk_rule = Example.mk_tile(Form.get("rule"));

let mk_example = str => {
  switch (Printer.zipper_of_string(0, str)) {
  | None => []
  | Some((z, _)) => Zipper.zip(z)
  };
};

let empty_hole_exp_group = "empty_hole_exp_group";
let empty_hole_exp: form = {
  let explanation = {
    message: "Empty hole. This marks an expression that needs to be filled in.",
    feedback: Unselected,
  };
  {
    id: "empty_hole_exp",
    syntactic_form: [exp("EmptyHole")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let multi_hole_exp_group = "multi_hole_exp_group";
let multi_hole_exp: form = {
  let explanation = {
    message: "Not recognized. This is an invalid term.",
    feedback: Unselected,
  };
  {
    id: "multi_hole_exp",
    syntactic_form: [exp("INVALID")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let triv_exp_group = "triv_exp_group";
let triv_exp: form = {
  let explanation = {message: "Trivial expression.", feedback: Unselected};
  {
    id: "triv_exp",
    syntactic_form: [exp("Triv")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let bool_exp_group = "bool_exp_group";
let bool_exp: form = {
  let explanation = {message: "Boolean literal.", feedback: Unselected};
  {
    id: "bool_exp",
    syntactic_form: [exp("BoolLit")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let int_exp_group = "int_exp_group";
let int_exp: form = {
  let explanation = {message: "Integer literal.", feedback: Unselected};
  {
    id: "int_exp",
    syntactic_form: [exp("IntLit")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let float_exp_group = "float_exp_group";
let float_exp: form = {
  let explanation = {
    message: "Floating-point literal.",
    feedback: Unselected,
  };
  {
    id: "float_exp",
    syntactic_form: [exp("FloatLit")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let string_exp_group = "string_exp_group";
let string_exp: form = {
  let explanation = {message: "Stromg literal.", feedback: Unselected};
  {
    id: "string_exp",
    syntactic_form: [exp("StringLit")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let list_exp_group = "list_exp_group";
let list_exp: form = {
  let int_list = {
    sub_id: "int_list",
    term: mk_example("[1, 2]"),
    message: "A list with two elements, 1 and 2.",
    feedback: Unselected,
  };
  let tuple_list = {
    sub_id: "tuple_list",
    term: mk_example("[(1,true), (2,false)]"),
    message: "A list with two elements, a tuple with 1 and true and a tuple with 2 and false.",
    feedback: Unselected,
  };
  let explanation = {
    message: "List literal with %i element(s).",
    feedback: Unselected,
  };
  {
    id: "list_exp",
    syntactic_form: [
      mk_list_exp([[exp("EXP1"), comma_exp(), exp("...")]]),
    ],
    expandable_id: None,
    explanation,
    examples: [int_list, tuple_list],
  };
};

let function_group = "function_group";
let function_empty_hole_group = "function_empty_hole_group";
let function_multi_hole_group = "function_multi_hole_group";
let function_wild_group = "function_wild_group";
let function_int_group = "function_int_group";
let function_float_group = "function_float_group";
let function_bool_group = "function_bool_group";
let function_str_group = "function_str_group";
let function_triv_group = "function_triv_group";
let function_listnil_group = "function_listnil_group";
let function_listlit_group = "function_listlit_group";
let function_cons_group = "function_cons_group";
let function_var_group = "function_var_group";
let function_tuple_group = "function_tuple_group";
let function_tuple_2_group = "function_tuple_2_group";
let function_tuple_3_group = "function_tuple_3_group";
let basic_fun_ex = {
  sub_id: "basic_fun_ex",
  term: mk_example("fun x -> x"),
  message: "The identity function. When given an argument, the function evaluates to that argument.",
  feedback: Unselected,
};
let wild_fun_ex = {
  sub_id: "wild_fun_ex",
  term: mk_example("fun _ -> 3"),
  message: "When given an argument, the function throws away the supplied argument and always evaluates to 3.",
  feedback: Unselected,
};
let intlit_fun_ex = {
  sub_id: "intlit_fun_ex",
  term: mk_example("fun 1 -> 2"),
  message: "When given an argument with value 1, the function throws away the supplied argument and always evaluates to 2.",
  feedback: Unselected,
};
let floatlit_fun_ex = {
  sub_id: "floatlit_fun_ex",
  term: mk_example("fun 1.1 -> 2"),
  message: "When given an argument with value 1.1, the function throws away the supplied argument and always evaluates to 2.",
  feedback: Unselected,
};
let boollit_fun_ex = {
  sub_id: "boollit_fun_ex",
  term: mk_example("fun true -> 2"),
  message: "When given an argument with value true, the function throws away the supplied argument and always evaluates to 2.",
  feedback: Unselected,
};
let strlit_fun_ex = {
  sub_id: "strlit_fun_ex",
  term: mk_example("fun \"abc\" -> 2"),
  message: "When given an argument with value \"abc\", the function throws away the supplied argument and always evaluates to 2.",
  feedback: Unselected,
};
let triv_fun_ex = {
  sub_id: "triv_fun_ex",
  term: mk_example("fun triv -> 2"),
  message: "When given an argument with the triv value, the function throws away the supplied argument and always evaluates to 2.",
  feedback: Unselected,
};
let listnil_fun_ex = {
  sub_id: "listnil_fun_ex",
  term: mk_example("fun nil -> 2"),
  message: "When given an argument with the empty list value, the function throws away the supplied argument and always evaluates to 2.",
  feedback: Unselected,
};
let listlist_fun_ex = {
  sub_id: "listlist_fun_ex",
  term: mk_example("fun [x, y] -> x"),
  message: "When given an argument that is a list of two elements, the function evaluates to the first element of that list.",
  feedback: Unselected,
};
let cons_hd_fun_ex = {
  sub_id: "cons_hd_fun_ex",
  term: mk_example("fun hd::tl -> hd"),
  message: "When given an argument that is a non-empty list, the function evaluates to the head of that list.",
  feedback: Unselected,
};
let cons_snd_fun_ex = {
  sub_id: "cons_snd_fun_ex",
  term: mk_example("fun fst::snd::tl -> snd"),
  message: "When given an argument that is a list with at least two elements, the function evaluates to the second element of that list.",
  feedback: Unselected,
};
let var_incr_fun_ex = {
  sub_id: "var_incr_fun_ex",
  term: mk_example("fun x -> x + 1"),
  message: "When given an integer argument, the function evaluates to the argument plus 1.",
  feedback: Unselected,
};
let var_and_fun_ex = {
  sub_id: "var_and_fun_ex",
  term: mk_example("fun b -> b && true"),
  message: "When given a boolean argument, the function evaluates to the logical-and of the argument and true, which evaluates to the truth value of the argument.",
  feedback: Unselected,
};
let tuple2_fun_ex = {
  sub_id: "tuple2_fun_ex",
  term: mk_example("fun (x, y) -> x + y"),
  message: "When given a 2-tuple of integers, the function evaluates to the sum of the two integers.",
  feedback: Unselected,
};
let tuple3_fun_ex = {
  sub_id: "tuple3_fun_ex",
  term: mk_example("fun (a, b, c) -> a && b && c"),
  message: "When given a 3-tuple of booleans, the function evaluates to the logical-and of the three booleans.",
  feedback: Unselected,
};
// TODO for shared examples, should the feedback be stored separately for each "instance"?
let function_exp: form = {
  let explanation = {
    message: "Function literal. When applied to an argument that matches the [*argument pattern*](%i), evaluates to the function [*body*](%i).",
    feedback: Unselected,
  };
  let pat = pat("PAT");
  let form = [mk_fun([[pat]]), exp("EXP")];
  {
    id: "function_exp",
    syntactic_form: form,
    expandable_id: Some(Piece.id(pat)),
    explanation,
    examples: [basic_fun_ex] // TODO What other examples should be here
  };
};
let function_empty_hole_exp: form = {
  let explanation = {
    message: "Function literal. When applied to an argument that matches the [*argument pattern*](%i), evaluates to the function [*body*](%i), after the [empty hole pattern](%i) is filled.",
    feedback: Unselected,
  };
  let pat = pat("EmptyHole");
  let form = [mk_fun([[pat]]), exp("EXP")];
  {
    id: "function_empty_hole_exp",
    syntactic_form: form,
    expandable_id: Some(Piece.id(pat)),
    explanation,
    examples: [basic_fun_ex],
  };
};
let function_multi_hole_exp: form = {
  let explanation = {
    message: "Function literal. When applied to an argument that matches the [*argument pattern*](%i), evaluates to the function [*body*](%i), after the [invalid argument pattern](%i) is corrected.",
    feedback: Unselected,
  };
  let pat = pat("INVALID");
  let form = [mk_fun([[pat]]), exp("EXP")];
  {
    id: "function_multi_hole_exp",
    syntactic_form: form,
    expandable_id: Some(Piece.id(pat)),
    explanation,
    examples: [basic_fun_ex],
  };
};
let function_wild_exp: form = {
  let explanation = {
    message: "Function literal. When applied to an argument that is ignored, evaluates to the function [*body*](%i).",
    feedback: Unselected,
  };
  let pat = pat("_");
  let form = [mk_fun([[pat]]), exp("EXP")];
  {
    id: "function_wild_exp",
    syntactic_form: form,
    expandable_id: Some(Piece.id(pat)),
    explanation,
    examples: [wild_fun_ex],
  };
};
let function_intlit_exp: form = {
  let explanation = {
    message: "Function literal. The only value that matches the [*argument pattern*](%i) is `%i`. When applied to an argument which matches the [*argument pattern*](%i), evaluates to the function [*body*](%i).",
    feedback: Unselected,
  };
  let pat = pat("IntLit");
  let form = [mk_fun([[pat]]), exp("EXP")];
  {
    id: "function_intlit_exp",
    syntactic_form: form,
    expandable_id: Some(Piece.id(pat)),
    explanation,
    examples: [intlit_fun_ex],
  };
};
let function_floatlit_exp: form = {
  let explanation = {
    message: "Function literal. The only value that matches the [*argument pattern*](%i) is `%f`. When applied to an argument which matches the [*argument pattern*](%i), evaluates to the function [*body*](%i).",
    feedback: Unselected,
  };
  let pat = pat("FloatLit");
  // TODO print out the float literal nicer
  let form = [mk_fun([[pat]]), exp("EXP")];
  {
    id: "function_floatlit_exp",
    syntactic_form: form,
    expandable_id: Some(Piece.id(pat)),
    explanation,
    examples: [floatlit_fun_ex],
  };
};
let function_boollit_exp: form = {
  let explanation = {
    message: "Function literal. The only value that matches the [*argument pattern*](%i) is `%b`. When applied to an argument which matches the [*argument pattern*](%i), evaluates to the function [*body*](%i).",
    feedback: Unselected,
  };
  let pat = pat("BoolLit");
  let form = [mk_fun([[pat]]), exp("EXP")];
  {
    id: "function_boollit_exp",
    syntactic_form: form,
    expandable_id: Some(Piece.id(pat)),
    explanation,
    examples: [boollit_fun_ex],
  };
};
let function_strlit_exp: form = {
  let explanation = {
    message: "Function literal. The only value that matches the [*argument pattern*](%i) is `%s`. When applied to an argument which matches the [*argument pattern*](%i), evaluates to the function [*body*](%i).",
    feedback: Unselected,
  };
  let pat = pat("StringLit");
  let form = [mk_fun([[pat]]), exp("EXP")];
  {
    id: "function_strlit_exp",
    syntactic_form: form,
    expandable_id: Some(Piece.id(pat)),
    explanation,
    examples: [strlit_fun_ex],
  };
};
let function_triv_exp: form = {
  let explanation = {
    message: "Function literal. The only value that matches the [*argument pattern*](%i) is the trivial value `triv`. When applied to an argument which matches the [*argument pattern*](%i), evaluates to the function [*body*](%i). This if functionally equivalent to a zero argument function.",
    feedback: Unselected,
  };
  let pat = pat("triv");
  let form = [mk_fun([[pat]]), exp("EXP")];
  {
    id: "function_triv_exp",
    syntactic_form: form,
    expandable_id: Some(Piece.id(pat)),
    explanation,
    examples: [triv_fun_ex],
  };
};
let function_listnil_exp: form = {
  let explanation = {
    message: "Function literal. The only value that matches the [*argument pattern*](%i) is the empty list `nil`. When applied to an argument which matches the [*argument pattern*](%i), evaluates to the function [*body*](%i).",
    feedback: Unselected,
  };
  let pat = pat("nil");
  let form = [mk_fun([[pat]]), exp("EXP")];
  {
    id: "function_listnil_exp",
    syntactic_form: form,
    expandable_id: Some(Piece.id(pat)),
    explanation,
    examples: [listnil_fun_ex],
  };
};
let function_listlit_exp: form = {
  let explanation = {
    message: "Function literal. The only values that match the [*argument pattern*](%i) are lists with %n-elements, each matching the corresponding element pattern. When applied to an argument which matches the [*argument pattern*](%i), evaluates to the function [*body*](%i).",
    feedback: Unselected,
  };
  let pat = mk_list_pat([[pat("PAT1"), comma_pat(), pat("...")]]);
  let form = [mk_fun([[pat]]), exp("EXP")];
  {
    id: "function_listlit_exp",
    syntactic_form: form,
    expandable_id: Some(Piece.id(pat)),
    explanation,
    examples: [listnil_fun_ex, listlist_fun_ex],
  };
};
let function_cons_exp: form = {
  let explanation = {
    message: "Function literal. The only values that match the *argument pattern* are non-empty lists that match the [*head pattern*](%i) and [*tail pattern*](%i). When applied to an argument which matches the *argument pattern*, evaluates to the function [*body*](%i).",
    feedback: Unselected,
  };
  let cons = cons_pat();
  let form = [
    mk_fun([[pat("PAT_hd"), cons, pat("PAT_tl")]]),
    exp("EXP"),
  ];
  {
    id: "function_cons_exp",
    syntactic_form: form,
    expandable_id: Some(Piece.id(cons)),
    explanation,
    examples: [cons_hd_fun_ex, cons_snd_fun_ex],
  };
};
let function_var_exp: form = {
  let explanation = {
    message: "Function literal. When applied to an argument which is bound to the [*variable*](%i) `%s`, evaluates to the function [*body*](%i).",
    feedback: Unselected,
  };
  let pat = pat("Var");
  let form = [mk_fun([[pat]]), exp("EXP")];
  {
    id: "function_var_exp",
    syntactic_form: form,
    expandable_id: Some(Piece.id(pat)),
    explanation,
    examples: [basic_fun_ex, var_incr_fun_ex, var_and_fun_ex],
  };
};
let function_tuple_exp: form = {
  let explanation = {
    message: "Function literal. The only values that match the [*argument pattern*](%i) are %i-tuples where each element matches the corresponding argument element pattern. When applied to an argument which matches the [*argument pattern*](%i), evaluates to the function [*body*](%i).",
    feedback: Unselected,
  };
  let comma = comma_pat();
  let form = [mk_fun([[pat("PAT1"), comma, pat("...")]]), exp("EXP")];
  {
    id: "function_tuple_exp",
    syntactic_form: form,
    expandable_id: Some(Piece.id(comma)),
    explanation,
    examples: [tuple2_fun_ex, tuple3_fun_ex],
  };
};
let function_tuple2_exp: form = {
  let explanation = {
    message: "Function literal. The only values that match the *argument pattern* are 2-tuples where the first element matches the [*first element pattern*](%i) and the second element matches the [*second element pattern*](%i). When applied to an argument which matches the *argument pattern*, evaluates to the function [*body*](%i).",
    feedback: Unselected,
  };
  let comma = comma_pat();
  let form = [mk_fun([[pat("PAT1"), comma, pat("PAT2")]]), exp("EXP")];
  {
    id: "function_tuple2_exp",
    syntactic_form: form,
    expandable_id: Some(Piece.id(comma)),
    explanation,
    examples: [tuple2_fun_ex],
  };
};
let function_tuple3_exp: form = {
  let explanation = {
    message: "Function literal. The only values that match the *argument pattern* are 2-tuples where the first element matches the [*first element pattern*](%i), the second element matches the [*second element pattern*](%i), and the third element matches the [*third element pattern*](%i). When applied to an argument which matches the *argument pattern*, evaluates to the function [*body*](%i).",
    feedback: Unselected,
  };
  let comma = comma_pat();
  let form = [
    mk_fun([[pat("PAT1"), comma_pat(), pat("PAT2"), comma, pat("PAT3")]]),
    exp("EXP"),
  ];
  {
    id: "function_tuple3_exp",
    syntactic_form: form,
    expandable_id: Some(Piece.id(comma)),
    explanation,
    examples: [tuple3_fun_ex],
  };
};

let tuple_exp_group = "tuple_exp_group";
let tuple_exp_2_group = "tuple_exp_2_group";
let tuple_exp_3_group = "tuple_exp_3_group";
let tuple_example_1 = {
  sub_id: "tuple_example_1",
  term: mk_example("(true, 1)"),
  message: "A tuple with first elment true and second element 1.",
  feedback: Unselected,
};
let tuple_example_2 = {
  sub_id: "tuple_example_2",
  term: mk_example("(1, 2, 3)"),
  message: "A tuple with first element 1, second element 2, and third element 3.",
  feedback: Unselected,
};
let tuple_exp: form = {
  let explanation = {
    message: "Tuple literal. The tuple has %i elements.",
    feedback: Unselected,
  };
  let comma = comma_exp();
  {
    id: "tuple_exp",
    syntactic_form: [exp("EXP1"), comma, exp("...")],
    expandable_id: Some(Piece.id(comma)),
    explanation,
    examples: [tuple_example_1, tuple_example_2],
  };
};
let tuple_exp_size2: form = {
  let explanation = {
    message: "Tuple literal. The 2-tuple has a [first](%i) and [second](%i) element.",
    feedback: Unselected,
  };
  let comma = comma_exp();
  {
    id: "tuple_exp_size2",
    syntactic_form: [exp("EXP1"), comma, exp("EXP2")],
    expandable_id: Some(Piece.id(comma)),
    explanation,
    examples: [tuple_example_1],
  };
};
let tuple_exp_size3: form = {
  let explanation = {
    message: "Tuple literal. The 3-tuple has a [first](%i), [second](%i), and [third](%i) element.",
    feedback: Unselected,
  };
  let comma = comma_exp();
  {
    id: "tuple_exp_size3",
    syntactic_form: [
      exp("EXP1"),
      comma_exp(),
      exp("EXP2"),
      comma,
      exp("EXP3"),
    ],
    expandable_id: Some(Piece.id(comma)),
    explanation,
    examples: [tuple_example_2],
  };
};

let var_exp_group = "var_exp_group";
let var_exp: form = {
  let explanation = {
    message: "Variable. Takes the value of the expression that it was bound to.",
    feedback: Unselected,
  };
  {
    id: "var_exp",
    syntactic_form: [exp("Var")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

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
let let_base_ex = {
  sub_id: "let_base_ex",
  term: mk_example("let x = 1 in x"),
  message: "The variable x is bound to 1, so the expression evaluates to 1",
  feedback: Unselected,
};
let let_wild_ex = {
  sub_id: "let_wild_ex",
  term: mk_example("let _ = 1 in 2"),
  message: "The 1 is thrown away, so the expression evaluates to 2.",
  feedback: Unselected,
};
let let_int_ex = {
  sub_id: "let_int_ex",
  term: mk_example("let 1 = 1 in 2"),
  message: "The 1 is thrown away, so the expression evaluates to 2.",
  feedback: Unselected,
};
let let_float_ex = {
  sub_id: "let_float_ex",
  term: mk_example("let 1.1 = 1.1 in 2"),
  message: "The 1.1 is thrown away, so the expression evaluates to 2.",
  feedback: Unselected,
};
let let_bool_ex = {
  sub_id: "let_bool_ex",
  term: mk_example("let true = true in 2"),
  message: "The true is thrown away, so the expression evaluates to 2.",
  feedback: Unselected,
};
let let_str_ex = {
  sub_id: "let_str_ex",
  term: mk_example("let \"abc\" = \"abc\" in 2"),
  message: "The true is thrown away, so the expression evaluates to 2.",
  feedback: Unselected,
};
let let_triv_ex = {
  sub_id: "let_triv_ex",
  term: mk_example("let triv = triv in 2"),
  message: "The triv is thrown away, so the expression evaluates to 2.",
  feedback: Unselected,
};
let let_listlit_ex = {
  sub_id: "let_listlit_ex",
  term: mk_example("let [x, y] = [1, 2] in x"),
  message: "The x is bound to 1 and the y is bound to 2, so the expression evaluates to 1.",
  feedback: Unselected,
};
let let_listnil_ex = {
  sub_id: "let_listnil_ex",
  term: mk_example("let nil = nil in 2"),
  message: "The empty list is thrown away, so the expression evaluates to 2.",
  feedback: Unselected,
};
let let_cons_hd_ex = {
  sub_id: "let_cons_hd_ex",
  term: mk_example("let hd::tl = 1::nil in hd"),
  message: "The hd is bound to 1 and the tl is bound to the empty list, so the expression evaluates to 1.",
  feedback: Unselected,
};
let let_cons_snd_ex = {
  sub_id: "let_cons_snd_ex",
  term: mk_example("let fst::snd::tl = true::false::nil in snd"),
  message: "The fst is bound to true, the snd is bound to false, and the tl is bound to the empty list, so the expression evaluates to false.",
  feedback: Unselected,
};
let let_var_ex = {
  sub_id: "let_var_ex",
  term: mk_example("let x = 1 in x + 2"),
  message: "The x is bound to 1, so the expression evaluates to 1 + 2, which is 3.",
  feedback: Unselected,
};
let let_tuple2_ex = {
  sub_id: "let_tuple2_ex",
  term: mk_example("let (x, y) = (1, 2) in x + y"),
  message: "The x is bound to 1 and the y is bound to 2, so the expression evaluates to 1 + 2, which is 3.",
  feedback: Unselected,
};
let let_tuple3_ex = {
  sub_id: "let_tuple3_ex",
  term: mk_example("let (x, y, z) = (1, 2, 3) in x + y + z"),
  message: "The x is bound to 1, the y is bound to 2, and the z is bound to 3, so the expression evaluates to 1 + 2 + 3, which is 6.",
  feedback: Unselected,
};
let let_base_exp: form = {
  let explanation = {
    message: "Let expression. Binds the [*pattern*](%i) to the [*definition*](%i) in the [*body*](%i).",
    feedback: Unselected,
  };
  let pat = pat("PAT");
  let form = [mk_let([[pat], [exp("EXP_def")]]), exp("EXP_body")];
  {
    id: "let_base_exp",
    syntactic_form: form,
    expandable_id: Some(Piece.id(pat)),
    explanation,
    examples: [let_base_ex],
  };
};
let let_empty_hole_exp: form = {
  let explanation = {
    message: "Let expression. After the [*empty hole pattern*](%i) is filled, binds the [*pattern*](%i) to the [*definition*](%i) in the [*body*](%i).",
    feedback: Unselected,
  };
  let pat = pat("EmptyHole");
  let form = [mk_let([[pat], [exp("EXP_def")]]), exp("EXP_body")];
  {
    id: "let_empty_hole_exp",
    syntactic_form: form,
    expandable_id: Some(Piece.id(pat)),
    explanation,
    examples: [let_base_ex],
  };
};
let let_multi_hole_exp: form = {
  let explanation = {
    message: "Let expression. After the [invalid pattern](%i) is corrected, binds the [*pattern*](%i) to the [*definition*](%i) in the [*body*](%i).",
    feedback: Unselected,
  };
  let pat = pat("INVALID");
  let form = [mk_let([[pat], [exp("EXP_def")]]), exp("EXP_body")];
  {
    id: "let_multi_hole_exp",
    syntactic_form: form,
    expandable_id: Some(Piece.id(pat)),
    explanation,
    examples: [let_base_ex],
  };
};
let let_wild_exp: form = {
  let explanation = {
    message: "Let expression. The [*definition*](%i) is evaluated and ignored. The [*definition*](%i) can't be referenced in the [*body*](%i).",
    feedback: Unselected,
  };
  let pat = pat("_");
  let form = [mk_let([[pat], [exp("EXP_def")]]), exp("EXP_body")];
  {
    id: "let_wild_exp",
    syntactic_form: form,
    expandable_id: Some(Piece.id(pat)),
    explanation,
    examples: [let_wild_ex],
  };
};
let let_int_exp: form = {
  let explanation = {
    message: "Let expression. The only value for the [*definition*](%i) that matches the [*pattern*](%i) is `%i`. The [*definition*](%i) can't be referenced in the [*body*](%i).",
    feedback: Unselected,
  };
  let pat = pat("IntLit");
  let form = [mk_let([[pat], [exp("EXP_def")]]), exp("EXP_body")];
  {
    id: "let_int_exp",
    syntactic_form: form,
    expandable_id: Some(Piece.id(pat)),
    explanation,
    examples: [let_int_ex],
  };
};
let let_float_exp: form = {
  let explanation = {
    message: "Let expression. The only value for the [*definition*](%i) that matches the [*pattern*](%i) is `%f`. The [*definition*](%i) can't be referenced in the [*body*](%i).",
    feedback: Unselected,
  };
  let pat = pat("FloatLit");
  let form = [mk_let([[pat], [exp("EXP_def")]]), exp("EXP_body")];
  {
    id: "let_float_exp",
    syntactic_form: form,
    expandable_id: Some(Piece.id(pat)),
    explanation,
    examples: [let_float_ex],
  };
};
let let_bool_exp: form = {
  let explanation = {
    message: "Let expression. The only value for the [*definition*](%i) that matches the [*pattern*](%i) is `%b`. The [*definition*](%i) can't be referenced in the [*body*](%i).",
    feedback: Unselected,
  };
  let pat = pat("BoolLit");
  let form = [mk_let([[pat], [exp("EXP_def")]]), exp("EXP_body")];
  {
    id: "let_bool_exp",
    syntactic_form: form,
    expandable_id: Some(Piece.id(pat)),
    explanation,
    examples: [let_bool_ex],
  };
};
let let_str_exp: form = {
  let explanation = {
    message: "Let expression. The only value for the [*definition*](%i) that matches the [*pattern*](%i) is `%s`. The [*definition*](%i) can't be referenced in the [*body*](%i).",
    feedback: Unselected,
  };
  let pat = pat("StringLit");
  let form = [mk_let([[pat], [exp("EXP_def")]]), exp("EXP_body")];
  {
    id: "let_str_exp",
    syntactic_form: form,
    expandable_id: Some(Piece.id(pat)),
    explanation,
    examples: [let_str_ex],
  };
};
let let_triv_exp: form = {
  let explanation = {
    message: "Let expression. The only value for the [*definition*](%i) that matches the [*pattern*](%i) is the trivial value `triv`. The [*definition*](%i) can't be referenced in the [*body*](%i).",
    feedback: Unselected,
  };
  let pat = pat("triv");
  let form = [mk_let([[pat], [exp("EXP_def")]]), exp("EXP_body")];
  {
    id: "let_triv_exp",
    syntactic_form: form,
    expandable_id: Some(Piece.id(pat)),
    explanation,
    examples: [let_triv_ex],
  };
};
let let_listlit_exp: form = {
  let explanation = {
    message: "Let expression. The only values for the [*definition*](%i) that match the [*pattern*](%i) are lists with %i-elements, where each element matches the corresponding element pattern. The matching element patterns are bound to the elements of the [*definition*](%i) in the [*body*](%i).",
    feedback: Unselected,
  };
  let pat = mk_list_pat([[pat("PAT1"), comma_pat(), pat("...")]]);
  let form = [mk_let([[pat], [exp("EXP_def")]]), exp("EXP_body")];
  {
    id: "let_listlit_exp",
    syntactic_form: form,
    expandable_id: Some(Piece.id(pat)),
    explanation,
    examples: [let_listlit_ex],
  };
};
let let_listnil_exp: form = {
  let explanation = {
    message: "Let expression. The only value for the [*definition*](%i) that matches the [*pattern*](%i) is the empty list `nil`. The [*definition*](%i) can't be referenced in the [*body*](%i).",
    feedback: Unselected,
  };
  let pat = pat("nil");
  let form = [mk_let([[pat], [exp("EXP_def")]]), exp("EXP_body")];
  {
    id: "let_listnil_exp",
    syntactic_form: form,
    expandable_id: Some(Piece.id(pat)),
    explanation,
    examples: [let_listnil_ex],
  };
};
let let_cons_exp: form = {
  let explanation = {
    message: "Let expression. The only values for the [*definition*](%i) that match the *pattern* are non-empty lists that match the [*head*](%i) and [*tail*](%i) patterns. Matching [*head*](%i) and [*tail*](%i) patterns are bound to the head and tail of the [*definition*](%i) in the [*body*](%i).",
    feedback: Unselected,
  };
  let cons = cons_pat();
  let form = [
    mk_let([[pat("PAT_hd"), cons, pat("PAT_tl")], [exp("EXP_def")]]),
    exp("EXP_body"),
  ];
  {
    id: "let_cons_exp",
    syntactic_form: form,
    expandable_id: Some(Piece.id(cons)),
    explanation,
    examples: [let_cons_hd_ex, let_cons_snd_ex],
  };
};
let let_var_exp: form = {
  let explanation = {
    message: "Let expression. The [*definition*](%i) is bound to the [*variable*](%i) `%s` in the [*body*](%i).",
    feedback: Unselected,
  };
  let pat = pat("Var");
  let form = [mk_let([[pat], [exp("EXP_def")]]), exp("EXP_body")];
  {
    id: "let_var_exp",
    syntactic_form: form,
    expandable_id: Some(Piece.id(pat)),
    explanation,
    examples: [let_var_ex],
    // TODO Does this example being slightly different actually add anything?
  };
};
let let_tuple_exp: form = {
  let explanation = {
    message: "Let expression. The only values for the [*definition*](%i) that match the [*pattern*](%i) are %i-tuples where each element matches the corresponding element pattern. The [*definition*](%i) is bound to the [*pattern*](%i) in the [*body*](%i).",
    feedback: Unselected,
  };
  let comma = comma_pat();
  let form = [
    mk_let([[pat("PAT1"), comma, pat("...")], [exp("EXP_def")]]),
    exp("EXP_body"),
  ];
  {
    id: "let_tuple_exp",
    syntactic_form: form,
    expandable_id: Some(Piece.id(comma)),
    explanation,
    examples: [let_tuple2_ex, let_tuple3_ex],
  };
};
let let_tuple2_exp: form = {
  let explanation = {
    message: "Let expression. The only values for the [*definition*](%i) that match the *pattern* are 2-tuples where the first element matches the [*first element pattern*](%i) and the second element matches the [*second element pattern*](%i). The [*definition*](%i) is bound to the *pattern* in the [*body*](%i).",
    feedback: Unselected,
  };
  let comma = comma_pat();
  let form = [
    mk_let([[pat("PAT1"), comma, pat("PAT2")], [exp("EXP_def")]]),
    exp("EXP_body"),
  ];
  {
    id: "let_tuple2_exp",
    syntactic_form: form,
    expandable_id: Some(Piece.id(comma)),
    explanation,
    examples: [let_tuple2_ex],
  };
};
let let_tuple3_exp: form = {
  let explanation = {
    message: "Let expression. The only values for the [*definition*](%i) that match the *pattern* are 3-tuples where the first element matches the [*first element pattern*](%i), the second element matches the [*second element pattern*](%i), and the third element matches the [*third element pattern*](%i). The [*definition*](%i) is bound to the *pattern* in the [*body*](%i).",
    feedback: Unselected,
  };
  let comma = comma_pat();
  let form = [
    mk_let([
      [pat("PAT1"), comma_pat(), pat("PAT2"), comma, pat("PAT3")],
      [exp("EXP_def")],
    ]),
    exp("EXP_body"),
  ];
  {
    id: "let_tuple3_exp",
    syntactic_form: form,
    expandable_id: Some(Piece.id(comma)),
    explanation,
    examples: [let_tuple3_ex],
  };
};

let funapp_exp_group = "funapp_exp_group";
let funapp_exp_ex = {
  sub_id: "funapp_exp_ex",
  term: mk_example("(fun x -> x)(1)"),
  message: "The identity function is applied to 1. The argument x is bound to 1 in the function body and the body evaluates to 1.",
  feedback: Unselected,
};
let funapp_exp: form = {
  let explanation = {
    message: "Function application. Apply the [*function*](%i) to the [*argument*](%i).",
    feedback: Unselected,
  };
  {
    id: "funapp_exp",
    syntactic_form: [exp("EXP_fun"), mk_ap([[exp("EXP_arg")]])],
    expandable_id: None,
    explanation,
    examples: [funapp_exp_ex],
  };
};

let if_exp_group = "if_exp_group";
let if_basic1_exp_ex = {
  sub_id: "if_basic1_exp_ex",
  term: mk_example("if (true) then 1 else 2"),
  message: "Since the condition is true, the if expression evaluates to the then branch, 1.",
  feedback: Unselected,
};
let if_basic2_exp_ex = {
  sub_id: "if_basic2_exp_ex",
  term: mk_example("if (2 < 1) then 3 else 4"),
  message: "Since the condition is 2 < 1 is false, the if expression evaluates to the else branch, 4.",
  feedback: Unselected,
};
let if_exp: form = {
  let explanation = {
    message: "If expression. If the [*condition*](%i) evaluates to `true`, evaluate the [*then branch*](%i). Otherwise, evaluate the [*else branch*](%i).",
    feedback: Unselected,
  };
  {
    id: "if_exp",
    syntactic_form: [
      mk_if([[exp("EXP_cond")], [exp("EXP_then")]]),
      exp("EXP_else"),
    ],
    expandable_id: None,
    explanation,
    examples: [if_basic1_exp_ex, if_basic2_exp_ex],
  };
};

let seq_exp_group = "sequence_exp_group";
let seq_basic_exp_ex = {
  sub_id: "seq_basic_exp_ex",
  term: mk_example("1; 2"),
  message: "The left expression evaluates to 1, which is ignored. Then the right expression is evaluated to 2.",
  feedback: Unselected,
};
// TODO are these really the correct messages/explanations
let seq_test_exp_ex = {
  sub_id: "seq_test_exp_ex",
  term: mk_example("test true end; 3"),
  message: "The left expression is evaluated and recorded as a passing test because the body of the test is true. Then the right expression is evalautes to 3.",
  feedback: Unselected,
};
let seq_exp: form = {
  let explanation = {
    message: "Expression sequence. The [left expression](%i) is evaluated, then the [right expression](%i) is evaluated.",
    feedback: Unselected,
  };
  {
    id: "seq_exp",
    syntactic_form: [exp("EXP1"), seq(), exp("EXP2")],
    expandable_id: None,
    explanation,
    examples: [seq_basic_exp_ex, seq_test_exp_ex],
  };
};

let test_group = "test_group";
let test_true_ex = {
  sub_id: "test_true_ex",
  term: mk_example("test true end"),
  message: "This is reported as a passing test because the body of the test is true.",
  feedback: Unselected,
};
// TODO are these really the correct messages/explanations; maybe include something about the result being triv
let test_false_ex = {
  sub_id: "test_false_ex",
  term: mk_example("test 3 < 1 end"),
  message: "This is reported as a failing test because the body of the test is 3 < 1 which evaluates to false.",
  feedback: Unselected,
};
let test_exp: form = {
  let explanation = {
    message: "Test expression. If the [*body*](%i) of the test evalutes to `true`, the test passes. Otherwise, the test fails.",
    feedback: Unselected,
  };
  {
    id: "test_exp",
    syntactic_form: [mk_test([[exp("EXP")]])],
    expandable_id: None,
    explanation,
    examples: [test_true_ex, test_false_ex],
  };
};

let cons_exp_group = "cons_exp_group";
let cons1_ex = {
  sub_id: "cons1_ex",
  term: [int("1"), cons_exp(), nil()],
  message: "A single element list of 1.",
  feedback: Unselected,
};
let cons2_ex = {
  sub_id: "cons2_ex",
  term: [int("true"), cons_exp(), int("false"), cons_exp(), nil()],
  message: "A list with two elements, true and false.",
  feedback: Unselected,
};
let cons_exp: form = {
  let explanation = {
    message: "Cons operator. Creates a list with [*head element*](%i) and [*tail element*](%i).",
    feedback: Unselected,
  };
  {
    id: "cons_exp",
    syntactic_form: [exp("EXP_hd"), cons_exp(), exp("EXP_tl")],
    expandable_id: None,
    explanation,
    examples: [cons1_ex, cons2_ex],
  };
};

let int_unary_minus_group = "int_unary_minus_group";
let int_plus_group = "int_plus_group";
let int_minus_group = "int_minus_group";
let int_times_group = "int_times_group";
let int_divide_group = "int_divide_group";
let int_lt_group = "int_lt_group";
let int_lte_group = "int_lte_group";
let int_gt_group = "int_gt_group";
let int_gte_group = "int_gte_group";
let int_eq_group = "int_eq_group";
let float_plus_group = "float_plus_group";
let float_minus_group = "float_minus_group";
let float_times_group = "float_times_group";
let float_divide_group = "float_divide_group";
let float_lt_group = "float_lt_group";
let float_lte_group = "float_lte_group";
let float_gt_group = "float_gt_group";
let float_gte_group = "float_gte_group";
let float_eq_group = "float_eq_group";
let bool_and_group = "bool_and_group";
let bool_or_group = "bool_or_group";
let str_eq_group = "str_eq_group";
let int_unary_minus_ex = {
  sub_id: "int_unary_minus_ex",
  term: mk_example("-1"),
  message: "The 1 is negated.",
  feedback: Unselected,
};
let int_plus_ex = {
  sub_id: "int_plus_ex",
  term: mk_example("1 + 2"),
  message: "1 added to 2 evalutes to 3.",
  feedback: Unselected,
};
let int_minus_ex = {
  sub_id: "int_minus_ex",
  term: mk_example("2 - 1"),
  message: "2 minus 1 evalutes to 1.",
  feedback: Unselected,
};
let int_times_ex = {
  sub_id: "int_times_ex",
  term: mk_example("1 * 2"),
  message: "1 multiplied be 2 evalutes to 2.",
  feedback: Unselected,
};
let int_divide_ex = {
  sub_id: "int_divide_ex",
  term: mk_example("6 / 3"),
  message: "6 divided by 3 evalutes to 2.",
  feedback: Unselected,
};
let int_lt1_ex = {
  sub_id: "int_lt1_ex",
  term: mk_example("1 < 2"),
  message: "1 is less than 2, so the expression evalutes to true.",
  feedback: Unselected,
};
let int_lt2_ex = {
  sub_id: "int_lt2_ex",
  term: mk_example("4 < 3"),
  message: "4 is less not less than 3, so the expression evaluates to false.",
  feedback: Unselected,
};
let int_lte1_ex = {
  sub_id: "int_lte1_ex",
  term: mk_example("1 <= 2"),
  message: "1 is less than 2, so the expression evalutes to true.",
  feedback: Unselected,
};
let int_lte2_ex = {
  sub_id: "int_lte2_ex",
  term: mk_example("4 <= 3"),
  message: "4 is less not less than or equal to 3, so the expression evaluates to false.",
  feedback: Unselected,
};
let int_lte3_ex = {
  sub_id: "int_lte3_ex",
  term: mk_example("5 <= 5"),
  message: "5 is equal to 5, so the expression evaluates to true.",
  feedback: Unselected,
};
let int_gt1_ex = {
  sub_id: "int_gt1_ex",
  term: mk_example("1 > 2"),
  message: "1 is not greater than 2, so the expression evaluates to false.",
  feedback: Unselected,
};
let int_gt2_ex = {
  sub_id: "int_gt2_ex",
  term: mk_example("4 > 3"),
  message: "4 is greater than 3, so the expression evaluates to true.",
  feedback: Unselected,
};
let int_gte1_ex = {
  sub_id: "int_gte1_ex",
  term: mk_example("1 >= 2"),
  message: "1 is not greater than or equal to 2, so the expression evaluates to false.",
  feedback: Unselected,
};
let int_gte2_ex = {
  sub_id: "int_gte2_ex",
  term: mk_example("4 >= 3"),
  message: "4 is greater than 3, so the expression evaluates to true.",
  feedback: Unselected,
};
let int_gte3_ex = {
  sub_id: "int_gte3_ex",
  term: mk_example("5 >= 5"),
  message: "5 is equal to 5, so the expression evaluates to true.",
  feedback: Unselected,
};
let int_eq1_ex = {
  sub_id: "int_eq1_ex",
  term: mk_example("1 == 2"),
  message: "1 does not equal 2, so the expression evaluates to false.",
  feedback: Unselected,
};
let int_eq2_ex = {
  sub_id: "int_eq2_ex",
  term: mk_example("3 == 3"),
  message: "3 is equal to 3, so the expression evaluates to true.",
  feedback: Unselected,
};
let float_plus_ex = {
  sub_id: "float_plus_ex",
  term: mk_example("1. +. 2.1"),
  message: "1. added to 2.1 evalutes to 3.1",
  feedback: Unselected,
};
let float_minus_ex = {
  sub_id: "float_minus_ex",
  term: mk_example("2. -. 1.1"),
  message: "2. minus 1.1 evalutes to 0.9",
  feedback: Unselected,
};
let float_times_ex = {
  sub_id: "float_times_ex",
  term: mk_example("1. *. 2.2"),
  message: "1 multiplied be 2.2 evalutes to 2.2.",
  feedback: Unselected,
};
let float_divide_ex = {
  sub_id: "float_divide_ex",
  term: mk_example("4.2 /. 2.1"),
  message: "4.2 divided by 2.1 evalutes to 2.",
  feedback: Unselected,
};
let float_lt1_ex = {
  sub_id: "float_lt1_ex",
  term: mk_example("1. <. 2.1"),
  message: "1. is less than 2.1, so the expression evalutes to true.",
  feedback: Unselected,
};
let float_lt2_ex = {
  sub_id: "float_lt2_ex",
  term: mk_example("4. <. 3.1"),
  message: "4. is less not less than 3.1, so the expression evaluates to false.",
  feedback: Unselected,
};
let float_lte1_ex = {
  sub_id: "float_lte1_ex",
  term: mk_example("1. <=. 2.1"),
  message: "1. is less than 2.1, so the expression evalutes to true.",
  feedback: Unselected,
};
let float_lte2_ex = {
  sub_id: "float_lte2_ex",
  term: mk_example("4. <=. 3.1"),
  message: "4. is less not less than or equal to 3.1, so the expression evaluates to false.",
  feedback: Unselected,
};
let float_lte3_ex = {
  sub_id: "float_lte3_ex",
  term: mk_example("5.5 <=. 5.5"),
  message: "5.5 is equal to 5.5, so the expression evaluates to true.",
  feedback: Unselected,
};
let float_gt1_ex = {
  sub_id: "float_gt1_ex",
  term: mk_example("1.1 >. 2.1"),
  message: "1.1 is not greater than 2.1, so the expression evaluates to false.",
  feedback: Unselected,
};
let float_gt2_ex = {
  sub_id: "float_gt2_ex",
  term: mk_example("4. >. 3.1"),
  message: "4. is greater than 3.1, so the expression evaluates to true.",
  feedback: Unselected,
};
let float_gte1_ex = {
  sub_id: "float_gte1_ex",
  term: mk_example("1.1 >=. 2.1"),
  message: "1.1 is not greater than or equal to 2.1, so the expression evaluates to false.",
  feedback: Unselected,
};
let float_gte2_ex = {
  sub_id: "float_gte2_ex",
  term: mk_example("4. >=. 3.1"),
  message: "4. is greater than 3.1, so the expression evaluates to true.",
  feedback: Unselected,
};
let float_gte3_ex = {
  sub_id: "float_gte3_ex",
  term: mk_example("5.5 >=. 5.5"),
  message: "5.5 is equal to 5.5, so the expression evaluates to true.",
  feedback: Unselected,
};
let float_eq1_ex = {
  sub_id: "float_eq1_ex",
  term: mk_example("1. ==. 2."),
  message: "1. does not equal 2., so the expression evaluates to false.",
  feedback: Unselected,
};
let float_eq2_ex = {
  sub_id: "float_eq2_ex",
  term: mk_example("3.1 ==. 3.1"),
  message: "3.1 is equal to 3.1, so the expression evaluates to true.",
  feedback: Unselected,
};
let bool_and1_ex = {
  sub_id: "bool_and1_ex",
  term: mk_example("true && false"),
  message: "The left operand is true, so evaluate the right operand. Since the right operand is false, the whole expression evaluates to false.",
  feedback: Unselected,
};
let bool_and2_ex = {
  sub_id: "bool_and2_ex",
  term: mk_example("1 < 2 && 3 < 4"),
  message: "The left operand evaluates to true, so evaluate the right operand. Since the right operand also evalutes to true, the whole expression evaluates to true.",
  feedback: Unselected,
};
let bool_or1_ex = {
  sub_id: "bool_or1_ex",
  term: mk_example("false || 2 < 1"),
  message: "The left operand evaluates to false, so evaluate the right operand. Since the right operand also evaluates to false, the whole expression evaluates to false.",
  feedback: Unselected,
};
let bool_or2_ex = {
  sub_id: "bool_or2_ex",
  term: mk_example("3 < 4 || false"),
  message: "The left operand evalutes to true, so the right operand is not evaluated. The whole expression evaluates to true.",
  feedback: Unselected,
};
let str_eq1_ex = {
  sub_id: "str_eq1_ex",
  term: mk_example("\"abc\" $== \"xyz\""),
  message: "\"abc\" does not equal \"xyz\", so the expression evaluates to false.",
  feedback: Unselected,
};
let str_eq2_ex = {
  sub_id: "str_eq2_ex",
  term: mk_example("\"abc\" $== \"abc\""),
  message: "\"abc\" is equal to \"abc\", so the expression evaluates to true.",
  feedback: Unselected,
};
let int_unary_minus_exp: form = {
  let explanation = {
    message: "Unary minus. Performs integer negation of the [*operand*](%i).",
    feedback: Unselected,
  };
  {
    id: "int_unary_minus_exp",
    syntactic_form: [unary_minus(), exp("EXP")],
    expandable_id: None,
    explanation,
    examples: [int_unary_minus_ex],
  };
};
let int_plus_exp: form = {
  let explanation = {
    message: "Integer addition. Gives the sum of the [*left*](%i) and [*right*](%i) operands.",
    feedback: Unselected,
  };
  {
    id: "int_plus_exp",
    syntactic_form: [exp("EXP1"), plus(), exp("EXP2")],
    expandable_id: None,
    explanation,
    examples: [int_plus_ex],
  };
};
let int_minus_exp: form = {
  let explanation = {
    message: "Integer subtraction. Gives the difference of the [*left*](%i) and [*right*](%i) operands.",
    feedback: Unselected,
  };
  {
    id: "int_minus_exp",
    syntactic_form: [exp("EXP1"), minus(), exp("EXP2")],
    expandable_id: None,
    explanation,
    examples: [int_minus_ex],
  };
};
let int_times_exp: form = {
  let explanation = {
    message: "Integer multiplication. Gives the product of the [*left*](%i) and [*right*](%i) operands.",
    feedback: Unselected,
  };
  {
    id: "int_times_exp",
    syntactic_form: [exp("EXP1"), times(), exp("EXP2")],
    expandable_id: None,
    explanation,
    examples: [int_times_ex],
  };
};
let int_divide_exp: form = {
  let explanation = {
    message: "Integer division. Gives the quotient of the [*left*](%i) and [*right*](%i) operands.",
    feedback: Unselected,
  };
  {
    id: "int_divide_exp",
    syntactic_form: [exp("EXP1"), divide(), exp("EXP2")],
    expandable_id: None,
    explanation,
    examples: [int_divide_ex],
  };
};
let int_lt_exp: form = {
  let explanation = {
    message: "Integer less than. If the [*left operand*](%i) is less than the [*right operand*](%i), evaluates to `true`. Otherwise evaluates to `false`.",
    feedback: Unselected,
  };
  {
    id: "int_lt_exp",
    syntactic_form: [exp("EXP1"), lt(), exp("EXP2")],
    expandable_id: None,
    explanation,
    examples: [int_lt1_ex, int_lt2_ex],
  };
};
let int_lte_exp: form = {
  let explanation = {
    message: "Integer less than or equal to. If the [*left operand*](%i) is less than or equal to the [*right operand*](%i), evaluates to `true`. Otherwise evaluates to `false`.",
    feedback: Unselected,
  };
  {
    id: "int_lte_exp",
    syntactic_form: [exp("EXP1"), lte(), exp("EXP2")],
    expandable_id: None,
    explanation,
    examples: [int_lte1_ex, int_lte2_ex, int_lte3_ex],
  };
};
let int_gt_exp: form = {
  let explanation = {
    message: "Integer greater than. If the [*left operand*](%i) is greater than the [*right operand*](%i), evaluates to `true`. Otherwise evaluates to `false`.",
    feedback: Unselected,
  };
  {
    id: "int_gt_exp",
    syntactic_form: [exp("EXP1"), gt(), exp("EXP2")],
    expandable_id: None,
    explanation,
    examples: [int_gt1_ex, int_gt2_ex],
  };
};
let int_gte_exp: form = {
  let explanation = {
    message: "Integer greater than or equal to. If the [*left operand*](%i) is greater than or equal to the [*right operand*](%i), evaluates to `true`. Otherwise evaluates to `false`.",
    feedback: Unselected,
  };
  {
    id: "int_gte_exp",
    syntactic_form: [exp("EXP1"), gte(), exp("EXP2")],
    expandable_id: None,
    explanation,
    examples: [int_gte1_ex, int_gte2_ex, int_gte3_ex],
  };
};
let int_eq_exp: form = {
  let explanation = {
    message: "Integer equality. If the [*left operand*](%i) is equal to the [*right operand*](%i), evaluates to `true`. Otherwise, evaluates to `false`.",
    feedback: Unselected,
  };
  {
    id: "int_eq_exp",
    syntactic_form: [exp("EXP1"), equals(), exp("EXP2")],
    expandable_id: None,
    explanation,
    examples: [int_eq1_ex, int_eq2_ex],
  };
};
let float_plus_exp: form = {
  let explanation = {
    message: "Floating-point addition. Gives the sum of the [*left*](%i) and [*right*](%i) operands.",
    feedback: Unselected,
  };
  {
    id: "float_plus_exp",
    syntactic_form: [exp("EXP1"), fplus(), exp("EXP2")],
    expandable_id: None,
    explanation,
    examples: [float_plus_ex],
  };
};
let float_minus_exp: form = {
  let explanation = {
    message: "Floating-point subtraction. Gives the difference of the [*left*](%i) and [*right*](%i) operands.",
    feedback: Unselected,
  };
  {
    id: "float_minus_exp",
    syntactic_form: [exp("EXP1"), fminus(), exp("EXP2")],
    expandable_id: None,
    explanation,
    examples: [float_minus_ex],
  };
};
let float_times_exp: form = {
  let explanation = {
    message: "Floating-point multiplication. Gives the product of the [*left*](%i) and [*right*](%i) operands.",
    feedback: Unselected,
  };
  {
    id: "float_times_exp",
    syntactic_form: [exp("EXP1"), ftimes(), exp("EXP2")],
    expandable_id: None,
    explanation,
    examples: [float_times_ex],
  };
};
let float_divide_exp: form = {
  let explanation = {
    message: "Floating-point division. Gives the quotient of the [*left*](%i) and [*right*](%i) operands.",
    feedback: Unselected,
  };
  {
    id: "float_divide_exp",
    syntactic_form: [exp("EXP1"), fdivide(), exp("EXP2")],
    expandable_id: None,
    explanation,
    examples: [float_divide_ex],
  };
};
let float_lt_exp: form = {
  let explanation = {
    message: "Floating-point less than. If the [*left operand*](%i) is less than the [*right operand*](%i), evaluates to `true`. Otherwise evaluates to `false`.",
    feedback: Unselected,
  };
  {
    id: "float_lt_exp",
    syntactic_form: [exp("EXP1"), flt(), exp("EXP2")],
    expandable_id: None,
    explanation,
    examples: [float_lt1_ex, float_lt2_ex],
  };
};
let float_lte_exp: form = {
  let explanation = {
    message: "Floating-point less than or equal to. If the [*left operand*](%i) is less than or equal to the [*right operand*](%i), evaluates to `true`. Otherwise evaluates to `false`.",
    feedback: Unselected,
  };
  {
    id: "float_lte_exp",
    syntactic_form: [exp("EXP1"), flte(), exp("EXP2")],
    expandable_id: None,
    explanation,
    examples: [float_lte1_ex, float_lte2_ex, float_lte3_ex],
  };
};
let float_gt_exp: form = {
  let explanation = {
    message: "Floating-point greater than. If the [*left operand*](%i) is greater than the [*right operand*](%i), evaluates to `true`. Otherwise evaluates to `false`.",
    feedback: Unselected,
  };
  {
    id: "float_gt_exp",
    syntactic_form: [exp("EXP1"), fgt(), exp("EXP2")],
    expandable_id: None,
    explanation,
    examples: [float_gt1_ex, float_gt2_ex],
  };
};
let float_gte_exp: form = {
  let explanation = {
    message: "Floating-point greater than or equal to. If the [*left operand*](%i) is greater than or equal to the [*right operand*](%i), evaluates to `true`. Otherwise evaluates to `false`.",
    feedback: Unselected,
  };
  {
    id: "float_gt_exp",
    syntactic_form: [exp("EXP1"), fgte(), exp("EXP2")],
    expandable_id: None,
    explanation,
    examples: [float_gte1_ex, float_gte2_ex, float_gte3_ex],
  };
};
let float_eq_exp: form = {
  let explanation = {
    message: "Floating-point equality. If the [*left operand*](%i) is equal to the [*right operand*](%i), evaluates to `true`. Otherwise, evaluates to `false`.",
    feedback: Unselected,
  };
  {
    id: "float_eq_exp",
    syntactic_form: [exp("EXP1"), fequals(), exp("EXP2")],
    expandable_id: None,
    explanation,
    examples: [float_eq1_ex, float_eq2_ex],
  };
};
// TODO Some of the examples are evaluating weirdly and can't type the || in the editor
let bool_and_exp: form = {
  let explanation = {
    message: "Boolean and. If the [*left operand*](%i) evaluates to `true`, evaluate the [*right operand*](%i). If that also evaluates to `true`, the whole expression evaluates to `true`. Otherwise, evaluates to `false`.",
    feedback: Unselected,
  };
  {
    id: "bool_and_exp",
    syntactic_form: [exp("EXP1"), logical_and(), exp("EXP2")],
    expandable_id: None,
    explanation,
    examples: [bool_and1_ex, bool_and2_ex],
  };
};
let bool_or_exp: form = {
  let explanation = {
    message: "Boolean or. If the [*left operand*](%i) evaluates to `true`, the whole expression evaluates to `true`. Otherwise, evaluate the [*right operand*](%i). If that evaluates to `true`, the whole expression evaluates to `true`. Otherwise, evaluates to `false`.",
    feedback: Unselected,
  };
  {
    id: "bool_or_exp",
    syntactic_form: [exp("EXP1"), logical_or(), exp("EXP2")],
    expandable_id: None,
    explanation,
    examples: [bool_or1_ex, bool_or2_ex],
  };
};

let str_eq_exp: form = {
  let explanation = {
    message: "String equality. If the [*left operand*](%i) is equal to the [*right operand*](%i), evaluates to `true`. Otherwise, evaluates to `false`.",
    feedback: Unselected,
  };
  {
    id: "str_eq_exp",
    syntactic_form: [exp("EXP1"), sequals(), exp("EXP2")],
    expandable_id: None,
    explanation,
    examples: [str_eq1_ex, str_eq2_ex],
  };
};

let case_exp_group = "case_exp_group";
let case_exp2_group = "case_exp2_group";
let case_exp3_group = "case_exp3_group";
let case_example_1 = {
  sub_id: "case_example_1",
  term: mk_example("case 1 | 1 => 1.1 | 2 => 2.2 | _ => 3.3 end"),
  message: "The scrutinee of the case expression is 1. Since the scrutinee matches the first pattern 1, the first branch is taken. The whole expression evaluates to the first clause 1.1.",
  feedback: Unselected,
};
let case_example_2 = {
  sub_id: "case_example_2",
  term: mk_example("case false | true => 1 | false => 2 end"),
  message: "The scrutinee of the case expression is false. The scrutinee does not match the first pattern true. Since, scrutinee does match the second pattern false, the second branch is taken. The whole expression evaluates to the second clause 2.",
  feedback: Unselected,
};
// TODO - I don't think changing specificity on the number of cases is really the most
// beneficial specificity change - I think instead have generic at top level
// and then have a slightly different setup for specific that is created more
// dynamically calling setup methods here but more
// work done in the LangDoc code - maybe just up to 3 or 4 branches?
let case_exp: form = {
  let explanation = {
    message: "Case expression. Consider each branch in order. For the first branch with a *pattern* that matches the [*scrutinee*](%i), evaluates to the corresponding *clause*.",
    feedback: Unselected,
  };
  let _dot = exp("...");
  {
    id: "case_exp",
    syntactic_form: [
      mk_case([
        [
          exp("EXP_scrut"),
          mk_rule([[pat("PAT1")]]),
          exp("EXP1"),
          mk_rule([[pat("...")]]),
          exp("..."),
        ],
      ]),
    ],
    expandable_id: None, //Some(Piece.id(dot)),
    explanation,
    examples: [case_example_1, case_example_2],
  };
};
let case_exp_rule2: form = {
  let explanation = {
    message: "Case expression. Consider each branch in order. \n-If the [*first pattern*](%i) matches the [*scrutinee*](%i), evaluate to the [*first clause*](%i). \n-Otherwise, if the [*second pattern*](%i) matches the [*scrutinee*](%i), evaluate to the [*second clause*](%i).",
    feedback: Unselected,
  };
  let exp2 = exp("EXP2");
  {
    id: "case_exp_rule2",
    syntactic_form: [exp("EXP1"), comma_exp(), exp2],
    expandable_id: Some(Piece.id(exp2)),
    explanation,
    examples: [case_example_2],
  };
};
let case_exp_rule3: form = {
  let explanation = {
    message: "Case expression. Consider each branch in order. \n-If the [*first pattern*](%i) matches the [*scrutinee*](%i), evaluate to the [*first clause*](%i). \n-Otherwise, if the [*second pattern*](%i) matches the [*scrutinee*](%i), evaluate to the [*second clause*](%i). \n-Otherwise, if the [*third pattern*](%i) matches the [*scrutinee*](%i), evaluate to the [*third clause*](%i).",
    feedback: Unselected,
  };
  let comma = comma_exp();
  {
    id: "case_exp_rule3",
    syntactic_form: [
      exp("EXP1"),
      comma_exp(),
      exp("EXP2"),
      comma,
      exp("EXP3"),
    ],
    expandable_id: Some(Piece.id(comma)),
    explanation,
    examples: [case_example_1],
  };
};

let empty_hole_pat_group = "empty_hole_pat_group";
let empty_hole_pat: form = {
  let explanation = {
    message: "Empty hole pattern. Expressions are not matched against the *empty hole pattern* until it is filled.",
    feedback: Unselected,
  };
  {
    id: "empty_hole_pat",
    syntactic_form: [pat("EmptyHole")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let multi_hole_pat_group = "multi_hole_pat_group";
let multi_hole_pat: form = {
  let explanation = {
    message: "Unrecognized pattern. Expressions are not matched against the invalid pattern until it is corrected.",
    feedback: Unselected,
  };
  {
    id: "multi_hole_pat",
    syntactic_form: [pat("Invalid")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let wild_pat_group = "wild_pat_group";
let wild_pat: form = {
  let explanation = {
    message: "Wildcard pattern. All expressions match the *wildcard pattern*.",
    feedback: Unselected,
  };
  {
    id: "wild_pat",
    syntactic_form: [pat("_")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let intlit_pat_group = "intlit_pat_group";
let intlit_pat: form = {
  let explanation = {
    message: "Integer literal pattern. Only expressions with value `%i` match the *`%i` pattern*.",
    feedback: Unselected,
  };
  {
    id: "intlit_pat",
    syntactic_form: [pat("IntLit")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let floatlit_pat_group = "floatlit_pat_group";
let floatlit_pat: form = {
  let explanation = {
    message: "Floating-point literal pattern. Only expressions with value `%f` match the *`%f` pattern*.",
    feedback: Unselected,
  };
  {
    id: "floatlit_pat",
    syntactic_form: [pat("FloatLit")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let boollit_pat_group = "boollit_pat_group";
let boollit_pat: form = {
  let explanation = {
    message: "Boolean literal pattern. Only expressions with value `%b` match the *`%b` pattern*.",
    feedback: Unselected,
  };
  {
    id: "boollit_pat",
    syntactic_form: [pat("BoolLit")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let strlit_pat_group = "strlit_pat_group";
let strlit_pat: form = {
  let explanation = {
    message: "String literal pattern. Only expressions with value `%s` match the *`%s` pattern*.",
    feedback: Unselected,
  };
  {
    id: "strlit_pat",
    syntactic_form: [pat("StringLit")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let triv_pat_group = "triv_pat_group";
let triv_pat: form = {
  let explanation = {
    message: "Triv pattern. Only expressions with the trivial value `triv` match the *trivial pattern `triv`*.",
    feedback: Unselected,
  };
  {
    id: "triv_pat",
    syntactic_form: [pat("triv")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let listlit_pat_group = "listlit_pat_group";
let listlit_pat: form = {
  let explanation = {
    message: "List literal pattern. Only expressions that are lists with %i-elements where each element matches the corresponding element pattern match this *list literal pattern*.",
    feedback: Unselected,
  };
  {
    id: "listlit_pat",
    syntactic_form: [
      mk_list_pat([[pat("PAT1"), comma_pat(), pat("...")]]),
    ],
    expandable_id: None,
    explanation,
    examples: [],
  };
};
let listnil_pat_group = "listnil_pat_group";
let listnil_pat: form = {
  let explanation = {
    message: "Empty list pattern. Only expressions that are empty lists `nil` match the *empty list `nil` pattern*.",
    feedback: Unselected,
  };
  {
    id: "listnil_pat",
    syntactic_form: [pat("nil")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let cons_pat_group = "cons_pat_group";
let cons2_pat_group = "cons2_pat_group";
let cons_base_pat: form = {
  let explanation = {
    message: "Non-empty list pattern. Only expressions that are non-empty lists with *head element* matching the [*head element pattern*](%i) and *tail* list matching the [*tail pattern*](%i) match this non-empty list pattern.",
    feedback: Unselected,
  };
  let tl = pat("PAT_tl");
  {
    id: "cons_base_pat",
    syntactic_form: [pat("PAT_hd"), cons_pat(), tl],
    expandable_id: Some(Piece.id(tl)),
    explanation,
    examples: [],
  };
};
let cons2_pat: form = {
  let explanation = {
    message: "Non-empty list pattern. Only expressions that are non-empty lists with *first element* matching the [*first element pattern*](%i), *second element* matching the [*second element pattern*](%i), and *tail* list matching the [*tail pattern*](%i) match this non-empty list pattern.",
    feedback: Unselected,
  };
  let c = cons_pat();
  {
    id: "cons2_pat",
    syntactic_form: [
      pat("PAT_fst"),
      cons_pat(),
      pat("PAT_snd"),
      c,
      pat("PAT_tl"),
    ],
    expandable_id: Some(Piece.id(c)),
    explanation,
    examples: [],
  };
};

let var_pat_group = "var_pat_group";
let var_pat: form = {
  let explanation = {
    message: "Variable pattern. All expressions match the *variable pattern*. The matching expression will be bound to variable `%s`.",
    feedback: Unselected,
  };
  {
    id: "var_pat",
    syntactic_form: [pat("Var")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let tuple_pat_group = "tuple_pat_group";
let tuple_pat_2_group = "tuple_pat_2_group";
let tuple_pat_3_group = "tuple_pat_3_group";
let tuple_pat: form = {
  let explanation = {
    message: "Tuple pattern. Only expressions that are %i-tuples with elements matching the corresponding element patterns match this tuple pattern.",
    feedback: Unselected,
  };
  let comma = comma_pat();
  {
    id: "tuple_pat",
    syntactic_form: [pat("PAT1"), comma, pat("...")],
    expandable_id: Some(Piece.id(comma)),
    explanation,
    examples: [],
  };
};
let tuple_pat_size2: form = {
  let explanation = {
    message: "Tuple pattern. Only expressions that are 2-tuples with first element matching the [first element pattern](%i) and second element matching the [second element pattern](%i) match this tuple pattern.",
    feedback: Unselected,
  };
  let comma = comma_pat();
  {
    id: "tuple_pat_size2",
    syntactic_form: [pat("PAT1"), comma, pat("PAT2")],
    expandable_id: Some(Piece.id(comma)),
    explanation,
    examples: [],
  };
};
let tuple_pat_size3: form = {
  let explanation = {
    message: "Tuple pattern. Only expressions that are 3-tuples with first element matching the [first element pattern](%i), second element matching the [second element pattern](%i), and third element matching the [third element pattern](%i) match this tuple pattern.",
    feedback: Unselected,
  };
  let comma = comma_pat();
  {
    id: "tuple_pat_size3",
    syntactic_form: [
      pat("PAT1"),
      comma_pat(),
      pat("PAT2"),
      comma,
      pat("PAT3"),
    ],
    expandable_id: Some(Piece.id(comma)),
    explanation,
    examples: [],
  };
};

let empty_hole_typ_group = "empty_hole_typ_group";
let empty_hole_typ: form = {
  let explanation = {
    message: "Empty hole type. This marks a type that needs to be filled in.",
    feedback: Unselected,
  };
  {
    id: "empty_hole_typ",
    syntactic_form: [typ("EmptyHole")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

// TODO Did get a case where in type position had space between two variables where got into weird state
let multi_hole_typ_group = "multi_hole_typ_group";
let multi_hole_typ: form = {
  let explanation = {
    message: "Multi hole type. This is an invalid type.",
    feedback: Unselected,
  };
  {
    id: "multi_hole_typ",
    syntactic_form: [typ("Invalid")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let int_typ_group = "int_typ_group";
let int_typ: form = {
  let explanation = {
    message: "Int type. The `Int` type classifies integer values.",
    feedback: Unselected,
  };
  {
    id: "int_typ",
    syntactic_form: [typ("Int")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let float_typ_group = "float_typ_group";
let float_typ: form = {
  let explanation = {
    message: "Float type. The `Float` type classifies floating-point values.",
    feedback: Unselected,
  };
  {
    id: "float_typ",
    syntactic_form: [typ("Float")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let bool_typ_group = "bool_typ_group";
let bool_typ: form = {
  let explanation = {
    message: "Bool type. The `Bool` type classifies boolean values.",
    feedback: Unselected,
  };
  {
    id: "bool_typ",
    syntactic_form: [typ("Bool")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let str_typ_group = "str_typ_group";
let str_typ: form = {
  let explanation = {
    message: "String type. The `String` type classifies string values.",
    feedback: Unselected,
  };
  {
    id: "str_typ",
    syntactic_form: [typ("String")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let list_typ_group = "list_typ_group";
let list_typ: form = {
  let explanation = {
    message: "List type. The list type classifies lists with elements with the corresponding [*element type*](%i).",
    feedback: Unselected,
  };
  {
    id: "list_typ",
    syntactic_form: [mk_list_typ([[typ("TYP_elem")]])],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let arrow_typ_group = "arrow_typ_group";
let arrow3_typ_group = "arrow3_typ_group";
let arrow_typ: form = {
  let explanation = {
    message: "Arrow type. This arrow type classifies functions with [*argument type*](%i) and [*output type*](%i).",
    feedback: Unselected,
  };
  let out = typ("TYP_out");
  {
    id: "arrow_typ",
    syntactic_form: [typ("TYP_arg"), arrow(), out],
    expandable_id: Some(Piece.id(out)),
    explanation,
    examples: [],
  };
};
let arrow3_typ: form = {
  let explanation = {
    message: "Arrow type. This arrow type classifies functions with [*first argument type*](%i), [*second argument type*](%i), and [*output type*](%i).",
    feedback: Unselected,
  };
  let arrow2 = arrow();
  {
    id: "arrow3_typ",
    syntactic_form: [
      typ("TYP_arg1"),
      arrow(),
      typ("TYP_arg2"),
      arrow2,
      typ("TYP_out"),
    ],
    expandable_id: Some(Piece.id(arrow2)),
    explanation,
    examples: [],
  };
};

let tuple_typ_group = "tuple_typ_group";
let tuple2_typ_group = "tuple2_typ_group";
let tuple3_typ_group = "tuple3_typ_group";
let tuple_typ: form = {
  let explanation = {
    message: "Tuple type. This tuple type classifies %i-tuples with corresponding element types.",
    feedback: Unselected,
  };
  let comma = comma_typ();
  {
    id: "tuple_typ",
    syntactic_form: [typ("TYP1"), comma, typ("...")],
    expandable_id: Some(Piece.id(comma)),
    explanation,
    examples: [],
  };
};
let tuple2_typ: form = {
  let explanation = {
    message: "Tuple type. This tuple type classifies %i-tuples with the first element of the [first element type](%i) and second element of the [second element type](%i).",
    feedback: Unselected,
  };
  let comma = comma_typ();
  {
    id: "tuple2_typ",
    syntactic_form: [typ("TYP1"), comma, typ("TYP2")],
    expandable_id: Some(Piece.id(comma)),
    explanation,
    examples: [],
  };
};
let tuple3_typ: form = {
  let explanation = {
    message: "Tuple type. This tuple type classifies %i-tuples with the first element of the [first element type](%i), second element of the [second element type](%i), and third element of the [third element type](%i).",
    feedback: Unselected,
  };
  let comma = comma_typ();
  {
    id: "tuple3_typ",
    syntactic_form: [
      typ("TYP1"),
      comma_typ(),
      typ("TYP2"),
      comma,
      typ("TYP3"),
    ],
    expandable_id: Some(Piece.id(comma)),
    explanation,
    examples: [],
  };
};

// Just have a flat list of forms w/ their explanations and examples
// Keep track of options/groups in a separate structure
[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  show: bool,
  highlight: bool,
  specificity_open: bool,
  forms: list(form),
  groups: list((string, form_group)),
};

let get_group = (group_id, doc: t) => {
  let (_, form_group) = List.find(((id, _)) => id == group_id, doc.groups);
  form_group;
};

let get_form_and_options = (group_id, doc: t) => {
  let form_group = get_group(group_id, doc);
  let (selected_id, _) =
    List.nth(form_group.options, form_group.current_selection);
  let form = List.find(({id, _}) => id == selected_id, doc.forms);
  (form, form_group.options);
};

let get_example = (example_sub_id, docs) =>
  List.find(({sub_id, _}) => sub_id == example_sub_id, docs);

let get_form = (form_id, docs) =>
  List.find(({id, _}) => id == form_id, docs);

let rec update_form = (new_form, docs) => {
  switch (docs) {
  | [] => []
  | [x, ...xs] =>
    if (x.id == new_form.id) {
      [new_form, ...xs];
    } else {
      [x, ...update_form(new_form, xs)];
    }
  };
};

let rec update_example = (new_example, docs) => {
  switch (docs) {
  | [] => []
  | [x, ...xs] =>
    if (x.sub_id == new_example.sub_id) {
      [new_example, ...xs];
    } else {
      [x, ...update_example(new_example, xs)];
    }
  };
};

let rec update_group = (group_id, new_selection, groups) => {
  switch (groups) {
  | [] => []
  | [(id, options) as x, ...xs] =>
    if (id == group_id) {
      [
        (id, {options: options.options, current_selection: new_selection}),
        ...xs,
      ];
    } else {
      [x, ...update_group(group_id, new_selection, xs)];
    }
  };
};

let init_options = options => {
  options,
  current_selection: List.length(options) - 1,
};

let init = {
  show: true,
  highlight: true,
  specificity_open: false,
  forms: [
    // Expressions
    empty_hole_exp,
    multi_hole_exp,
    triv_exp,
    bool_exp,
    int_exp,
    float_exp,
    string_exp,
    list_exp,
    function_exp,
    function_empty_hole_exp,
    function_multi_hole_exp,
    function_wild_exp,
    function_intlit_exp,
    function_floatlit_exp,
    function_boollit_exp,
    function_strlit_exp,
    function_triv_exp,
    function_listnil_exp,
    function_listlit_exp,
    function_cons_exp,
    function_var_exp,
    function_tuple_exp,
    function_tuple2_exp,
    function_tuple3_exp,
    tuple_exp,
    tuple_exp_size2,
    tuple_exp_size3,
    var_exp,
    let_base_exp,
    let_empty_hole_exp,
    let_multi_hole_exp,
    let_wild_exp,
    let_int_exp,
    let_float_exp,
    let_bool_exp,
    let_str_exp,
    let_triv_exp,
    let_listlit_exp,
    let_listnil_exp,
    let_cons_exp,
    let_var_exp,
    let_tuple_exp,
    let_tuple2_exp,
    let_tuple3_exp,
    funapp_exp,
    if_exp,
    seq_exp,
    test_exp,
    cons_exp,
    int_unary_minus_exp,
    int_plus_exp,
    int_minus_exp,
    int_times_exp,
    int_divide_exp,
    int_lt_exp,
    int_lte_exp,
    int_gt_exp,
    int_gte_exp,
    int_eq_exp,
    float_plus_exp,
    float_minus_exp,
    float_times_exp,
    float_divide_exp,
    float_lt_exp,
    float_lte_exp,
    float_gt_exp,
    float_gte_exp,
    float_eq_exp,
    bool_and_exp,
    bool_or_exp,
    str_eq_exp,
    case_exp,
    // Rules
    // Patterns
    empty_hole_pat,
    multi_hole_pat,
    wild_pat,
    intlit_pat,
    floatlit_pat,
    boollit_pat,
    strlit_pat,
    triv_pat,
    listlit_pat,
    listnil_pat,
    cons_base_pat,
    cons2_pat,
    var_pat,
    tuple_pat,
    tuple_pat_size2,
    tuple_pat_size3,
    // Types
    empty_hole_typ,
    multi_hole_typ,
    int_typ,
    float_typ,
    bool_typ,
    str_typ,
    list_typ,
    arrow_typ,
    arrow3_typ,
    tuple_typ,
    tuple2_typ,
    tuple3_typ,
  ],
  groups: [
    // Expressions
    (empty_hole_exp_group, init_options([(empty_hole_exp.id, [])])),
    (multi_hole_exp_group, init_options([(multi_hole_exp.id, [])])),
    (triv_exp_group, init_options([(triv_exp.id, [])])),
    (bool_exp_group, init_options([(bool_exp.id, [])])),
    (int_exp_group, init_options([(int_exp.id, [])])),
    (float_exp_group, init_options([(float_exp.id, [])])),
    (string_exp_group, init_options([(string_exp.id, [])])),
    (list_exp_group, init_options([(list_exp.id, [])])),
    (function_group, init_options([(function_exp.id, [])])),
    (
      function_empty_hole_group,
      init_options([
        (function_exp.id, [pat("PAT")]),
        (function_empty_hole_exp.id, [pat("EMPTYHOLE")]),
      ]),
    ),
    (
      function_multi_hole_group,
      init_options([
        (function_exp.id, [pat("PAT")]),
        (function_multi_hole_exp.id, [pat("INVALID")]),
      ]),
    ),
    (
      function_wild_group,
      init_options([
        (function_exp.id, [pat("PAT")]),
        (function_wild_exp.id, [pat("_")]),
      ]),
    ),
    (
      function_int_group,
      init_options([
        (function_exp.id, [pat("PAT")]),
        (function_intlit_exp.id, [pat("IntLit")]),
      ]),
    ),
    (
      function_float_group,
      init_options([
        (function_exp.id, [pat("PAT")]),
        (function_floatlit_exp.id, [pat("FloatLit")]),
      ]),
    ),
    (
      function_bool_group,
      init_options([
        (function_exp.id, [pat("PAT")]),
        (function_boollit_exp.id, [pat("BoolLit")]),
      ]),
    ),
    (
      function_str_group,
      init_options([
        (function_exp.id, [pat("PAT")]),
        (function_strlit_exp.id, [pat("StringLit")]),
      ]),
    ),
    (
      function_triv_group,
      init_options([
        (function_exp.id, [pat("PAT")]),
        (function_triv_exp.id, [pat("triv")]),
      ]),
    ),
    (
      function_listnil_group,
      init_options([
        (function_exp.id, [pat("PAT")]),
        (function_listnil_exp.id, [pat("nil")]),
      ]),
    ),
    (
      function_listlit_group,
      init_options([
        (function_exp.id, [pat("PAT")]),
        (
          function_listlit_exp.id,
          [mk_list_pat([[pat("PAT1"), comma_pat(), pat("...")]])],
        ),
      ]),
    ),
    (
      function_cons_group,
      init_options([
        (function_exp.id, [pat("PAT")]),
        (function_cons_exp.id, [pat("PAT_hd"), cons_pat(), pat("PAT_tl")]),
      ]),
    ),
    (
      function_var_group,
      init_options([
        (function_exp.id, [pat("PAT")]),
        (function_var_exp.id, [pat("Var")]),
      ]),
    ),
    (
      function_tuple_group,
      init_options([
        (function_exp.id, [pat("PAT")]),
        (function_tuple_exp.id, [pat("PAT1"), comma_pat(), pat("...")]),
      ]),
    ),
    (
      function_tuple_2_group,
      init_options([
        (function_exp.id, [pat("PAT")]),
        (function_tuple_exp.id, [pat("PAT1"), comma_pat(), pat("...")]),
        (function_tuple2_exp.id, [pat("PAT1"), comma_pat(), pat("PAT2")]),
      ]),
    ),
    (
      function_tuple_3_group,
      init_options([
        (function_exp.id, [pat("PAT")]),
        (function_tuple_exp.id, [pat("PAT1"), comma_pat(), pat("...")]),
        (
          function_tuple3_exp.id,
          [
            pat("PAT1"),
            comma_pat(),
            pat("PAT2"),
            comma_pat(),
            pat("PAT3"),
          ],
        ),
      ]),
    ),
    (tuple_exp_group, init_options([(tuple_exp.id, [])])),
    (
      tuple_exp_2_group,
      init_options([
        (tuple_exp.id, [exp("EXP1"), comma_exp(), exp("...")]),
        (tuple_exp_size2.id, [exp("EXP1"), comma_exp(), exp("EXP2")]),
      ]),
    ),
    (
      tuple_exp_3_group,
      init_options([
        (tuple_exp.id, [exp("EXP1"), comma_exp(), exp("...")]),
        (
          tuple_exp_size3.id,
          [
            exp("EXP1"),
            comma_exp(),
            exp("EXP2"),
            comma_exp(),
            exp("EXP3"),
          ],
        ),
      ]),
    ),
    (var_exp_group, init_options([(var_exp.id, [])])),
    (let_base_exp_group, init_options([(let_base_exp.id, [])])),
    (
      let_empty_hole_exp_group,
      init_options([
        (let_base_exp.id, [pat("PAT")]),
        (let_empty_hole_exp.id, [pat("EmptyHole")]),
      ]),
    ),
    (
      let_multi_hole_exp_group,
      init_options([
        (let_base_exp.id, [pat("PAT")]),
        (let_multi_hole_exp.id, [pat("INVALID")]),
      ]),
    ),
    (
      let_wild_exp_group,
      init_options([
        (let_base_exp.id, [pat("PAT")]),
        (let_wild_exp.id, [pat("_")]),
      ]),
    ),
    (
      let_int_exp_group,
      init_options([
        (let_base_exp.id, [pat("PAT")]),
        (let_int_exp.id, [pat("IntLit")]),
      ]),
    ),
    (
      let_float_exp_group,
      init_options([
        (let_base_exp.id, [pat("PAT")]),
        (let_float_exp.id, [pat("FloatLit")]),
      ]),
    ),
    (
      let_bool_exp_group,
      init_options([
        (let_base_exp.id, [pat("PAT")]),
        (let_bool_exp.id, [pat("BoolLit")]),
      ]),
    ),
    (
      let_str_exp_group,
      init_options([
        (let_base_exp.id, [pat("PAT")]),
        (let_str_exp.id, [pat("StringLit")]),
      ]),
    ),
    (
      let_triv_exp_group,
      init_options([
        (let_base_exp.id, [pat("PAT")]),
        (let_triv_exp.id, [pat("triv")]),
      ]),
    ),
    (
      let_listlit_exp_group,
      init_options([
        (let_base_exp.id, [pat("PAT")]),
        (let_listlit_exp.id, [pat("PAT1"), comma_pat(), pat("...")]),
      ]),
    ),
    (
      let_listnil_exp_group,
      init_options([
        (let_base_exp.id, [pat("PAT")]),
        (let_listnil_exp.id, [pat("nil")]),
      ]),
    ),
    (
      let_cons_exp_group,
      init_options([
        (let_base_exp.id, [pat("PAT")]),
        (let_cons_exp.id, [pat("PAT_hd"), cons_pat(), pat("PAT_tl")]),
      ]),
    ),
    (
      let_var_exp_group,
      init_options([
        (let_base_exp.id, [pat("PAT")]),
        (let_var_exp.id, [pat("Var")]),
      ]),
    ),
    (
      let_tuple_base_exp_group,
      init_options([
        (let_base_exp.id, [pat("PAT")]),
        (let_tuple_exp.id, [pat("PAT1"), comma_pat(), pat("...")]),
      ]),
    ),
    (
      let_tuple2_exp_group,
      init_options([
        (let_base_exp.id, [pat("PAT")]),
        (let_tuple_exp.id, [pat("PAT1"), comma_pat(), pat("...")]),
        (let_tuple2_exp.id, [pat("PAT1"), comma_pat(), pat("PAT2")]),
      ]),
    ),
    (
      let_tuple3_exp_group,
      init_options([
        (let_base_exp.id, [pat("PAT")]),
        (let_tuple_exp.id, [pat("PAT1"), comma_pat(), pat("...")]),
        (
          let_tuple3_exp.id,
          [
            pat("PAT1"),
            comma_pat(),
            pat("PAT2"),
            comma_pat(),
            pat("PAT3"),
          ],
        ),
      ]),
    ),
    (funapp_exp_group, init_options([(funapp_exp.id, [])])),
    (if_exp_group, init_options([(if_exp.id, [])])),
    (seq_exp_group, init_options([(seq_exp.id, [])])),
    (test_group, init_options([(test_exp.id, [])])),
    (cons_exp_group, init_options([(cons_exp.id, [])])),
    (int_unary_minus_group, init_options([(int_unary_minus_exp.id, [])])),
    (int_plus_group, init_options([(int_plus_exp.id, [])])),
    (int_minus_group, init_options([(int_minus_exp.id, [])])),
    (int_times_group, init_options([(int_times_exp.id, [])])),
    (int_divide_group, init_options([(int_divide_exp.id, [])])),
    (int_lt_group, init_options([(int_lt_exp.id, [])])),
    (int_lte_group, init_options([(int_lte_exp.id, [])])),
    (int_gt_group, init_options([(int_gt_exp.id, [])])),
    (int_gte_group, init_options([(int_gte_exp.id, [])])),
    (int_eq_group, init_options([(int_eq_exp.id, [])])),
    (float_plus_group, init_options([(float_plus_exp.id, [])])),
    (float_minus_group, init_options([(float_minus_exp.id, [])])),
    (float_times_group, init_options([(float_times_exp.id, [])])),
    (float_divide_group, init_options([(float_divide_exp.id, [])])),
    (float_lt_group, init_options([(float_lt_exp.id, [])])),
    (float_lte_group, init_options([(float_lte_exp.id, [])])),
    (float_gt_group, init_options([(float_gt_exp.id, [])])),
    (float_gte_group, init_options([(float_gte_exp.id, [])])),
    (float_eq_group, init_options([(float_eq_exp.id, [])])),
    (bool_and_group, init_options([(bool_and_exp.id, [])])),
    (bool_or_group, init_options([(bool_or_exp.id, [])])),
    (str_eq_group, init_options([(str_eq_exp.id, [])])),
    (case_exp_group, init_options([(case_exp.id, [])])),
    // Rules
    // Patterns
    (empty_hole_pat_group, init_options([(empty_hole_pat.id, [])])),
    (multi_hole_pat_group, init_options([(multi_hole_pat.id, [])])),
    (wild_pat_group, init_options([(wild_pat.id, [])])),
    (intlit_pat_group, init_options([(intlit_pat.id, [])])),
    (floatlit_pat_group, init_options([(floatlit_pat.id, [])])),
    (boollit_pat_group, init_options([(boollit_pat.id, [])])),
    (strlit_pat_group, init_options([(strlit_pat.id, [])])),
    (triv_pat_group, init_options([(triv_pat.id, [])])),
    (listlit_pat_group, init_options([(listlit_pat.id, [])])),
    (listnil_pat_group, init_options([(listnil_pat.id, [])])),
    (cons_pat_group, init_options([(cons_base_pat.id, [])])),
    (
      cons2_pat_group,
      init_options([
        (cons_base_pat.id, [pat("PAT_tl")]),
        (cons2_pat.id, [pat("PAT_snd"), cons_pat(), pat("PAT_tl")]),
      ]),
    ),
    (var_pat_group, init_options([(var_pat.id, [])])),
    (tuple_pat_group, init_options([(tuple_pat.id, [])])),
    (
      tuple_pat_2_group,
      init_options([
        (tuple_pat.id, [pat("PAT1"), comma_pat(), pat("...")]),
        (tuple_pat_size2.id, [pat("PAT1"), comma_pat(), pat("PAT2")]),
      ]),
    ),
    (
      tuple_pat_3_group,
      init_options([
        (tuple_pat.id, [pat("PAT1"), comma_pat(), pat("...")]),
        (
          tuple_pat_size3.id,
          [
            pat("PAT1"),
            comma_pat(),
            pat("PAT2"),
            comma_pat(),
            pat("PAT3"),
          ],
        ),
      ]),
    ),
    // Types
    (empty_hole_typ_group, init_options([(empty_hole_typ.id, [])])),
    (multi_hole_typ_group, init_options([(multi_hole_typ.id, [])])),
    (int_typ_group, init_options([(int_typ.id, [])])),
    (float_typ_group, init_options([(float_typ.id, [])])),
    (bool_typ_group, init_options([(bool_typ.id, [])])),
    (str_typ_group, init_options([(str_typ.id, [])])),
    (list_typ_group, init_options([(list_typ.id, [])])),
    (arrow_typ_group, init_options([(arrow_typ.id, [])])),
    (
      arrow3_typ_group,
      init_options([
        (arrow_typ.id, [typ("TYP_out")]),
        (arrow3_typ.id, [typ("TYP_arg2"), arrow(), typ("TYP_out")]),
      ]),
    ),
    (tuple_typ_group, init_options([(tuple_typ.id, [])])),
    (
      tuple2_typ_group,
      init_options([
        (tuple_typ.id, [typ("TYP1"), comma_typ(), typ("...")]),
        (tuple2_typ.id, [typ("TYP1"), comma_typ(), typ("TYP2")]),
      ]),
    ),
    (
      tuple3_typ_group,
      init_options([
        (tuple_typ.id, [typ("TYP1"), comma_typ(), typ("...")]),
        (
          tuple3_typ.id,
          [
            typ("TYP1"),
            comma_typ(),
            typ("TYP2"),
            comma_typ(),
            typ("TYP3"),
          ],
        ),
      ]),
    ),
  ],
};

[@deriving (show({with_path: false}), sexp, yojson)]
type update =
  | ToggleShow
  | ToggleHighlight
  | SpecificityOpen(bool)
  | ToggleExplanationFeedback(string, feedback_option)
  | ToggleExampleFeedback(string, string, feedback_option)
  | UpdateGroupSelection(string, int);

let set_update = (docLangMessages: t, u: update): t => {
  switch (u) {
  | ToggleShow => {...docLangMessages, show: !docLangMessages.show}
  | ToggleHighlight => {
      ...docLangMessages,
      highlight: !docLangMessages.highlight,
    }
  | SpecificityOpen(b) => {...docLangMessages, specificity_open: b}
  | ToggleExplanationFeedback(id, feedback_option) =>
    let form = get_form(id, docLangMessages.forms);
    let explanation =
      switch (form.explanation.feedback, feedback_option) {
      | (Unselected, _) => {...form.explanation, feedback: feedback_option}
      | (ThumbsUp, ThumbsUp)
      | (ThumbsDown, ThumbsDown) => {
          ...form.explanation,
          feedback: Unselected,
        }
      | (ThumbsUp, ThumbsDown)
      | (ThumbsDown, ThumbsUp)
      | (_, Unselected) => {...form.explanation, feedback: feedback_option}
      };
    {
      ...docLangMessages,
      forms: update_form({...form, explanation}, docLangMessages.forms),
    };
  | ToggleExampleFeedback(id, sub_id, feedback_option) =>
    let form = get_form(id, docLangMessages.forms);
    let example = get_example(sub_id, form.examples);
    let new_example =
      switch (example.feedback, feedback_option) {
      | (Unselected, _) => {...example, feedback: feedback_option}
      | (ThumbsUp, ThumbsUp)
      | (ThumbsDown, ThumbsDown) => {...example, feedback: Unselected}
      | (ThumbsUp, ThumbsDown)
      | (ThumbsDown, ThumbsUp)
      | (_, Unselected) => {...example, feedback: feedback_option}
      };
    {
      ...docLangMessages,
      forms:
        update_form(
          {...form, examples: update_example(new_example, form.examples)},
          docLangMessages.forms,
        ),
    };
  | UpdateGroupSelection(group_id, new_selection_index) => {
      ...docLangMessages,
      groups:
        update_group(group_id, new_selection_index, docLangMessages.groups),
    }
  };
};

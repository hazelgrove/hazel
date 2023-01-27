open Sexplib.Std;
open Haz3lcore;
// TODO Make unified way of using consistent metavariables for syntactic forms
// TODO Use /tau instead of ty when can do that and still have highlighting work
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

// TODO Make sure using this for all the forms that should, like wild and nil
// TODO Should this have its own ID generator or is using the Example one fine?
let cons_exp = () => Example.mk_monotile(Form.get("cons_exp"));
let cons_pat = () => Example.mk_monotile(Form.get("cons_pat"));
let seq = () => Example.mk_monotile(Form.get("cell-join"));
let exp = v =>
  Example.mk_monotile(Form.mk(Form.ss, [v], Mold.(mk_op(Exp, []))));
let pat = v =>
  Example.mk_monotile(Form.mk(Form.ss, [v], Mold.(mk_op(Pat, []))));
let typ = t =>
  Example.mk_monotile(Form.mk(Form.ss, [t], Mold.(mk_op(Typ, []))));
let tpat = v =>
  Example.mk_monotile(Form.mk(Form.ss, [v], Mold.(mk_op(TPat, []))));
let typ_pat_var = t => Example.mk_monotile(Form.mk_atomic(TPat, t));
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
let power = () => Example.mk_monotile(Form.get("power"));
let divide = () => Example.mk_monotile(Form.get("divide"));
let equals = () => Example.mk_monotile(Form.get("equals"));
let lt = () => Example.mk_monotile(Form.get("lt"));
let lte = () => Example.mk_monotile(Form.get("lte"));
let gt = () => Example.mk_monotile(Form.get("gt"));
let gte = () => Example.mk_monotile(Form.get("gte"));
let fplus = () => Example.mk_monotile(Form.get("fplus"));
let fminus = () => Example.mk_monotile(Form.get("fminus"));
let ftimes = () => Example.mk_monotile(Form.get("ftimes"));
let fpower = () => Example.mk_monotile(Form.get("fpower"));
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
let typeann = () => Example.mk_monotile(Form.get("typeann"));
let mk_fun = Example.mk_tile(Form.get("fun_"));
let mk_ap_exp = Example.mk_tile(Form.get("ap_exp"));
let mk_ap_pat = Example.mk_tile(Form.get("ap_pat"));
let mk_let = Example.mk_tile(Form.get("let_"));
let mk_tyalias = Example.mk_tile(Form.get("type_alias"));

let mk_if = Example.mk_tile(Form.get("if_"));
let mk_test = Example.mk_tile(Form.get("test"));
let mk_case = Example.mk_tile(Form.get("case"));
let mk_rule = Example.mk_tile(Form.get("rule"));
let linebreak = () => Example.mk_secondary(Secondary.linebreak);
let space = () => Example.mk_secondary(Secondary.space);

let mk_example = str => {
  switch (Printer.zipper_of_string(0, str)) {
  | None => []
  | Some((z, _)) => Zipper.zip(z)
  };
};

let empty_hole_exp_group = "empty_hole_exp_group";
let empty_hole_tpat_group = "empty_hole_tpat_group";
let empty_hole_template = (sort, str, id): form => {
  let explanation = {
    message:
      Printf.sprintf(
        "Empty hole. This marks %s that needs to be filled in.",
        str,
      ),
    feedback: Unselected,
  };
  {
    id,
    syntactic_form: [sort("EmptyHole")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};
let empty_hole_exp: form =
  empty_hole_template(exp, "an expression", "empty_hole_exp");
let empty_hole_tpat: form =
  empty_hole_template(tpat, "a type pattern", "empty_hole_tpat");

let multi_hole_exp_group = "multi_hole_exp_group";
let multi_hole_tpat_group = "multi_hole_tpat_group";

let multi_hole_template = (sort, id: string): form => {
  let explanation = {
    message: "Not recognized. This is an invalid term.",
    feedback: Unselected,
  };
  {
    id,
    syntactic_form: [sort("INVALID")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};
let multi_hole_exp: form = multi_hole_template(exp, "multi_hole_exp");
let multi_hole_tpat: form = multi_hole_template(tpat, "multi_hole_tpat");

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
  let explanation = {message: "String literal.", feedback: Unselected};
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
    term: mk_example("[(1, true), (2, false)]"),
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
      mk_list_exp([[exp("e1"), comma_exp(), space(), exp("...")]]),
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
let function_tag_group = "function_tag_group";
let function_ap_group = "function_ap_group";
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
let tag_fun_ex = {
  sub_id: "tag_fun_ex",
  term: mk_example("fun None -> 1"),
  message: "When given a None constructor argument, the function evaluates 1.",
  feedback: Unselected,
};
let ap_fun_ex = {
  sub_id: "ap_fun_ex",
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
    id: "function_exp",
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
    id: "function_empty_hole_exp",
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
    id: "function_multi_hole_exp",
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
    id: "function_wild_exp",
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
    id: "function_intlit_exp",
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
    id: "function_floatlit_exp",
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
    id: "function_boollit_exp",
    syntactic_form: form,
    expandable_id: Some(Piece.id(_pat)),
    explanation,
    examples: [boollit_fun_ex],
  };
};
let _pat = pat("StringLit");
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
    syntactic_form: [exp("e1"), comma, space(), exp("...")],
    expandable_id: Some(Piece.id(comma)),
    explanation,
    examples: [tuple_example_1, tuple_example_2],
  };
};
let _exp1 = exp("e1");
let _exp2 = exp("e2");
let tuple_exp_size2_coloring_ids =
    (~exp1_id: Id.t, ~exp2_id: Id.t): list((Id.t, Id.t)) => {
  [(Piece.id(_exp1), exp1_id), (Piece.id(_exp2), exp2_id)];
};
let tuple_exp_size2: form = {
  let explanation = {
    message: "Tuple literal. The 2-tuple has a [first](%i) and [second](%i) element.",
    feedback: Unselected,
  };
  let comma = comma_exp();
  {
    id: "tuple_exp_size2",
    syntactic_form: [_exp1, comma, space(), _exp2],
    expandable_id: Some(Piece.id(comma)),
    explanation,
    examples: [tuple_example_1],
  };
};
let _exp1 = exp("e1");
let _exp2 = exp("e2");
let _exp3 = exp("e3");
let tuple_exp_size3_coloring_ids =
    (~exp1_id: Id.t, ~exp2_id: Id.t, ~exp3_id: Id.t): list((Id.t, Id.t)) => {
  [
    (Piece.id(_exp1), exp1_id),
    (Piece.id(_exp2), exp2_id),
    (Piece.id(_exp3), exp3_id),
  ];
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
      _exp1,
      comma_exp(),
      space(),
      _exp2,
      comma,
      space(),
      _exp3,
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
    syntactic_form: [exp("x")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let tag_exp_group = "tag_exp_group";
let tag_exp: form = {
  let explanation = {
    message: "`%s` is a constructor for an algebraic data type.",
    feedback: Unselected,
  };
  {
    id: "tag_exp",
    syntactic_form: [exp("Constructor")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let let_base_exp_group = "let_base_exp_group";
let let_empty_hole_exp_group = "let_empty_hole_exp_group";
let let_empty_hole_tpat_group = "let_empty_hole_tpat_group";
let let_multi_hole_exp_group = "let_multi_hole_exp_group";
let let_multi_hole_tpat_group = "let_multi_hole_tpat_group";
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

let tyalias_exp_group = "tyalias_exp_group";
let _tpat = tpat("p");
let _typ_def = typ("ty_def");
let tyalias_base_exp_coloring_ids = (~tpat_id: Id.t, ~def_id: Id.t) => [
  (Piece.id(_tpat), tpat_id),
  (Piece.id(_typ_def), def_id),
];
let tyalias_base_exp: form = {
  let explanation = {
    message: "Type Alias expression. The [*definition*](%i) is bound to the [*name*](%i).",
    feedback: Unselected,
  };
  let form = [
    mk_tyalias([[space(), _tpat, space()], [space(), _typ_def, space()]]),
    linebreak(),
    exp("e_body"),
  ];
  {
    id: "tyalias_base_exp",
    syntactic_form: form,
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let funapp_exp_group = "funapp_exp_group";
let conapp_exp_group = "conapp_exp_group";
let funapp_exp_ex = {
  sub_id: "funapp_exp_ex",
  term: mk_example("(fun x -> x)(1)"),
  message: "The identity function is applied to 1. The argument x is bound to 1 in the function body and the body evaluates to 1.",
  feedback: Unselected,
};
// TODO Has a red box around it in the result
let conapp_exp_ex = {
  sub_id: "conapp_exp_ex",
  term: mk_example("Some(1)"),
  message: "The constructor Some is applied to 1, which evaluates to Some(1).",
  feedback: Unselected,
};
let _exp_fun = exp("e_fun");
let _exp_arg = exp("e_arg");
let funapp_exp_coloring_ids =
    (~x_id: Id.t, ~arg_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_exp_fun), x_id),
  (Piece.id(_exp_arg), arg_id),
];
let funapp_exp: form = {
  let explanation = {
    message: "Function application. Apply the [*function*](%i) to the [*argument*](%i).",
    feedback: Unselected,
  };
  {
    id: "funapp_exp",
    syntactic_form: [_exp_fun, mk_ap_exp([[_exp_arg]])],
    expandable_id: None,
    explanation,
    examples: [funapp_exp_ex],
  };
};
let _exp_con = exp("e_con");
let _exp_arg = exp("e_arg");
let conapp_exp_coloring_ids =
    (~x_id: Id.t, ~arg_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_exp_con), x_id),
  (Piece.id(_exp_arg), arg_id),
];
let conapp_exp: form = {
  let explanation = {
    message: "Constructor application. Apply the [*`%s` constructor*](%i) to the [*argument*](%i).",
    feedback: Unselected,
  };
  {
    id: "conapp_exp",
    syntactic_form: [_exp_con, mk_ap_exp([[_exp_arg]])],
    expandable_id: None,
    explanation,
    examples: [conapp_exp_ex],
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
let _exp_cond = exp("e_cond");
let _exp_then = exp("e_then");
let _exp_else = exp("e_else");
let if_exp_coloring_ids =
    (~cond_id: Id.t, ~then_id: Id.t, ~else_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_exp_cond), cond_id),
  (Piece.id(_exp_then), then_id),
  (Piece.id(_exp_else), else_id),
];
let if_exp: form = {
  let explanation = {
    message: "If expression. If the [*condition*](%i) evaluates to `true`, evaluate the [*then branch*](%i). Otherwise, evaluate the [*else branch*](%i).",
    feedback: Unselected,
  };
  {
    id: "if_exp",
    syntactic_form: [
      mk_if([
        [space(), _exp_cond, linebreak()],
        [space(), _exp_then, linebreak()],
      ]),
      space(),
      _exp_else,
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
let _exp1 = exp("e1");
let _exp2 = exp("e2");
let seq_exp_coloring_ids =
    (~exp1_id: Id.t, ~exp2_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_exp1), exp1_id),
  (Piece.id(_exp2), exp2_id),
];
let seq_exp: form = {
  let explanation = {
    message: "Expression sequence. The [left expression](%i) is evaluated, then the [right expression](%i) is evaluated.",
    feedback: Unselected,
  };
  {
    id: "seq_exp",
    syntactic_form: [_exp1, seq(), space(), _exp2],
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
let _exp_body = exp("e");
let test_exp_coloring_ids = (~body_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_exp_body), body_id),
];
let test_exp: form = {
  let explanation = {
    message: "Test expression. If the [*body*](%i) of the test evalutes to `true`, the test passes. Otherwise, the test fails.",
    feedback: Unselected,
  };
  {
    id: "test_exp",
    syntactic_form: [mk_test([[space(), _exp_body, space()]])],
    expandable_id: None,
    explanation,
    examples: [test_true_ex, test_false_ex],
  };
};

let cons_exp_group = "cons_exp_group";
let cons1_ex = {
  sub_id: "cons1_ex",
  term: mk_example("1::nil"),
  message: "A single element list of 1.",
  feedback: Unselected,
};
let cons2_ex = {
  sub_id: "cons2_ex",
  term: mk_example("true::false::nil"),
  message: "A list with two elements, true and false.",
  feedback: Unselected,
};
let _exp_hd = exp("e_hd");
let _exp_tl = exp("e_tl");
let cons_exp_coloring_ids = (~hd_id: Id.t, ~tl_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_exp_hd), hd_id),
  (Piece.id(_exp_tl), tl_id),
];
let cons_exp: form = {
  let explanation = {
    message: "Cons operator. Creates a list with [*head element*](%i) and [*tail element*](%i).",
    feedback: Unselected,
  };
  {
    id: "cons_exp",
    syntactic_form: [_exp_hd, cons_exp(), _exp_tl],
    expandable_id: None,
    explanation,
    examples: [cons1_ex, cons2_ex],
  };
};

let int_unary_minus_group = "int_unary_minus_group";
let int_plus_group = "int_plus_group";
let int_minus_group = "int_minus_group";
let int_times_group = "int_times_group";
let int_power_group = "int_power_group";
let int_divide_group = "int_divide_group";
let int_lt_group = "int_lt_group";
let int_lte_group = "int_lte_group";
let int_gt_group = "int_gt_group";
let int_gte_group = "int_gte_group";
let int_eq_group = "int_eq_group";
let float_plus_group = "float_plus_group";
let float_minus_group = "float_minus_group";
let float_times_group = "float_times_group";
let float_power_group = "float_power_group";
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
let int_power_ex = {
  sub_id: "int_power_ex",
  term: mk_example("2 ** 4"),
  message: "2 raised to 4 evaluates to 16",
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
let float_power_ex = {
  sub_id: "float_power_ex",
  term: mk_example("2. **. 4."),
  message: "2. raised to 4. evaluates to 16.",
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
let _exp = exp("e");
let int_unary_minus_exp_coloring_ids = (~exp_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_exp), exp_id),
];
let int_unary_minus_exp: form = {
  let explanation = {
    message: "Unary minus. Performs integer negation of the [*operand*](%i).",
    feedback: Unselected,
  };
  {
    id: "int_unary_minus_exp",
    syntactic_form: [unary_minus(), _exp],
    expandable_id: None,
    explanation,
    examples: [int_unary_minus_ex],
  };
};
let _binop_exp_coloring_ids =
    (sf_left_id: Id.t, sf_right_id: Id.t, ~left_id: Id.t, ~right_id: Id.t)
    : list((Id.t, Id.t)) => {
  [(sf_left_id, left_id), (sf_right_id, right_id)];
};
let _exp1 = exp("e1");
let _exp2 = exp("e2");
let int_plus_exp_coloring_ids =
    (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
  _binop_exp_coloring_ids(
    Piece.id(_exp1),
    Piece.id(_exp2),
    ~left_id,
    ~right_id,
  );
let int_plus_exp: form = {
  let explanation = {
    message: "Integer addition. Gives the sum of the [*left*](%i) and [*right*](%i) operands.",
    feedback: Unselected,
  };
  {
    id: "int_plus_exp",
    syntactic_form: [_exp1, space(), plus(), space(), _exp2],
    expandable_id: None,
    explanation,
    examples: [int_plus_ex],
  };
};
let _exp1 = exp("e1");
let _exp2 = exp("e2");
let int_minus_exp_coloring_ids =
    (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
  _binop_exp_coloring_ids(
    Piece.id(_exp1),
    Piece.id(_exp2),
    ~left_id,
    ~right_id,
  );
let int_minus_exp: form = {
  let explanation = {
    message: "Integer subtraction. Gives the difference of the [*left*](%i) and [*right*](%i) operands.",
    feedback: Unselected,
  };
  {
    id: "int_minus_exp",
    syntactic_form: [_exp1, space(), minus(), space(), _exp2],
    expandable_id: None,
    explanation,
    examples: [int_minus_ex],
  };
};
let _exp1 = exp("e1");
let _exp2 = exp("e2");
let int_times_exp_coloring_ids =
    (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
  _binop_exp_coloring_ids(
    Piece.id(_exp1),
    Piece.id(_exp2),
    ~left_id,
    ~right_id,
  );
let int_times_exp: form = {
  let explanation = {
    message: "Integer multiplication. Gives the product of the [*left*](%i) and [*right*](%i) operands.",
    feedback: Unselected,
  };
  {
    id: "int_times_exp",
    syntactic_form: [_exp1, space(), times(), space(), _exp2],
    expandable_id: None,
    explanation,
    examples: [int_times_ex],
  };
};
let int_power_exp_coloring_ids =
    (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
  _binop_exp_coloring_ids(
    Piece.id(_exp1),
    Piece.id(_exp2),
    ~left_id,
    ~right_id,
  );
let int_power_exp: form = {
  let explanation = {
    message: "Integer exponentiation. Gives the result of raising [*left*](%i) ro the [*right*](%i).",
    feedback: Unselected,
  };
  {
    id: "int_power_exp",
    syntactic_form: [_exp1, space(), power(), space(), _exp2],
    expandable_id: None,
    explanation,
    examples: [int_power_ex],
  };
};
let _exp1 = exp("e1");
let _exp2 = exp("e2");
let int_divide_exp_coloring_ids =
    (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
  _binop_exp_coloring_ids(
    Piece.id(_exp1),
    Piece.id(_exp2),
    ~left_id,
    ~right_id,
  );
let int_divide_exp: form = {
  let explanation = {
    message: "Integer division. Gives the quotient of the [*left*](%i) and [*right*](%i) operands.",
    feedback: Unselected,
  };
  {
    id: "int_divide_exp",
    syntactic_form: [_exp1, space(), divide(), space(), _exp2],
    expandable_id: None,
    explanation,
    examples: [int_divide_ex],
  };
};
let _exp1 = exp("e1");
let _exp2 = exp("e2");
let int_lt_exp_coloring_ids =
    (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
  _binop_exp_coloring_ids(
    Piece.id(_exp1),
    Piece.id(_exp2),
    ~left_id,
    ~right_id,
  );
let int_lt_exp: form = {
  let explanation = {
    message: "Integer less than. If the [*left operand*](%i) is less than the [*right operand*](%i), evaluates to `true`. Otherwise evaluates to `false`.",
    feedback: Unselected,
  };
  {
    id: "int_lt_exp",
    syntactic_form: [_exp1, space(), lt(), space(), _exp2],
    expandable_id: None,
    explanation,
    examples: [int_lt1_ex, int_lt2_ex],
  };
};
let _exp1 = exp("e1");
let _exp2 = exp("e2");
let int_lte_exp_coloring_ids =
    (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
  _binop_exp_coloring_ids(
    Piece.id(_exp1),
    Piece.id(_exp2),
    ~left_id,
    ~right_id,
  );
let int_lte_exp: form = {
  let explanation = {
    message: "Integer less than or equal to. If the [*left operand*](%i) is less than or equal to the [*right operand*](%i), evaluates to `true`. Otherwise evaluates to `false`.",
    feedback: Unselected,
  };
  {
    id: "int_lte_exp",
    syntactic_form: [_exp1, space(), lte(), space(), _exp2],
    expandable_id: None,
    explanation,
    examples: [int_lte1_ex, int_lte2_ex, int_lte3_ex],
  };
};
let _exp1 = exp("e1");
let _exp2 = exp("e2");
let int_gt_exp_coloring_ids =
    (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
  _binop_exp_coloring_ids(
    Piece.id(_exp1),
    Piece.id(_exp2),
    ~left_id,
    ~right_id,
  );
let int_gt_exp: form = {
  let explanation = {
    message: "Integer greater than. If the [*left operand*](%i) is greater than the [*right operand*](%i), evaluates to `true`. Otherwise evaluates to `false`.",
    feedback: Unselected,
  };
  {
    id: "int_gt_exp",
    syntactic_form: [_exp1, space(), gt(), space(), _exp2],
    expandable_id: None,
    explanation,
    examples: [int_gt1_ex, int_gt2_ex],
  };
};
let _exp1 = exp("e1");
let _exp2 = exp("e2");
let int_gte_exp_coloring_ids =
    (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
  _binop_exp_coloring_ids(
    Piece.id(_exp1),
    Piece.id(_exp2),
    ~left_id,
    ~right_id,
  );
let int_gte_exp: form = {
  let explanation = {
    message: "Integer greater than or equal to. If the [*left operand*](%i) is greater than or equal to the [*right operand*](%i), evaluates to `true`. Otherwise evaluates to `false`.",
    feedback: Unselected,
  };
  {
    id: "int_gte_exp",
    syntactic_form: [_exp1, space(), gte(), space(), _exp2],
    expandable_id: None,
    explanation,
    examples: [int_gte1_ex, int_gte2_ex, int_gte3_ex],
  };
};
let _exp1 = exp("e1");
let _exp2 = exp("e2");
let int_eq_exp_coloring_ids =
    (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
  _binop_exp_coloring_ids(
    Piece.id(_exp1),
    Piece.id(_exp2),
    ~left_id,
    ~right_id,
  );
let int_eq_exp: form = {
  let explanation = {
    message: "Integer equality. If the [*left operand*](%i) is equal to the [*right operand*](%i), evaluates to `true`. Otherwise, evaluates to `false`.",
    feedback: Unselected,
  };
  {
    id: "int_eq_exp",
    syntactic_form: [_exp1, space(), equals(), space(), _exp2],
    expandable_id: None,
    explanation,
    examples: [int_eq1_ex, int_eq2_ex],
  };
};
let _exp1 = exp("e1");
let _exp2 = exp("e2");
let float_plus_exp_coloring_ids =
    (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
  _binop_exp_coloring_ids(
    Piece.id(_exp1),
    Piece.id(_exp2),
    ~left_id,
    ~right_id,
  );
let float_plus_exp: form = {
  let explanation = {
    message: "Floating-point addition. Gives the sum of the [*left*](%i) and [*right*](%i) operands.",
    feedback: Unselected,
  };
  {
    id: "float_plus_exp",
    syntactic_form: [_exp1, space(), fplus(), space(), _exp2],
    expandable_id: None,
    explanation,
    examples: [float_plus_ex],
  };
};
let _exp1 = exp("e1");
let _exp2 = exp("e2");
let float_minus_exp_coloring_ids =
    (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
  _binop_exp_coloring_ids(
    Piece.id(_exp1),
    Piece.id(_exp2),
    ~left_id,
    ~right_id,
  );
let float_minus_exp: form = {
  let explanation = {
    message: "Floating-point subtraction. Gives the difference of the [*left*](%i) and [*right*](%i) operands.",
    feedback: Unselected,
  };
  {
    id: "float_minus_exp",
    syntactic_form: [_exp1, space(), fminus(), space(), _exp2],
    expandable_id: None,
    explanation,
    examples: [float_minus_ex],
  };
};
let _exp1 = exp("e1");
let _exp2 = exp("e2");
let float_times_exp_coloring_ids =
    (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
  _binop_exp_coloring_ids(
    Piece.id(_exp1),
    Piece.id(_exp2),
    ~left_id,
    ~right_id,
  );
let float_times_exp: form = {
  let explanation = {
    message: "Floating-point multiplication. Gives the product of the [*left*](%i) and [*right*](%i) operands.",
    feedback: Unselected,
  };
  {
    id: "float_times_exp",
    syntactic_form: [_exp1, space(), ftimes(), space(), _exp2],
    expandable_id: None,
    explanation,
    examples: [float_times_ex],
  };
};
let float_power_exp_coloring_ids =
    (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
  _binop_exp_coloring_ids(
    Piece.id(_exp1),
    Piece.id(_exp2),
    ~left_id,
    ~right_id,
  );
let float_power_exp: form = {
  let explanation = {
    message: "Floating-point exponentiation.  Gives the result of raising [*left*](%i) to the [*right*](%i).",
    feedback: Unselected,
  };
  {
    id: "float_power_exp",
    syntactic_form: [_exp1, space(), fpower(), space(), _exp2],
    expandable_id: None,
    explanation,
    examples: [float_power_ex],
  };
};
let _exp1 = exp("e1");
let _exp2 = exp("e2");
let float_divide_exp_coloring_ids =
    (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
  _binop_exp_coloring_ids(
    Piece.id(_exp1),
    Piece.id(_exp2),
    ~left_id,
    ~right_id,
  );
let float_divide_exp: form = {
  let explanation = {
    message: "Floating-point division. Gives the quotient of the [*left*](%i) and [*right*](%i) operands.",
    feedback: Unselected,
  };
  {
    id: "float_divide_exp",
    syntactic_form: [_exp1, space(), fdivide(), space(), _exp2],
    expandable_id: None,
    explanation,
    examples: [float_divide_ex],
  };
};
let _exp1 = exp("e1");
let _exp2 = exp("e2");
let float_lt_exp_coloring_ids =
    (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
  _binop_exp_coloring_ids(
    Piece.id(_exp1),
    Piece.id(_exp2),
    ~left_id,
    ~right_id,
  );
let float_lt_exp: form = {
  let explanation = {
    message: "Floating-point less than. If the [*left operand*](%i) is less than the [*right operand*](%i), evaluates to `true`. Otherwise evaluates to `false`.",
    feedback: Unselected,
  };
  {
    id: "float_lt_exp",
    syntactic_form: [_exp1, space(), flt(), space(), _exp2],
    expandable_id: None,
    explanation,
    examples: [float_lt1_ex, float_lt2_ex],
  };
};
let _exp1 = exp("e1");
let _exp2 = exp("e2");
let float_lte_exp_coloring_ids =
    (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
  _binop_exp_coloring_ids(
    Piece.id(_exp1),
    Piece.id(_exp2),
    ~left_id,
    ~right_id,
  );
let float_lte_exp: form = {
  let explanation = {
    message: "Floating-point less than or equal to. If the [*left operand*](%i) is less than or equal to the [*right operand*](%i), evaluates to `true`. Otherwise evaluates to `false`.",
    feedback: Unselected,
  };
  {
    id: "float_lte_exp",
    syntactic_form: [_exp1, space(), flte(), space(), _exp2],
    expandable_id: None,
    explanation,
    examples: [float_lte1_ex, float_lte2_ex, float_lte3_ex],
  };
};
let _exp1 = exp("e1");
let _exp2 = exp("e2");
let float_gt_exp_coloring_ids =
    (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
  _binop_exp_coloring_ids(
    Piece.id(_exp1),
    Piece.id(_exp2),
    ~left_id,
    ~right_id,
  );
let float_gt_exp: form = {
  let explanation = {
    message: "Floating-point greater than. If the [*left operand*](%i) is greater than the [*right operand*](%i), evaluates to `true`. Otherwise evaluates to `false`.",
    feedback: Unselected,
  };
  {
    id: "float_gt_exp",
    syntactic_form: [_exp1, space(), fgt(), space(), _exp2],
    expandable_id: None,
    explanation,
    examples: [float_gt1_ex, float_gt2_ex],
  };
};
let _exp1 = exp("e1");
let _exp2 = exp("e2");
let float_gte_exp_coloring_ids =
    (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
  _binop_exp_coloring_ids(
    Piece.id(_exp1),
    Piece.id(_exp2),
    ~left_id,
    ~right_id,
  );
let float_gte_exp: form = {
  let explanation = {
    message: "Floating-point greater than or equal to. If the [*left operand*](%i) is greater than or equal to the [*right operand*](%i), evaluates to `true`. Otherwise evaluates to `false`.",
    feedback: Unselected,
  };
  {
    id: "float_gte_exp",
    syntactic_form: [_exp1, space(), fgte(), space(), _exp2],
    expandable_id: None,
    explanation,
    examples: [float_gte1_ex, float_gte2_ex, float_gte3_ex],
  };
};
let _exp1 = exp("e1");
let _exp2 = exp("e2");
let float_eq_exp_coloring_ids =
    (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
  _binop_exp_coloring_ids(
    Piece.id(_exp1),
    Piece.id(_exp2),
    ~left_id,
    ~right_id,
  );
let float_eq_exp: form = {
  let explanation = {
    message: "Floating-point equality. If the [*left operand*](%i) is equal to the [*right operand*](%i), evaluates to `true`. Otherwise, evaluates to `false`.",
    feedback: Unselected,
  };
  {
    id: "float_eq_exp",
    syntactic_form: [_exp1, space(), fequals(), space(), _exp2],
    expandable_id: None,
    explanation,
    examples: [float_eq1_ex, float_eq2_ex],
  };
};
let _exp1 = exp("e1");
let _exp2 = exp("e2");
let bool_and_exp_coloring_ids =
    (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
  _binop_exp_coloring_ids(
    Piece.id(_exp1),
    Piece.id(_exp2),
    ~left_id,
    ~right_id,
  );
let bool_and_exp: form = {
  let explanation = {
    message: "Boolean and. If the [*left operand*](%i) evaluates to `true`, evaluate the [*right operand*](%i). If that also evaluates to `true`, the whole expression evaluates to `true`. Otherwise, evaluates to `false`.",
    feedback: Unselected,
  };
  {
    id: "bool_and_exp",
    syntactic_form: [_exp1, space(), logical_and(), space(), _exp2],
    expandable_id: None,
    explanation,
    examples: [bool_and1_ex, bool_and2_ex],
  };
};
let _exp1 = exp("e1");
let _exp2 = exp("e2");
let bool_or_exp_coloring_ids =
    (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
  _binop_exp_coloring_ids(
    Piece.id(_exp1),
    Piece.id(_exp2),
    ~left_id,
    ~right_id,
  );
// TODO Some of the examples are evaluating weirdly and can't type the || in the editor
let bool_or_exp: form = {
  let explanation = {
    message: "Boolean or. If the [*left operand*](%i) evaluates to `true`, the whole expression evaluates to `true`. Otherwise, evaluate the [*right operand*](%i). If that evaluates to `true`, the whole expression evaluates to `true`. Otherwise, evaluates to `false`.",
    feedback: Unselected,
  };
  {
    id: "bool_or_exp",
    syntactic_form: [_exp1, space(), logical_or(), space(), _exp2],
    expandable_id: None,
    explanation,
    examples: [bool_or1_ex, bool_or2_ex],
  };
};
let _exp1 = exp("e1");
let _exp2 = exp("e2");
let str_eq_exp_coloring_ids =
    (~left_id: Id.t, ~right_id: Id.t): list((Id.t, Id.t)) =>
  _binop_exp_coloring_ids(
    Piece.id(_exp1),
    Piece.id(_exp2),
    ~left_id,
    ~right_id,
  );
let str_eq_exp: form = {
  let explanation = {
    message: "String equality. If the [*left operand*](%i) is equal to the [*right operand*](%i), evaluates to `true`. Otherwise, evaluates to `false`.",
    feedback: Unselected,
  };
  {
    id: "str_eq_exp",
    syntactic_form: [_exp1, space(), sequals(), space(), _exp2],
    expandable_id: None,
    explanation,
    examples: [str_eq1_ex, str_eq2_ex],
  };
};

let case_exp_group = "case_exp_group";
let case_rules_group = "case_rules_group";
let case_example_wild_simple = {
  sub_id: "case_example_wild_simple",
  term: mk_example("case 1 \n| 2 => 3 \n| _ => 4 \nend"),
  message: "The scrutinee of the case expression is 1. Since the scrutinee does not match the the first pattern 2. Since the scrutinee does match the second pattern which is a wildcard, the second branch is taken. The whole expression evaluates to the second clause 4.",
  feedback: Unselected,
};
let case_example_wild_tuple = {
  sub_id: "case_example_wild_tuple",
  term: mk_example("case (1, 2) \n| (_, 2) => 3 \n| _ => 4 \nend"),
  message: "The scrutinee of the case expression is (1, 2). Since the scrutinee matches the first pattern (_, 2), the first branch is taken. This pattern is matched because the first element 1 matches the first element pattern, which is a wildcard, and the second element 2 matches the second element pattern 2. The whole expression evaluates to the first clause 3.",
  feedback: Unselected,
};
let case_example_int = {
  sub_id: "case_example_int",
  term: mk_example("case 1 \n| 1 => 1.1 \n| 2 => 2.2 \n| _ => 3.3 \nend"),
  message: "The scrutinee of the case expression is 1. Since the scrutinee matches the first pattern 1, the first branch is taken. The whole expression evaluates to the first clause 1.1.",
  feedback: Unselected,
};
let case_example_bool = {
  sub_id: "case_example_bool",
  term: mk_example("case false \n| true => 1 | \nfalse => 2 \nend"),
  message: "The scrutinee of the case expression is false. The scrutinee does not match the first pattern true. Since, scrutinee does match the second pattern false, the second branch is taken. The whole expression evaluates to the second clause 2.",
  feedback: Unselected,
};
// TODO - I don't think changing specificity on the number of cases is really the most
// beneficial specificity change - I think instead have generic at top level
// and then have a slightly different setup for specific that is created more
// dynamically calling setup methods here but more
// work done in the LangDoc code - maybe just up to 3 or 4 branches?
let _exp_scrut = exp("e_scrut");
let case_exp_coloring_ids = (~scrut_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_exp_scrut), scrut_id),
];
let case_exp: form = {
  let explanation = {
    message: "Case expression. Consider each branch in order. For the first branch with a *pattern* that matches the [*scrutinee*](%i), evaluates to the corresponding *clause*.",
    feedback: Unselected,
  };
  let case =
    mk_case([
      [
        space(),
        _exp_scrut,
        linebreak(),
        mk_rule([[space(), pat("p1"), space()]]),
        space(),
        exp("e1"),
        linebreak(),
        mk_rule([[space(), pat("..."), space()]]),
        space(),
        exp("..."),
        linebreak(),
      ],
    ]);
  {
    id: "case_exp",
    syntactic_form: [case],
    expandable_id: Some(Piece.id(case)),
    explanation,
    examples: [case_example_int, case_example_bool],
  };
};
/*let case_exp_rules: form = {
    let explanation = {
      message: "Case expression. Consider each branch in order. If the [*scrutinee*] matches:",
      feedback: Unselected,
    };
    let case =
      mk_case([
        [
          exp("EXP_scrut"),
          mk_rule([[pat("PAT1")]]),
          exp("EXP1"),
          mk_rule([[pat("...")]]),
          exp("..."),
        ],
      ]);
    {
      id: "case_exp_rules",
      syntactic_form: [case],
      expandable_id: Some(Piece.id(case)),
      explanation,
      examples: [case_example_int, case_example_bool],
    };
  };*/

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
      mk_list_pat([[pat("p1"), comma_pat(), space(), pat("...")]]),
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
let _pat_hd = pat("p_hd");
let _pat_tl = pat("p_tl");
let cons_base_pat_coloring_ids =
    (~hd_id: Id.t, ~tl_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_pat_hd), hd_id),
  (Piece.id(_pat_tl), tl_id),
];
let cons_base_pat: form = {
  let explanation = {
    message: "Non-empty list pattern. Only expressions that are non-empty lists with *head element* matching the [*head element pattern*](%i) and *tail* list matching the [*tail pattern*](%i) match this non-empty list pattern.",
    feedback: Unselected,
  };
  {
    id: "cons_base_pat",
    syntactic_form: [_pat_hd, cons_pat(), _pat_tl],
    expandable_id: Some(Piece.id(_pat_tl)),
    explanation,
    examples: [],
  };
};
let _pat_fst = pat("p_fst");
let _pat_snd = pat("p_snd");
let _pat_tl = pat("p_tl");
let cons2_pat_coloring_ids =
    (~fst_id: Id.t, ~snd_id: Id.t, ~tl_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_pat_fst), fst_id),
  (Piece.id(_pat_snd), snd_id),
  (Piece.id(_pat_tl), tl_id),
];
let cons2_pat: form = {
  let explanation = {
    message: "Non-empty list pattern. Only expressions that are non-empty lists with *first element* matching the [*first element pattern*](%i), *second element* matching the [*second element pattern*](%i), and *tail* list matching the [*tail pattern*](%i) match this non-empty list pattern.",
    feedback: Unselected,
  };
  let c = cons_pat();
  {
    id: "cons2_pat",
    syntactic_form: [_pat_fst, cons_pat(), _pat_snd, c, _pat_tl],
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
    syntactic_form: [pat("x")],
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
    syntactic_form: [pat("p1"), comma, space(), pat("...")],
    expandable_id: Some(Piece.id(comma)),
    explanation,
    examples: [],
  };
};
let _pat1 = pat("p1");
let _pat2 = pat("p2");
let tuple_pat_size2_coloring_ids =
    (~elem1_id: Id.t, ~elem2_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_pat1), elem1_id),
  (Piece.id(_pat2), elem2_id),
];
let tuple_pat_size2: form = {
  let explanation = {
    message: "Tuple pattern. Only expressions that are 2-tuples with first element matching the [first element pattern](%i) and second element matching the [second element pattern](%i) match this tuple pattern.",
    feedback: Unselected,
  };
  let comma = comma_pat();
  {
    id: "tuple_pat_size2",
    syntactic_form: [_pat1, comma, space(), _pat2],
    expandable_id: Some(Piece.id(comma)),
    explanation,
    examples: [],
  };
};
let _pat1 = pat("p1");
let _pat2 = pat("p2");
let _pat3 = pat("p3");
let tuple_pat_size3_coloring_ids =
    (~elem1_id: Id.t, ~elem2_id: Id.t, ~elem3_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_pat1), elem1_id),
  (Piece.id(_pat2), elem2_id),
  (Piece.id(_pat3), elem3_id),
];
let tuple_pat_size3: form = {
  let explanation = {
    message: "Tuple pattern. Only expressions that are 3-tuples with first element matching the [first element pattern](%i), second element matching the [second element pattern](%i), and third element matching the [third element pattern](%i) match this tuple pattern.",
    feedback: Unselected,
  };
  let comma = comma_pat();
  {
    id: "tuple_pat_size3",
    syntactic_form: [
      _pat1,
      comma_pat(),
      space(),
      _pat2,
      comma,
      space(),
      _pat3,
    ],
    expandable_id: Some(Piece.id(comma)),
    explanation,
    examples: [],
  };
};

let tag_pat_group = "tag_pat_group";
let tag_pat: form = {
  let explanation = {
    message: "Constructor pattern. Only expressions that match the *`%s` constructor* match this constructor pattern.",
    feedback: Unselected,
  };
  {
    id: "tag_pat",
    syntactic_form: [pat("Constructor")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let ap_pat_group = "ap_pat_group";
let _pat_con = pat("p_con");
let _pat_arg = pat("p_arg");
let ap_pat_coloring_ids = (~con_id: Id.t, ~arg_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_pat_con), con_id),
  (Piece.id(_pat_arg), arg_id),
];
let ap_pat: form = {
  let explanation = {
    message: "Constructor application pattern. Only expressions that match the [*constructor*](%i) with an *argument* matching the [*argument pattern*](%i) match this *constructor application pattern*.",
    feedback: Unselected,
  };
  {
    id: "ap_pat",
    syntactic_form: [_pat_con, mk_ap_pat([[_pat_arg]])],
    expandable_id: None,
    explanation,
    examples: [],
  };
};
let typann_pat_group = "typann_pat_group";
let _pat = pat("p");
let _typ = typ("ty");
let typann_pat_coloring_ids =
    (~pat_id: Id.t, ~typ_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_pat), pat_id),
  (Piece.id(_typ), typ_id),
];
let typann_pat: form = {
  let explanation = {
    message: "Type annotation pattern. Only expressions that match the [type annotated pattern](%i) and have the [indicated type](%i) match this type annotation pattern.",
    feedback: Unselected,
  };
  {
    id: "typann_pat",
    syntactic_form: [_pat, space(), typeann(), space(), _typ],
    expandable_id: None,
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
let _typ_elem = typ("ty_elem");
// TODO Syntactic form coloring looks off for this one and other types ones...
let list_typ_coloring_ids = (~elem_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_typ_elem), elem_id),
];
let list_typ: form = {
  let explanation = {
    message: "List type. The list type classifies lists with elements with the corresponding [*element type*](%i).",
    feedback: Unselected,
  };
  {
    id: "list_typ",
    syntactic_form: [mk_list_typ([[_typ_elem]])],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let arrow_typ_group = "arrow_typ_group";
let arrow3_typ_group = "arrow3_typ_group";
let _typ_arg = typ("ty_arg");
let _typ_out = typ("ty_out");
let arrow_typ_coloring_ids =
    (~arg_id: Id.t, ~result_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_typ_arg), arg_id),
  (Piece.id(_typ_out), result_id),
];
let arrow_typ: form = {
  let explanation = {
    message: "Arrow type. This arrow type classifies functions with [*argument type*](%i) and [*output type*](%i).",
    feedback: Unselected,
  };
  {
    id: "arrow_typ",
    syntactic_form: [_typ_arg, space(), arrow(), space(), _typ_out],
    expandable_id: Some(Piece.id(_typ_out)),
    explanation,
    examples: [],
  };
};
let _typ_arg1 = typ("ty_arg1");
let _typ_arg2 = typ("ty_arg2");
let _typ_out = typ("ty_out");
let arrow3_typ_coloring_ids =
    (~arg1_id: Id.t, ~arg2_id: Id.t, ~result_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_typ_arg1), arg1_id),
  (Piece.id(_typ_arg2), arg2_id),
  (Piece.id(_typ_out), result_id),
];
let arrow3_typ: form = {
  let explanation = {
    message: "Arrow type. This arrow type classifies functions with [*first argument type*](%i), [*second argument type*](%i), and [*output type*](%i).",
    feedback: Unselected,
  };
  let arrow2 = arrow();
  {
    id: "arrow3_typ",
    syntactic_form: [
      _typ_arg1,
      space(),
      arrow(),
      space(),
      _typ_arg2,
      space(),
      arrow2,
      space(),
      _typ_out,
    ],
    expandable_id: Some(Piece.id(arrow2)),
    explanation,
    examples: [],
  };
};

let labelled_sum_typ_group = "labelled_sum_typ_group";
let labelled_sum_typ: form = {
  let explanation = {
    message: "Sum type. This type combines one or more labelled types, each consisting of a constructor name and (optionally) a type parameter, into a single type of alternatives.",
    feedback: Unselected,
  };
  let divider = Example.mk_monotile(Form.get("typ_plus"));
  {
    id: "labelled_sum_typ",
    syntactic_form: [
      space(),
      typ("Cons(ty)"),
      space(),
      divider,
      space(),
      typ("..."),
      space(),
    ],
    expandable_id: Some(Piece.id(divider)),
    explanation,
    examples: [],
  };
};
let sum_typ_unary_constructor_def_group = "sum_typ_unary_constructor_def_group";
let sum_typ_unary_constructor_def: form = {
  let explanation = {
    message: "Constructor application definition. This appends an optional type parameter to a sum type variant.",
    feedback: Unselected,
  };
  {
    id: "sum_typ_unary_constructor_def",
    syntactic_form: [typ("Constructor(type)")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let sum_typ_nullary_constructor_def_group = "sum_typ_nullary_constructor_def_group";
let sum_typ_nullary_constructor_def: form = {
  let explanation = {
    message: "Constructor definition. This defines a variant of a sum type. Constructor names must be unique within a sum.",
    feedback: Unselected,
  };
  {
    id: "sum_typ_nullary_constructor_def",
    syntactic_form: [typ("Constructor")],
    expandable_id: None,
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
    syntactic_form: [typ("ty1"), comma, space(), typ("...")],
    expandable_id: Some(Piece.id(comma)),
    explanation,
    examples: [],
  };
};
let _typ_elem1 = typ("ty1");
let _typ_elem2 = typ("ty2");
let tuple2_typ_coloring_ids =
    (~elem1_id: Id.t, ~elem2_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_typ_elem1), elem1_id),
  (Piece.id(_typ_elem2), elem2_id),
];
let tuple2_typ: form = {
  let explanation = {
    message: "Tuple type. This tuple type classifies 2-tuples with the first element of the [first element type](%i) and second element of the [second element type](%i).",
    feedback: Unselected,
  };
  let comma = comma_typ();
  {
    id: "tuple2_typ",
    syntactic_form: [_typ_elem1, comma, space(), _typ_elem2],
    expandable_id: Some(Piece.id(comma)),
    explanation,
    examples: [],
  };
};
let _typ_elem1 = typ("ty1");
let _typ_elem2 = typ("ty2");
let _typ_elem3 = typ("ty3");
let tuple3_typ_coloring_ids =
    (~elem1_id: Id.t, ~elem2_id: Id.t, ~elem3_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(_typ_elem1), elem1_id),
  (Piece.id(_typ_elem2), elem2_id),
  (Piece.id(_typ_elem3), elem3_id),
];
let tuple3_typ: form = {
  let explanation = {
    message: "Tuple type. This tuple type classifies 3-tuples with the first element of the [first element type](%i), second element of the [second element type](%i), and third element of the [third element type](%i).",
    feedback: Unselected,
  };
  let comma = comma_typ();
  {
    id: "tuple3_typ",
    syntactic_form: [
      _typ_elem1,
      comma_typ(),
      space(),
      _typ_elem2,
      comma,
      space(),
      _typ_elem3,
    ],
    expandable_id: Some(Piece.id(comma)),
    explanation,
    examples: [],
  };
};

let var_typ_group = "var_typ_group";
let var_typ: form = {
  let explanation = {
    message: "`%s` is a type variable reference.",
    feedback: Unselected,
  };
  {
    id: "var_typ",
    syntactic_form: [typ("T")],
    expandable_id: None,
    explanation,
    examples: [],
  };
};

let var_typ_pat_group = "var_typ_pat_group";
let var_typ_pat: form = {
  let explanation = {
    message: "`%s` is a new type variable name.",
    feedback: Unselected,
  };
  {
    id: "var_typ_pat",
    syntactic_form: [typ_pat_var("T")],
    expandable_id: None,
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

let find = (p: 'a => bool, xs: list('a), err: string): 'a =>
  switch (List.find_opt(p, xs)) {
  | Some(x) => x
  | None => failwith(err)
  };

let get_group = (group_id, doc: t) => {
  let (_, form_group) =
    find(
      ((id, _)) => id == group_id,
      doc.groups,
      "group not found: " ++ group_id,
    );
  form_group;
};

let get_form = (form_id, docs) =>
  find(({id, _}) => id == form_id, docs, "form not found: " ++ form_id);

let get_example = (example_sub_id, examples) =>
  find(
    ({sub_id, _}) => sub_id == example_sub_id,
    examples,
    "example not found: " ++ example_sub_id,
  );

let get_form_and_options = (group_id, doc: t) => {
  let form_group = get_group(group_id, doc);
  let (selected_id, _) =
    List.nth(form_group.options, form_group.current_selection);
  let form = get_form(selected_id, doc.forms);
  (form, form_group.options);
};

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
    empty_hole_tpat,
    multi_hole_exp,
    multi_hole_tpat,
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
    function_tag_exp,
    function_ap_exp,
    tuple_exp,
    tuple_exp_size2,
    tuple_exp_size3,
    var_exp,
    tag_exp,
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
    let_tag_exp,
    let_ap_exp,
    tyalias_base_exp,
    funapp_exp,
    conapp_exp,
    if_exp,
    seq_exp,
    test_exp,
    cons_exp,
    int_unary_minus_exp,
    int_plus_exp,
    int_minus_exp,
    int_times_exp,
    int_power_exp,
    int_divide_exp,
    int_lt_exp,
    int_lte_exp,
    int_gt_exp,
    int_gte_exp,
    int_eq_exp,
    float_plus_exp,
    float_minus_exp,
    float_times_exp,
    float_power_exp,
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
    tag_pat,
    ap_pat,
    typann_pat,
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
    labelled_sum_typ,
    sum_typ_unary_constructor_def,
    sum_typ_nullary_constructor_def,
    tuple_typ,
    tuple2_typ,
    tuple3_typ,
    var_typ,
    var_typ_pat,
  ],
  groups: [
    // Expressions
    (empty_hole_exp_group, init_options([(empty_hole_exp.id, [])])),
    (empty_hole_tpat_group, init_options([(empty_hole_tpat.id, [])])),
    (multi_hole_exp_group, init_options([(multi_hole_exp.id, [])])),
    (multi_hole_tpat_group, init_options([(multi_hole_tpat.id, [])])),
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
        (function_exp.id, [pat("p")]),
        (function_empty_hole_exp.id, [pat("EMPTYHOLE")]),
      ]),
    ),
    (
      function_multi_hole_group,
      init_options([
        (function_exp.id, [pat("p")]),
        (function_multi_hole_exp.id, [pat("INVALID")]),
      ]),
    ),
    (
      function_wild_group,
      init_options([
        (function_exp.id, [pat("p")]),
        (function_wild_exp.id, [pat("_")]),
      ]),
    ),
    (
      function_int_group,
      init_options([
        (function_exp.id, [pat("p")]),
        (function_intlit_exp.id, [pat("IntLit")]),
      ]),
    ),
    (
      function_float_group,
      init_options([
        (function_exp.id, [pat("p")]),
        (function_floatlit_exp.id, [pat("FloatLit")]),
      ]),
    ),
    (
      function_bool_group,
      init_options([
        (function_exp.id, [pat("p")]),
        (function_boollit_exp.id, [pat("BoolLit")]),
      ]),
    ),
    (
      function_str_group,
      init_options([
        (function_exp.id, [pat("p")]),
        (function_strlit_exp.id, [pat("StringLit")]),
      ]),
    ),
    (
      function_triv_group,
      init_options([
        (function_exp.id, [pat("p")]),
        (function_triv_exp.id, [pat("triv")]),
      ]),
    ),
    (
      function_listnil_group,
      init_options([
        (function_exp.id, [pat("p")]),
        (function_listnil_exp.id, [pat("nil")]),
      ]),
    ),
    (
      function_listlit_group,
      init_options([
        (function_exp.id, [pat("p")]),
        (
          function_listlit_exp.id,
          [mk_list_pat([[pat("p1"), comma_pat(), pat("...")]])],
        ),
      ]),
    ),
    (
      function_cons_group,
      init_options([
        (function_exp.id, [pat("p")]),
        (function_cons_exp.id, [pat("p_hd"), cons_pat(), pat("p_tl")]),
      ]),
    ),
    (
      function_var_group,
      init_options([
        (function_exp.id, [pat("p")]),
        (function_var_exp.id, [pat("x")]),
      ]),
    ),
    (
      function_tuple_group,
      init_options([
        (function_exp.id, [pat("p")]),
        (function_tuple_exp.id, [pat("p1"), comma_pat(), pat("...")]),
      ]),
    ),
    (
      function_tuple_2_group,
      init_options([
        (function_exp.id, [pat("p")]),
        (function_tuple_exp.id, [pat("p1"), comma_pat(), pat("...")]),
        (function_tuple2_exp.id, [pat("p1"), comma_pat(), pat("p2")]),
      ]),
    ),
    (
      function_tuple_3_group,
      init_options([
        (function_exp.id, [pat("p")]),
        (function_tuple_exp.id, [pat("p1"), comma_pat(), pat("...")]),
        (
          function_tuple3_exp.id,
          [pat("p1"), comma_pat(), pat("p2"), comma_pat(), pat("p3")],
        ),
      ]),
    ),
    (
      function_tag_group,
      init_options([
        (function_exp.id, [pat("p")]),
        (function_tag_exp.id, [pat("C")]),
      ]),
    ),
    (
      function_ap_group,
      init_options([
        (function_exp.id, [pat("p")]),
        (
          function_ap_exp.id,
          [pat("p_con"), mk_ap_pat([[pat("p_arg")]])],
        ),
      ]),
    ),
    (tuple_exp_group, init_options([(tuple_exp.id, [])])),
    (
      tuple_exp_2_group,
      init_options([
        (tuple_exp.id, [exp("e1"), comma_exp(), exp("...")]),
        (tuple_exp_size2.id, [exp("e1"), comma_exp(), exp("e2")]),
      ]),
    ),
    (
      tuple_exp_3_group,
      init_options([
        (tuple_exp.id, [exp("e1"), comma_exp(), exp("...")]),
        (
          tuple_exp_size3.id,
          [exp("e1"), comma_exp(), exp("e2"), comma_exp(), exp("e3")],
        ),
      ]),
    ),
    (var_exp_group, init_options([(var_exp.id, [])])),
    (tag_exp_group, init_options([(tag_exp.id, [])])),
    (let_base_exp_group, init_options([(let_base_exp.id, [])])),
    (
      let_empty_hole_exp_group,
      init_options([
        (let_base_exp.id, [pat("p")]),
        (let_empty_hole_exp.id, [pat("EmptyHole")]),
      ]),
    ),
    (
      let_multi_hole_exp_group,
      init_options([
        (let_base_exp.id, [pat("p")]),
        (let_multi_hole_exp.id, [pat("INVALID")]),
      ]),
    ),
    (
      let_wild_exp_group,
      init_options([
        (let_base_exp.id, [pat("p")]),
        (let_wild_exp.id, [pat("_")]),
      ]),
    ),
    (
      let_int_exp_group,
      init_options([
        (let_base_exp.id, [pat("p")]),
        (let_int_exp.id, [pat("IntLit")]),
      ]),
    ),
    (
      let_float_exp_group,
      init_options([
        (let_base_exp.id, [pat("p")]),
        (let_float_exp.id, [pat("FloatLit")]),
      ]),
    ),
    (
      let_bool_exp_group,
      init_options([
        (let_base_exp.id, [pat("p")]),
        (let_bool_exp.id, [pat("BoolLit")]),
      ]),
    ),
    (
      let_str_exp_group,
      init_options([
        (let_base_exp.id, [pat("p")]),
        (let_str_exp.id, [pat("StringLit")]),
      ]),
    ),
    (
      let_triv_exp_group,
      init_options([
        (let_base_exp.id, [pat("p")]),
        (let_triv_exp.id, [pat("triv")]),
      ]),
    ),
    (
      let_listlit_exp_group,
      init_options([
        (let_base_exp.id, [pat("p")]),
        (let_listlit_exp.id, [pat("p1"), comma_pat(), pat("...")]),
      ]),
    ),
    (
      let_listnil_exp_group,
      init_options([
        (let_base_exp.id, [pat("p")]),
        (let_listnil_exp.id, [pat("nil")]),
      ]),
    ),
    (
      let_cons_exp_group,
      init_options([
        (let_base_exp.id, [pat("p")]),
        (let_cons_exp.id, [pat("p_hd"), cons_pat(), pat("p_tl")]),
      ]),
    ),
    (
      let_var_exp_group,
      init_options([
        (let_base_exp.id, [pat("p")]),
        (let_var_exp.id, [pat("x")]),
      ]),
    ),
    (
      let_tuple_base_exp_group,
      init_options([
        (let_base_exp.id, [pat("p")]),
        (let_tuple_exp.id, [pat("p1"), comma_pat(), pat("...")]),
      ]),
    ),
    (
      let_tuple2_exp_group,
      init_options([
        (let_base_exp.id, [pat("p")]),
        (let_tuple_exp.id, [pat("p1"), comma_pat(), pat("...")]),
        (let_tuple2_exp.id, [pat("p1"), comma_pat(), pat("p2")]),
      ]),
    ),
    (
      let_tuple3_exp_group,
      init_options([
        (let_base_exp.id, [pat("p")]),
        (let_tuple_exp.id, [pat("p1"), comma_pat(), pat("...")]),
        (
          let_tuple3_exp.id,
          [pat("p1"), comma_pat(), pat("p2"), comma_pat(), pat("p3")],
        ),
      ]),
    ),
    (
      let_tag_exp_group,
      init_options([
        (let_base_exp.id, [pat("p")]),
        (let_tag_exp.id, [pat("C")]),
      ]),
    ),
    (
      let_ap_exp_group,
      init_options([
        (let_base_exp.id, [pat("p")]),
        (let_ap_exp.id, [pat("p_con"), mk_ap_pat([[pat("p_arg")]])]),
      ]),
    ),
    (tyalias_exp_group, init_options([(tyalias_base_exp.id, [])])),
    (funapp_exp_group, init_options([(funapp_exp.id, [])])),
    (conapp_exp_group, init_options([(conapp_exp.id, [])])),
    (if_exp_group, init_options([(if_exp.id, [])])),
    (seq_exp_group, init_options([(seq_exp.id, [])])),
    (test_group, init_options([(test_exp.id, [])])),
    (cons_exp_group, init_options([(cons_exp.id, [])])),
    (int_unary_minus_group, init_options([(int_unary_minus_exp.id, [])])),
    (int_plus_group, init_options([(int_plus_exp.id, [])])),
    (int_minus_group, init_options([(int_minus_exp.id, [])])),
    (int_times_group, init_options([(int_times_exp.id, [])])),
    (int_power_group, init_options([(int_power_exp.id, [])])),
    (int_divide_group, init_options([(int_divide_exp.id, [])])),
    (int_lt_group, init_options([(int_lt_exp.id, [])])),
    (int_lte_group, init_options([(int_lte_exp.id, [])])),
    (int_gt_group, init_options([(int_gt_exp.id, [])])),
    (int_gte_group, init_options([(int_gte_exp.id, [])])),
    (int_eq_group, init_options([(int_eq_exp.id, [])])),
    (float_plus_group, init_options([(float_plus_exp.id, [])])),
    (float_minus_group, init_options([(float_minus_exp.id, [])])),
    (float_times_group, init_options([(float_times_exp.id, [])])),
    (float_power_group, init_options([(float_power_exp.id, [])])),
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
        (cons_base_pat.id, [pat("p_tl")]),
        (cons2_pat.id, [pat("p_snd"), cons_pat(), pat("p_tl")]),
      ]),
    ),
    (var_pat_group, init_options([(var_pat.id, [])])),
    (tuple_pat_group, init_options([(tuple_pat.id, [])])),
    (
      tuple_pat_2_group,
      init_options([
        (tuple_pat.id, [pat("p1"), comma_pat(), pat("...")]),
        (tuple_pat_size2.id, [pat("p1"), comma_pat(), pat("p2")]),
      ]),
    ),
    (
      tuple_pat_3_group,
      init_options([
        (tuple_pat.id, [pat("p1"), comma_pat(), pat("...")]),
        (
          tuple_pat_size3.id,
          [pat("p1"), comma_pat(), pat("p2"), comma_pat(), pat("p3")],
        ),
      ]),
    ),
    (tag_pat_group, init_options([(tag_pat.id, [])])),
    (ap_pat_group, init_options([(ap_pat.id, [])])),
    (typann_pat_group, init_options([(typann_pat.id, [])])),
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
        (arrow_typ.id, [typ("ty_out")]),
        (arrow3_typ.id, [typ("ty_arg2"), arrow(), typ("ty_out")]),
      ]),
    ),
    (labelled_sum_typ_group, init_options([(labelled_sum_typ.id, [])])),
    (
      sum_typ_unary_constructor_def_group,
      init_options([(sum_typ_unary_constructor_def.id, [])]),
    ),
    (
      sum_typ_nullary_constructor_def_group,
      init_options([(sum_typ_nullary_constructor_def.id, [])]),
    ),
    (tuple_typ_group, init_options([(tuple_typ.id, [])])),
    (
      tuple2_typ_group,
      init_options([
        (tuple_typ.id, [typ("ty1"), comma_typ(), typ("...")]),
        (tuple2_typ.id, [typ("ty1"), comma_typ(), typ("ty2")]),
      ]),
    ),
    (
      tuple3_typ_group,
      init_options([
        (tuple_typ.id, [typ("ty1"), comma_typ(), typ("...")]),
        (
          tuple3_typ.id,
          [typ("ty1"), comma_typ(), typ("ty2"), comma_typ(), typ("ty3")],
        ),
      ]),
    ),
    (var_typ_group, init_options([(var_typ.id, [])])),
    (var_typ_pat_group, init_options([(var_typ_pat.id, [])])),
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

[@deriving (show({with_path: false}), sexp, yojson)]
type persistent_example = {
  sub_id: string,
  feedback: feedback_option,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type persistent_form = {
  id: string,
  explanation_feedback: feedback_option,
  examples: list(persistent_example),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type persistent_form_group = {
  id: string,
  current_selection: int,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type persistent_state = {
  show: bool,
  highlight: bool,
  specificity_open: bool,
  forms: list(persistent_form),
  groups: list(persistent_form_group),
};

let persist =
    ({show, highlight, specificity_open, forms, groups, _}: t)
    : persistent_state => {
  let persist_example = ({sub_id, feedback, _}: example): persistent_example => {
    {sub_id, feedback};
  };
  let persist_form = ({id, explanation, examples, _}: form): persistent_form => {
    let {feedback, _}: explanation = explanation;
    {
      id,
      explanation_feedback: feedback,
      examples: List.map(persist_example, examples),
    };
  };

  {
    show,
    highlight,
    specificity_open,
    forms: List.map(persist_form, forms),
    groups:
      List.map(
        ((group_id, group: form_group)) =>
          {id: group_id, current_selection: group.current_selection},
        groups,
      ),
  };
};

// TODO Make more robust to added messages
let unpersist =
    ({show, highlight, specificity_open, forms, groups}: persistent_state): t => {
  let unpersist_examples = (persistent_examples, examples) => {
    List.map(
      ({sub_id, feedback}: persistent_example) => {
        let init_example = get_example(sub_id, examples);
        {
          sub_id,
          term: init_example.term,
          message: init_example.message,
          feedback,
        };
      },
      persistent_examples,
    );
  };
  let forms_unpersist =
    List.map(
      ({id, explanation_feedback, examples}: persistent_form) => {
        let init_form = get_form(id, init.forms);
        {
          id,
          syntactic_form: init_form.syntactic_form,
          expandable_id: init_form.expandable_id,
          explanation: {
            message: init_form.explanation.message,
            feedback: explanation_feedback,
          },
          examples: unpersist_examples(examples, init_form.examples),
        };
      },
      forms,
    );
  let groups_unpersist =
    List.map(
      ({id, current_selection}: persistent_form_group) => {
        let init_group = get_group(id, init);
        (id, {options: init_group.options, current_selection});
      },
      groups,
    );
  {
    show,
    highlight,
    specificity_open,
    forms: forms_unpersist,
    groups: groups_unpersist,
  };
};

let serialize = (langDocMessages: t): string => {
  persist(langDocMessages)
  |> sexp_of_persistent_state
  |> Sexplib.Sexp.to_string;
};

let deserialize = (data: string) => {
  Sexplib.Sexp.of_string(data) |> persistent_state_of_sexp |> unpersist;
};

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
let let_sint_ex = {
  sub_id: Let(IntLit),
  term: mk_example("let 1 = 1 in \n2"),
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
  term: mk_example("let (x, y, z) = (1, 2, 3)\nin x + y + z"),
  message: "The variable x is bound to 1, the y is bound to 2, and the z is bound to 3, so the expression evaluates to 1 + 2 + 3, which is 6.",
};
let let_ctr_ex = {
  sub_id: Let(Ctr),
  term: mk_example("type T = None + Some(Int)\n in let None = None\nin 2"),
  message: "The None is thrown away, so the expression evaluates to 2.",
};
let let_conap_ex = {
  sub_id: Let(Ap),
  term:
    mk_example("type T = None + Some(Int)\n in let Some(a) = Some(2)\nin a"),
  message: "The a is bound to 2, so the expression evaluates to 2.",
};
let let_funap_ex = {
  sub_id: Let(Ap),
  term: mk_example("let f(x) = x*2\nin f(3)"),
  message: "The variable f is bound to a function transforming x to x * 2, so the expression evaluates to 3 * 2 = 6.",
};
let pat_def_body_let_exp_coloring_ids =
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
let pat_def_let_exp_coloring_ids =
    (sf_pat_id: Id.t, sf_def_id: Id.t, ~pat_id: Id.t, ~def_id: Id.t)
    : list((Id.t, Id.t)) => {
  [(sf_pat_id, pat_id), (sf_def_id, def_id)];
};
let p = pat("p");
let exp_def = exp("e_def");
let let_base_exp_coloring_ids =
  pat_def_let_exp_coloring_ids(Piece.id(p), Piece.id(exp_def));
let let_base_exp_id: form_id = LetExp(Base);
let let_base_exp_form = [
  mk_let([[space(), p, space()], [space(), exp_def, space()]]),
  linebreak(),
  exp("e_body"),
];
let let_base_exp_explanation = (~def_id: Id.t, ~pat_id: Id.t): string =>
  Printf.sprintf(
    "The [*definition*](%s) is matched against the [*pattern*](%s).",
    Id.to_string(def_id),
    Id.to_string(pat_id),
  );
let let_base_exp = (~def_id: Id.t, ~pat_id: Id.t): form => {
  id: let_base_exp_id,
  syntactic_form: let_base_exp_form,
  colorings: [],
  expandable_id: Some((Piece.id(p), [pat("p")])),
  explanation: let_base_exp_explanation(~def_id, ~pat_id),
  examples: [let_base_ex],
};
let p =
  Piece.Grout({
    id: Id.mk(),
    shape: Convex,
  });
let exp_def = exp("e_def");
let let_empty_hole_exp_coloring_ids =
  pat_def_let_exp_coloring_ids(Piece.id(p), Piece.id(exp_def));
let let_empty_hole_exp_id: form_id = LetExp(EmptyHole);
let let_empty_hole_exp_form = [
  mk_let([[space(), p, space()], [space(), exp_def, space()]]),
  linebreak(),
  exp("e_body"),
];
let let_empty_hole_exp_expandable =
  Piece.Grout({
    id: Id.mk(),
    shape: Convex,
  });
let let_empty_hole_exp = (~pat_id: Id.t, ~def_id: Id.t): form => {
  id: let_empty_hole_exp_id,
  syntactic_form: let_empty_hole_exp_form,
  colorings: [],
  expandable_id: Some((Piece.id(p), [let_empty_hole_exp_expandable])),
  explanation:
    Printf.sprintf(
      "After the [*empty hole pattern*](%s) is filled, the [*definition*](%s) is matched against the [*pattern*](%s).",
      Id.to_string(pat_id),
      Id.to_string(def_id),
      Id.to_string(pat_id),
    ),
  examples: [let_base_ex],
};
let p = pat("INVALID");
let exp_def = exp("e_def");
let let_multi_hole_exp_coloring_ids =
  pat_def_let_exp_coloring_ids(Piece.id(p), Piece.id(exp_def));
let let_multi_hole_exp_id: form_id = LetExp(MultiHole);
let let_multi_hole_exp_form = [
  mk_let([[space(), p, space()], [space(), exp_def, space()]]),
  linebreak(),
  exp("e_body"),
];
let let_multi_hole_exp = (~pat_id: Id.t, ~def_id: Id.t): form => {
  id: let_multi_hole_exp_id,
  syntactic_form: let_multi_hole_exp_form,
  colorings: [],
  expandable_id: Some((Piece.id(p), [pat("INVALID")])),
  explanation:
    Printf.sprintf(
      "After the [invalid pattern](%s) is corrected, the [*definition*](%s) is matched against the [*pattern*](%s).",
      Id.to_string(pat_id),
      Id.to_string(def_id),
      Id.to_string(pat_id),
    ),
  examples: [let_base_ex],
};
let exp_def = exp("e_def");
let exp_body = exp("e_body");
let let_wild_exp_coloring_ids =
    (~def_id: Id.t, ~body_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(exp_def), def_id),
  (Piece.id(exp_body), body_id),
];
let let_wild_exp_id: form_id = LetExp(Wild);
let let_wild_exp_pat = pat("_");
let let_wild_exp_form = [
  mk_let([
    [space(), let_wild_exp_pat, space()],
    [space(), exp_def, space()],
  ]),
  linebreak(),
  exp_body,
];
let let_wild_exp = (~def_id: Id.t, ~body_id: Id.t): form => {
  id: let_wild_exp_id,
  syntactic_form: let_wild_exp_form,
  colorings: [],
  expandable_id: Some((Piece.id(let_wild_exp_pat), [pat("_")])),
  explanation:
    Printf.sprintf(
      "The [*definition*](%s) is evaluated and ignored. The [*definition*](%s) can't be referenced in the [*body*](%s).",
      Id.to_string(def_id),
      Id.to_string(def_id),
      Id.to_string(body_id),
    ),
  examples: [let_wild_ex],
};
let p = pat("IntLit");
let exp_def = exp("e_def");
let exp_body = exp("e_body");
let let_int_exp_coloring_ids =
  pat_def_body_let_exp_coloring_ids(
    Piece.id(p),
    Piece.id(exp_def),
    Piece.id(exp_body),
  );
let let_int_exp_id: form_id = LetExp(Int);
let let_int_exp_form = [
  mk_let([[space(), p, space()], [space(), exp_def, space()]]),
  linebreak(),
  exp_body,
];
let let_int_exp =
    (~def_id: Id.t, ~pat_id: Id.t, ~i: Bigint.t, ~body_id: Id.t): form => {
  id: let_int_exp_id,
  syntactic_form: let_int_exp_form,
  colorings: [],
  expandable_id: Some((Piece.id(p), [pat("IntLit")])),
  explanation:
    Printf.sprintf(
      "The only value for the [*definition*](%s) that matches the [*pattern*](%s) is `%s`. The [*definition*](%s) can't be referenced in the [*body*](%s).",
      Id.to_string(def_id),
      Id.to_string(pat_id),
      Bigint.to_string(i),
      Id.to_string(def_id),
      Id.to_string(body_id),
    ),
  examples: [let_int_ex],
};

let p = pat("SIntLit");
let exp_def = exp("e_def");
let exp_body = exp("e_body");
let let_sint_exp_coloring_ids =
  pat_def_body_let_exp_coloring_ids(
    Piece.id(p),
    Piece.id(exp_def),
    Piece.id(exp_body),
  );
let let_sint_exp_id: form_id = LetExp(SInt);
let let_sint_exp_form = [
  mk_let([[space(), p, space()], [space(), exp_def, space()]]),
  linebreak(),
  exp_body,
];
let let_sint_exp =
    (~def_id: Id.t, ~pat_id: Id.t, ~i: int, ~body_id: Id.t): form => {
  id: let_sint_exp_id,
  syntactic_form: let_sint_exp_form,
  colorings: [],
  expandable_id: Some((Piece.id(p), [pat("IntLit")])),
  explanation:
    Printf.sprintf(
      "The only value for the [*definition*](%s) that matches the [*pattern*](%s) is `%d`. The [*definition*](%s) can't be referenced in the [*body*](%s).",
      Id.to_string(def_id),
      Id.to_string(pat_id),
      i,
      Id.to_string(def_id),
      Id.to_string(body_id),
    ),
  examples: [let_sint_ex],
};

let p = pat("FloatLit");
let exp_def = exp("e_def");
let exp_body = exp("e_body");
let let_float_exp_coloring_ids =
  pat_def_body_let_exp_coloring_ids(
    Piece.id(p),
    Piece.id(exp_def),
    Piece.id(exp_body),
  );
let let_float_exp_id: form_id = LetExp(Float);
let let_float_exp_form = [
  mk_let([[space(), p, space()], [space(), exp_def, space()]]),
  linebreak(),
  exp_body,
];
let let_float_exp =
    (~def_id: Id.t, ~pat_id: Id.t, ~f: float, ~body_id: Id.t): form => {
  id: let_float_exp_id,
  syntactic_form: let_float_exp_form,
  colorings: [],
  expandable_id: Some((Piece.id(p), [pat("FloatLit")])),
  explanation:
    Printf.sprintf(
      "The only value for the [*definition*](%s) that matches the [*pattern*](%s) is `%f`. The [*definition*](%s) can't be referenced in the [*body*](%s).",
      Id.to_string(def_id),
      Id.to_string(pat_id),
      f,
      Id.to_string(def_id),
      Id.to_string(body_id),
    ),
  examples: [let_float_ex],
};
let p = pat("BoolLit");
let exp_def = exp("e_def");
let exp_body = exp("e_body");
let let_bool_exp_coloring_ids =
  pat_def_body_let_exp_coloring_ids(
    Piece.id(p),
    Piece.id(exp_def),
    Piece.id(exp_body),
  );
let let_bool_exp_id: form_id = LetExp(Bool);
let let_bool_exp_form = [
  mk_let([[space(), p, space()], [space(), exp_def, space()]]),
  linebreak(),
  exp_body,
];
let let_bool_exp =
    (~def_id: Id.t, ~pat_id: Id.t, ~b: bool, ~body_id: Id.t): form => {
  id: let_bool_exp_id,
  syntactic_form: let_bool_exp_form,
  colorings: [],
  expandable_id: Some((Piece.id(p), [pat("BoolLit")])),
  explanation:
    Printf.sprintf(
      "The only value for the [*definition*](%s) that matches the [*pattern*](%s) is `%b`. The [*definition*](%s) can't be referenced in the [*body*](%s).",
      Id.to_string(def_id),
      Id.to_string(pat_id),
      b,
      Id.to_string(def_id),
      Id.to_string(body_id),
    ),
  examples: [let_bool_ex],
};
let p = pat("StringLit");
let exp_def = exp("e_def");
let exp_body = exp("e_body");
let let_str_exp_coloring_ids =
  pat_def_body_let_exp_coloring_ids(
    Piece.id(p),
    Piece.id(exp_def),
    Piece.id(exp_body),
  );
let let_str_exp_id: form_id = LetExp(String);
let let_str_exp_form = [
  mk_let([[space(), p, space()], [space(), exp_def, space()]]),
  linebreak(),
  exp_body,
];
let let_str_exp =
    (~def_id: Id.t, ~pat_id: Id.t, ~s: string, ~body_id: Id.t): form => {
  id: let_str_exp_id,
  syntactic_form: let_str_exp_form,
  colorings: [],
  expandable_id: Some((Piece.id(p), [pat("StringLit")])),
  explanation:
    Printf.sprintf(
      "The only value for the [*definition*](%s) that matches the [*pattern*](%s) is `%s`. The [*definition*](%s) can't be referenced in the [*body*](%s).",
      Id.to_string(def_id),
      Id.to_string(pat_id),
      s,
      Id.to_string(def_id),
      Id.to_string(body_id),
    ),
  examples: [let_str_ex],
};
let p = pat("()");
let exp_def = exp("e_def");
let exp_body = exp("e_body");
let let_triv_exp_coloring_ids =
  pat_def_body_let_exp_coloring_ids(
    Piece.id(p),
    Piece.id(exp_def),
    Piece.id(exp_body),
  );
let let_triv_exp_id: form_id = LetExp(Triv);
let let_triv_exp_form = [
  mk_let([[space(), p, space()], [space(), exp_def, space()]]),
  linebreak(),
  exp_body,
];
let let_triv_exp = (~def_id: Id.t, ~pat_id: Id.t, ~body_id: Id.t): form => {
  id: let_triv_exp_id,
  syntactic_form: let_triv_exp_form,
  colorings: [],
  expandable_id: Some((Piece.id(p), [pat("()")])),
  explanation:
    Printf.sprintf(
      "The only value for the [*definition*](%s) that matches the [*pattern*](%s) is the trivial value `()`. The [*definition*](%s) can't be referenced in the [*body*](%s).",
      Id.to_string(def_id),
      Id.to_string(pat_id),
      Id.to_string(def_id),
      Id.to_string(body_id),
    ),
  examples: [let_triv_ex],
};
let p = mk_list_pat([[pat("p1"), comma_pat(), space(), pat("...")]]);
let exp_def = exp("e_def");
let let_listlit_exp_coloring_ids =
  pat_def_let_exp_coloring_ids(Piece.id(p), Piece.id(exp_def));
let let_listlit_exp_id: form_id = LetExp(ListLit);
let let_listlit_exp_form = [
  mk_let([[space(), p, space()], [space(), exp_def, space()]]),
  linebreak(),
  exp("e_body"),
];
let let_listlit_exp = (~def_id: Id.t, ~pat_id: Id.t, ~n: int): form => {
  id: let_listlit_exp_id,
  syntactic_form: let_listlit_exp_form,
  colorings: [],
  expandable_id:
    Some((Piece.id(p), [pat("p1"), comma_pat(), pat("...")])),
  explanation:
    Printf.sprintf(
      "The only values for the [*definition*](%s) that match the [*pattern*](%s) are lists with %d-elements, where each element matches the corresponding element pattern.",
      Id.to_string(def_id),
      Id.to_string(pat_id),
      n,
    ),
  examples: [let_listlit_ex],
};
let p = pat("[]");
let exp_def = exp("e_def");
let exp_body = exp("e_body");
let let_listnil_exp_coloring_ids =
  pat_def_body_let_exp_coloring_ids(
    Piece.id(p),
    Piece.id(exp_def),
    Piece.id(exp_body),
  );
let let_listnil_exp_id: form_id = LetExp(ListNil);
let let_listnil_exp_form = [
  mk_let([[space(), p, space()], [space(), exp_def, space()]]),
  linebreak(),
  exp_body,
];
let let_listnil_exp = (~def_id: Id.t, ~pat_id: Id.t, ~body_id: Id.t): form => {
  id: let_listnil_exp_id,
  syntactic_form: let_listnil_exp_form,
  colorings: [],
  expandable_id: Some((Piece.id(p), [pat("[]")])),
  explanation:
    Printf.sprintf(
      "The only value for the [*definition*](%s) that matches the [*pattern*](%s) is the empty list `[]`. The [*definition*](%s) can't be referenced in the [*body*](%s).",
      Id.to_string(def_id),
      Id.to_string(pat_id),
      Id.to_string(def_id),
      Id.to_string(body_id),
    ),
  examples: [let_listnil_ex],
};
let pat_hd = pat("p_hd");
let pat_tl = pat("p_tl");
let exp_def = exp("e_def");
let let_cons_exp_coloring_ids =
    (~hd_id: Id.t, ~tl_id: Id.t, ~def_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(pat_hd), hd_id),
  (Piece.id(pat_tl), tl_id),
  (Piece.id(exp_def), def_id),
];
let let_cons_exp_id: form_id = LetExp(ListCons);
let let_cons_exp_cons = cons_pat();
let let_cons_exp_form = [
  mk_let([
    [space(), pat_hd, let_cons_exp_cons, pat_tl, space()],
    [space(), exp_def, space()],
  ]),
  linebreak(),
  exp("e_body"),
];
let let_cons_exp = (~def_id: Id.t, ~hd_id: Id.t, ~tl_id: Id.t): form => {
  id: let_cons_exp_id,
  syntactic_form: let_cons_exp_form,
  colorings: [],
  expandable_id:
    Some((
      Piece.id(let_cons_exp_cons),
      [pat("p_hd"), cons_pat(), pat("p_tl")],
    )),
  explanation:
    Printf.sprintf(
      "The only values for the [*definition*](%s) that match the *pattern* are non-empty lists that match the [*head*](%s) and [*tail*](%s) patterns.",
      Id.to_string(def_id),
      Id.to_string(hd_id),
      Id.to_string(tl_id),
    ),
  examples: [let_cons_hd_ex, let_cons_snd_ex],
};
let p = pat("x");
let exp_def = exp("e_def");
let exp_body = exp("e_body");
let let_var_exp_coloring_ids =
  pat_def_body_let_exp_coloring_ids(
    Piece.id(p),
    Piece.id(exp_def),
    Piece.id(exp_body),
  );
let let_var_exp_id: form_id = LetExp(Var);
let let_var_exp_form = [
  mk_let([[space(), p, space()], [space(), exp_def, space()]]),
  linebreak(),
  exp_body,
];
let let_var_exp =
    (~def_id: Id.t, ~pat_id: Id.t, ~name: string, ~body_id: Id.t): form => {
  id: let_var_exp_id,
  syntactic_form: let_var_exp_form,
  colorings: [],
  expandable_id: Some((Piece.id(p), [pat("x")])),
  explanation:
    Printf.sprintf(
      "The [*definition*](%s) is bound to the [*variable*](%s) `%s` in the [*body*](%s).",
      Id.to_string(def_id),
      Id.to_string(pat_id),
      name,
      Id.to_string(body_id),
    ),
  examples: [let_var_ex],
  // TODO Does this example being slightly different actually add anything?
};
let comma = comma_pat();
let exp_def = exp("e_def");
let let_tuple_exp_coloring_ids =
  pat_def_let_exp_coloring_ids(Piece.id(comma), Piece.id(exp_def));
let let_tuple_exp_id: form_id = LetExp(Tuple);
let let_tuple_exp_form = [
  mk_let([
    [space(), pat("p1"), comma, space(), pat("..."), space()],
    [space(), exp_def, space()],
  ]),
  linebreak(),
  exp("e_body"),
];
let let_tuple_exp_explanation =
    (~def_id: Id.t, ~pat_id: Id.t, ~n: int): string =>
  Printf.sprintf(
    "The only values for the [*definition*](%s) that match the [*pattern*](%s) are %d-tuples where each element matches the corresponding element pattern.",
    Id.to_string(def_id),
    Id.to_string(pat_id),
    n,
  );
let let_tuple_exp = (~def_id: Id.t, ~pat_id: Id.t, ~n: int): form => {
  id: let_tuple_exp_id,
  syntactic_form: let_tuple_exp_form,
  colorings: [],
  expandable_id:
    Some((Piece.id(comma), [pat("p1"), comma_pat(), pat("...")])),
  explanation: let_tuple_exp_explanation(~def_id, ~pat_id, ~n),
  examples: [let_tuple2_ex, let_tuple3_ex],
};
let pat1 = pat("p1");
let pat2 = pat("p2");
let exp_def = exp("e_def");
let let_tuple2_exp_coloring_ids =
    (~pat1_id: Id.t, ~pat2_id: Id.t, ~def_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(pat1), pat1_id),
  (Piece.id(pat2), pat2_id),
  (Piece.id(exp_def), def_id),
];
let let_tuple2_exp_id: form_id = LetExp(Tuple2);
let let_tuple2_exp_comma = comma_pat();
let let_tuple2_exp_form = [
  mk_let([
    [space(), pat1, let_tuple2_exp_comma, space(), pat2, space()],
    [space(), exp_def, space()],
  ]),
  linebreak(),
  exp("e_body"),
];
let let_tuple2_exp = (~def_id: Id.t, ~pat1_id: Id.t, ~pat2_id: Id.t): form => {
  id: let_tuple2_exp_id,
  syntactic_form: let_tuple2_exp_form,
  colorings: [],
  expandable_id:
    Some((
      Piece.id(let_tuple2_exp_comma),
      [pat("p1"), comma_pat(), pat("p2")],
    )),
  explanation:
    Printf.sprintf(
      "The only values for the [*definition*](%s) that match the *pattern* are 2-tuples where the first element matches the [*first element pattern*](%s) and the second element matches the [*second element pattern*](%s).",
      Id.to_string(def_id),
      Id.to_string(pat1_id),
      Id.to_string(pat2_id),
    ),
  examples: [let_tuple2_ex],
};
let pat1 = pat("p1");
let pat2 = pat("p2");
let pat3 = pat("p3");
let exp_def = exp("e_def");
let let_tuple3_exp_coloring_ids =
    (~pat1_id: Id.t, ~pat2_id: Id.t, ~pat3_id: Id.t, ~def_id: Id.t)
    : list((Id.t, Id.t)) => [
  (Piece.id(pat1), pat1_id),
  (Piece.id(pat2), pat2_id),
  (Piece.id(pat3), pat3_id),
  (Piece.id(exp_def), def_id),
];
let let_tuple3_exp_id: form_id = LetExp(Tuple3);
let let_tuple3_exp_comma = comma_pat();
let let_tuple3_exp_form = [
  mk_let([
    [
      space(),
      pat1,
      comma_pat(),
      space(),
      pat2,
      let_tuple3_exp_comma,
      space(),
      pat3,
      space(),
    ],
    [space(), exp_def, space()],
  ]),
  linebreak(),
  exp("e_body"),
];
let let_tuple3_exp =
    (~def_id: Id.t, ~pat1_id: Id.t, ~pat2_id: Id.t, ~pat3_id: Id.t): form => {
  id: let_tuple3_exp_id,
  syntactic_form: let_tuple3_exp_form,
  colorings: [],
  expandable_id:
    Some((
      Piece.id(let_tuple3_exp_comma),
      [pat("p1"), comma_pat(), pat("p2"), comma_pat(), pat("p3")],
    )),
  explanation:
    Printf.sprintf(
      "The only values for the [*definition*](%s) that match the *pattern* are 3-tuples where the first element matches the [*first element pattern*](%s), the second element matches the [*second element pattern*](%s), and the third element matches the [*third element pattern*](%s).",
      Id.to_string(def_id),
      Id.to_string(pat1_id),
      Id.to_string(pat2_id),
      Id.to_string(pat3_id),
    ),
  examples: [let_tuple3_ex],
};
let p = pat("C");
let exp_def = exp("e_def");
let exp_body = exp("e_body");
let let_ctr_exp_coloring_ids =
  pat_def_body_let_exp_coloring_ids(
    Piece.id(p),
    Piece.id(exp_def),
    Piece.id(exp_body),
  );
let let_ctr_exp_id: form_id = LetExp(Ctr);
let let_ctr_exp_form = [
  mk_let([[space(), p, space()], [space(), exp_def, space()]]),
  linebreak(),
  exp_body,
];
let let_ctr_exp =
    (~def_id: Id.t, ~pat_id: Id.t, ~name: string, ~body_id: Id.t): form => {
  id: let_ctr_exp_id,
  syntactic_form: let_ctr_exp_form,
  colorings: [],
  expandable_id: Some((Piece.id(p), [pat("C")])),
  explanation:
    Printf.sprintf(
      "The only value for the [*definition*](%s) that matches the [*pattern*](%s) is the *`%s` constructor*. The [*definition*](%s) can't be referenced in the [*body*](%s).",
      Id.to_string(def_id),
      Id.to_string(pat_id),
      name,
      Id.to_string(def_id),
      Id.to_string(body_id),
    ),
  examples: [let_ctr_ex],
};
let pat_con = pat("p_con");
let pat_arg = pat("p_arg");
let exp_def = exp("e_def");
let let_conap_exp_coloring_ids =
    (~x_id: Id.t, ~arg_id: Id.t, ~def_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(pat_con), x_id),
  (Piece.id(pat_arg), arg_id),
  (Piece.id(exp_def), def_id),
];
let let_conap_exp_id: form_id = LetExp(ApCons);
let let_conap_exp_ap = mk_ap_pat([[pat_arg]]);
let let_conap_exp_form = [
  mk_let([
    [space(), pat_con, let_conap_exp_ap, space()],
    [space(), exp_def, space()],
  ]),
  linebreak(),
  exp("e_body"),
];
let let_conap_exp = (~def_id: Id.t, ~x_id: Id.t, ~arg_id: Id.t): form => {
  id: let_conap_exp_id,
  syntactic_form: let_conap_exp_form,
  colorings: [],
  expandable_id:
    Some((Piece.id(let_conap_exp_ap), [pat_con, mk_ap_pat([[pat_arg]])])),
  explanation:
    Printf.sprintf(
      "The only values for the [*definition*](%s) that match the *pattern* are the [*constructor*](%s) where the *argument* matches the [*argument pattern*](%s).",
      Id.to_string(def_id),
      Id.to_string(x_id),
      Id.to_string(arg_id),
    ),
  examples: [let_conap_ex],
};

let pat_fun = pat("p_fun");
let pat_arg = pat("p_arg");
let exp_def = exp("e_def");
let let_funap_exp_coloring_ids =
    (~x_id: Id.t, ~arg_id: Id.t, ~def_id: Id.t): list((Id.t, Id.t)) => [
  (Piece.id(pat_fun), x_id),
  (Piece.id(pat_arg), arg_id),
  (Piece.id(exp_def), def_id),
];
let let_funap_exp_id: form_id = LetExp(ApFunc);
let let_funap_exp_ap = mk_ap_pat([[pat_arg]]);
let let_funap_exp_form = [
  mk_let([
    [space(), pat_fun, let_funap_exp_ap, space()],
    [space(), exp_def, space()],
  ]),
  linebreak(),
  exp("e_body"),
];
let let_funap_exp = (~def_id: Id.t, ~x_id: Id.t, ~arg_id: Id.t): form => {
  id: let_funap_exp_id,
  syntactic_form: let_funap_exp_form,
  colorings: [],
  expandable_id:
    Some((Piece.id(let_funap_exp_ap), [pat_fun, mk_ap_pat([[pat_arg]])])),
  explanation:
    Printf.sprintf(
      "The only values for the [*definition*](%s) that match the *pattern* are the [*function*](%s) where the *argument* matches the [*argument pattern*](%s).",
      Id.to_string(def_id),
      Id.to_string(x_id),
      Id.to_string(arg_id),
    ),
  examples: [let_funap_ex],
};

let lets_emptyhole = (~def_id: Id.t, ~pat_id: Id.t): group => {
  id: LetExp(EmptyHole),
  forms: [
    let_empty_hole_exp(~pat_id, ~def_id),
    let_base_exp(~def_id, ~pat_id),
  ],
};

let lets_multihole = (~def_id: Id.t, ~pat_id: Id.t): group => {
  id: LetExp(MultiHole),
  forms: [
    let_multi_hole_exp(~pat_id, ~def_id),
    let_base_exp(~def_id, ~pat_id),
  ],
};

let lets_wild = (~def_id: Id.t, ~pat_id: Id.t, ~body_id: Id.t): group => {
  id: LetExp(Wild),
  forms: [let_wild_exp(~def_id, ~body_id), let_base_exp(~def_id, ~pat_id)],
};

let lets_int =
    (~def_id: Id.t, ~pat_id: Id.t, ~i: Bigint.t, ~body_id: Id.t): group => {
  id: LetExp(Int),
  forms: [
    let_int_exp(~def_id, ~pat_id, ~i, ~body_id),
    let_base_exp(~def_id, ~pat_id),
  ],
};

let lets_sint = (~def_id: Id.t, ~pat_id: Id.t, ~i: int, ~body_id: Id.t): group => {
  id: LetExp(SInt),
  forms: [
    let_sint_exp(~def_id, ~pat_id, ~i, ~body_id),
    let_base_exp(~def_id, ~pat_id),
  ],
};

let lets_float =
    (~def_id: Id.t, ~pat_id: Id.t, ~f: float, ~body_id: Id.t): group => {
  id: LetExp(Float),
  forms: [
    let_float_exp(~def_id, ~pat_id, ~f, ~body_id),
    let_base_exp(~def_id, ~pat_id),
  ],
};

let lets_bool =
    (~def_id: Id.t, ~pat_id: Id.t, ~b: bool, ~body_id: Id.t): group => {
  id: LetExp(Bool),
  forms: [
    let_bool_exp(~def_id, ~pat_id, ~b, ~body_id),
    let_base_exp(~def_id, ~pat_id),
  ],
};

let lets_str =
    (~def_id: Id.t, ~pat_id: Id.t, ~s: string, ~body_id: Id.t): group => {
  id: LetExp(String),
  forms: [
    let_str_exp(~def_id, ~pat_id, ~s, ~body_id),
    let_base_exp(~def_id, ~pat_id),
  ],
};

let lets_triv = (~def_id: Id.t, ~pat_id: Id.t, ~body_id: Id.t): group => {
  id: LetExp(Triv),
  forms: [
    let_triv_exp(~def_id, ~pat_id, ~body_id),
    let_base_exp(~def_id, ~pat_id),
  ],
};

let lets_listlit = (~def_id: Id.t, ~pat_id: Id.t, ~n: int): group => {
  id: LetExp(ListLit),
  forms: [
    let_listlit_exp(~def_id, ~pat_id, ~n),
    let_base_exp(~def_id, ~pat_id),
  ],
};

let lets_listnil = (~def_id: Id.t, ~pat_id: Id.t, ~body_id: Id.t): group => {
  id: LetExp(ListNil),
  forms: [
    let_listnil_exp(~def_id, ~pat_id, ~body_id),
    let_base_exp(~def_id, ~pat_id),
  ],
};

let lets_cons =
    (~def_id: Id.t, ~hd_id: Id.t, ~tl_id: Id.t, ~pat_id: Id.t): group => {
  id: LetExp(ListCons),
  forms: [
    let_cons_exp(~def_id, ~hd_id, ~tl_id),
    let_base_exp(~def_id, ~pat_id),
  ],
};

let lets_var =
    (~def_id: Id.t, ~pat_id: Id.t, ~name: string, ~body_id: Id.t): group => {
  id: LetExp(Var),
  forms: [
    let_var_exp(~def_id, ~pat_id, ~name, ~body_id),
    let_base_exp(~def_id, ~pat_id),
  ],
};

let lets_tuple = (~def_id: Id.t, ~pat_id: Id.t, ~n: int): group => {
  id: LetExp(Tuple),
  forms: [
    let_tuple_exp(~def_id, ~pat_id, ~n),
    let_base_exp(~def_id, ~pat_id),
  ],
};

let lets_tuple2 =
    (~def_id: Id.t, ~pat1_id: Id.t, ~pat2_id: Id.t, ~pat_id: Id.t, ~n: int)
    : group => {
  id: LetExp(Tuple2),
  forms: [
    let_tuple2_exp(~def_id, ~pat1_id, ~pat2_id),
    let_tuple_exp(~def_id, ~pat_id, ~n),
    let_base_exp(~def_id, ~pat_id),
  ],
};

let lets_tuple3 =
    (
      ~def_id: Id.t,
      ~pat1_id: Id.t,
      ~pat2_id: Id.t,
      ~pat3_id: Id.t,
      ~pat_id: Id.t,
      ~n: int,
    )
    : group => {
  id: LetExp(Tuple3),
  forms: [
    let_tuple3_exp(~def_id, ~pat1_id, ~pat2_id, ~pat3_id),
    let_tuple_exp(~def_id, ~pat_id, ~n),
    let_base_exp(~def_id, ~pat_id),
  ],
};

let lets_ctr =
    (~def_id: Id.t, ~pat_id: Id.t, ~name: string, ~body_id: Id.t): group => {
  id: LetExp(Ctr),
  forms: [
    let_ctr_exp(~def_id, ~pat_id, ~name, ~body_id),
    let_base_exp(~def_id, ~pat_id),
  ],
};

let lets_conap =
    (~def_id: Id.t, ~x_id: Id.t, ~arg_id: Id.t, ~pat_id: Id.t): group => {
  id: LetExp(ApCons),
  forms: [
    let_conap_exp(~def_id, ~x_id, ~arg_id),
    let_base_exp(~def_id, ~pat_id),
  ],
};

let lets_funap =
    (~def_id: Id.t, ~x_id: Id.t, ~arg_id: Id.t, ~pat_id: Id.t): group => {
  id: LetExp(ApFunc),
  forms: [
    let_funap_exp(~def_id, ~x_id, ~arg_id),
    let_base_exp(~def_id, ~pat_id),
  ],
};

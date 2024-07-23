open Haz3lcore;

let mk_secondary: string => Piece.t =
  content => Secondary({id: Id.mk(), content: Whitespace(content)});

let mk_tile: (Form.t, list(list(Piece.t))) => Piece.t =
  //TODO: asserts
  (form, children) =>
    Tile({
      id: Id.mk(),
      label: form.label,
      mold: form.mold,
      shards: List.mapi((i, _) => i, form.label),
      children,
    });

let mk_ancestor: (Form.t, (list(Segment.t), list(Segment.t))) => Ancestor.t =
  //TODO: asserts
  (form, (l, _) as children) => {
    id: Id.mk(),
    label: form.label,
    mold: form.mold,
    shards:
      List.mapi((i, _) => i, form.label)
      |> Util.ListUtil.split_n(List.length(l) + 1),
    children,
  };

let mk_monotile = form => mk_tile(form, []); //TODO: asserts
let int = n => mk_monotile(Form.mk_atomic(Exp, n));
let exp = v => mk_monotile(Form.mk_atomic(Exp, v));
let pat = v => mk_monotile(Form.mk_atomic(Pat, v));
let mk_parens_exp = mk_tile(Form.get("parens_exp"));
let mk_fun = mk_tile(Form.get("fun_"));
let mk_fun_ancestor = mk_ancestor(Form.get("fun_"));
let mk_parens_ancestor = mk_ancestor(Form.get("parens_exp"));
let mk_let_ancestor = mk_ancestor(Form.get("let_"));
let plus = mk_monotile(Form.get("plus"));

let l_sibling: Segment.t = [plus, Grout({id: Id.mk(), shape: Convex})];
let r_sibling: Segment.t = [mk_parens_exp([[int("1"), plus, int("2")]])];

let content: Segment.t = [exp("foo"), Grout({id: Id.mk(), shape: Concave})];

let ancestors: Ancestors.t = [
  (mk_parens_ancestor(([], [])), ([mk_fun([[pat("bar")]])], [])),
  (mk_parens_ancestor(([], [])), ([mk_fun([[pat("taz")]])], [])),
  (mk_let_ancestor(([[pat("foo")]], [])), ([], [int("2")])),
];

let backpack: Backpack.t = [Selection.mk([exp("foo")])];

let zipper: Zipper.t = {
  selection: Selection.mk(content),
  backpack,
  relatives: {
    siblings: (l_sibling, r_sibling),
    ancestors,
  },
  caret: Outer,
};

// TODO Make sure using this for all the forms that should, like wild and nil
// TODO Should this have its own ID generator or is using the Example one fine?
let cons_exp = () => mk_monotile(Form.get("cons_exp"));
let list_concat_exp = () => mk_monotile(Form.get("list_concat"));
let cons_pat = () => mk_monotile(Form.get("cons_pat"));
let seq = () => mk_monotile(Form.get("cell-join"));
let exp = v => mk_monotile(Form.mk(Form.ss, [v], Mold.(mk_op(Exp, []))));
let pat = v => mk_monotile(Form.mk(Form.ss, [v], Mold.(mk_op(Pat, []))));
let typ = t => mk_monotile(Form.mk(Form.ss, [t], Mold.(mk_op(Typ, []))));
let tpat = v => mk_monotile(Form.mk(Form.ss, [v], Mold.(mk_op(TPat, []))));
let typ_pat_var = t => mk_monotile(Form.mk_atomic(TPat, t));
let mk_parens_exp = mk_tile(Form.get("parens_exp"));
let mk_parens_pat = mk_tile(Form.get("parens_pat"));
let mk_parens_typ = mk_tile(Form.get("parens_typ"));
let mk_list_exp = mk_tile(Form.get("list_lit_exp"));
let mk_list_pat = mk_tile(Form.get("list_lit_pat"));
let mk_list_typ = mk_tile(Form.get("list_typ"));
let mk_forall = mk_tile(Form.get("forall"));
let mk_rec = mk_tile(Form.get("rec"));
let arrow = () => mk_monotile(Form.get("type-arrow"));
let unary_minus = () => mk_monotile(Form.get("unary_minus"));
let unary_not = () => mk_monotile(Form.get("not"));
let plus = () => mk_monotile(Form.get("plus"));
let minus = () => mk_monotile(Form.get("minus"));
let times = () => mk_monotile(Form.get("times"));
let power = () => mk_monotile(Form.get("power"));
let divide = () => mk_monotile(Form.get("divide"));
let equals = () => mk_monotile(Form.get("equals"));
let not_equals = () => mk_monotile(Form.get("not_equals"));
let lt = () => mk_monotile(Form.get("lt"));
let lte = () => mk_monotile(Form.get("lte"));
let gt = () => mk_monotile(Form.get("gt"));
let gte = () => mk_monotile(Form.get("gte"));
let fplus = () => mk_monotile(Form.get("fplus"));
let fminus = () => mk_monotile(Form.get("fminus"));
let ftimes = () => mk_monotile(Form.get("ftimes"));
let fpower = () => mk_monotile(Form.get("fpower"));
let fdivide = () => mk_monotile(Form.get("fdivide"));
let fequals = () => mk_monotile(Form.get("fequals"));
let fnot_equals = () => mk_monotile(Form.get("fnot_equals"));
let flt = () => mk_monotile(Form.get("flt"));
let flte = () => mk_monotile(Form.get("flte"));
let fgt = () => mk_monotile(Form.get("fgt"));
let fgte = () => mk_monotile(Form.get("fgte"));
let sequals = () => mk_monotile(Form.get("string_equals"));
let sconcat = () => mk_monotile(Form.get("string_concat"));
let logical_and = () => mk_monotile(Form.get("logical_and"));
let logical_or = () => mk_monotile(Form.get("logical_or"));
let comma_exp = () => mk_monotile(Form.get("comma_exp"));
let comma_pat = () => mk_monotile(Form.get("comma_pat"));
let comma_typ = () => mk_monotile(Form.get("comma_typ"));
let pipeline = () => mk_monotile(Form.get("pipeline"));
let nil = () => exp("[]");
let deferral = () => exp("_");
let typeann = () => mk_monotile(Form.get("typeann"));
let mk_typfun = mk_tile(Form.get("typfun"));
let mk_fun = mk_tile(Form.get("fun_"));
let mk_ap_exp_typ = mk_tile(Form.get("ap_exp_typ"));
let mk_ap_exp = mk_tile(Form.get("ap_exp"));
let mk_ap_pat = mk_tile(Form.get("ap_pat"));
let mk_let = mk_tile(Form.get("let_"));
let mk_tyalias = mk_tile(Form.get("type_alias"));
let mk_if = mk_tile(Form.get("if_"));
let mk_test = mk_tile(Form.get("test"));
let mk_hinted_test = mk_tile(Form.get("test"));
let mk_case = mk_tile(Form.get("case"));
let mk_rule = mk_tile(Form.get("rule"));
let mk_hide = mk_tile(Form.get("filter_hide"));
let mk_eval = mk_tile(Form.get("filter_eval"));
let mk_pause = mk_tile(Form.get("filter_pause"));
let mk_debug = mk_tile(Form.get("filter_debug"));
let mk_unquote = mk_tile(Form.get("unquote"));
let linebreak = () => mk_secondary(Form.linebreak);
let space = () => mk_secondary(Form.space);

let mk_example = str => {
  switch (Printer.zipper_of_string(str)) {
  | None => []
  | Some(z) => Zipper.zip(z)
  };
};

/* Int param below should be ~ width of sidebar */
let abbreviate = Util.StringUtil.abbreviate(20);

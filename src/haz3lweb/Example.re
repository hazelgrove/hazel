open Haz3lcore;

let id_gen: ref(int) = ref(0);

let mk_id = (): int => {
  let uid = id_gen^;
  id_gen := id_gen^ + 1;
  uid;
};

let mk_secondary: string => Piece.t =
  content => Secondary({id: mk_id(), content: Whitespace(content)});

let mk_tile: (Form.t, list(list(Piece.t))) => Piece.t =
  //TODO: asserts
  (form, children) =>
    Tile({
      id: mk_id(),
      label: form.label,
      mold: form.mold,
      shards: List.mapi((i, _) => i, form.label),
      children,
    });

let mk_ancestor: (Form.t, (list(Segment.t), list(Segment.t))) => Ancestor.t =
  //TODO: asserts
  (form, (l, _) as children) => {
    id: mk_id(),
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

let l_sibling: Segment.t = [plus, Grout({id: mk_id(), shape: Convex})];
let r_sibling: Segment.t = [mk_parens_exp([[int("1"), plus, int("2")]])];

let content: Segment.t = [exp("foo"), Grout({id: mk_id(), shape: Concave})];

let ancestors: Ancestors.t = [
  (mk_parens_ancestor(([], [])), ([mk_fun([[pat("bar")]])], [])),
  (mk_parens_ancestor(([], [])), ([mk_fun([[pat("taz")]])], [])),
  (mk_let_ancestor(([[pat("foo")]], [])), ([], [int("2")])),
];

let backpack: Backpack.t = [{focus: Left, content: [exp("foo")]}];

let zipper: Zipper.t = {
  selection: {
    focus: Left,
    content,
  },
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
let cons_pat = () => mk_monotile(Form.get("cons_pat"));
let seq = () => mk_monotile(Form.get("cell-join"));
let exp = v => mk_monotile(Form.mk(Form.ss, [v], Mold.(mk_op(Exp, []))));
let pat = v => mk_monotile(Form.mk(Form.ss, [v], Mold.(mk_op(Pat, []))));
let typ = t => mk_monotile(Form.mk(Form.ss, [t], Mold.(mk_op(Typ, []))));
let int = n => mk_monotile(Form.mk_atomic(Exp, n));
let bool = b => mk_monotile(Form.mk_atomic(Exp, b));
let mk_parens_exp = mk_tile(Form.get("parens_exp"));
let mk_parens_pat = mk_tile(Form.get("parens_pat"));
let mk_parens_typ = mk_tile(Form.get("parens_typ"));
let mk_list_exp = mk_tile(Form.get("list_lit_exp"));
let mk_list_pat = mk_tile(Form.get("list_lit_pat"));
let mk_list_typ = mk_tile(Form.get("list_typ"));
let arrow = () => mk_monotile(Form.get("type-arrow"));
let unary_minus = () => mk_monotile(Form.get("unary_minus"));
let plus = () => mk_monotile(Form.get("plus"));
let minus = () => mk_monotile(Form.get("minus"));
let times = () => mk_monotile(Form.get("times"));
let power = () => mk_monotile(Form.get("power"));
let divide = () => mk_monotile(Form.get("divide"));
let equals = () => mk_monotile(Form.get("equals"));
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
let flt = () => mk_monotile(Form.get("flt"));
let flte = () => mk_monotile(Form.get("flte"));
let fgt = () => mk_monotile(Form.get("fgt"));
let fgte = () => mk_monotile(Form.get("fgte"));
let sequals = () => mk_monotile(Form.get("string_equals"));
let logical_and = () => mk_monotile(Form.get("logical_and"));
let logical_or = () => mk_monotile(Form.get("logical_or"));
let comma_exp = () => mk_monotile(Form.get("comma_exp"));
let comma_pat = () => mk_monotile(Form.get("comma_pat"));
let comma_typ = () => mk_monotile(Form.get("comma_typ"));
let nil = () => exp("[]");
let typeann = () => mk_monotile(Form.get("typeann"));
let mk_fun = mk_tile(Form.get("fun_"));
let mk_ap_exp = mk_tile(Form.get("ap_exp"));
let mk_ap_pat = mk_tile(Form.get("ap_pat"));
let mk_let = mk_tile(Form.get("let_"));
let mk_if = mk_tile(Form.get("if_"));
let mk_test = mk_tile(Form.get("test"));
let mk_case = mk_tile(Form.get("case"));
let mk_rule = mk_tile(Form.get("rule"));
let linebreak = () => mk_secondary(Form.linebreak);
let space = () => mk_secondary(Form.space);

let mk_example = str => {
  switch (Printer.zipper_of_string(0, str)) {
  | None => []
  | Some((z, _)) => Zipper.zip(z)
  };
};

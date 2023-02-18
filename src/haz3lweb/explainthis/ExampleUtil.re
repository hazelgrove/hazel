open Haz3lcore;

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

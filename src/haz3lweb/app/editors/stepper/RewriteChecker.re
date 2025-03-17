open Haz3lcore;
open Js_of_ocaml;

let rec print_exp_for_algebrite = (exp: Exp.t): string =>
  switch (exp.term) {
  | Int(value) => string_of_int(value)
  | Float(value) => string_of_float(value)
  | Bool(value) => string_of_bool(value)
  | String(value) => value
  | BinOp(op, exp_left, exp_right) =>
    "("
    ++ print_exp_for_algebrite(exp_left)
    ++ " "
    ++ Operators.bin_op_to_string(op)
    ++ " "
    ++ print_exp_for_algebrite(exp_right)
    ++ ")"
  | Parens(exp) => "(" ++ print_exp_for_algebrite(exp) ++ ")"
  | Var(value) => value
  | _ => "Unknown"
  };

let checkEquality = (expr1, expr2): bool => {
  let algebrite = Js.Unsafe.global##.Algebrite;
  let diffExpr = Printf.sprintf("simplify((%s)-(%s))", expr1, expr2);
  let algebrite_result = algebrite##run(Js.string(diffExpr));
  switch (Js.to_string(algebrite_result)) {
  | "0" => true
  | _ => false
  };
};

// underscores indicate unused arguments
let check_rewrite = (_from: Exp.t, _to: Exp.t): bool => {
  let left_str = print_exp_for_algebrite(_from);
  let right_str = print_exp_for_algebrite(_to);
  if (left_str == "Unknown" || right_str == "Unknown") {
    false;
  } else {
    checkEquality(left_str, right_str);
  };
};

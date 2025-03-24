open Haz3lcore;
open Js_of_ocaml;

let rec print_exp_for_algebrite = (exp: Exp.t): string =>
  switch (exp.term) {
  | Int(value) => string_of_int(value)
  // TODO Nats?
  | Float(value) => string_of_float(value)
  | Bool(value) => string_of_bool(value)
  | BinOp(op, exp_left, exp_right) =>
    "("
    ++ print_exp_for_algebrite(exp_left)
    ++ " "
    ++ Operators.bin_op_to_string(op)
    ++ " "
    ++ print_exp_for_algebrite(exp_right)
    ++ ")"
  | UnOp(Int(Minus), exp) =>
    "(" ++ "-" ++ print_exp_for_algebrite(exp) ++ ")"
  | Parens(exp) => "(" ++ print_exp_for_algebrite(exp) ++ ")"
  | Var(value) => value
  // TODO: think harder about weird corner cases where we'd want to ensure the types in Cast are valid
  | Cast(exp, _, _) => print_exp_for_algebrite(exp)
  // If we don't know how to print the expression, we can use a hash to create a unique string
  // Modulo keeps it short
  | _ => "unknown_" ++ string_of_int(Hashtbl.hash(exp) mod 100000)
  };

let checkEquality = (expr1, expr2): bool => {
  let algebrite = Js.Unsafe.global##.Algebrite;
  print_endline(expr1);
  print_endline(expr2);
  let diffExpr = Printf.sprintf("simplify((%s)-(%s))", expr1, expr2);
  print_endline(diffExpr);
  let algebrite_result = algebrite##run(Js.string(diffExpr));
  switch (Js.to_string(algebrite_result)) {
  | "0" => true
  | _ => false
  };
};

// underscores indicate unused arguments
let check_rewrite = (_from: Exp.t, _to: Exp.t): bool => {
  // TODO maybe type-check a bit here so that we don't have to handle
  // differing types on the Algebrite side
  // Or maybe Matt will guarantee _from and _to always have the same type
  // perhaps some Cast

  // TODO return Some(bool) instead of bool in case we encounter a case we can't handle?
  print_endline(Exp.show(_from));
  print_endline(Exp.show(_to));
  let left_str = print_exp_for_algebrite(_from);
  let right_str = print_exp_for_algebrite(_to);
  if (left_str == "Unknown" || right_str == "Unknown") {
    false;
  } else {
    checkEquality(left_str, right_str);
  };
};

open Language;
open Js_of_ocaml;

let rec print_exp_for_algebrite = (exp: Exp.t): string =>
  switch (exp.term) {
  | Atom(Int(value)) => Bigint.to_string(value)
  | Atom(Nat(value)) => Bigint.to_string(value)
  | Atom(Float(value)) => string_of_float(value)
  | Atom(Bool(value)) => string_of_bool(value)
  // We have to manually map ** (power) to ^ in Algebrite.
  | BinOp(Int(Power), exp_left, exp_right) =>
    "("
    ++ print_exp_for_algebrite(exp_left)
    ++ " ^ "
    ++ print_exp_for_algebrite(exp_right)
    ++ ")"
  // The other operators should work fine as-is.
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
  // TODO(nishant): think harder about weird corner cases where we'd want to ensure the types in Cast are valid
  | Asc(exp, _) => print_exp_for_algebrite(exp)
  | Ap(_, outer, inner) =>
    print_exp_for_algebrite(outer)
    ++ "("
    ++ print_exp_for_algebrite(inner)
    ++ ")"
  // Just return built-in functions using their exact name
  | BuiltinFun(str) => str
  // Handle functions
  // TODO(nishant): need to handle more here
  | Fun(pat, exp, _, var) =>
    print_endline("pat: " ++ Pat.show(pat));
    print_endline(print_exp_for_algebrite(exp));
    switch (var) {
    | Some(v) => print_endline(Var.show(v))
    | None => print_endline("None")
    };
    print_exp_for_algebrite(exp);
  // Handle lists
  | ListLit(list) =>
    "["
    ++ String.concat(", ", List.map(print_exp_for_algebrite, list))
    ++ "]"
  | ListConcat(exp1, exp2) =>
    "["
    ++ String.sub(
         print_exp_for_algebrite(exp1),
         1,
         String.length(print_exp_for_algebrite(exp1)) - 2,
       )  // remove the [] brackets
    ++ ", "
    ++ String.sub(
         print_exp_for_algebrite(exp2),
         1,
         String.length(print_exp_for_algebrite(exp2)) - 2,
       )  // remove the [] brackets
    ++ "]"
  // If we don't know how to print the expression, we can use a hash to create a unique string
  // Modulo keeps it short
  | _ =>
    print_endline(
      "Algebrite rewrite checker received unknown value of type "
      ++ Exp.show(exp),
    );
    "unknown_" ++ string_of_int(Hashtbl.hash(exp) mod 100000);
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
  // TODO maybe type-check a bit here so that we don't have to handle
  // differing types on the Algebrite side
  // Or maybe the stepper itself will guarantee _from and _to always have the same type
  // perhaps some Cast

  // TODO return Some(bool) instead of bool in case we encounter a case we can't handle?
  let left_str = print_exp_for_algebrite(_from);
  let right_str = print_exp_for_algebrite(_to);
  if (left_str == "Unknown" || right_str == "Unknown") {
    false;
  } else {
    checkEquality(left_str, right_str);
  };
};

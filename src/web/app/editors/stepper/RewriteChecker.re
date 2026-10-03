open Util;
open Language;
open Js_of_ocaml;

let name_other = (): (Exp.t => string) => {
  let names: ref(list((Exp.t, string))) = ref([]);
  (exp: Exp.t) => (
    {
      switch (ListUtil.assoc_opt_by(Equality.semantic.exp, exp, names^)) {
      | Some(name) => name
      | None =>
        let new_name = "unknown_" ++ string_of_int(List.length(names^));
        names := [(exp, new_name)] @ names^;
        new_name;
      };
    }: string
    // If we don't know how to print the expression, give it a fresh symbol
    // (shared with any semantically equal expression)
  );
};

exception Incomplete;

let rec print_exp_for_algebrite = (~name_other, exp: Exp.t): string =>
  switch (exp.term) {
  // Holes are not equal to anything, not even other holes
  | EmptyHole
  | MultiHole(_)
  | Invalid(_) => raise(Incomplete)
  | Atom(Int(value)) => Bigint.to_string(value)
  | Atom(Nat(value)) => Bigint.to_string(value)
  | Atom(Float(value)) => string_of_float(value)
  | Atom(Bool(value)) => string_of_bool(value)
  // We have to manually map ** (power) to ^ in Algebrite.
  | BinOp(Int(Power) | Nat(Power), exp_left, exp_right) =>
    "("
    ++ print_exp_for_algebrite(~name_other, exp_left)
    ++ " ^ "
    ++ print_exp_for_algebrite(~name_other, exp_right)
    ++ ")"
  // Only send operators whose meaning agrees with Algebrite's. Integer
  // division truncates, natural subtraction saturates, and comparisons,
  // booleans, floats etc. aren't polynomial arithmetic, so those subterms
  // are treated as opaque atoms below instead.
  | BinOp(
      (Int(Plus | Minus | Times) | Nat(Plus | Times)) as op,
      exp_left,
      exp_right,
    ) =>
    "("
    ++ print_exp_for_algebrite(~name_other, exp_left)
    ++ " "
    ++ FormId.bin_op_to_string(op)
    ++ " "
    ++ print_exp_for_algebrite(~name_other, exp_right)
    ++ ")"
  | UnOp(Int(Minus), exp) =>
    "(" ++ "-" ++ print_exp_for_algebrite(~name_other, exp) ++ ")"
  | Parens(exp) => "(" ++ print_exp_for_algebrite(~name_other, exp) ++ ")"
  // TODO: think harder about weird corner cases where we'd want to ensure the types in Cast are valid
  | Asc(exp, _) => print_exp_for_algebrite(~name_other, exp)
  // Variables get fresh symbols too, so that Hazel names can't clash with
  // Algebrite built-ins (e.g. i, e, pi) or fail to parse (e.g. x')
  | _ => name_other(exp)
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
let check_rewrite = (from_: Exp.t, to_: Exp.t): bool => {
  // TODO maybe type-check a bit here so that we don't have to handle
  // differing types on the Algebrite side
  // Or maybe the stepper itself will guarantee _from and _to always have the same type
  // perhaps some Cast

  // TODO return Some(bool) instead of bool in case we encounter a case we can't handle?
  let name_other = name_other();
  let from_ = DHExp.strip_ascriptions(from_);
  let to_ = DHExp.strip_ascriptions(to_);
  switch (
    print_exp_for_algebrite(~name_other, from_),
    print_exp_for_algebrite(~name_other, to_),
  ) {
  | (left_str, right_str) => checkEquality(left_str, right_str)
  | exception Incomplete => false
  };
};

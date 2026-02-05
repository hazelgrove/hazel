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
        let new_name =
          "unknown_" ++ string_of_int(Hashtbl.hash(exp) mod 100000);
        names := [(exp, new_name)] @ names^;
        new_name;
      };
    }: string
    // If we don't know how to print the expression, we can use a hash to create a unique string
    // Modulo keeps it short
  );
};

let rec print_exp_for_algebrite = (~name_other, exp: Exp.t): string =>
  switch (exp.term) {
  | Atom(Int(value)) => Bigint.to_string(value)
  | Atom(Nat(value)) => Bigint.to_string(value)
  | Atom(Float(value)) => string_of_float(value)
  | Atom(Bool(value)) => string_of_bool(value)
  // We have to manually map ** (power) to ^ in Algebrite.
  | BinOp(Int(Power), exp_left, exp_right) =>
    "("
    ++ print_exp_for_algebrite(~name_other, exp_left)
    ++ " ^ "
    ++ print_exp_for_algebrite(~name_other, exp_right)
    ++ ")"
  // The other operators should work fine as-is.
  | BinOp(op, exp_left, exp_right) =>
    "("
    ++ print_exp_for_algebrite(~name_other, exp_left)
    ++ " "
    ++ Operators.bin_op_to_string(op)
    ++ " "
    ++ print_exp_for_algebrite(~name_other, exp_right)
    ++ ")"
  | UnOp(Int(Minus), exp) =>
    "(" ++ "-" ++ print_exp_for_algebrite(~name_other, exp) ++ ")"
  | Parens(exp) => "(" ++ print_exp_for_algebrite(~name_other, exp) ++ ")"
  | Var(value) => value
  // TODO: think harder about weird corner cases where we'd want to ensure the types in Cast are valid
  | Asc(exp, _) => print_exp_for_algebrite(~name_other, exp)
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
  let left_str = print_exp_for_algebrite(~name_other, from_);
  let right_str = print_exp_for_algebrite(~name_other, to_);
  print_endline("Checking rewrite:");
  print_endline("From: " ++ left_str);
  print_endline("To:   " ++ right_str);
  print_endline("e1: " ++ Exp.show(from_));
  print_endline("e2: " ++ Exp.show(to_));
  if (left_str == "Unknown" || right_str == "Unknown") {
    false;
  } else {
    checkEquality(left_str, right_str);
  };
};

let check_written_step =
    (~settings, ~env, from_: Exp.t, to_: Exp.t): option(string) => {
  let rec take_auto_steps = (exp: Exp.t): Exp.t => {
    switch (EvaluatorStep.get_status(~settings, exp, env)) {
    | EvaluatorStep.AutoStep(step) =>
      switch (EvaluatorStep.take_step(step)) {
      | Some(next_exp) => take_auto_steps(next_exp)
      | None => exp
      }
    | AvailableSteps(_) => exp
    };
  };
  let take_and_justify = (es: EvaluatorStep.step): option((string, Exp.t)) => {
    switch (EvaluatorStep.take_step(es)) {
    | Some(next_exp) =>
      let kind = EvaluatorStep.get_step_kind(es);
      let justification = Transition.stepper_justification(kind);
      let final_exp = take_auto_steps(next_exp);
      Some((justification, final_exp));
    | None => None
    };
  };
  // checking using evaluation steps
  let rec get_next_exps = (exp: Exp.t): list((string, Exp.t)) => {
    switch (EvaluatorStep.get_status(~settings, exp, env)) {
    | EvaluatorStep.AutoStep(step) =>
      switch (EvaluatorStep.take_step(step)) {
      | Some(next_exp) => get_next_exps(next_exp)
      | None => []
      }
    | AvailableSteps(steps) => List.filter_map(take_and_justify, steps)
    };
  };
  let next_exps = get_next_exps(from_);
  List.find_opt(
    ((_, e)) =>
      Equality.equality(
        Equality.{
          ...Equality.semantic_settings,
          env1: Some(env),
          env2: Some(env),
          ignore_ascriptions: true,
        },
      ).
        exp(
        e,
        to_,
      ),
    next_exps,
  )
  |> Option.map(fst);
};

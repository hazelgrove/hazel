/*
 Coq export for Hazel arithmetic expressions
 */
open Util;

type domain =
  | Integers
  | Reals;

let is_float_pi = value => abs_float(value -. Float.pi) < 0.000001;

let is_real_builtin = name =>
  name == "sin" || name == "cos" || name == "tan" || name == "diff";

let rec requires_reals = (d: Language.DHExp.t) =>
  switch (Language.Exp.term_of(d |> Language.DHExp.strip_ascriptions)) {
  | Parens(exp)
  | Asc(exp, _) => requires_reals(exp)
  | Tuple([exp]) => requires_reals(exp)
  | Atom(Float(_)) => true
  | Var("pi") => true
  | Var(name) when is_real_builtin(name) => true
  | BuiltinFun(name) => is_real_builtin(name)
  | BinOp(Language.Operators.Float(_), _, _) => true
  | BinOp(_, arg1, arg2) => requires_reals(arg1) || requires_reals(arg2)
  | UnOp(Language.Operators.Float(_), _) => true
  | UnOp(_, exp) => requires_reals(exp)
  | Ap(_, fn, arg) => requires_reals(fn) || requires_reals(arg)
  | _ => false
  };

let rec real_nat_exponent = (d: Language.DHExp.t) =>
  switch (Language.Exp.term_of(d |> Language.DHExp.strip_ascriptions)) {
  | Parens(exp)
  | Asc(exp, _) => real_nat_exponent(exp)
  | Tuple([exp]) => real_nat_exponent(exp)
  | Atom(Int(n))
  | Atom(Nat(n)) => Bigint.to_int(n)
  | Atom(SInt(n)) when n >= 0 => Some(n)
  | Atom(Float(value)) when value >= 0.0 && value == Float.round(value) =>
    Some(int_of_float(value))
  | _ => None
  };

let real_float_op_to_string = op =>
  switch (op) {
  | Language.Operators.Plus => "+"
  | Minus => "-"
  | Times => "*"
  | Power => "^"
  | Divide => "/"
  | LessThan => "<"
  | LessThanOrEqual => "<="
  | GreaterThan => ">"
  | GreaterThanOrEqual => ">="
  | Equals => "="
  | NotEquals => "<>"
  };

let string_of_op = (~domain, op) =>
  switch (domain, op) {
  | (
      Integers,
      Language.Operators.Int(Language.Operators.Power) | Nat(Power) |
      SInt(Power),
    ) => "^"
  | (Integers, Language.Operators.Int(op) | Nat(op) | SInt(op))
  | (Reals, Language.Operators.Int(op) | Nat(op) | SInt(op)) =>
    Language.Operators.int_op_to_string(op)
  | (Reals, Language.Operators.Float(op)) => real_float_op_to_string(op)
  | (_, Language.Operators.Float(op)) =>
    Language.Operators.float_op_to_string(op)
  | _ => Language.Operators.bin_op_to_string(op)
  };

let integer_op_to_string = (op: Language.Operators.op_bin_num) =>
  switch (op) {
  | Language.Operators.Power => "^"
  | op => Language.Operators.int_op_to_string(op)
  };

let rec unique_vars_in_ast_helper =
        (d: Language.DHExp.t, unique_vars: Hashtbl.t(string, unit)) => {
  switch (Language.Exp.term_of(d)) {
  | Parens(exp) => unique_vars_in_ast_helper(exp, unique_vars)
  | Tuple(exps) =>
    exps |> List.iter(exp => unique_vars_in_ast_helper(exp, unique_vars))
  | BinOp(_, arg1, arg2) =>
    unique_vars_in_ast_helper(arg1, unique_vars);
    unique_vars_in_ast_helper(arg2, unique_vars);
  | UnOp(_, exp)
  | Ap(_, _, exp) => unique_vars_in_ast_helper(exp, unique_vars)
  | Var(x) =>
    if (x != "pi" && !Hashtbl.mem(unique_vars, x)) {
      Hashtbl.add(unique_vars, x, ());
    } else {
      ();
    }
  | _ => ()
  };
};

let unique_vars_in_ast = (d: Language.DHExp.t) => {
  let unique_vars = Hashtbl.create(1);
  unique_vars_in_ast_helper(d, unique_vars);
  List.of_seq(Hashtbl.to_seq_keys(unique_vars));
};

// Count all occurrences of an integer v in the AST v
let rec index_of_like_terms_helper_dhexp =
        (d: Language.DHExp.t, v: Language.DHExp.t) => {
  switch (Language.Exp.term_of(d)) {
  | _ when Language.DHExp.fast_equal(d, v) => 1
  | Parens(exp) => index_of_like_terms_helper_dhexp(exp, v)
  | BinOp(Int(_) | Nat(_) | SInt(_), argL, argR) =>
    index_of_like_terms_helper_dhexp(argL, v)
    + index_of_like_terms_helper_dhexp(argR, v)
  | _ => 0
  };
};

// Count all occurrences of integer v that are not to the right of the marker,
// including the marker itself. This function assumes there is always
// a marker somewhere in the AST.
let rec index_of_like_terms_helper_ctx =
        (d: Language.EvalCtx.t, v: Language.DHExp.t) => {
  switch (d) {
  | Mark => 1
  | Term({term, _}) =>
    switch (term) {
    | Parens(arg) => index_of_like_terms_helper_ctx(arg, v)
    // When the left argument is a context (contains the mark) and the right one doesn't
    | BinOp1(_, argL, _) => index_of_like_terms_helper_ctx(argL, v)
    // vice versa
    | BinOp2(_, argL, argR) =>
      index_of_like_terms_helper_dhexp(argL, v)
      + index_of_like_terms_helper_ctx(argR, v)
    | _ => 0
    }
  };
};

// For some integer literal t and context AST d, find out how many occurrences of t do not occur to the right of the Mark in d.

let index_of_like_terms = (d: Language.EvalCtx.t, v: Language.DHExp.t) => {
  index_of_like_terms_helper_ctx(d, v);
};

let string_of_d = (d: Language.DHExp.t) => {
  let rec loop = d =>
    switch (Language.Exp.term_of(d)) {
    | Parens(exp) => loop(exp)
    | BinOp(Int(op) | Nat(op) | SInt(op), arg1, arg2) =>
      "("
      ++ loop(arg1)
      ++ ""
      ++ integer_op_to_string(op)
      ++ ""
      ++ loop(arg2)
      ++ ")"
    | UnOp(Int(Minus) | SInt(Minus), exp) => "(-" ++ loop(exp) ++ ")"
    | Atom(Int(n))
    | Atom(Nat(n)) => Bigint.to_string(n)
    | Atom(SInt(n)) => string_of_int(n)
    | Var(x) => x
    | _ =>
      failwith(
        "unsupported Coq integer export term: "
        ++ Language.Exp.show_cls(
             Language.Exp.cls_of_term(Language.Exp.term_of(d)),
           ),
      )
    };

  loop(d);
};

let string_of_d_reals = (d: Language.DHExp.t) => {
  let rec loop = d =>
    switch (Language.Exp.term_of(d)) {
    | Parens(exp) => loop(exp)
    | Asc(exp, _) => loop(exp)
    /* In the math editor a parenthesized scalar may be represented as a
     * singleton tuple.  It is grouping, not a Rocq product.  Multi-element
     * tuples remain unsupported here and are handled separately for syntax
     * such as diff(expression, variable). */
    | Tuple([exp]) => loop(exp)
    | BinOp(
        Int(Power) | Nat(Power) | SInt(Power) | Float(Power),
        arg1,
        arg2,
      ) =>
      switch (real_nat_exponent(arg2)) {
      | Some(2) => "Rsqr (" ++ loop(arg1) ++ ")"
      | Some(n) => "(" ++ loop(arg1) ++ " ^ " ++ string_of_int(n) ++ ")"
      | None =>
        failwith("unsupported non-literal real exponent in Coq real export")
      }
    | BinOp(op, arg1, arg2) =>
      "("
      ++ loop(arg1)
      ++ " "
      ++ string_of_op(~domain=Reals, op)
      ++ " "
      ++ loop(arg2)
      ++ ")"
    | UnOp(Int(Minus) | SInt(Minus) | Float(Minus), exp) =>
      "(-" ++ loop(exp) ++ ")"
    | Atom(Int(n))
    | Atom(Nat(n)) => Bigint.to_string(n)
    | Atom(SInt(n)) => string_of_int(n)
    | Atom(Float(value)) when is_float_pi(value) => "PI"
    | Atom(Float(value)) when value == Float.round(value) =>
      string_of_int(int_of_float(value))
    | Atom(Float(value)) =>
      failwith(
        "unsupported non-symbolic float in Coq real export: "
        ++ string_of_float(value),
      )
    | Var("pi") => "PI"
    | Var(x) => x
    | BuiltinFun(name) => name
    | Ap(Language.Operators.Forward, fn, arg) =>
      loop(fn) ++ " (" ++ loop(arg) ++ ")"
    | _ =>
      failwith(
        "unsupported Coq real export term: "
        ++ Language.Exp.show_cls(
             Language.Exp.cls_of_term(Language.Exp.term_of(d)),
           ),
      )
    };
  loop(d);
};

let string_of_d_for_domain = (~domain, d) =>
  switch (domain) {
  | Integers => string_of_d(d)
  | Reals => string_of_d_reals(d)
  };
// Takes a single step
// let single_step_export = (ind, step, forall_str) => {
//   let {expr, next_step, state, editor, step_kind, hidden} = step;

//   let oldFragmentString = string_of_d(expr);
//   let newFragmentString = string_of_d(next_step.expr);

//   //Printf.printf("Step: %s -> %s\n", oldFragmentString, newFragmentString);
//   let oldExprString = string_of_d(EvalCtx.compose(ctx, expr));
//   let newExpr = EvalCtx.compose(ctx, next_step.expr);
//   let newExprString = string_of_d(newExpr);
//   //Printf.printf("old: %s\n", oldExprString);
//   //Printf.printf("new: %s\n", newExprString);
//   // TODO(nishant): unpack the axiom correctly
//   let evalTactic =
// TODO these will be from AxiomSteps
//     switch (step.step_kind) {
//     //   switch (step.name) {
//     //   | IdPlusL => "rewrite Qplus_0_l"
//     //   | CommPlus => "rewrite Qplus_comm"
//     // | AssocPlusL => "rewrite Qplus_assoc"
//     // | AssocPlusR => "rewrite Qplus_assoc"
//     // | IdTimesL => "rewrite Qmult_1_r"
//     // | CommTimes => "rewrite Qmult_comm"
//     // | AssocTimesL => "rewrite Qmult_assoc"
//     // | AssocTimesR => "rewrite Qmult_assoc"
//     // | DistPlusTimesL => "rewrite Qmult_plus_distr_l"
//     // | DistPlusTimesR => "rewrite Qmult_plus_distr_l"
//     // | DistPlusTimesLC => "rewrite Qmult_plus_distr_r"
//     // | DistPlusTimesRC => "rewrite Qmult_plus_distr_r"
//     // | DistPlusDivL => "unfold Qdiv. rewrite Qmult_plus_distr_l"
//     // | DistPlusDivR => "unfold Qdiv. rewrite Qmult_plus_distr_l"
//     // | DefDivL => "unfold Qdiv. rewrite Qmult_1_l"
//     // | DefDivR => "unfold Qdiv. rewrite Qmult_1_l"
//     // | NilTimesL => "rewrite Qmult_0_l"
//     // | AssocTimesDivL => "unfold Qdiv. rewrite Qmult_assoc"
//     // | AssocTimesDivR => "unfold Qdiv. rewrite Qmult_assoc"
//     // };
//     | _ => "cbv"
//     };
//   let rewriteIndex = index_of_like_terms(ctx, d_loc');
//   let coqLemmaString =
//     Printf.sprintf(
//       "Lemma equiv_exp%d:%s%s == %s.\nProof.\nintros.\ncut (%s==%s).\n- intros. rewrite <- H at %d. reflexivity.\n- intros. %s. reflexivity.\nQed.",
//       ind,
//       forall_str,
//       newExprString,
//       oldExprString,
//       oldFragmentString,
//       newFragmentString,
//       rewriteIndex,
//       evalTactic,
//     );
//   //Printf.printf("Coq proof:\n%s\n", coqLemmaString);
//   coqLemmaString;
// };

/* Recursively collect all linked steps starting from a single step */

// /* Get every step in the stepper, in order */
// let all_steps_of_stepper = (model: StepperBase.Model): list(step) =>
// all_steps_of_step(model.root);

// Takes a list of steps and generates the Coq proof of equivalence between the first and last steps
// let exportCoq = model => {
//   let rec all_steps_of_step = (step): StepperBase.Model.Stepper.step => {
//     switch (step.next_step) {
//     | None => [step]
//     | Some(next_step) => [step] @ all_steps_of_step(next_step)
//     };
//   };

//   let steps = all_steps_of_step(model.stepper.root);
//   print_endline("Called ExportCoq");
// };
// if (List.length(steps) == 0) {
//   "Not exporting proof with no steps";
// } else {
//   let firstD = List.nth(steps, List.length(steps) - 1).d;
//   let unique_vars = unique_vars_in_ast(firstD);
//   let forall_str =
//     if (List.length(unique_vars) == 0) {
//       "";
//     } else {
//       "forall " ++ String.concat(" ", unique_vars) ++ ",";
//     };

//   let lemmasAndInvocations =
//     List.mapi(
//       (ind, step) =>
//         (
//           single_step_export(List.length(steps) - ind, step, forall_str),
//           Printf.sprintf(
//             "rewrite -> equiv_exp%d.",
//             List.length(steps) - ind,
//           ),
//         ),
//       steps,
//     );
//   let (lemmas, invocations) = List.split(lemmasAndInvocations);

//   let finalExpr =
//     string_of_d(
//       EvalCtx.compose(List.hd(steps).ctx, List.hd(steps).d_loc'),
//     );
//   let firstExpr = string_of_d(firstD);

//   Printf.sprintf(
//     "Require Import QArith.\nRequire Export Plus.\nRequire Export Mult.\n%s\nTheorem equiv_exp:%s%s==%s.\nProof.\nintros.\n%s\nreflexivity. Qed.",
//     String.concat("\n", lemmas),
//     forall_str,
//     finalExpr,
//     firstExpr,
//     String.concat("\n", invocations),
//   );
// };

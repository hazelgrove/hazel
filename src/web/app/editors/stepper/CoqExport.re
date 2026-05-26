/*
 Coq export for Hazel arithmetic expressions
 */
open Util;

let rec unique_vars_in_ast_helper =
        (d: Language.DHExp.t, unique_vars: Hashtbl.t(string, unit)) => {
  switch (Language.Exp.term_of(d)) {
  | BinOp(Int(_), arg1, arg2) =>
    unique_vars_in_ast_helper(arg1, unique_vars);
    unique_vars_in_ast_helper(arg2, unique_vars);
  | Var(x) =>
    if (!Hashtbl.mem(unique_vars, x)) {
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
  | BinOp(Int(_), argL, argR) =>
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

let rec string_of_d = (d: Language.DHExp.t) => {
  "("
  ++ (
    switch (Language.Exp.term_of(d)) {
    | BinOp(Int(op), arg1, arg2) =>
      string_of_d(arg1)
      ++ ""
      ++ Language.Operators.int_op_to_string(op)
      ++ ""
      ++ string_of_d(arg2)
    | Atom(Int(n))
    | Atom(Nat(n)) => Bigint.to_string(n)
    | Var(x) => x
    | _ =>
      print_endline(
        "unknown term: "
        ++ Language.Exp.show_cls(
             Language.Exp.cls_of_term(Language.Exp.term_of(d)),
           ),
      );
      "ERROR";
    }
  )
  ++ ")" /*   }*/;
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

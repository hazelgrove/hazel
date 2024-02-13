/*
 Coq export for Hazel arithmetic expressions
 */

open EvaluatorStep;

let rec unique_vars_in_ast_helper =
        (d: DHExp.t, unique_vars: Hashtbl.t(string, unit)) => {
  switch (d) {
  | BinIntOp(_, arg1, arg2) =>
    unique_vars_in_ast_helper(arg1, unique_vars);
    unique_vars_in_ast_helper(arg2, unique_vars);
  | NonEmptyHole(_, _, _, FreeVar(_, _, x)) =>
    if (!Hashtbl.mem(unique_vars, x)) {
      Hashtbl.add(unique_vars, x, ());
    } else {
      ();
    }
  | _ => ()
  };
};

let unique_vars_in_ast = (d: DHExp.t) => {
  let unique_vars = Hashtbl.create(1);
  unique_vars_in_ast_helper(d, unique_vars);
  List.of_seq(Hashtbl.to_seq_keys(unique_vars));
};

// Count all occurrences of an integer v in the AST v
let rec index_of_like_terms_helper_dhexp = (d: DHExp.t, v: int) => {
  switch (d) {
  | BinIntOp(_, argL, argR) =>
    index_of_like_terms_helper_dhexp(argL, v)
    + index_of_like_terms_helper_dhexp(argR, v)
  | IntLit(arg) when arg == v => 1
  | _ => 0
  };
};

// Count all occurrences of integer v that are not to the right of the marker,
// including the marker itself. This function assumes there is always
// a marker somewhere in the AST.
let rec index_of_like_terms_helper_ctx = (d: EvalCtx.t, v: int) => {
  switch (d) {
  // When the left argument is a context (contains the mark) and the right one doesn't
  | BinIntOp1(_, argL, _) => index_of_like_terms_helper_ctx(argL, v)
  // vice versa
  | BinIntOp2(_, argL, argR) =>
    index_of_like_terms_helper_dhexp(argL, v)
    + index_of_like_terms_helper_ctx(argR, v)
  | Mark => 1
  | _ => 0
  };
};

// For some integer literal t and context AST d, find out how many occurrences of t do not occur to the right of the Mark in d.

let index_of_like_terms = (d: EvalCtx.t, v: DHExp.t) => {
  switch (v) {
  | IntLit(arg) => index_of_like_terms_helper_ctx(d, arg)
  | _ => 0
  };
};

let rec string_of_d = (d: DHExp.t) => {
  switch (d) {
  | BinIntOp(op, arg1, arg2) =>
    string_of_d(arg1)
    ++ ""
    ++ TermBase.UExp.int_op_to_string(op)
    ++ ""
    ++ string_of_d(arg2)
  | IntLit(n) => string_of_int(n)
  | NonEmptyHole(_, _, _, FreeVar(_, _, x)) => x
  | _ => "ERROR"
  };
};
// Takes a single step
let single_step_export = (ind, step, forall_str) => {
  let {d_loc, d_loc', ctx, _} = step;
  let oldFragmentString = string_of_d(d_loc);
  let newFragmentString = string_of_d(d_loc');

  //Printf.printf("Step: %s -> %s\n", oldFragmentString, newFragmentString);
  let oldExprString = string_of_d(compose(ctx, d_loc));
  let newExpr = compose(ctx, d_loc');
  let newExprString = string_of_d(newExpr);
  //Printf.printf("old: %s\n", oldExprString);
  //Printf.printf("new: %s\n", newExprString);
  let evalTactic =
    switch (step) {
    | RewriteStep(rule) =>
      switch (rule) {
      | IdPlusL
      | CommPlus(_, _) => "rewrite Nat.add_comm. "
      | AssocPlusL(_, _, _) => "rewrite Nat.add_assoc. "
      | AssocPlusR(_, _, _) => "rewrite Nat.add_assoc. "
      | IdTimesL(_, _) => "rewrite Nat.mul_1_l. "
      | CommTimes(_, _) => "rewrite Nat.mul_comm. "
      | AssocTimesL(_, _, _) => "rewrite Nat.mul_assoc. "
      | AssocTimesR(_, _, _) => "rewrite Nat.mul_assoc. "
      | DistPlusTimesL(_, _, _) => "rewrite Nat.mul_add_distr_l. "
      | DistPlusTimesR(_, _, _) => "rewrite Nat.mul_add_distr_r. "
      | DistPlusDivL(_, _, _) => "rewrite Nat.div_add_l. "
      | DistPlusDivR(_, _, _) => "rewrite Nat.div_add_r. "
      | DivDefL(_, _) => "rewrite Nat.div_mod. "
      | DivDefR(_, _) => "rewrite Nat.div_mod. "
      | _ => "cbv"
      }
    };
  let extraTactic =
    // Only add extra tactics for cbv
    if (evalTactic == "cbv") {
      switch (d_loc) {
      | BinIntOp(Plus, _, _) => "repeat rewrite Nat.add_assoc. "
      | _ => ""
      };
    } else {
      "";
    };
  let rewriteIndex = index_of_like_terms(ctx, d_loc');
  let coqLemmaString =
    Printf.sprintf(
      "Lemma equiv_exp%d:%s%s = %s.\nProof.\nintros.\ncut (%s=%s).\n- intros. rewrite <- H at %d. %s reflexivity.\n- intros. %s. reflexivity.\nQed.",
      ind,
      forall_str,
      newExprString,
      oldExprString,
      oldFragmentString,
      newFragmentString,
      rewriteIndex,
      extraTactic, // Make sure to turn this of if
      evalTactic,
    );
  //Printf.printf("Coq proof:\n%s\n", coqLemmaString);
  coqLemmaString;
};

// Takes a list of steps and generates the Coq proof of equivalence between the first and last steps
let exportCoq = steps =>
  if (List.length(steps) == 0) {
    "Not exporting proof with no steps";
  } else {
    let firstD = List.nth(steps, List.length(steps) - 1).d;
    let unique_vars = unique_vars_in_ast(firstD);
    let forall_str =
      if (List.length(unique_vars) == 0) {
        "";
      } else {
        "forall " ++ String.concat(" ", unique_vars) ++ ",";
      };
    let lemmasAndInvocations =
      List.mapi(
        (ind, step) =>
          (
            single_step_export(List.length(steps) - ind, step, forall_str),
            Printf.sprintf(
              "rewrite -> equiv_exp%d.",
              List.length(steps) - ind,
            ),
          ),
        steps,
      );
    let (lemmas, invocations) = List.split(lemmasAndInvocations);
    let finalExpr =
      string_of_d(compose(List.hd(steps).ctx, List.hd(steps).d_loc'));
    let firstExpr = string_of_d(firstD);
    // Return a string that is the Coq proof but don't print to console
    Printf.sprintf(
      "Require Import Nat.\nRequire Export Plus.\nRequire Export Mult.\n%s\nTheorem equiv_exp:%s%s=%s.\nProof.\nintros.\n%s\nreflexivity. Qed.",
      String.concat("\n", lemmas),
      forall_str,
      finalExpr,
      firstExpr,
      String.concat("\n", invocations),
    );
  };

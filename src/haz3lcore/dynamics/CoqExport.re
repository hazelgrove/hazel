/*
 Coq export for Hazel arithmetic expressions
 */

open EvaluatorStep;

let rec string_of_d = (d: DHExp.t) => {
  switch (d) {
  | BinIntOp(op, arg1, arg2) =>
    string_of_d(arg1)
    ++ ""
    ++ TermBase.UExp.int_op_to_string(op)
    ++ ""
    ++ string_of_d(arg2)
  | IntLit(n) => string_of_int(n)
  | _ => "ERROR"
  };
};
// Takes a single step
let single_step_export = (ind, step) => {
  let {d_loc, d_loc', ctx, _} = step;
  let oldFragmentString =
    switch (d_loc) {
    | BinIntOp(op, IntLit(arg1), IntLit(arg2)) =>
      let oldExprOperString = TermBase.UExp.int_op_to_string(op);
      String.concat(
        "",
        [string_of_int(arg1), oldExprOperString, string_of_int(arg2)],
      );
    | _ => "ERROR"
    };
  let newFragmentString =
    switch (d_loc') {
    | IntLit(arg) => string_of_int(arg)
    | _ => "ERROR"
    };

  //Printf.printf("Step: %s -> %s\n", oldFragmentString, newFragmentString);
  let oldExprString = string_of_d(compose(ctx, d_loc));
  let newExpr = compose(ctx, d_loc');
  let newExprString = string_of_d(newExpr);
  //Printf.printf("old: %s\n", oldExprString);
  //Printf.printf("new: %s\n", newExprString);
  let extraTactic =
    switch (d_loc) {
    | BinIntOp(Plus, _, _) => "repeat rewrite Nat.add_assoc. "
    | _ => ""
    };
  let coqLemmaString =
    Printf.sprintf(
      "Lemma equiv_exp%d:%s = %s.\nProof.\nintros.\ncut (%s=%s).\n- intros. rewrite <- H at 1. %s reflexivity.\n- intros. cbv. reflexivity.\nQed.",
      ind,
      newExprString,
      oldExprString,
      oldFragmentString,
      newFragmentString,
      extraTactic,
    );
  //Printf.printf("Coq proof:\n%s\n", coqLemmaString);
  coqLemmaString;
};

// Takes a list of steps and generates the Coq proof of equivalence between the first and last steps
let exportCoq = steps =>
  if (List.length(steps) == 0) {
    print_endline("Not exporting proof with no steps");
  } else {
    let lemmasAndInvocations =
      List.mapi(
        (ind, step) =>
          (
            single_step_export(List.length(steps) - ind, step),
            Printf.sprintf(
              "rewrite -> equiv_exp%d.",
              List.length(steps) - ind,
            ),
          ),
        steps,
      );
    let (lemmas, invocations) = List.split(lemmasAndInvocations);
    let finalExpr = string_of_d(List.hd(steps).d_loc');
    let firstExpr = string_of_d(List.nth(steps, List.length(steps) - 1).d);
    Printf.printf(
      "Require Import Nat.\nRequire Export Plus.\nRequire Export Mult.\n%s\nTheorem equiv_exp:%s=%s.\nProof.\nintros.\n%s\nreflexivity. Qed.",
      String.concat("\n", lemmas),
      finalExpr,
      firstExpr,
      String.concat("\n", invocations),
    );
    ();
  };

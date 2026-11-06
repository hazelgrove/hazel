open Alcotest;
open Test_ProofMap.Harness;

/* A small proof library: a Bool fold and its accumulator laws, where each
 * later lemma cites the earlier ones. */

let library = {|let foldl = fun (xs : [Bool], f, acc : Bool) ->
  case xs | [] => acc | x :: rest => foldl(rest, f, f(acc, x)) end in
let conj = fun (acc : Bool, x : Bool) -> acc && x in
theorem true_and = forall (p : Bool) -> (true && p) == p proof
  induction p
  | true => eval true && true at 0 end;
    eval true == true at 0 end
  | false => eval true && false at 0 end;
    eval false == false at 0 end
  end
in
theorem fold_conj_false = forall (xs : [Bool]) -> foldl(xs, conj, false) == false proof
  induction xs
  | [] => eval foldl([], conj, false) at 0 end;
    eval false == false at 0 end
  | h :: t => eval foldl(h :: t, conj, false) at 0 end;
    eval conj(false, h) at 0 end;
    eval false && h at 0 end;
    axiom ih at 0 on foldl(t, conj, false) end;
    eval false == false at 0 end
  end
in
theorem fold_conj_acc = forall (b : Bool) -> forall (xs : [Bool]) ->
  foldl(xs, conj, b) == (b && foldl(xs, conj, true)) proof
  induction b
  | true => axiom true_and at 0 on true && foldl(xs, conj, true) end;
    axiom refl_eq at 0 on foldl(xs, conj, true) == foldl(xs, conj, true) end
  | false => axiom fold_conj_false at 0 on foldl(xs, conj, false) end;
    eval false && foldl(xs, conj, true) at 0 end;
    eval false == false at 0 end
  end
in
theorem fold_conj_cons = forall (h : Bool) -> forall (t : [Bool]) ->
  foldl(h :: t, conj, true) == (h && foldl(t, conj, true)) proof
  eval foldl(h :: t, conj, true) at 0 end;
  eval conj(true, h) at 0 end;
  eval true && h at 0 end;
  axiom fold_conj_acc at 0 on foldl(t, conj, h) end;
  induction h
  | true => axiom true_and at 0 on true && foldl(t, conj, true) end;
    axiom true_and at 0 on true && foldl(t, conj, true) end;
    axiom refl_eq at 0 on foldl(t, conj, true) == foldl(t, conj, true) end
  | false => eval false && foldl(t, conj, true) at 0 end;
    eval false && foldl(t, conj, true) at 0 end;
    eval false == false at 0 end
  end
in
0|};

let proven = (name: string) =>
  test_case(
    name ++ " is proven",
    `Quick,
    () => {
      let (pm, proof) = run_named(name, library);
      check_proven(name, pm, proof);
    },
  );

let tests = (
  "ProofLibrary.ListFold",
  [
    proven("true_and"),
    proven("fold_conj_false"),
    proven("fold_conj_acc"),
    proven("fold_conj_cons"),
  ],
);

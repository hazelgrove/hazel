open Alcotest;
open Test_ProofMap.Harness;

/* A small proof library: a Bool fold and its accumulator laws, where each
 * later lemma cites the earlier ones. */

let library = {|let foldl = fun (xs : [Bool], f, acc : Bool) ->
  case xs | [] => acc | x :: rest => foldl(rest, f, f(acc, x)) end in
let conj = fun (acc : Bool, x : Bool) -> acc && x in
theorem true_and = forall (p : Bool) -> (true && p) == p proof
  induction p
  | true => from true && true do eval end;
    from true == true do eval end
  | false => from true && false do eval end;
    from false == false do eval end
  end
in
theorem fold_conj_false = forall (xs : [Bool]) -> foldl(xs, conj, false) == false proof
  induction xs
  | [] => from foldl([], conj, false) do eval end;
    from false == false do eval end
  | h :: t => from foldl(h :: t, conj, false) do eval end;
    from conj(false, h) do eval end;
    from false && h do eval end;
    from foldl(t, conj, false) do axiom ih end end;
    from false == false do eval end
  end
in
theorem fold_conj_acc = forall (b : Bool) -> forall (xs : [Bool]) ->
  foldl(xs, conj, b) == (b && foldl(xs, conj, true)) proof
  induction b
  | true => from true && foldl(xs, conj, true) do axiom true_and end end;
    from foldl(xs, conj, true) == foldl(xs, conj, true) do axiom refl_eq end end
  | false => from foldl(xs, conj, false) do axiom fold_conj_false end end;
    from false && foldl(xs, conj, true) do eval end;
    from false == false do eval end
  end
in
theorem fold_conj_cons = forall (h : Bool) -> forall (t : [Bool]) ->
  foldl(h :: t, conj, true) == (h && foldl(t, conj, true)) proof
  from foldl(h :: t, conj, true) do eval end;
  from conj(true, h) do eval end;
  from true && h do eval end;
  from foldl(t, conj, h) do axiom fold_conj_acc end end;
  induction h
  | true => from true && foldl(t, conj, true) do axiom true_and end end;
    from true && foldl(t, conj, true) do axiom true_and end end;
    from foldl(t, conj, true) == foldl(t, conj, true) do axiom refl_eq end end
  | false => from false && foldl(t, conj, true) do eval end;
    from false && foldl(t, conj, true) do eval end;
    from false == false do eval end
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

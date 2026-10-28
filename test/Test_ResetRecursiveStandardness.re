/* Recursive-result standardness and canonical `@` (approved delta on
   2134803c): `std_structural` accepts a recursive application only when
   StructuralRecursion certifies descent AND the whole body (every branch
   of a covered case) is standard with the parameter standard and the
   self-name usable only as a certified call head; `a @ b` of standard
   lists is standard. Each control witnesses an `exists` with the
   application and reads the witness's `!!v` discharge. */
open Alcotest;
open Language;

module RE = Test_ResetExistentials;

let wit = (~decls, ~ty, ~v) =>
  decls
  ++ "theorem w = forall (xs : [Int]) -> exists (l : "
  ++ ty
  ++ ") -> l == l proof == true by witness "
  ++ v
  ++ " end in 0";

let len_decl = "let len : [Int] -> Int = fun xs -> case xs | [] => 0 | _ :: t => 1 + len(t) end in\n";
let rev_decl = "let rev : [Int] -> [Int] = fun xs -> case xs | [] => [] | x :: t => rev(t) @ [x] end in\n";

let positives = [
  ("len", wit(~decls=len_decl, ~ty="Int", ~v="len(xs)")),
  ("rev", wit(~decls=rev_decl, ~ty="[Int]", ~v="rev(xs)")),
  ("concat", wit(~decls="", ~ty="[Int]", ~v="xs @ [1]")),
  (
    "rebinding",
    wit(
      ~decls="let r : [Int] -> Int = fun xs -> case xs | [] => 0 | _ :: t => (fun (r : Int) -> r)(r(t)) end in\n",
      ~ty="Int",
      ~v="r(xs)",
    ),
  ),
];

let pending_negatives = [
  (
    "base_divides_by_zero",
    wit(
      ~decls="let f : [Int] -> Int = fun xs -> case xs | [] => 1 / 0 | _ :: t => f(t) end in\n",
      ~ty="Int",
      ~v="f(xs)",
    ),
  ),
  (
    "partial_function_in_data",
    wit(
      ~decls="let g : [Int] -> [Int -> Int] = fun xs -> case xs | [] => [fun (k : Int) -> 1 / k] | _ :: t => g(t) end in\n",
      ~ty="[Int -> Int]",
      ~v="g(xs)",
    ),
  ),
  (
    "coverage_not_shown",
    wit(
      ~decls="let k : [Int] -> Int = fun xs -> case xs | [] => 0 | [x] => 1 | x :: y :: t => k(t) end in\n",
      ~ty="Int",
      ~v="k(xs)",
    ),
  ),
  (
    "adt_to_int_limitation",
    "type Nt = +Z + S(Nt) in\nlet to_int : Nt -> Int = fun n -> case n | Z => 0 | S(m) => 1 + to_int(m) end in\n"
    ++ "theorem w = forall (n : Nt) -> exists (k : Int) -> k == k proof == true by witness to_int(n) end in 0",
  ),
];

/* Not standard by the rule; the witness may instead be refused outright
   (descent not certified: PossiblyDivergentInstantiation). */
let not_structural = [
  (
    "non_structural_descent",
    wit(
      ~decls="let h : [Int] -> Int = fun xs -> case xs | [] => 0 | _ :: t => h(t @ [1]) end in\n",
      ~ty="Int",
      ~v="h(xs)",
    ),
  ),
  (
    "self_escape",
    wit(
      ~decls="let e : [Int] -> Int = fun xs -> case xs | [] => 0 | _ :: t => (fun (u : [Int] -> Int) -> u(t))(e) end in\n",
      ~ty="Int",
      ~v="e(xs)",
    ),
  ),
  (
    "guarded_catch_all",
    wit(
      ~decls="let q : [Int] -> Int = fun xs -> case xs | [] => 0 | z where false => 1 end in\n",
      ~ty="Int",
      ~v="q(xs)",
    ),
  ),
];

let discharge = (o: RE.obs) => RE.std_discharge(RE.node(o, "w", Proof.RelEqBy));

let tests = (
  "ResetRecursiveStandardness",
  [
    test_case("positives: certified recursion / canonical @ / rebinding are exactly Structural", `Quick, () =>
      List.iter(
        ((label, src)) => {
          let o = RE.observe(label, src);
          RE.valid(label, o);
          check(string, label, "Structural", discharge(o));
        },
        positives,
      )
    ),
    test_case("negatives: partial base, partial function in data, uncovered case, ADT stay Pending", `Quick, () =>
      List.iter(
        ((label, src)) => {
          let o = RE.observe(label, src);
          RE.valid(label, o);
          check(string, label, "Pending", discharge(o));
        },
        pending_negatives,
      )
    ),
    test_case("negatives: uncertified descent / self escape / guarded catch-all are never Structural", `Quick, () =>
      List.iter(
        ((label, src)) => {
          let o = RE.observe(label, src);
          let d = discharge(o);
          print_endline("RECSTD " ++ label ++ " => " ++ d ++ " | static errors " ++ string_of_int(List.length(o.errors)));
          check(bool, label ++ " not Structural", false, d == "Structural");
        },
        not_structural,
      )
    ),
  ],
);

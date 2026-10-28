/* Recursive-result standardness and canonical `@` (std_judge).
   One test case per program, so no early assertion hides another. Every
   case prints its witness `!!v` discharge kind, the judgement counters
   (nodes, certifications, certification nodes, cache hits) and CPU time.
   Shape evidence: a nil/cons case is covered only over an expression
   KNOWN to be a list (literal/cons/concat, a list-typed variable, a cons
   tail, a list-evidenced parameter); a tuple binder only against a
   literal tuple. Unknown is not a precise annotation. */
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

let len_ann = "let len : [Int] -> Int = fun xs -> case xs | [] => 0 | _ :: t => 1 + len(t) end in\n";
let len_un = "let len = fun xs -> case xs | [] => 0 | _ :: t => 1 + len(t) end in\n";
let rev_un = "let rev = fun xs -> case xs | [] => [] | x :: t => rev(t) @ [x] end in\n";

/* Constructor skeleton of an expression (receipt for the elaborated
   wrapper question; nothing is asserted from it). */
let rec skel = (d: int, e: Exp.t): string =>
  if (d <= 0) {
    "…";
  } else {
    let k = skel(d - 1);
    switch (e.term) {
    | Valued(a) => "Valued(" ++ k(a) ++ ")"
    | Parens(a) => "Parens(" ++ k(a) ++ ")"
    | Asc(a, t) => "Asc(" ++ k(a) ++ " : " ++ Typ.pretty_print(t) ++ ")"
    | Closure(_, a) => "Closure(" ++ k(a) ++ ")"
    | FixF(p, a, _) => "FixF(" ++ (Pat.get_var(p) |> Option.value(~default="?")) ++ ", " ++ k(a) ++ ")"
    | Fun(p, a, t, _) =>
      "Fun("
      ++ Pat.show_cls(Pat.cls_of_term(p.term))
      ++ " field="
      ++ (t |> Option.map(Typ.pretty_print) |> Option.value(~default="None"))
      ++ ", "
      ++ k(a)
      ++ ")"
    | Ap(_, f, a) => "Ap(" ++ k(f) ++ ", " ++ k(a) ++ ")"
    | Match(s, rules) => "Match(" ++ k(s) ++ ", " ++ string_of_int(List.length(rules)) ++ " rules)"
    | Var(x) => "Var " ++ x
    | Atom(_) => "Atom"
    | _ => Exp.show_cls(Exp.cls_of_term(e.term))
    };
  };

let witness_gate = (o: RE.obs) =>
  RE.node(o, "w", Proof.RelEqBy).obligations
  |> List.find_opt((ob: Obligation.t) =>
       switch (ob.goal.term) {
       | Valued(_) => true
       | _ => false
       }
     );

/* Observe one program with counters and timing; return the witness
   gate's discharge ("NO !!v OBLIGATION" when the witness was refused). */
let run = (label, src) => {
  ProofCheck.std_stats_reset();
  let t0 = Sys.time();
  let o = RE.observe(label, src);
  let dt = Sys.time() -. t0;
  let gate = witness_gate(o);
  let d =
    switch (gate) {
    | Some(ob) => Obligation.show_discharge(ob.discharge)
    | None => "NO !!v OBLIGATION"
    };
  print_endline(
    "RECSTD "
    ++ label
    ++ " => "
    ++ d
    ++ " | static errors "
    ++ string_of_int(List.length(o.errors))
    ++ " | nodes "
    ++ string_of_int(ProofCheck.std_stat_nodes^)
    ++ " certs "
    ++ string_of_int(ProofCheck.std_stat_certifications^)
    ++ " cert-nodes "
    ++ string_of_int(ProofCheck.std_stat_certification_nodes^)
    ++ " cache-hits "
    ++ string_of_int(ProofCheck.std_stat_cache_hits^)
    ++ " | cpu "
    ++ Printf.sprintf("%.3f", dt)
    ++ "s",
  );
  switch (gate) {
  | Some(ob) => print_endline("RECSTD " ++ label ++ " skeleton " ++ skel(9, ob.goal))
  | None => ()
  };
  (o, d);
};

let positive = (label, src) =>
  test_case("positive " ++ label ++ ": exactly Structural", `Quick, () => {
    let (o, d) = run(label, src);
    RE.valid(label, o);
    check(string, label, "Structural", d);
  });

let pending = (label, src) =>
  test_case("negative " ++ label ++ ": Pending", `Quick, () => {
    let (o, d) = run(label, src);
    RE.valid(label, o);
    check(string, label, "Pending", d);
  });

let not_structural = (label, src) =>
  test_case("negative " ++ label ++ ": never Structural", `Quick, () => {
    let (_, d) = run(label, src);
    check(bool, label ++ " not Structural", false, d == "Structural");
  });

let std_unit = (label, src, expected) =>
  test_case("judgement " ++ label, `Quick, () => {
    let ctx =
      SemanticCtx.of_ctx_and_env(
        Builtins.ctx_init(Some(Operators.default_mode)),
        Builtins.env_init,
      );
    check(
      bool,
      label,
      expected,
      ProofCheck.std_structural(ctx, Test_Evaluator_Prelude.parse_exp(src)),
    );
  });

let len3 =
  len_un
  ++ "theorem w = exists (n : Int) -> n == len(3) proof == true by witness len(3) end; axiom refl_eq at 0 on len(3) == len(3) end in 0";

let tests = (
  "ResetRecursiveStandardness",
  [
    positive("len_annotated", wit(~decls=len_ann, ~ty="Int", ~v="len(xs)")),
    positive("len_unannotated_list_arg", wit(~decls=len_un, ~ty="Int", ~v="len(xs)")),
    positive("rev_unannotated", wit(~decls=rev_un, ~ty="[Int]", ~v="rev(xs)")),
    positive("concat", wit(~decls="", ~ty="[Int]", ~v="xs @ [1]")),
    positive(
      "rebinding_inner_lambda",
      wit(
        ~decls="let r : [Int] -> Int = fun xs -> case xs | [] => 0 | _ :: t => (fun (r : Int) -> r)(r(t)) end in\n",
        ~ty="Int",
        ~v="r(xs)",
      ),
    ),
    pending(
      "base_divides_by_zero",
      wit(
        ~decls="let f : [Int] -> Int = fun xs -> case xs | [] => 1 / 0 | _ :: t => f(t) end in\n",
        ~ty="Int",
        ~v="f(xs)",
      ),
    ),
    pending(
      "partial_function_in_returned_data",
      wit(
        ~decls="let g : [Int] -> [Int -> Int] = fun xs -> case xs | [] => [fun (k : Int) -> 1 / k] | _ :: t => g(t) end in\n",
        ~ty="[Int -> Int]",
        ~v="g(xs)",
      ),
    ),
    pending(
      "coverage_not_shown",
      wit(
        ~decls="let k : [Int] -> Int = fun xs -> case xs | [] => 0 | [x] => 1 | x :: y :: t => k(t) end in\n",
        ~ty="Int",
        ~v="k(xs)",
      ),
    ),
    pending(
      "adt_to_int_limitation",
      "type Nt = +Z + S(Nt) in\nlet to_int : Nt -> Int = fun n -> case n | Z => 0 | S(m) => 1 + to_int(m) end in\n"
      ++ "theorem w = forall (n : Nt) -> exists (k : Int) -> k == k proof == true by witness to_int(n) end in 0",
    ),
    not_structural(
      "non_structural_descent",
      wit(
        ~decls="let h : [Int] -> Int = fun xs -> case xs | [] => 0 | _ :: t => h(t @ [1]) end in\n",
        ~ty="Int",
        ~v="h(xs)",
      ),
    ),
    not_structural(
      "self_escape",
      wit(
        ~decls="let e : [Int] -> Int = fun xs -> case xs | [] => 0 | _ :: t => (fun (u : [Int] -> Int) -> u(t))(e) end in\n",
        ~ty="Int",
        ~v="e(xs)",
      ),
    ),
    not_structural(
      "guarded_catch_all",
      wit(
        ~decls="let q : [Int] -> Int = fun xs -> case xs | [] => 0 | z where false => 1 end in\n",
        ~ty="Int",
        ~v="q(xs)",
      ),
    ),
    /* G1 (reviewer): unannotated len applied to an Int (statics-valid,
       gradual). Its case cannot match 3: not standard, so not Proven. */
    test_case("G1 len(3): statics, runtime, gate Pending, theorem not Proven", `Quick, () => {
      let (o, d) = run("len3", len3);
      RE.valid("len3", o);
      print_endline(
        "RECSTD len3 runtime len(3) ~> "
        ++ RE.eval_print(len_un ++ "len(3)"),
      );
      check(string, "gate", "Pending", d);
      check(bool, "not Proven", false, RE.full(o, "w") == "Proven");
    }),
    /* G2 (reviewer): Unknown is not a precise annotation, and a product
       parameter type is not tuple evidence for an Unknown-typed argument;
       a literal tuple and a known list are. */
    std_unit("G2 fun (y : ?) -> list case is not std_fun", "fun (y : ?) -> case y | [] => 0 | _ :: t => 1 end", false),
    std_unit("G2 tuple binder at an Unknown-typed 3 is refused", "(fun ((a, b) : (Int, Int)) -> a)((fun z -> z)(3))", false),
    std_unit("G2 tuple binder at a literal tuple is accepted", "(fun ((a, b) : (Int, Int)) -> a)((1, 2))", true),
    std_unit("G2 list case at a literal list argument is accepted", "(fun ys -> case ys | [] => 0 | _ :: t => 1 end)([1])", true),
    std_unit("G2 list case at an Int argument is refused", "(fun ys -> case ys | [] => 0 | _ :: t => 1 end)(3)", false),
  ],
);

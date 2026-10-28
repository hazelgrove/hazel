/* Reset-existentials-implementation: primitive `exists` (D75), the
   witness intro `== true by witness v end` (exI), `obtain` (∃-elim),
   one-way `!!` standardness (D81) and the D78 binder fact
   `forall (x : T !! h) =>` cited as `== true by h end` (D76).

   Each program is parsed with the tile parser, elaborated by the real
   statics and evaluated once with the big-step proof hook; assertions read
   the reference's own ProofMap (marks, obligations, outgoing, full
   status). Receipts are printed for every reached node. */
open Alcotest;
open Language;
open Test_Evaluator_Prelude;

let print_exp = (e: Exp.t): string =>
  e
  |> Haz3lcore.ExpToSegment.exp_to_segment(
       ~settings=Haz3lcore.ExpToSegment.Settings.editable(~inline=true),
     )
  |> Haz3lcore.Printer.of_segment(~holes="?", ~refractors=[]);

let rec proof_nodes = (p: Proof.t): list(Proof.t) =>
  [p]
  @ (
    switch (p.term) {
    | Seq(a, b) => proof_nodes(a) @ proof_nodes(b)
    | Parens(b)
    | Forall(_, b)
    | Funext(_, b)
    | Assume(_, _, b)
    | Alias(_, _, b)
    | Generalize(_, b)
    | Revert(_, _, b)
    | Obtain(_, _, _, b) => proof_nodes(b)
    | Have(_, s, b) => proof_nodes(s) @ proof_nodes(b)
    | RelEqBy(_, j) => proof_nodes(j)
    | Induction(_, _, cases) =>
      List.concat_map(((_, b)) => proof_nodes(b), cases)
    | EmptyHole
    | Invalid(_)
    | MultiHole(_)
    | AxiomStep(_)
    | AlgebriteStep(_)
    | EvalStep(_)
    | Witness(_)
    | Cite(_)
    | Contradiction(_) => []
    }
  );

type obs = {
  errors: list(string),
  pm: ProofMap.t,
  theorems: list((string, Proof.t)),
  runtime: Exp.t,
};

let observe = (label: string, src: string): obs => {
  let (statics, elab) =
    Statics.mk(
      CoreSettings.on,
      Builtins.ctx_init(Some(Operators.default_mode)),
      parse_exp(src),
    );
  let errors = Haz3lcore.ErrorPrint.all(statics);
  List.iter(
    msg => print_endline("EXISTS " ++ label ++ " static_error: " ++ msg),
    errors,
  );
  let (result, state) =
    Evaluator.evaluate(~statics, ~env=Builtins.env_init, elab);
  let pm = EvaluatorState.get_proof_map(state);
  let found = ref([]);
  let _ =
    Exp.map_term(
      ~f_exp=
        (continue, e: Exp.t) => {
          switch (e.term) {
          | Theorem(p, _, proof, _) =>
            found :=
              [
                (Pat.get_var(p) |> Option.value(~default="?"), proof),
                ...found^,
              ]
          | _ => ()
          };
          continue(e);
        },
      elab,
    );
  let theorems = List.rev(found^);
  print_endline("EXISTS " ++ label ++ " runtime: " ++ print_exp(result));
  List.iter(
    ((name, proof)) => {
      print_endline(
        "EXISTS "
        ++ label
        ++ " theorem "
        ++ name
        ++ " full: "
        ++ ProofMap.show_full_status(ProofMap.full_status_of_proof(pm, proof)),
      );
      List.iter(
        (n: Proof.t) =>
          switch (ProofMap.lookup(Proof.rep_id(n), pm)) {
          | None => ()
          | Some(entry) =>
            print_endline(
              "EXISTS   "
              ++ Proof.show_cls(Proof.cls_of_term(n.term))
              ++ " in: "
              ++ (
                entry.incoming
                |> Option.map(print_exp)
                |> Option.value(~default="-")
              )
              ++ " | out: "
              ++ (
                entry.outgoing
                |> Option.map(print_exp)
                |> Option.value(~default="-")
              )
              ++ " | marks: "
              ++ String.concat("; ", List.map(ProofMark.show, entry.marks))
              ++ " | obligations: "
              ++ String.concat(
                   "; ",
                   List.map(Obligation.show, entry.obligations),
                 ),
            )
          },
        proof_nodes(proof),
      );
    },
    theorems,
  );
  {
    errors,
    pm,
    theorems,
    runtime: result,
  };
};

let theorem = (o: obs, name) =>
  switch (List.assoc_opt(name, o.theorems)) {
  | Some(p) => p
  | None => Alcotest.fail("no theorem " ++ name)
  };

let full = (o: obs, name) =>
  ProofMap.show_full_status(
    ProofMap.full_status_of_proof(o.pm, theorem(o, name)),
  );

let valid = (label, o: obs) =>
  check(list(string), label ++ ": no static errors", [], o.errors);

/* Marks / outgoing / obligations of the FIRST node of class `cls`. */
let node = (o: obs, name, cls: Proof.cls): ProofMap.entry => {
  let n =
    proof_nodes(theorem(o, name))
    |> List.find_opt((n: Proof.t) => Proof.cls_of_term(n.term) == cls);
  switch (n) {
  | None => Alcotest.fail("no " ++ Proof.show_cls(cls) ++ " in " ++ name)
  | Some(n) =>
    switch (ProofMap.lookup(Proof.rep_id(n), o.pm)) {
    | Some(e) => e
    | None => Alcotest.fail("unreached " ++ Proof.show_cls(cls))
    }
  };
};

let nodes = (o: obs, name, cls: Proof.cls): list(ProofMap.entry) =>
  proof_nodes(theorem(o, name))
  |> List.filter((n: Proof.t) => Proof.cls_of_term(n.term) == cls)
  |> List.filter_map((n: Proof.t) => ProofMap.lookup(Proof.rep_id(n), o.pm));

let refused_rules = (e: ProofMap.entry): list(string) =>
  List.filter_map(
    (m: ProofMark.t) =>
      switch (m) {
      | ProofMark.ProverStepRefused({rule, _}) => Some(rule)
      | _ => None
      },
    e.marks,
  );

let has_reason = (e: ProofMap.entry, sub: string): bool =>
  List.exists(
    (m: ProofMark.t) =>
      switch (m) {
      | ProofMark.ProverStepRefused({reason, _}) =>
        let n = String.length(sub);
        let rec go = i =>
          i + n <= String.length(reason)
          && (String.sub(reason, i, n) == sub || go(i + 1));
        go(0);
      | _ => false
      },
    e.marks,
  );

let out = (e: ProofMap.entry) =>
  e.outgoing |> Option.map(print_exp) |> Option.value(~default="-");

let pending = (e: ProofMap.entry) =>
  List.filter(Obligation.is_pending, e.obligations) |> List.length;

/* The witness's standardness side condition `!!v` and how it was
   discharged ("Evaluated", "Structural", "(Remote …)", "Pending"). */
let std_discharge = (e: ProofMap.entry): string =>
  switch (
    List.find_opt(
      (o: Obligation.t) =>
        switch (o.goal.term) {
        | Valued(_) => true
        | _ => false
        },
      e.obligations,
    )
  ) {
  | Some(o) => Obligation.show_discharge(o.discharge)
  | None => "NO !!v OBLIGATION"
  };

/* --- programs ------------------------------------------------------- */

let sq9 = "exists (n : Int) -> n * n == 9";

let positive =
  "theorem w = "
  ++ sq9
  ++ " proof\n  == true by witness 3 end;\n  eval 3 * 3 at 0 end;\n  eval 9 == 9 at 0 end\nin 0";

let alone = "theorem w = " ++ sq9 ++ " proof == true by witness 3 end in 0";

let bad =
  "theorem w = "
  ++ sq9
  ++ " proof\n  == true by witness 2 end;\n  eval 2 * 2 at 0 end;\n  eval 4 == 9 at 0 end\nin 0";

let wrong_type =
  "theorem w = " ++ sq9 ++ " proof == true by witness true end in 0";

let divergent =
  "let loop : Int -> Int = fun k -> loop(k) in\n"
  ++ "theorem w = exists (n : Int) -> n == n proof == true by witness loop(0) end in 0";

let partial =
  "theorem w = exists (n : Int) -> n == n proof == true by witness 1 / 0 end in 0";

let lambda_divergent =
  "let loop : Int -> Int = fun k -> loop(k) in\n"
  ++ "theorem w = exists (f : Int -> Int) -> f(1) == f(1) proof == true by witness fun (k : Int) -> loop(k) end in 0";

let lambda_total =
  "theorem w = exists (f : Int -> Int) -> f(1) == f(1) proof == true by witness fun (k : Int) -> k end in 0";

let lambda_partial =
  "theorem w = exists (f : Int -> Int) -> f(1) == f(1) proof == true by witness fun (k : Int) -> 1 / k end in 0";

let open_scoped =
  "theorem w = forall (m : Int) -> exists (n : Int) -> n == m + 1 proof\n"
  ++ "  == true by witness m + 1 end\nin 0";

let capture =
  "theorem w = forall (k : Int) -> exists (n : Int) -> (fun (k : Int) -> n + k)(0) == k proof\n"
  ++ "  == true by witness k end\nin 0";

let nested =
  "theorem w = exists (a : Int) -> exists (b : Int) -> a + b == 3 proof\n"
  ++ "  == true by witness 1 end;\n  == true by witness 2 end;\n"
  ++ "  eval 1 + 2 at 0 end;\n  eval 3 == 3 at 0 end\nin 0";

let untyped =
  "theorem w = exists n -> n == 1 proof == true by witness 1 end in 0";

let not_exists =
  "theorem w = 1 == 1 proof == true by witness 1 end in 0";

let ex5 = "(exists (n : Int) -> n == 5)";

let obtain_ok =
  "theorem t = "
  ++ ex5
  ++ " ==> (exists (m : Int) -> m == 5) proof\n"
  ++ "  assume "
  ++ ex5
  ++ " as he =>\n  obtain x as hx from he =>\n"
  ++ "  == true by witness x end;\n  == true by hx end\nin 0";

let obtain_escape =
  "theorem t = "
  ++ ex5
  ++ " ==> (exists (m : Int) -> m == 5) proof\n"
  ++ "  assume "
  ++ ex5
  ++ " as he =>\n  obtain x as hx from he =>\n"
  ++ "  == true by witness x end\nin 0";

let obtain_unknown =
  "theorem t = 5 == 5 proof obtain x as hx from nope => ? in 0";

let obtain_unavailable =
  "theorem t = 5 == 5 proof obtain x as hx from " ++ ex5 ++ " => ? in 0";

let obtain_not_fresh =
  "theorem t = forall (x : Int) -> "
  ++ ex5
  ++ " ==> x == x proof\n"
  ++ "  assume "
  ++ ex5
  ++ " as he =>\n  obtain x as hx from he => ?\nin 0";

let obtain_shadow =
  "theorem t = "
  ++ ex5
  ++ " ==> (exists (m : Int) -> m == 5) proof\n"
  ++ "  assume "
  ++ ex5
  ++ " as he =>\n"
  ++ "  obtain x as h from he =>\n  obtain y as h from he =>\n"
  ++ "  == true by witness x end;\n  == true by h end\nin 0";

let std_named =
  "theorem s = true ==> (forall (x : Bool) -> !!x) proof\n"
  ++ "  assume true => forall (x : Bool !! h) => == true by h end\nin 0";

let std_shadow =
  "theorem s = true ==> (forall (x : Bool) -> forall (y : Bool) -> !!x) proof\n"
  ++ "  assume true => forall (x : Bool !! h) => forall (y : Bool !! h) => == true by h end\nin 0";

let std_escape =
  "theorem a = true ==> (forall (x : Bool) -> !!x) proof assume true => forall (x : Bool !! h) => ? in\n"
  ++ "theorem b = true ==> (forall (z : Bool) -> !!z) proof assume true => forall z => == true by h end in 0";

let std_unnamed =
  "theorem s = true ==> (forall (x : Bool) -> !!x) proof assume true => forall x => == true by h end in 0";

let std_mismatch =
  "theorem s = true ==> (forall (x : Bool) -> !!x) proof assume true => forall (w : Bool !! h) => == true by h end in 0";

let std_exp_level = "forall (x : Bool !! h) -> x";

let std_pattern =
  "theorem s = true ==> (forall (x : Bool) -> !!x) proof assume true => forall (x !! h) => ? in 0";

let cite_alone =
  "theorem s = true ==> (forall (x : Bool) -> !!x) proof assume true => forall (x : Bool !! h) => h in 0";

/* D80 logical origin: a quantifier- or obtain-bound FUNCTION variable is
   standard; a program lambda or an induction case variable is not. */
let fn_stmt_binder =
  "theorem w = forall (f : Int -> Int) -> exists (g : Int -> Int) -> g(0) == f(0) proof\n"
  ++ "  == true by witness f end\nin 0";

let fn_obtained =
  "theorem w = (exists (h : Int -> Int) -> h(0) == 0) ==> (exists (g : Int -> Int) -> g(0) == 0) proof\n"
  ++ "  assume (exists (h : Int -> Int) -> h(0) == 0) as he =>\n"
  ++ "  obtain k as hk from he =>\n  == true by witness k end;\n  == true by hk end\nin 0";

let fn_program =
  "let idf = fun (k : Int) -> 1 / k in\n"
  ++ "theorem w = exists (g : Int -> Int) -> g(0) == 0 proof == true by witness idf end in 0";

let fn_case_shadow =
  "theorem w = forall (f : Int -> Int) -> forall (fs : [Int -> Int]) -> exists (g : Int -> Int) -> g(0) == g(0) proof\n"
  ++ "  induction fs\n  | [] => ?\n  | f :: t => == true by witness f end\n  end\nin 0";

/* Computed scrutinee whose standardness is not shown (a partial lambda
   element): its case variable `f`, shadowing the standard statement `f`,
   must not inherit standardness. */
let fn_case_shadow_computed =
  "theorem w = forall (f : Int -> Int) -> forall (fs : [Int -> Int]) -> exists (g : Int -> Int) -> g(0) == g(0) proof\n"
  ++ "  induction [fun (k : Int) -> 1 / k] @ fs\n  | [] => ?\n  | f :: t => == true by witness f end\n  end\nin 0";

/* Existing-status control: a proof with NO witness step still reads
   Refuted when it computes the goal to `false`. */
let refuted_control =
  "theorem r = 1 == 2 proof eval 1 == 2 at 0 end in 0";

/* Keyword receipt: the global builtin `exists` (alias of `any`,
   BuiltinsList) as baseline source would call it. */
let alias_call = "exists([1, 2], fun (x : Int) -> x > 1)";
let any_call = "any([1, 2], fun (x : Int) -> x > 1)";

/* B1 (independent review): a first-order witness built with a PARTIAL
   builtin is not standard, so the existential must not become Proven,
   even though `refl_eq` closes the instantiated body. Control: a literal
   witness is Proven by the same proof shape. */
let b1_head =
  "theorem w = exists (n : Int) -> n == (head([]) : Int) proof\n"
  ++ "  == true by witness (head([]) : Int) end;\n"
  ++ "  axiom refl_eq at 0 on (head([]) : Int) == (head([]) : Int) end\nin 0";

let b1_tail =
  "theorem w = exists (l : [Int]) -> l == (tail([]) : [Int]) proof\n"
  ++ "  == true by witness (tail([]) : [Int]) end;\n"
  ++ "  axiom refl_eq at 0 on (tail([]) : [Int]) == (tail([]) : [Int]) end\nin 0";

let b1_control =
  "theorem w = exists (n : Int) -> n == 3 proof\n"
  ++ "  == true by witness 3 end;\n  axiom refl_eq at 0 on 3 == 3 end\nin 0";

let roundtrip_programs = [
  ("exists", "exists (n : Int) -> n * n == 9"),
  ("valued", "!!(1, [2])"),
  ("positive", positive),
  ("obtain", obtain_ok),
  ("std_named", std_named),
];

/* Evaluate a closed Bool expression and print the result. */
let eval_print = (src: string): string => {
  let (_, elab) =
    Statics.mk(
      CoreSettings.on,
      Builtins.ctx_init(Some(Operators.default_mode)),
      parse_exp(src),
    );
  let (result, _) = Evaluator.evaluate(~env=Builtins.env_init, elab);
  let s = print_exp(result);
  print_endline("EXISTS eval " ++ src ++ " ~> " ++ s);
  s;
};

let tests = (
  "ResetExistentials",
  [
    test_case("roundtrip: parse/print fixed point, statics clean", `Quick, () =>
      List.iter(
        ((label, src)) => {
          let t1 = print_exp(parse_exp(src));
          let t2 = print_exp(parse_exp(t1));
          print_endline("EXISTS roundtrip " ++ label ++ ": " ++ t1);
          check(string, label ++ ": fixed point", t1, t2);
          valid(label, observe("rt-" ++ label, src));
        },
        roundtrip_programs,
      )
    ),
    test_case("witness 3: Proven; receipt and instantiated body", `Quick, () => {
      let o = observe("positive", positive);
      valid("positive", o);
      let w = node(o, "w", RelEqBy);
      check(list(string), "no refusal", [], refused_rules(w));
      check(string, "instantiated body", "3 * 3 == 9", out(w));
      check(string, "!!3 discharged by evaluation", "Evaluated", std_discharge(w));
      check(int, "nothing pending", 0, pending(w));
      check(int, "receipt", 1, List.length(w.substitutions));
      check(string, "full", "Proven", full(o, "w"));
    }),
    test_case("witness alone never proves the existential", `Quick, () => {
      let o = observe("alone", alone);
      valid("alone", o);
      check(string, "outgoing is P[3]", "3 * 3 == 9", out(node(o, "w", RelEqBy)));
      check(string, "full", "Incomplete", full(o, "w"));
    }),
    test_case("bad witness: false subgoal is NOT a refutation", `Quick, () => {
      let o = observe("bad", bad);
      valid("bad", o);
      check(list(string), "witness itself accepted", [], refused_rules(node(o, "w", RelEqBy)));
      check(string, "full", "Incomplete", full(o, "w"));
    }),
    test_case("wrong-type witness refused", `Quick, () => {
      let o = observe("wrong_type", wrong_type);
      let w = node(o, "w", RelEqBy);
      check(list(string), "refused", ["witness"], refused_rules(w));
      check(
        string,
        "goal passes through",
        w.incoming |> Option.map(print_exp) |> Option.value(~default="-"),
        out(w),
      );
      check(string, "full", "Incomplete", full(o, "w"));
    }),
    test_case("divergent witness refused (no obligation)", `Quick, () => {
      let o = observe("divergent", divergent);
      let w = node(o, "w", RelEqBy);
      check(bool, "PossiblyDivergentInstantiation", true,
        List.exists((m: ProofMark.t) => switch (m) { | ProofMark.PossiblyDivergentInstantiation(_) => true | _ => false }, w.marks));
      check(string, "full", "Incomplete", full(o, "w"));
    }),
    test_case("partial witness 1/0: pending domain obligation, never Proven", `Quick, () => {
      let o = observe("partial", partial);
      let w = node(o, "w", RelEqBy);
      check(bool, "pending obligation", true, pending(w) >= 1);
      check(bool, "not Proven", false, full(o, "w") == "Proven");
    }),
    test_case("lambda with divergent body refused; std_fun for a total lambda; partial lambda pending", `Quick, () => {
      let o = observe("lambda_divergent", lambda_divergent);
      let w = node(o, "w", RelEqBy);
      check(bool, "divergent lambda refused as divergent", true,
        List.exists((m: ProofMark.t) => switch (m) { | ProofMark.PossiblyDivergentInstantiation(_) => true | _ => false }, w.marks));
      let o2 = observe("lambda_total", lambda_total);
      let w2 = node(o2, "w", RelEqBy);
      check(list(string), "total lambda accepted", [], refused_rules(w2));
      check(string, "std_fun: !!(fun k -> k) structural", "Structural", std_discharge(w2));
      let o3 = observe("lambda_partial", lambda_partial);
      let w3 = node(o3, "w", RelEqBy);
      check(string, "fun k -> 1 / k is not standard", "Pending", std_discharge(w3));
      check(bool, "not Proven", false, full(o3, "w") == "Proven");
    }),
    test_case("open scoped witness over a forall binder (D80 auto)", `Quick, () => {
      let o = observe("open_scoped", open_scoped);
      valid("open_scoped", o);
      let w = node(o, "w", RelEqBy);
      check(list(string), "accepted", [], refused_rules(w));
      check(string, "instantiated", "m + 1 == m + 1", out(w));
      check(string, "!!(m + 1) by the structural rules", "Structural", std_discharge(w));
      check(int, "nothing pending", 0, pending(w));
    }),
    test_case("capturing witness refused", `Quick, () => {
      let o = observe("capture", capture);
      let w = node(o, "w", RelEqBy);
      check(list(string), "refused", ["witness"], refused_rules(w));
      check(bool, "capture reason", true, has_reason(w, "capture"));
    }),
    test_case("nested existentials: two witnesses, Proven", `Quick, () => {
      let o = observe("nested", nested);
      valid("nested", o);
      check(string, "full", "Proven", full(o, "w"));
    }),
    test_case("untyped binder / non-exists goal refused", `Quick, () => {
      let o = observe("untyped", untyped);
      check(list(string), "untyped", ["witness"], refused_rules(node(o, "w", RelEqBy)));
      let o2 = observe("not_exists", not_exists);
      check(list(string), "not exists", ["witness"], refused_rules(node(o2, "w", RelEqBy)));
      check(bool, "not Proven", false, full(o2, "w") == "Proven");
    }),
    test_case("obtain: fresh x and h : P[x], Proven (assume gate !!(exists n -> n == 5) now discharged structurally)", `Quick, () => {
      let o = observe("obtain_ok", obtain_ok);
      valid("obtain_ok", o);
      check(list(string), "obtain accepted", [], refused_rules(node(o, "t", Obtain)));
      check(string, "full", "Proven", full(o, "t"));
    }),
    test_case("obtain: eigenvariable escape refused", `Quick, () => {
      let o = observe("obtain_escape", obtain_escape);
      let ob = node(o, "t", Obtain);
      check(list(string), "escape", ["obtain-escape"], refused_rules(ob));
      check(bool, "not Proven", false, full(o, "t") == "Proven");
    }),
    test_case("obtain: unknown / unavailable fact refused", `Quick, () => {
      let o = observe("obtain_unknown", obtain_unknown);
      check(bool, "static error", true, o.errors != []);
      check(
        list(string),
        "unknown name (capture-aware citation path)",
        ["(UnknownEquality \"nope\")"],
        List.map(ProofMark.show, node(o, "t", Obtain).marks),
      );
      let o2 = observe("obtain_unavailable", obtain_unavailable);
      check(list(string), "spelled-out but not a fact", ["obtain"], refused_rules(node(o2, "t", Obtain)));
    }),
    test_case("obtain: non-fresh name refused; shadowed fact name", `Quick, () => {
      let o = observe("obtain_not_fresh", obtain_not_fresh);
      check(bool, "not fresh", true, has_reason(node(o, "t", Obtain), "fresh"));
      let o2 = observe("obtain_shadow", obtain_shadow);
      /* inner `h : y == 5` shadows outer `h : x == 5`: citing `h` for
         `x == 5` must fail. */
      let cites = nodes(o2, "t", RelEqBy);
      check(int, "two steps", 2, List.length(cites));
      check(list(string), "shadowed citation refused", ["cite"], refused_rules(List.nth(cites, 1)));
      check(bool, "not Proven", false, full(o2, "t") == "Proven");
    }),
    test_case("D78 binder fact: forall (x : Bool !! h) => == true by h end", `Quick, () => {
      let o = observe("std_named", std_named);
      valid("std_named", o);
      check(string, "full", "Proven", full(o, "s"));
    }),
    test_case("D78 scoping: shadowing, non-escape, unnamed, mismatch", `Quick, () => {
      let o = observe("std_shadow", std_shadow);
      check(list(string), "inner h is !!y", ["cite"], refused_rules(node(o, "s", RelEqBy)));
      let o2 = observe("std_escape", std_escape);
      check(bool, "h not in scope in b (statics)", true, o2.errors != []);
      check(bool, "b not Proven", false, full(o2, "b") == "Proven");
      let o3 = observe("std_unnamed", std_unnamed);
      check(bool, "unnamed: no h", false, full(o3, "s") == "Proven");
      let o4 = observe("std_mismatch", std_mismatch);
      check(bool, "mismatch refused", true, List.mem("std-binder", refused_rules(node(o4, "s", Forall))));
      check(bool, "mismatch not Proven", false, full(o4, "s") == "Proven");
    }),
    test_case("D78 binder off a proof forall / unresolved shapes: static error", `Quick, () => {
      check(bool, "exp-level", true, observe("std_exp", std_exp_level).errors != []);
      check(bool, "pattern without type", true, observe("std_pattern", std_pattern).errors != []);
      check(bool, "bare citation as a step", true, observe("cite_alone", cite_alone).errors != []);
    }),
    test_case("D80 origin: quantifier/obtain function variables are standard; program lambda and case variable are not", `Quick, () => {
      let o = observe("fn_stmt_binder", fn_stmt_binder);
      valid("fn_stmt_binder", o);
      let w = node(o, "w", RelEqBy);
      check(list(string), "accepted", [], refused_rules(w));
      check(string, "!!f: quantifier origin", "Structural", std_discharge(w));
      check(int, "nothing pending", 0, pending(w));
      let o2 = observe("fn_obtained", fn_obtained);
      valid("fn_obtained", o2);
      let ws = nodes(o2, "w", RelEqBy);
      check(string, "!!k: obtain origin (someStd)", "Structural", std_discharge(List.hd(ws)));
      check(string, "full", "Proven", full(o2, "w"));
      let o3 = observe("fn_program", fn_program);
      let w3 = node(o3, "w", RelEqBy);
      check(list(string), "program lambda accepted as a witness", [], refused_rules(w3));
      check(string, "its !!v stays pending", "Pending", std_discharge(w3));
      let o4 = observe("fn_case_shadow", fn_case_shadow);
      let w4 = node(o4, "w", RelEqBy);
      check(list(string), "case variable accepted as a witness", [], refused_rules(w4));
      /* U1 contract: induction on the STANDARD binder `fs` makes its case
         variable `f` standard (kernel induct), even though it shadows the
         statement's `f`. */
      check(string, "case variable of a standard scrutinee is standard (U1)", "Structural", std_discharge(w4));
      let o5 = observe("fn_case_shadow_computed", fn_case_shadow_computed);
      valid("fn_case_shadow_computed", o5);
      let w5 = node(o5, "w", RelEqBy);
      check(list(string), "computed-scrutinee case variable accepted as a witness", [], refused_rules(w5));
      check(string, "computed (non-standard) scrutinee: shadowing case variable stays Pending", "Pending", std_discharge(w5));
    }),
    test_case("B1: partial-builtin first-order witness is never Proven (literal control is)", `Quick, () => {
      let c = observe("b1_control", b1_control);
      valid("b1_control", c);
      check(string, "control: literal witness + refl_eq", "Proven", full(c, "w"));
      List.iter(
        ((label, src)) => {
          let o = observe(label, src);
          valid(label, o);
          check(bool, label ++ ": not Proven", false, full(o, "w") == "Proven");
        },
        [("b1_head", b1_head), ("b1_tail", b1_tail)],
      );
    }),
    test_case("structural standardness judgement: total forms only, builtins fail closed", `Quick, () => {
      let ctx =
        SemanticCtx.of_ctx_and_env(
          Builtins.ctx_init(Some(Operators.default_mode)),
          Builtins.env_init,
        );
      let std = (~ctx=ctx, src) => ProofCheck.std_structural(ctx, parse_exp(src));
      check(bool, "literal", true, std("3"));
      check(bool, "tuple/list", true, std("(1, [true, false])"));
      check(bool, "Int arithmetic on literals", true, std("1 + 2 * 3"));
      check(bool, "division", false, std("6 / 2"));
      check(bool, "builtin head", false, std("head([1])"));
      check(bool, "std_fun total body", true, std("fun (k : Int) -> k + 1"));
      check(bool, "std_fun partial body", false, std("fun (k : Int) -> 1 / k"));
      check(bool, "untyped parameter", false, std("fun k -> k"));
      check(bool, "unmarked variable", false, std("x"));
      check(bool, "marked variable", true, std(~ctx=SemanticCtx.mark_standard(ctx, ["x"]), "x + 1"));
    }),
    test_case("existing status untouched: a witness-free false proof is still Refuted", `Quick, () => {
      let o = observe("refuted_control", refuted_control);
      valid("refuted_control", o);
      check(string, "full", "Refuted", full(o, "r"));
    }),
    test_case("keyword receipt: builtin `exists` call vs `any`", `Quick, () => {
      check(string, "any still works", "true", eval_print(any_call));
      let alias =
        switch (Haz3lcore.Parser.to_term(alias_call, ~root=Exp)) {
        | None => "PARSE FAILURE"
        | Some(e) =>
          let (statics, elab) =
            Statics.mk(
              CoreSettings.on,
              Builtins.ctx_init(Some(Operators.default_mode)),
              e,
            );
          let errs = Haz3lcore.ErrorPrint.all(statics);
          let (r, _) = Evaluator.evaluate(~env=Builtins.env_init, elab);
          "parsed as "
          ++ Exp.show_cls(Exp.cls_of_term(e.term))
          ++ "; static errors "
          ++ string_of_int(List.length(errs))
          ++ "; value "
          ++ print_exp(r);
        };
      print_endline("EXISTS alias-call " ++ alias_call ++ " => " ++ alias);
      check(bool, "the alias call no longer evaluates to true (collision)", true, alias != "true");
    }),
    test_case("one-way standardness: no quantifier rewrite, no enumeration", `Quick, () => {
      check(string, "!!3", "true", eval_print("!!3"));
      check(string, "!!(1, [2])", "true", eval_print("!!(1, [2])"));
      let not_bool = s => s != "true" && s != "false";
      check(bool, "!!(1/0) not decided", true, eval_print("!!(1 / 0)") != "true");
      check(bool, "!!(forall) stuck", true, not_bool(eval_print("!!(forall (x : Int) -> x == x)")));
      check(bool, "!!(exists) stuck", true, not_bool(eval_print("!!(exists (x : Int) -> x == x)")));
      check(bool, "exists with all-false body not computed", true, not_bool(eval_print("exists (n : Int) -> false")));
      check(bool, "!!lambda is a forall, not true", true, eval_print("!!(fun (k : Int) -> k)") != "true");
    }),
    /* Receipt (no assertion): standardness-judgement CPU accumulated
       over every core test above in this process, versus the process CPU
       so far. */
    test_case("receipt: standardness judgement CPU share of this group", `Quick, () =>
      print_endline(
        "CORE std cpu "
        ++ Printf.sprintf("%.3f", ProofCheck.std_stat_cpu^)
        ++ "s in "
        ++ string_of_int(ProofCheck.std_stat_nodes^)
        ++ " nodes, "
        ++ string_of_int(ProofCheck.std_stat_certifications^)
        ++ " certifications | obligations "
        ++ string_of_int(ProofCheck.std_stat_obligations^)
        ++ " taking "
        ++ Printf.sprintf("%.3f", ProofCheck.std_stat_obligation_cpu^)
        ++ "s | process cpu "
        ++ Printf.sprintf("%.3f", Sys.time())
        ++ "s",
      )
    ),
  ],
);

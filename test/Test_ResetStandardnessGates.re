/* Existentials finish stage: the decided standardness gates.
   - D47/D51: `assume P` introducing `P ==> Q` records `!!P`, checked in the
     PRE-assumption context; unresolved gates stay Pending (never Proven).
   - D81 forward std_all / std_ex: `!!(forall|exists x. P)` from a genuine
     `forall x. !!P` (an uncaptured in-scope premise, or the structural
     rules), never the converse.
   - D80: funext only at first-order domains.
   - D66: closed evaluation never runs a fixpoint.
   Assertions read the reference's own ProofMap (marks, obligations with
   their discharge kind and bindings, full status) through the same
   harness as ^ResetExistentials$ (statics supplied). */
open Alcotest;
open Language;

module RE = Test_ResetExistentials;

/* The `!!` obligations recorded at the first node of class `cls`. */
let gates = (o: RE.obs, name, cls: Proof.cls): list(Obligation.t) =>
  RE.nodes(o, name, cls)
  |> List.concat_map((e: ProofMap.entry) => e.obligations)
  |> List.filter((ob: Obligation.t) =>
       switch (ob.goal.term) {
       | Valued(_) => true
       | _ => false
       }
     );

let kinds = (obs: list(Obligation.t)): list(string) =>
  List.map((ob: Obligation.t) => Obligation.show_discharge(ob.discharge), obs);

let starts_with = (s, p) =>
  String.length(s) >= String.length(p)
  && String.sub(s, 0, String.length(p)) == p;

let binds_fact = (ob: Obligation.t, h: string): bool =>
  List.exists(
    (e: Ctx.entry) =>
      switch (e) {
      | TheoremEntry({name, _}) => name == h
      | _ => false
      },
    ob.bindings,
  );

let loop = "let loop : Int -> Int = fun k -> loop(k) in\n";

let assume_true =
  "theorem t = true ==> 1 == 1 proof assume true => eval 1 == 1 at 0 end in 0";
let assume_false =
  "theorem t = false ==> 1 == 1 proof assume false => eval 1 == 1 at 0 end in 0";

let curry =
  loop
  ++ "theorem t = (loop(0) == 0) ==> 1 == 1 proof assume loop(0) == 0 => eval 1 == 1 at 0 end in 0";

let self_gate =
  loop
  ++ "theorem t = (!!(loop(0))) ==> !!(loop(0)) proof assume !!(loop(0)) as h => == true by h end in 0";

let std_exists =
  "theorem t = (exists (n : Int) -> n == 5) ==> (exists (m : Int) -> m == 5) proof\n"
  ++ "  assume (exists (n : Int) -> n == 5) as he =>\n"
  ++ "  obtain x as hx from he =>\n  == true by witness x end;\n  == true by hx end\nin 0";

let partial_exists =
  "theorem t = (exists (n : Int) -> 1 / n == 1) ==> 1 == 1 proof\n"
  ++ "  assume (exists (n : Int) -> 1 / n == 1) => eval 1 == 1 at 0 end\nin 0";

let partial_forall =
  "theorem t = (forall (n : Int) -> 1 / n == 1) ==> 1 == 1 proof\n"
  ++ "  assume (forall (n : Int) -> 1 / n == 1) => eval 1 == 1 at 0 end\nin 0";

let premise_ok =
  "theorem t = forall (f : Int -> Int) -> (forall (n : Int) -> !!(f(n) + 1 / f(n) == 0)) ==> ((exists (n : Int) -> f(n) + 1 / f(n) == 0) ==> 1 == 1) proof\n"
  ++ "  assume (forall (n : Int) -> !!(f(n) + 1 / f(n) == 0)) as hp =>\n"
  ++ "  assume (exists (n : Int) -> f(n) + 1 / f(n) == 0) =>\n"
  ++ "  eval 1 == 1 at 0 end\nin 0";

let premise_other_body =
  "theorem t = forall (f : Int -> Int) -> (forall (n : Int) -> !!(f(n) + 1 / f(n) == 1)) ==> ((exists (n : Int) -> f(n) + 1 / f(n) == 0) ==> 1 == 1) proof\n"
  ++ "  assume (forall (n : Int) -> !!(f(n) + 1 / f(n) == 1)) as hp =>\n"
  ++ "  assume (exists (n : Int) -> f(n) + 1 / f(n) == 0) =>\n"
  ++ "  eval 1 == 1 at 0 end\nin 0";

let premise_captured =
  "theorem t = forall (f : Int -> Int) -> (forall (n : Int) -> !!(f(n) + 1 / f(n) == 0)) ==> (forall (f : Int -> Int) -> (exists (n : Int) -> f(n) + 1 / f(n) == 0) ==> 1 == 1) proof\n"
  ++ "  assume (forall (n : Int) -> !!(f(n) + 1 / f(n) == 0)) as hp =>\n"
  ++ "  forall f =>\n"
  ++ "  assume (exists (n : Int) -> f(n) + 1 / f(n) == 0) =>\n"
  ++ "  eval 1 == 1 at 0 end\nin 0";

let fn_origin =
  "theorem w = forall (f : Int -> Int) -> exists (g : Int -> Int) -> g(0) == f(0) proof == true by witness f end in 0";

let fn_obtained =
  "theorem w = (exists (h : Int -> Int) -> h(0) == 0) ==> (exists (g : Int -> Int) -> g(0) == 0) proof\n"
  ++ "  assume (exists (h : Int -> Int) -> h(0) == 0) as he =>\n"
  ++ "  obtain k as hk from he =>\n  == true by witness k end;\n  == true by hk end\nin 0";

let funext_fo =
  "theorem t = (fun (x : Int) -> x + 1) == (fun (x : Int) -> 1 + x) proof funext x => ? in 0";
let funext_ho =
  "theorem t = (fun (g : Int -> Int) -> g(0)) == (fun (g : Int -> Int) -> g(0)) proof funext g => ? in 0";
let funext_tuple =
  "theorem t = (fun (p : (Int, Int -> Int)) -> 1) == (fun (p : (Int, Int -> Int)) -> 1) proof funext p => ? in 0";
let funext_alias =
  "type F = Int -> Int in\ntheorem t = (fun (g : F) -> 1) == (fun (g : F) -> 1) proof funext g => ? in 0";

let funext_refusals = (o: RE.obs) =>
  RE.nodes(o, "t", Proof.Funext) |> List.concat_map(RE.refused_rules);

let tests = (
  "ResetStandardnessGates",
  [
    test_case("assume true / false: gate discharged by evaluation, Proven", `Quick, () => {
      List.iter(
        ((label, src)) => {
          let o = RE.observe(label, src);
          RE.valid(label, o);
          check(list(string), label ++ ": gate", ["Evaluated"], kinds(gates(o, "t", Proof.Assume)));
          check(string, label ++ ": full", "Proven", RE.full(o, "t"));
        },
        [("assume_true", assume_true), ("assume_false", assume_false)],
      )
    }),
    test_case("Curry / nonstandard antecedent: real Pending gate, not Proven", `Quick, () => {
      let o = RE.observe("curry", curry);
      RE.valid("curry", o);
      check(list(string), "gate (D66: no fixpoint run)", ["Pending"], kinds(gates(o, "t", Proof.Assume)));
      check(bool, "not Proven", false, RE.full(o, "t") == "Proven");
    }),
    test_case("the hypothesis cannot discharge its own gate", `Quick, () => {
      let o = RE.observe("self_gate", self_gate);
      RE.valid("self_gate", o);
      let g = gates(o, "t", Proof.Assume);
      check(list(string), "gate", ["Pending"], kinds(g));
      check(bool, "gate checked BEFORE h exists", false, List.exists(ob => binds_fact(ob, "h"), g));
      check(bool, "not Proven", false, RE.full(o, "t") == "Proven");
    }),
    test_case("standard-body existential passes the gate (obtain then Proven)", `Quick, () => {
      let o = RE.observe("std_exists", std_exists);
      RE.valid("std_exists", o);
      check(list(string), "gate", ["Structural"], kinds(gates(o, "t", Proof.Assume)));
      check(string, "full", "Proven", RE.full(o, "t"));
    }),
    test_case("partial-body exists / forall: gate Pending absent a premise", `Quick, () => {
      List.iter(
        ((label, src)) => {
          let o = RE.observe(label, src);
          RE.valid(label, o);
          check(list(string), label ++ ": gate", ["Pending"], kinds(gates(o, "t", Proof.Assume)));
          check(bool, label ++ ": not Proven", false, RE.full(o, "t") == "Proven");
        },
        [("partial_exists", partial_exists), ("partial_forall", partial_forall)],
      )
    }),
    test_case("forward std_ex from a genuine premise; body/capture negatives", `Quick, () => {
      let o = RE.observe("premise_ok", premise_ok);
      RE.valid("premise_ok", o);
      let ks = kinds(gates(o, "t", Proof.Assume));
      check(int, "two gates", 2, List.length(ks));
      check(string, "premise's own gate: Pending (an assumed premise)", "Pending", List.nth(ks, 0));
      check(bool, "exists gate: Remote via the premise", true, starts_with(List.nth(ks, 1), "(Remote"));
      check(bool, "not Proven (premise gate pending)", false, RE.full(o, "t") == "Proven");
      List.iter(
        ((label, src)) => {
          let o = RE.observe(label, src);
          RE.valid(label, o);
          let ks = kinds(gates(o, "t", Proof.Assume));
          check(int, label ++ ": two gates", 2, List.length(ks));
          check(string, label ++ ": exists gate", "Pending", List.nth(ks, 1));
        },
        [("premise_other_body", premise_other_body), ("premise_captured", premise_captured)],
      );
    }),
    test_case("function / obtain origins of any type are standard", `Quick, () => {
      let o = RE.observe("fn_origin", fn_origin);
      RE.valid("fn_origin", o);
      check(string, "!!f", "Structural", RE.std_discharge(RE.node(o, "w", Proof.RelEqBy)));
      let o2 = RE.observe("fn_obtained", fn_obtained);
      RE.valid("fn_obtained", o2);
      check(list(string), "assume gate !!(exists h. h(0) == 0): std application", ["Structural"], kinds(gates(o2, "w", Proof.Assume)));
      check(string, "!!k", "Structural", RE.std_discharge(List.hd(RE.nodes(o2, "w", Proof.RelEqBy))));
      check(string, "full", "Proven", RE.full(o2, "w"));
    }),
    test_case("funext: first-order domain allowed; function / tuple-with-function / alias domains refused", `Quick, () => {
      let o = RE.observe("funext_fo", funext_fo);
      RE.valid("funext_fo", o);
      check(list(string), "first-order", [], funext_refusals(o));
      List.iter(
        ((label, src)) => {
          let o = RE.observe(label, src);
          RE.valid(label, o);
          check(list(string), label, ["funext-domain"], funext_refusals(o));
        },
        [("funext_ho", funext_ho), ("funext_tuple", funext_tuple), ("funext_alias", funext_alias)],
      );
    }),
    test_case("ordinary beta of a partial function at a safe argument still evaluates", `Quick, () => {
      check(string, "(fun x -> 6 / x)(2)", "3", RE.eval_print("(fun (x : Int) -> 6 / x)(2)"));
    }),
  ],
);

/**
 * Parser/Printer round-trip tests for refractor (probe) serialization.
 *
 * These tests verify that parse(code) |> print == code, which naturally tests:
 * - Whitespace preservation in refractor_seg_to_seg
 * - Probe/refractor serialization (^^probe syntax)
 * - Correct ordering of syntax elements
 */
open Alcotest;
open Haz3lcore;

/* Round-trip test: parse then print should give back the original. */
let test_round_trip = (~name: string, ~code: string): test_case(_) =>
  test_case(name, `Quick, () => {
    switch (Parser.to_zipper(~root=Exp, code)) {
    | None => fail("Failed to parse: " ++ code)
    | Some(z) =>
      let result = Printer.of_zipper(z);
      print_endline("Input:  " ++ code);
      print_endline("Output: " ++ result);
      check(string, "Round trip", code, result);
    }
  });

/* Basic round-trip tests without probes */
let basic_tests = [
  /* Simple expressions */
  test_round_trip(~name="Integer", ~code="1"),
  test_round_trip(~name="Binary operation", ~code="1 + 2"),
  test_round_trip(~name="Multiple spaces", ~code="1  +  2"),
  test_round_trip(~name="Let expression", ~code="let x = 1 in x"),
  test_round_trip(~name="Lambda", ~code="fun x -> x + 1"),
  test_round_trip(~name="If expression", ~code="if true then 1 else 2"),
  /* More realistic programs */
  test_round_trip(
    ~name="Factorial",
    ~code=
      "let fact = fun n -> if n <= 0 then 1 else n * fact(n - 1) in fact(5)",
  ),
  test_round_trip(
    ~name="List map",
    ~code=
      "let map = fun f -> fun xs -> case xs | [] => [] | hd::tl => f(hd)::map(f)(tl) end in map(fun x -> x + 1)([1, 2, 3])",
  ),
  test_round_trip(
    ~name="Fibonacci",
    ~code=
      "let fib = fun n -> if n <= 1 then n else fib(n - 1) + fib(n - 2) in fib(10)",
  ),
  test_round_trip(
    ~name="Fold left",
    ~code=
      "let fold = fun f -> fun acc -> fun xs -> case xs | [] => acc | hd::tl => fold(f)(f(acc, hd))(tl) end in fold(fun (a, b) -> a + b)(0)([1, 2, 3, 4, 5])",
  ),
  test_round_trip(
    ~name="Filter",
    ~code=
      "let filter = fun p -> fun xs -> case xs | [] => [] | hd::tl => if p(hd) then hd::filter(p)(tl) else filter(p)(tl) end in filter(fun x -> x > 2)([1, 2, 3, 4])",
  ),
];

/* Probe round-trip tests */
let probe_tests = [
  /* Simple probes */
  test_round_trip(~name="Probe on atom", ~code="^^probe(1)"),
  test_round_trip(~name="Probe on binop", ~code="^^probe(1 + 2)"),
  test_round_trip(~name="Probe in let def", ~code="let x = ^^probe(1) in x"),
  test_round_trip(~name="Nested probes", ~code="^^probe(1 + ^^probe(2 * 3))"),
  /* Probed factorial - probes on recursive call and result */
  test_round_trip(
    ~name="Probed factorial",
    ~code=
      "let fact = fun n -> ^^probe(if n <= 0 then 1 else ^^probe(n * ^^probe(fact(n - 1)))) in fact(5)",
  ),
  /* Probed map - probes on function application and recursive structure */
  test_round_trip(
    ~name="Probed map",
    ~code=
      "let map = fun f -> fun xs -> case xs | [] => [] | hd::tl => ^^probe(f(hd))::^^probe(map(f)(tl)) end in map(fun x -> ^^probe(x + 1))([1, 2, 3])",
  ),
  /* Probed fibonacci - probes on both recursive branches and sum */
  test_round_trip(
    ~name="Probed fibonacci",
    ~code=
      "let fib = fun n -> if n <= 1 then n else ^^probe(^^probe(fib(n - 1)) + ^^probe(fib(n - 2))) in ^^probe(fib(10))",
  ),
  /* Probed fold - probes on accumulator and fold steps */
  test_round_trip(
    ~name="Probed fold",
    ~code=
      "let fold = fun f -> fun acc -> fun xs -> case xs | [] => ^^probe(acc) | hd::tl => ^^probe(fold(f)(^^probe(f(acc, hd)))(tl)) end in fold(fun (a, b) -> a + b)(0)([1, 2, 3])",
  ),
  /* Probed filter with nested conditional probes */
  test_round_trip(
    ~name="Probed filter",
    ~code=
      "let filter = fun p -> fun xs -> case xs | [] => [] | hd::tl => if ^^probe(p(hd)) then hd::^^probe(filter(p)(tl)) else ^^probe(filter(p)(tl)) end in filter(fun x -> ^^probe(x > 2))([1, 2, 3, 4])",
  ),
  /* Deeply nested probes in arithmetic */
  test_round_trip(
    ~name="Deep nested probes (note this depends on associativity)",
    ~code=
      "^^probe(^^probe(^^probe(1 * 2) + ^^probe(3 * 4)) , ^^probe(^^probe(5) + 6))",
  ),
  /* Probed let chain */
  test_round_trip(
    ~name="Probed let chain",
    ~code=
      "let x = ^^probe(1 + 2) in let y = ^^probe(x * 3) in let z = ^^probe(y - 1) in ^^probe(x + y + z)",
  ),
];

/* Module round-trip tests */
let module_tests = [
  test_round_trip(~name="Module single binding", ~code="{ let x = 1 }"),
  test_round_trip(
    ~name="Module multiple bindings",
    ~code="{ let x = 1; let y = 2 }",
  ),
  test_round_trip(
    ~name="Module with type alias",
    ~code="{ type T = Int; let x : T = 1 }",
  ),
  test_round_trip(
    ~name="Module keyword",
    ~code="module M = { let x = 1 } in M.x",
  ),
  test_round_trip(
    ~name="Module keyword lowercase",
    ~code="module m = { let x = 1 } in m.x",
  ),
  test_round_trip(
    ~name="Module in let binding",
    ~code="let m = { let x = 1 } in m.x",
  ),
  test_round_trip(
    ~name="Probe inside module",
    ~code="{ let x = ^^probe(1 + 2) }",
  ),
  test_round_trip(
    ~name="Probe on module expression",
    ~code="^^probe({ let x = 1 })",
  ),
];

/* Grouping of refractor triggers in a text read whole (#2442). Loading,
   the CLI and doc slides go through PersistentZipper.parse_text, which is
   FastParse first. There a trigger's parens group its target as any
   parens would, so `^^probe(1 + 2) * 3` is `(1 + 2) * 3`; the editor adds
   parens only where the bare target would regroup. (Typing is different:
   see the Editing.RefractorTyping tests.) */
let load = (code: string): Zipper.t =>
  switch (PersistentZipper.parse_text(~source="test", ~root=Exp, code)) {
  | Some(z) => z
  | None => fail("Failed to load: " ++ code)
  };

let term_of = (z: Zipper.t): Language.Exp.t =>
  MakeTerm.from_zip_for_sem(z, ~root=Exp).term;

/* Strict on parens: the point is where the grouping lands. Projector
   wrappers are not syntax the trigger text spells. */
let testable_term =
  testable(
    Fmt.using(Language.Exp.show, Fmt.string),
    Language.Equality.(
      equality({
        ...syntactic_settings,
        ignore_projectors: true,
      }).
        exp
    ),
  );

/* Load `code`: its term is the one `term` spells without triggers, it
   prints as `printed`, and the printed text loads back to the same term
   and the same text (no paren accretion across saves). */
let test_load =
    (~name: string, ~code: string, ~term: string, ~printed: string) =>
  test_case(
    name,
    `Quick,
    () => {
      let z = load(code);
      check(testable_term, "term", term_of(load(term)), term_of(z));
      let p1 = Printer.of_zipper(z);
      check(string, "printed", printed, p1);
      let z2 = load(p1);
      check(testable_term, "reloaded term", term_of(z), term_of(z2));
      check(string, "reprinted", p1, Printer.of_zipper(z2));
    },
  );

/* No parens needed: the text survives the load byte for byte. */
let test_load_unchanged = (~name: string, ~code: string, ~term: string) =>
  test_load(~name, ~code, ~term, ~printed=code);

/* Run the loaded program with its manual refractors sampled: the result
   and each probe's samples. */
let run = (z: Zipper.t): (Language.Exp.t, Language.Sample.Map.t) => {
  open Language;
  let (_, elaborated) =
    Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term_of(z));
  let targets =
    List.fold_left(
      (acc, (id, _)) => Id.Map.add(id, Sample.{refs: []}, acc),
      Id.Map.empty,
      z.refractors.manuals,
    );
  let (result, state) =
    Evaluator.evaluate(
      ~eval_info=EvalInfo.of_targets(targets),
      ~env=Builtins.env_init,
      elaborated,
    );
  (result, EvaluatorState.get_probes(state));
};

let int_value = (e: Language.Exp.t): option(int) =>
  switch (e.term) {
  | Atom(Int(n)) => Util.Bigint.to_int(n)
  | _ => None
  };

let grouping_tests = [
  test_case(
    "Probe on a sum under * evaluates as the text groups it",
    `Quick,
    () => {
      let z = load("^^probe(1 + 2) * 3");
      let (result, probes) = run(z);
      check(option(int), "result", Some(9), int_value(result));
      /* the probe is on the sum, not on the whole product */
      check(
        list(list(option(int))),
        "probe samples",
        [[Some(3)]],
        Id.Map.bindings(probes)
        |> List.map(((_, samples)) =>
             List.map((s: Language.Sample.t) => int_value(s.value), samples)
           ),
      );
    },
  ),
  test_load(
    ~name="Probe on a sum under *",
    ~code="^^probe(1 + 2) * 3",
    ~term="(1 + 2) * 3",
    ~printed="(^^probe(1 + 2)) * 3",
  ),
  test_load(
    ~name="Probe on a sum as the right operand of *",
    ~code="3 * ^^probe(1 + 2)",
    ~term="3 * (1 + 2)",
    ~printed="3 * (^^probe(1 + 2))",
  ),
  test_load(
    ~name="Probe on a difference as the right operand of -",
    ~code="1 - ^^probe(2 - 3)",
    ~term="1 - (2 - 3)",
    ~printed="1 - (^^probe(2 - 3))",
  ),
  test_load(
    ~name="Probes on both operands of *",
    ~code="^^probe(1 + 2) * ^^probe(3 + 4)",
    ~term="(1 + 2) * (3 + 4)",
    ~printed="(^^probe(1 + 2)) * (^^probe(3 + 4))",
  ),
  test_load(
    ~name="Probe on a fun, then applied",
    ~code="^^probe(fun x -> x)(1)",
    ~term="(fun x -> x)(1)",
    ~printed="(^^probe(fun x -> x))(1)",
  ),
  test_load(
    ~name="Probe on an if under +",
    ~code="^^probe(if true then 1 else 2) + 1",
    ~term="(if true then 1 else 2) + 1",
    ~printed="(^^probe(if true then 1 else 2)) + 1",
  ),
  test_load(
    ~name="Probe on a let under +",
    ~code="^^probe(let y = 1 in y) + 1",
    ~term="(let y = 1 in y) + 1",
    ~printed="(^^probe(let y = 1 in y)) + 1",
  ),
  test_load(
    ~name="Statics on a sum under *",
    ~code="^^statics(1 + 2) * 3",
    ~term="(1 + 2) * 3",
    ~printed="(^^statics(1 + 2)) * 3",
  ),
  test_load(
    ~name="Probe on a sum under * inside a let",
    ~code="let x = ^^probe(1 + 2) * 3 in x",
    ~term="let x = (1 + 2) * 3 in x",
    ~printed="let x = (^^probe(1 + 2)) * 3 in x",
  ),
  test_load(
    ~name="Whitespace inside the trigger moves inside the parens",
    ~code="^^probe( 1 + 2 ) * 3",
    ~term="(1 + 2) * 3",
    ~printed="( ^^probe(1 + 2) ) * 3",
  ),
  /* Nested triggers add one pair, and the round trip is stable. A term
     holds one refractor (ZipperBase.add_manual), so of two triggers on the
     same target the inner one stays, as before. */
  test_load(
    ~name="Probe on a probed sum under *",
    ~code="^^probe(^^probe(1 + 2)) * 3",
    ~term="(1 + 2) * 3",
    ~printed="(^^probe(1 + 2)) * 3",
  ),
  test_load(
    ~name="Statics on a probed sum under *",
    ~code="^^statics(^^probe(1 + 2)) * 3",
    ~term="(1 + 2) * 3",
    ~printed="(^^probe(1 + 2)) * 3",
  ),
  test_load(
    ~name="Probe on a statics'd sum under *",
    ~code="^^probe(^^statics(1 + 2)) * 3",
    ~term="(1 + 2) * 3",
    ~printed="(^^statics(1 + 2)) * 3",
  ),
  test_load(
    ~name="Probe on a product of a probed sum",
    ~code="^^probe(^^probe(1 + 2) * 3)",
    ~term="(1 + 2) * 3",
    ~printed="^^probe((^^probe(1 + 2)) * 3)",
  ),
  /* Grouped already: no parens added, the text survives byte for byte */
  test_load_unchanged(
    ~name="Probe on a let definition (the probes doc)",
    ~code="let chips = ^^probe(10 + 10) in chips",
    ~term="let chips = 10 + 10 in chips",
  ),
  test_load_unchanged(
    ~name="Probe on a sum under a looser +",
    ~code="^^probe(1 + 2) + 3",
    ~term="1 + 2 + 3",
  ),
  test_load_unchanged(
    ~name="Probe on a product under +",
    ~code="1 + ^^probe(2 * 3)",
    ~term="1 + 2 * 3",
  ),
  test_load_unchanged(
    ~name="Probe on a fun body",
    ~code="fun x -> ^^probe(x + 1)",
    ~term="fun x -> x + 1",
  ),
  test_load_unchanged(
    ~name="Probe on a parenthesized sum",
    ~code="^^probe((1 + 2)) * 3",
    ~term="(1 + 2) * 3",
  ),
  test_load_unchanged(
    ~name="Probe on a sum the user parenthesized",
    ~code="(^^probe(1 + 2)) * 3",
    ~term="(1 + 2) * 3",
  ),
  test_load_unchanged(
    ~name="Probe on an application under *",
    ~code="let f = fun x -> x in ^^probe(f(1)) * 3",
    ~term="let f = fun x -> x in f(1) * 3",
  ),
  test_load_unchanged(
    ~name="Probe on a function, then applied",
    ~code="let f = fun x -> x in ^^probe(f)(1 + 2)",
    ~term="let f = fun x -> x in f(1 + 2)",
  ),
  test_load_unchanged(
    ~name="Probe on a field access under +",
    ~code="let r = (x=1) in ^^probe(r.x) + 1",
    ~term="let r = (x=1) in r.x + 1",
  ),
  test_load_unchanged(
    ~name="Probe on an operand under prefix minus",
    ~code="let f = fun x -> x in -^^probe(f(1))",
    ~term="let f = fun x -> x in -f(1)",
  ),
  test_load_unchanged(
    ~name="Probe on a negation under +",
    ~code="^^probe(-1) + 3",
    ~term="-1 + 3",
  ),
  test_load(
    ~name="Probe on a sum under * in a case rule",
    ~code="case 1 | x => ^^probe(x + 1) * 2 end",
    ~term="case 1 | x => (x + 1) * 2 end",
    ~printed="case 1 | x => (^^probe(x + 1)) * 2 end",
  ),
  test_case(
    "Probe text view keeps the grouping",
    `Quick,
    () => {
      /* the agent and the CLI read ⟦⟧ as display brackets, not parens */
      let z = load("^^probe(1 + 2) * 3");
      let text =
        ProbeText.of_segment(
          ~probe_map=Language.Sample.Map.empty,
          ~refractors=z.refractors.manuals,
          Zipper.unselect_and_zip(z),
        );
      /* the program, before the probe's (here empty) samples */
      let program = "(⟦1 + 2⟧) * 3";
      check(
        string,
        "probe text",
        program,
        String.sub(
          text,
          0,
          min(String.length(program), String.length(text)),
        ),
      );
    },
  ),
];

let tests = [
  ("RoundTrip.Basic", basic_tests),
  ("RoundTrip.Probes", probe_tests),
  ("RoundTrip.ProbeGrouping", grouping_tests),
  ("RoundTrip.Modules", module_tests),
];

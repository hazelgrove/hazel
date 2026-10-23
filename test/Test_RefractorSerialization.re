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
  /* The trigger's parens group the probed term (#2442) */
  test_round_trip(
    ~name="Probe on binop under a tighter operator",
    ~code="^^probe(1 + 2) * 3",
  ),
  test_round_trip(~name="Probe on prefix form", ~code="^^probe(-1) * 3"),
  test_round_trip(
    ~name="Probe on fun then applied",
    ~code="^^probe(fun x -> x)(1)",
  ),
  test_round_trip(
    ~name="Probe on parenthesized binop",
    ~code="^^probe((1 + 2)) * 3",
  ),
  test_round_trip(
    ~name="Probe on probed binop",
    ~code="^^probe(^^probe(1 + 2)) * 3",
  ),
  test_round_trip(
    ~name="Probe on parenthesized operand",
    ~code="^^probe((x))",
  ),
];

/* A probe is a decoration: the term must be the one the probe's parens
   spell, as the menhir parser reads it (#2442). */
let test_same_term = (~name: string, ~code: string, ~expected: string) =>
  test_case(
    name,
    `Quick,
    () => {
      let parse = s =>
        switch (Parser.to_term(s, ~root=Exp)) {
        | Some(e) => e
        | None => fail("Failed to parse: " ++ s)
        };
      check(
        testable(
          Fmt.using(Language.Exp.show, Fmt.string),
          Language.Exp.fast_equal,
        ),
        code,
        parse(expected),
        parse(code),
      );
    },
  );

let grouping_tests = [
  test_same_term(
    ~name="Probe keeps grouping under a tighter operator",
    ~code="^^probe(1 + 2) * 3",
    ~expected="(1 + 2) * 3",
  ),
  test_same_term(
    ~name="Probe keeps grouping of an applied fun",
    ~code="^^probe(fun x -> x)(1)",
    ~expected="(fun x -> x)(1)",
  ),
  test_same_term(
    ~name="Probe on an operand adds no parens",
    ~code="^^probe(x) * 3",
    ~expected="x * 3",
  ),
  test_same_term(
    ~name="Probe on an application adds no parens",
    ~code="^^probe(f(1)) * 3",
    ~expected="f(1) * 3",
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

let tests = [
  ("RoundTrip.Basic", basic_tests),
  ("RoundTrip.Probes", probe_tests),
  ("RoundTrip.ProbeGrouping", grouping_tests),
  ("RoundTrip.Modules", module_tests),
];

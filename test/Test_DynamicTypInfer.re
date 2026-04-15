open Alcotest;
open Haz3lcore;
open Language;

/* Pipeline: parse code with probes → statics → elaborate → evaluate → get samples.
   Returns (samples_by_probe_id, info_map) so tests can exercise
   DynamicTypInfer.dynamic_typ_of_samples with the real program context. */
let evaluate_probes = (code: string): (Sample.Map.t, Statics.Map.t) => {
  switch (Parser.to_zipper(code)) {
  | None => (Sample.Map.empty, Statics.Map.empty)
  | Some(z) =>
    let MakeTerm.{term, _} = MakeTerm.from_zip_for_sem(z);
    let probe_ids =
      Id.Map.union(
        (_, _, _) => Some(),
        Id.Map.map(_ => (), Id.Map.of_list(z.refractors.manuals)),
        Id.Map.map(_ => (), z.refractors.multis.ephemerals),
      );
    let info_map =
      Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term);
    let targets: Sample.targets =
      Id.Map.fold(
        (id, (), acc) => {
          let refs =
            switch (Statics.Map.lookup(id, info_map)) {
            | Some(InfoExp(_)) => Statics.Map.refs_in(info_map, id)
            | Some(InfoPat(_)) => Statics.Map.bound_in(info_map, id)
            | _ => []
            };
          Id.Map.add(id, Sample.{refs: refs}, acc);
        },
        probe_ids,
        Id.Map.empty,
      );
    let elaborated = Elaborator.elaborate(info_map, term) |> fst;
    let (_, state) =
      Evaluator.evaluate(~targets, ~env=Builtins.env_init, elaborated);
    (EvaluatorState.get_probes(state), info_map);
  };
};

/* Get the first probe's samples from the evaluation result */
let first_probe_samples = (probes: Sample.Map.t): list(Sample.t) =>
  switch (Id.Map.bindings(probes)) {
  | [(_, samples), ..._] => samples
  | [] => []
  };

/* Pretty-print a type for readable test output */
let typ_to_string = (ty: Typ.t): string => {
  let seg =
    ExpToSegment.typ_to_segment(
      ~settings={
        secondary: AutoFormat,
        parenthesization: Defensive,
        label_format: QuoteWhenNecessary,
        inline: true,
        fold_case_clauses: false,
        fold_fn_bodies: `NoFold,
        hide_fixpoints: false,
        show_filters: true,
        show_unknown_as_hole: true,
        show_ascriptions: true,
        raise_if_padding: false,
      },
      ty,
    );
  Printer.of_segment(~holes="?", ~indent="", ~is_single_line=true, seg);
};

let testable_typ_string = testable(Fmt.string, String.equal);

/* Test helper: given code with a probe, compute the dynamic type and check
   it matches `expected`. Pass `Some("Int")` for a successful meet or
   `None` when sample types are inconsistent. */
let dynamic_typ_test = (name: string, code: string, expected: option(string)) =>
  test_case(
    name,
    `Quick,
    () => {
      let (probes, info_map) = evaluate_probes(code);
      let samples = first_probe_samples(probes);
      let ctx =
        switch (Id.Map.bindings(probes)) {
        | [(probe_id, _), ..._] =>
          switch (Statics.Map.lookup(probe_id, info_map)) {
          | Some(info) => Info.ctx_of(info)
          | None => Builtins.ctx_init(Some(Int))
          }
        | [] => Builtins.ctx_init(Some(Int))
        };
      let result = DynamicTypInfer.dynamic_typ_of_samples(~ctx, samples);
      check(
        option(testable_typ_string),
        name,
        expected,
        Option.map(typ_to_string, result),
      );
    },
  );

/* === Basic type inference tests === */

let basic_tests = [
  dynamic_typ_test("Int literal", {|^^probe(42)|}, Some("Int")),
  dynamic_typ_test("String literal", {|^^probe("hello")|}, Some("String")),
  dynamic_typ_test("Bool literal", {|^^probe(true)|}, Some("Bool")),
  dynamic_typ_test("List of ints", {|^^probe([1, 2, 3])|}, Some("[Int]")),
  dynamic_typ_test("Tuple", {|^^probe((1, "a"))|}, Some("(Int, String)")),
  dynamic_typ_test(
    "Arrow via function",
    {|let f = fun x -> x + 1 in ^^probe(f)|},
    Some("? -> Int"),
  ),
];

/* === Multiple sample tests (meet behavior) === */

let meet_tests = [
  dynamic_typ_test(
    "Multiple int samples meet to Int",
    {|let f = fun x -> ^^probe(x * 2)
in [f(1), f(2), f(3)]|},
    Some("Int"),
  ),
  dynamic_typ_test(
    "Samples from conditional branches",
    {|let f = fun b -> ^^probe(if b then 1 else 2)
in [f(true), f(false)]|},
    Some("Int"),
  ),
  dynamic_typ_test(
    "Inconsistent sample types return None",
    {|let f = fun b -> ^^probe(if b then 1 else "hi")
in [f(true), f(false)]|},
    None,
  ),
  dynamic_typ_test(
    "No samples from unevaluated branch",
    {|if false then ^^probe(42) else 0|},
    Some("?"),
  ),
];

/* === User-defined type tests === */

let user_type_tests = [
  dynamic_typ_test(
    "User-defined ADT constructor",
    {|type T = A + B in ^^probe(A)|},
    Some("+ A + B"),
  ),
  dynamic_typ_test(
    "User-defined ADT with payload",
    {|type T = Some(Int) + None in ^^probe(Some(42))|},
    Some("+ Some(Int) + None"),
  ),
  dynamic_typ_test(
    "User-defined ADT through function",
    {|type T = Leaf(Int) + Node(T, T)
in let f = fun x -> ^^probe(Leaf(x))
in f(1)|},
    Some("rec T -> + Leaf(Int) + Node((T, T))"),
  ),
  dynamic_typ_test(
    "User-defined type alias in context",
    {|type Pair = (Int, String)
in let p : Pair = (1, "a")
in ^^probe(p)|},
    Some("(Int, String)"),
  ),
  dynamic_typ_test(
    "Multiple samples with user-defined ADT",
    {|type T = A(Int) + B(String)
in let f = fun n -> ^^probe(A(n))
in [f(1), f(2)]|},
    Some("+ A(Int) + B(String)"),
  ),
  dynamic_typ_test(
    "Meeting multiple ADT branches",
    {|type T = A(Int) + B(Int)
in let f = fun b -> ^^probe(if b then A(1) else B(2))
in [f(true), f(false)]|},
    Some("+ A(Int) + B(Int)"),
  ),
];

let tests = [
  ("DynamicTypInfer.Basic", basic_tests),
  ("DynamicTypInfer.Meet", meet_tests),
  ("DynamicTypInfer.UserTypes", user_type_tests),
];

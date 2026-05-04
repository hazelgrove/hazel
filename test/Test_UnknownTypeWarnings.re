open Alcotest;
open Language;
open Test_Statics_Prelude;

/* Count info entries that carry a ContainsUnknown warning. We don't try to
   pin down which specific id holds the warning here — checking presence is
   enough to verify the source-attribution logic without coupling tests to
   id allocation. */
let unknown_warning_count = (s: Statics.Map.t): int =>
  Id.Map.fold(
    (id, info: Info.t, acc) =>
      Id.equal(id, Info.id_of(info))
      && List.exists(
           (w: Warning.list_item) =>
             switch (w) {
             | Exp(ContainsUnknown(_))
             | Pat(ContainsUnknown(_))
             | Typ(ContainsUnknown(_)) => true
             | _ => false
             },
           Info.warnings_of(info),
         )
        ? acc + 1 : acc,
    s,
    0,
  );

let check_count = (name, input, expected) =>
  test_case(
    name,
    `Quick,
    () => {
      let exp = parse_exp(input);
      let s = statics(exp);
      check(int, name, expected, unknown_warning_count(s));
    },
  );

let tests = (
  "Contains-Unknown Type Warnings",
  [
    /* Fully concrete: no unknowns anywhere → no warnings. */
    check_count("concrete: integer literal", "1", 0),
    check_count("concrete: simple let", "let x = 1 in x + 1", 0),
    /* A `?` literal is itself an Unknown term — suppressed (the syntax
       already shows ?), so no warning on the hole. The surrounding tuple's
       unknown is sourced at `?`, but suppressed there. */
    check_count("hole literal alone", "?", 0),
    /* fun x -> x: parameter `x` synthesizes type ?, source is the pattern
       (no children carry the unknown). The body Var x also has type ?,
       which is "inherited" from the pattern's binding, but the Var itself
       has no children — so it counts as a source too. The fun expression
       inherits unknown from the body, so it is NOT a source. */
    check_count("fun: unknown-typed parameter", "fun x -> x", 2),
    /* Concrete annotation — no unknown. */
    check_count("fun: typed parameter", "fun x : Int -> x", 0),
    /* Body is concrete (Int), but the function's parameter is unknown. */
    check_count("fun: unused unknown parameter", "fun x -> 1", 1),
  ],
);

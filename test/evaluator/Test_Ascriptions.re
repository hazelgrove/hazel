/* Pushing an ascription through a value (Ascriptions.transition_multiple):
   a value that already has the type comes back as the same object, not a
   copy, so what is kept by identity downstream still knows it. */
open Alcotest;
open Language;
open Test_Evaluator_Prelude;

let typ = (src: string): Typ.t =>
  switch (
    Haz3lcore.Parser.to_segment(src, ~root=Typ)
    |> Option.map(Haz3lcore.MakeTerm.for_projection)
  ) {
  | Some(Some(Typ(t))) => t
  | _ => failf("could not parse the type `%s`", src)
  };

let push = (v: Exp.t, t: Typ.t): Exp.t =>
  Ascriptions.transition_multiple(Asc(v, t) |> DHExp.fresh);

let tests = (
  "Ascriptions",
  [
    test_case(
      "a value that already has the type is given back, not copied",
      `Quick,
      () => {
        let v = parse_and_evaluate({|[(a = 1, b = "x"), (a = 2, b = "y")]|});
        let t = typ("[(a = Int, b = String)]");
        check(bool, "the same object", true, push(v, t) === v);
      },
    ),
    test_case(
      "a value ascribed to a non-contractive type is given back",
      `Quick,
      () => {
        let v = parse_and_evaluate("[1, 2]");
        let t = typ("rec x -> x");
        check(bool, "the same object", true, push(v, t) === v);
      },
    ),
    test_case(
      "a value with a function in it is still wrapped",
      `Quick,
      () => {
        let v = parse_and_evaluate("[fun x -> x + 1]");
        let t = typ("[Int -> Int]");
        check(bool, "a new value", false, push(v, t) === v);
      },
    ),
  ],
);

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
      "a value with a function in it is still wrapped",
      `Quick,
      () => {
        let v = parse_and_evaluate("[fun x -> x + 1]");
        let t = typ("[Int -> Int]");
        check(bool, "a new value", false, push(v, t) === v);
        check(bool, "and again", false, push(v, t) === v);
        check(
          bool,
          "never remembered",
          false,
          Ascriptions.known_to_have(v, t),
        );
      },
    ),
    /* A typed recursive function over a list is given the list's tail at
       each call, as a new ListLit around the same cells (Unboxing). After
       one push through the list, every tail is known to have the type, so
       the next push is a lookup, not a walk. */
    test_case(
      "after one push, a list's tails are known to have its type",
      `Quick,
      () => {
        let v = parse_and_evaluate("[1, 2, 3]");
        let t = typ("[Int]");
        check(bool, "the same object", true, push(v, t) === v);
        let tail =
          switch (DHExp.term_of(v)) {
          | ListLit([_, ...tl]) => ListLit(tl) |> DHExp.fresh
          | _ => failf("not a list value")
          };
        check(
          bool,
          "the tail is known",
          true,
          Ascriptions.known_to_have(tail, t),
        );
        check(bool, "and given back", true, push(tail, t) === tail);
        check(
          bool,
          "not for another type",
          false,
          Ascriptions.known_to_have(tail, typ("[String]")),
        );
      },
    ),
    test_case(
      "a value without the type is not remembered as having it",
      `Quick,
      () => {
        let v = parse_and_evaluate("[1, 2]");
        let t = typ("[String]");
        check(bool, "not given back", false, push(v, t) === v);
        check(
          bool,
          "not remembered",
          false,
          Ascriptions.known_to_have(v, t),
        );
        check(bool, "still not given back", false, push(v, t) === v);
      },
    ),
  ],
);

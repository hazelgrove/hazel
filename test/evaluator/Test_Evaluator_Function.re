open Alcotest;
open Haz3lcore;
open Test_Evaluator_Prelude;

open IdTagged.FreshGrammar;
open Exp;

let tests = (
  "Evaluator.Functions",
  [
    test_case("Function deferral", `Quick, () =>
      evaluation_test(
        "string_sub(\"hello\", 1, _)(2)",
        string("el"),
        ap(
          Forward,
          deferred_ap(
            var("string_sub"),
            [string("hello"), int(1), deferral(InAp)],
          ),
          int(2),
        ),
      )
    ),
    test_case("Ascribed lambda applied", `Quick, () =>
      parse_and_evaluate_test("2", {|((fun a -> a):  ? -> ? )(2:  ?): Int|})
    ),
    test_case("eg", `Quick, () =>
      parse_and_evaluate_test("2", {|(fun (a=a): ((a=Int)) -> a: ?)(a=2)|})
    ),
    test_case("Deferral applied to hole", `Quick, () =>
      evaluation_test(
        "?(_, _, 3)(1., true)",
        ap(Forward, empty_hole(), tuple([float(1.), bool(true), int(3)])),
        ap(
          Forward,
          deferred_ap(
            empty_hole(),
            [deferral(InAp), deferral(InAp), int(3)],
          ),
          tuple([float(1.), bool(true)]),
        ),
      )
    ),
    test_case("Variable capture", `Quick, () =>
      evaluation_test(
        {|let u = 5 in let f = fun () -> u in let u = 3 in f()|},
        int(5),
        let_(
          Pat.(var("u")),
          int(5),
          let_(
            Pat.(var("f")),
            fn(Pat.(tuple([])), var("u"), None, None),
            let_(
              Pat.(var("u")),
              int(3),
              ap(Forward, var("f"), tuple([])),
            ),
          ),
        ),
      )
    ),
    test_case("Unbound lookup", `Quick, () =>
      evaluation_test(
        "(fun x -> x)(x)",
        var("x"),
        ap(Forward, fn(Pat.(var("x")), var("x"), None, None), var("x")),
      )
    ),
    test_case(
      "Typfun application",
      `Quick,
      () => {
        evaluation_test(
          "(typfun T -> fun x : T-> x)@<Int>(2)",
          int(2),
          ap(
            Forward,
            typ_ap(
              typ_fun(
                TPat.(var("T")),
                fn(
                  Pat.(asc(var("x"), Typ.var("T"))),
                  var("x"),
                  None,
                  None,
                ),
                None,
              ),
              Typ.int(),
            ),
            int(2),
          ),
        );
        parse_and_evaluate_test(
          {|(1,1)|},
          {|let dub = typfun T -> fun x : T -> (x, x) : (T, T) in
          let ascribed = dub : forall a -> a -> (a, a) in
          ascribed@<Int>(1)|},
        );
      },
    ),
  ],
);

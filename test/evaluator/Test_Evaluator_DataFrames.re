open Alcotest;
open Language;
open Test_Evaluator_Prelude;

let tests = (
  "Evaluator.DataFrames",
  [
    test_case("Projection from list of labeled tuples", `Quick, () =>
      check(
        dhexp_typ,
        "[1, 2]",
        parse_exp("[1, 2]"),
        parse_and_evaluate({|[(a=1, b=false), (a=2, b=true)].a|}),
      )
    ),
    test_case("Primitive pivot of list of labeled tuple", `Quick, () =>
      check(
        dhexp_typ,
        {|primitive_pivot([(l="a", j=1, 3), (l="b", j=2, 9), (l="c", j=3, 9)], 'l')|},
        parse_exp({|a=[(j=1, 3)], b=[(j=2, 9)], c=[(j=3, 9)]|}),
        parse_and_evaluate(
          {|primitive_pivot([(l="a", j=1, 3), (l="b", j=2, 9), (l="c", j=3, 9)], 'l')|},
        ),
      )
    ),
    test_case("Projection of pivoted list of labeled tuples", `Quick, () =>
      check(
        dhexp_typ,
        {|primitive_pivot([(l="a", j=1, 3), (l="b", j=2, 9), (l="c", j=3, 9)], 'l').a|},
        parse_exp({|[(j=1, 3)]|}),
        parse_and_evaluate(
          {|primitive_pivot([(l="a", j=1, 3), (l="b", j=2, 9), (l="c", j=3, 9)], 'l').a|},
        ),
      )
    ),
    test_case(
      "Nested projection of pivoted list of labeled tuples", `Quick, () =>
      check(
        dhexp_typ,
        {|primitive_pivot([(l="a", j=1, 3), (l="b", j=2, 9), (l="c", j=3, 9)], 'l')|},
        parse_exp({|[1]|}),
        parse_and_evaluate(
          {|primitive_pivot([(l="a", j=1, 3), (l="b", j=2, 9), (l="c", j=3, 9)], 'l').a.j|},
        ),
      )
    ),
    test_case("Pivoting list bound to variable", `Quick, () =>
      check(
        dhexp_typ,
        {|let i = [(l="a", j=1, 3)] in primitive_pivot(i, 'l').a|},
        parse_exp({|[(j=1, 3)]|}),
        parse_and_evaluate(
          {|let i = [(l="a", j=1, 3)] in primitive_pivot(i, 'l').a|},
        ),
      )
    ),
    test_case(
      "pivoted list of labeled tuples with multiple entries", `Quick, () =>
      check(
        dhexp_typ,
        {|primitive_pivot([(l="a", 1, true), (l="b", 2, true), (l="c", 3, true), (l="a", 4, true)], 'l')|},
        parse_exp(
          {|(a=[(1, true), (4, true)], b=[(2, true)], c=[(3, true)])|},
        ),
        parse_and_evaluate(
          {|primitive_pivot([(l="a", 1, true), (l="b", 2, true), (l="c", 3, true), (l="a", 4, true)], 'l')|},
        ),
      )
    ),
    test_case("melted labeled tuple with multiple entries", `Quick, () =>
      check(
        dhexp_typ,
        {|melt(quiz1=12, quiz2=8, quiz3=9, quiz4=77)|},
        parse_exp(
          {|[(label="quiz1", value=12),
             (label="quiz2", value=8),
             (label="quiz3", value=9),
             (label="quiz4", value=77)]|},
        ),
        parse_and_evaluate({|melt(quiz1=12, quiz2=8, quiz3=9, quiz4=77)|}),
      )
    ),
    test_case(
      "Intermediate cast doesn't break evaluation",
      `Quick,
      () => {
        let program = {|let (var=a, val=b) = (var="get_acne", val=true) : ? in b|};
        let elaborated = elaborate(parse_exp(program));
        print_endline("Elaborated: " ++ DHExp.show(elaborated));

        check(
          dhexp_typ,
          program,
          parse_exp({|true|}),
          parse_and_evaluate(program),
        );
      },
    ),
    test_case(
      "Minimized melt",
      `Quick,
      () => {
        let program = {|case melt((a=true, b=false))
          | (x :: xs) => x
          | [] =>
        end|};
        check(
          dhexp_typ,
          program,
          parse_exp({|(label="a", value=true)|}),
          parse_and_evaluate(program),
        );
      },
    ),
    test_case(
      "Projection of melted data",
      `Quick,
      () => {
        let program = {|let filter = typfun a -> fun (pred :a -> Bool, xs : [a]) -> case xs
  | [] => []
  | (x :: xs) => (if pred(x) then [x] else []) @ filter@<a>(pred, xs)
end in

let jellyAnon : (get_acne=Bool, red=Bool, green=Bool) =
  (true, false, true) in

let melted : [(label=String, value=Bool)] = melt(jellyAnon) in

filter@<(label=String, value=Bool)>(fun a,b ->b, melted).label|};
        check(
          dhexp_typ,
          program,
          parse_exp({|["get_acne", "green"]|}),
          parse_and_evaluate(program),
        );
      },
    ),
  ],
);

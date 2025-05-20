open Alcotest;
open Haz3lcore;
open Test_Evaluator_Prelude;

let tests = [
  test_case("Projection from list of labeled tuples", `Quick, () =>
    check(
      dhexp_typ,
      "[1, 2]",
      parse_exp("[1, 2]"),
      DHExp.strip_casts(
        evaluate(
          elaborate(parse_exp({|[(a=1, b=false), (a=2, b=true)].a|})),
        ),
      ),
    )
  ),
  test_case("Primitive pivot of list of labeled tuple", `Quick, () =>
    check(
      dhexp_typ,
      {|primitive_pivot('l',  [(l="a", j=1, 3), (l="b", j=2, 9), (l="c", j=3, 9)])|},
      parse_exp({|a=[(j=1, 3)], b=[(j=2, 9)], c=[(j=3, 9)]|}),
      DHExp.strip_casts(
        evaluate(
          elaborate(
            parse_exp(
              {|primitive_pivot('l',  [(l="a", j=1, 3), (l="b", j=2, 9), (l="c", j=3, 9)])|},
            ),
          ),
        ),
      ),
    )
  ),
  test_case("Projection of pivoted list of labeled tuples", `Quick, () =>
    check(
      dhexp_typ,
      {|primitive_pivot('l',  [(l="a", j=1, 3), (l="b", j=2, 9), (l="c", j=3, 9)]).a|},
      parse_exp({|[(j=1, 3)]|}),
      DHExp.strip_casts(
        evaluate(
          elaborate(
            parse_exp(
              {|primitive_pivot('l',  [(l="a", j=1, 3), (l="b", j=2, 9), (l="c", j=3, 9)]).a|},
            ),
          ),
        ),
      ),
    )
  ),
  test_case("Nested projection of pivoted list of labeled tuples", `Quick, () =>
    check(
      dhexp_typ,
      {|primitive_pivot('l',  [(l="a", j=1, 3), (l="b", j=2, 9), (l="c", j=3, 9)])|},
      parse_exp({|[1]|}),
      DHExp.strip_casts(
        evaluate(
          elaborate(
            parse_exp(
              {|primitive_pivot('l',  [(l="a", j=1, 3), (l="b", j=2, 9), (l="c", j=3, 9)]).a.j|},
            ),
          ),
        ),
      ),
    )
  ),
  test_case("Pivoting list bound to variable", `Quick, () =>
    check(
      dhexp_typ,
      {|let i = [(l="a", j=1, 3)] in primitive_pivot('l', i).a|},
      parse_exp({|[(j=1, 3)]|}),
      DHExp.strip_casts(
        evaluate(
          elaborate(
            parse_exp(
              {|let i = [(l="a", j=1, 3)] in primitive_pivot('l', i).a|},
            ),
          ),
        ),
      ),
    )
  ),
  test_case("pivoted list of labeled tuples with multiple entries", `Quick, () =>
    check(
      dhexp_typ,
      {|primitive_pivot('l',  [(l="a", 1, true), (l="b", 2, true), (l="c", 3, true), (l="a", 4, true))])|},
      parse_exp({|(a=[(1, true), (4, true)], b=[(2, true)], c=[(3, true)])|}),
      DHExp.strip_casts(
        evaluate(
          elaborate(
            parse_exp(
              {|primitive_pivot('l',  [(l="a", 1, true), (l="b", 2, true), (l="c", 3, true), (l="a", 4, true)])|},
            ),
          ),
        ),
      ),
    )
  ),
  test_case("melted list of labeled tuples with multiple entries", `Quick, () =>
    check(
      dhexp_typ,
      {|melt('column', 'value', [(quiz1=12, quiz2=8, quiz3=9, quiz4=77), (quiz1=12, quiz2=8, quiz3=12, quiz4=77)])|},
      parse_exp(
        {|[(column="quiz1", value=12), (column="quiz2", value=8), (column="quiz3", value=9), (column="quiz4", value=77), (column="quiz1", value=12), (column="quiz2", value=8), (column="quiz3", value=12), (column="quiz4", value=77)]|},
      ),
      DHExp.strip_casts(
        evaluate(
          elaborate(
            parse_exp(
              {|melt('column', 'value', [(quiz1=12, quiz2=8, quiz3=9, quiz4=77), (quiz1=12, quiz2=8, quiz3=12, quiz4=77)])|},
            ),
          ),
        ),
      ),
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
        DHExp.strip_casts(evaluate(elaborated)),
      );
    },
  ),
  test_case(
    "Minimized melt",
    `Quick,
    () => {
      let program = {|case melt('var', 'val', [(a=true, b=false)])
          | (x :: xs) => x
          | [] =>
        end|};
      check(
        dhexp_typ,
        program,
        parse_exp({|(var="a", val=true)|}),
        DHExp.strip_casts(evaluate(elaborate(parse_exp(program)))),
      );
    },
  ),
  test_case(
    "Melt result being passed to function",
    `Quick,
    () => {
      let program = {|
        let filter = fun (pred :(var=String, val=Bool) -> Bool, xs : [(var=String, val=Bool)]) -> case xs
            | [] => []
            | (x :: xs) => (if pred(x) then [x] else []) @ filter(pred, xs)
          end in
          let jellyAnon : [(get_acne=Bool, red=Bool)] = [
            (true, false),
            (true, false),
            (false, false),
            (false, true)
          ] in

          let melted : [(var=String, val=Bool)] = melt('var', 'val', jellyAnon) in

          filter(fun a,b ->b, melted)
        |};
      check(
        dhexp_typ,
        program,
        parse_exp(
          {|[(var="get_acne", val=true), (var="get_acne", val=true), (var="red", val=true)]|},
        ),
        DHExp.strip_casts(evaluate(elaborate(parse_exp(program)))),
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

let jellyAnon : [(get_acne=Bool, red=Bool)] = [
  (true, false),
  (true, true)
] in

let melted : [(var=String, val=Bool)] = melt('var', 'val', jellyAnon) in

filter@<(var=String, val=Bool)>(fun a,b ->b, melted).var|};
      check(
        dhexp_typ,
        program,
        parse_exp({|["get_acne", "get_acne", "red"]|}),
        DHExp.strip_casts(evaluate(elaborate(parse_exp(program)))),
      );
    },
  ),
];

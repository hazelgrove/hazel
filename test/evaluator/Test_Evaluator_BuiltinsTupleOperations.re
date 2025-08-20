open Alcotest;
open Language;
open Test_Evaluator_Prelude;

let tests = (
  "Evaluator.BuiltinsTupleOperations",
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
        {|group_by_label([(l="a", j=1, 3), (l="b", j=2, 9), (l="c", j=3, 9)], `l`)|},
        parse_exp({|a=[(j=1, 3)], b=[(j=2, 9)], c=[(j=3, 9)]|}),
        parse_and_evaluate(
          {|group_by_label([(l="a", j=1, 3), (l="b", j=2, 9), (l="c", j=3, 9)], `l`)|},
        ),
      )
    ),
    test_case("Projection of pivoted list of labeled tuples", `Quick, () =>
      check(
        dhexp_typ,
        {|group_by_label([(l="a", j=1, 3), (l="b", j=2, 9), (l="c", j=3, 9)], `l`).a|},
        parse_exp({|[(j=1, 3)]|}),
        parse_and_evaluate(
          {|group_by_label([(l="a", j=1, 3), (l="b", j=2, 9), (l="c", j=3, 9)], `l`).a|},
        ),
      )
    ),
    test_case(
      "Nested projection of pivoted list of labeled tuples", `Quick, () =>
      check(
        dhexp_typ,
        {|group_by_label([(l="a", j=1, 3), (l="b", j=2, 9), (l="c", j=3, 9)], `l`)|},
        parse_exp({|[1]|}),
        parse_and_evaluate(
          {|group_by_label([(l="a", j=1, 3), (l="b", j=2, 9), (l="c", j=3, 9)], `l`).a.j|},
        ),
      )
    ),
    test_case("Pivoting list bound to variable", `Quick, () =>
      check(
        dhexp_typ,
        {|let i = [(l="a", j=1, 3)] in group_by_label(i, `l`).a|},
        parse_exp({|[(j=1, 3)]|}),
        parse_and_evaluate(
          {|let i = [(l="a", j=1, 3)] in group_by_label(i, `l`).a|},
        ),
      )
    ),
    test_case(
      "pivoted list of labeled tuples with multiple entries", `Quick, () =>
      check(
        dhexp_typ,
        {|group_by_label([(l="a", 1, true), (l="b", 2, true), (l="c", 3, true), (l="a", 4, true)], `l`)|},
        parse_exp(
          {|(a=[(1, true), (4, true)], b=[(2, true)], c=[(3, true)])|},
        ),
        parse_and_evaluate(
          {|group_by_label([(l="a", 1, true), (l="b", 2, true), (l="c", 3, true), (l="a", 4, true)], `l`)|},
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
    test_case(
      "From entries with singleton list of tuples",
      `Quick,
      () => {
        let program = {|from_entries([(label="col", value=3)])|};
        check(
          dhexp_typ,
          program,
          parse_exp({|(col=3)|}),
          parse_and_evaluate(program),
        );
      },
    ),
    test_case(
      "From entries with multiple entries",
      `Quick,
      () => {
        let program = {|from_entries([(label="col1", value=3), (label="col2", value=true)])|};
        check(
          dhexp_typ,
          program,
          parse_exp({|(col1=3, col2=true)|}),
          parse_and_evaluate(program),
        );
      },
    ),
    test_case(
      "From entries with empty list",
      `Quick,
      () => {
        let program = {|from_entries([])|};
        check(
          dhexp_typ,
          program,
          parse_exp({|()|}),
          parse_and_evaluate(program),
        );
      },
    ),
    test_case(
      "From entries with bad data",
      `Quick,
      () => {
        let program = {|from_entries([(x=1)])|};
        check(
          dhexp_typ,
          program,
          IdTagged.FreshGrammar.(
            Exp.(
              ap(
                Forward,
                builtin_fun("from_entries"),
                list_lit([tuple([tup_label(label("x"), int(1))])]),
              )
            )
          ),
          parse_and_evaluate(program),
        );
      },
    ),
    test_case(
      "From entries with label holes",
      `Quick,
      () => {
        let program = {|from_entries([(label="col1", value=3), (label="col2", value=true), (label=?, value=5)])|};
        check(
          dhexp_typ,
          program,
          parse_exp({|(col1=3, col2=true, ?=5)|}),
          parse_and_evaluate(program),
        );
      },
    ),
    test_case(
      "Project labels to singleton",
      `Quick,
      () => {
        let program = {|project_labels((a=1, b=2), `a`)|};
        check(
          dhexp_typ,
          program,
          parse_exp({|1|}),
          parse_and_evaluate(program),
        );
      },
    ),
    test_case(
      "Omit labels to unlabeled singleton",
      `Quick,
      () => {
        let program = {|omit_labels((a=1, 2), `a`)|};
        check(
          dhexp_typ,
          program,
          parse_exp({|2|}),
          parse_and_evaluate(program),
        );
      },
    ),
    test_case(
      "Drop labels to singleton",
      `Quick,
      () => {
        let program = {|drop_labels((a=1))|};
        check(
          dhexp_typ,
          program,
          parse_exp({|1|}),
          parse_and_evaluate(program),
        );
      },
    ),
  ],
);

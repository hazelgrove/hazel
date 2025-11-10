open Alcotest;
open Language;

let ellipsis = "…";

let run_abbreviation = (~available: int, exp: Exp.t): Exp.t => {
  let (abbreviated: Exp.t, _length: int) =
    Abbreviate.abbreviate_exp(~available, exp);
  abbreviated;
};

let collect_labels = (elements: list(Exp.t)): list(string) =>
  elements
  |> List.filter_map((element: Exp.t) =>
       switch (element.term) {
       | TupLabel(label_exp, _value_exp) =>
         switch (label_exp.term) {
         | Label(name) => Some(name)
         | _ => None
         }
       | _ => None
       }
     );

let is_flat_ellipses_exp = (exp: Exp.t): bool =>
  switch (exp.term) {
  | Invalid(str)
  | Atom(String(str))
  | Constructor(str, _)
  | Var(str) => str == ellipsis
  | _ => false
  };

let rec exp_contains_flat_ellipses = (exp: Exp.t): bool =>
  if (is_flat_ellipses_exp(exp)) {
    true;
  } else {
    switch (exp.term) {
    | Tuple(elements)
    | ListLit(elements) => List.exists(exp_contains_flat_ellipses, elements)
    | _ => false
    };
  };

let tests = (
  "Abbreviate",
  [
    test_case(
      "labeled tuples keep field names under tight budget",
      `Quick,
      (): unit => {
        open IdTagged.FreshGrammar;
        open Exp;
        let original: Exp.t =
          tuple([
            tup_label(label("alpha"), string("aaaaaaaaaaaa")),
            tup_label(label("beta"), string("bbbbbbbbbbbb")),
            tup_label(label("gamma"), string("cccccccccccc")),
          ]);
        let abbreviated: Exp.t = run_abbreviation(~available=24, original);
        switch (abbreviated.term) {
        | Tuple(elements) =>
          check(Alcotest.int, "field count", 3, List.length(elements));
          let labels: list(string) = collect_labels(elements);
          check(Alcotest.int, "label count", 3, List.length(labels));
          List.iter(
            (label: string) =>
              check(
                Alcotest.bool,
                "label not empty",
                true,
                String.length(label) > 0,
              ),
            labels,
          );
        | _ => fail("expected tuple after abbreviation")
        };
      },
    ),
    test_case(
      "label retains prefix before value elides",
      `Quick,
      (): unit => {
        open IdTagged.FreshGrammar;
        open Exp;
        let original: Exp.t =
          tuple([
            tup_label(
              label("capacity"),
              list_lit([int(1), int(2), int(3), int(4)]),
            ),
          ]);
        let abbreviated: Exp.t = run_abbreviation(~available=8, original);
        switch (abbreviated.term) {
        | Tuple([{term: TupLabel(label_exp, value_exp), _}]) =>
          let label_text: string =
            switch (label_exp.term) {
            | Label(text) => text
            | _ => fail("expected label expression")
            };
          check(
            Alcotest.bool,
            "label keeps prefix",
            true,
            String.length(label_text) > 0
            && label_text != ellipsis
            && label_text.[0] == 'c',
          );
          check(
            Alcotest.bool,
            "value ellides at least as eagerly",
            true,
            exp_contains_flat_ellipses(value_exp),
          );
        | _ => fail("expected tuple with TupLabel")
        };
      },
    ),
    test_case(
      "split_evenly distributes fairly",
      `Quick,
      (): unit => {
        let budgets: list(int) =
          Abbreviate.AbbrevBudget.split_evenly(~total=10, ~parts=3);
        check(
          Alcotest.list(Alcotest.int),
          "even split",
          [4, 3, 3],
          budgets,
        );
      },
    ),
  ],
);

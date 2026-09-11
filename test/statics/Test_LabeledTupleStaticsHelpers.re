open Alcotest;
open Language;
module F = IdTagged.FreshGrammar;

/* shape_mismatch describes how two tuple types differ in arity and labels;
   it is None when the shapes agree (the difference lies in a component) or
   when either side is not a tuple type. */
let ctx = Builtins.ctx_init(None);
let ti = F.Typ.int();
let ts = F.Typ.string();
let lab = (l, t) => F.Typ.tup_label(F.Typ.label(l), t);
let prod = F.Typ.prod;
/* Rendered as "expected | actual | missing | unexpected", labels comma
   separated, an unlabeled element as "_". */
let shape = (ana, syn) =>
  LabeledTupleStaticsHelpers.shape_mismatch(ctx, ~ana, ~syn)
  |> Option.map((sm: LabeledTupleStaticsHelpers.shape_mismatch) => {
       let per_element = l =>
         String.concat(",", List.map(Option.value(~default="_"), l));
       String.concat(
         " | ",
         [
           per_element(sm.expected_labels),
           per_element(sm.actual_labels),
           String.concat(",", sm.missing_labels),
           String.concat(",", sm.unexpected_labels),
         ],
       );
     });
let summary = option(string);

let tests = (
  "LabeledTupleStaticsHelpers",
  [
    test_case("same shape is not a shape mismatch", `Quick, () =>
      check(
        summary,
        "component only",
        None,
        shape(prod([ti, ti]), prod([ti, ts])),
      )
    ),
    test_case(
      "a non-tuple side is not a shape mismatch",
      `Quick,
      () => {
        check(summary, "int", None, shape(ti, prod([ti])));
        check(summary, "tuple vs int", None, shape(prod([ti]), ti));
      },
    ),
    test_case("different arity", `Quick, () =>
      check(
        summary,
        "two vs three",
        Some("_,_ | _,_,_ |  | "),
        shape(prod([ti, ti]), prod([ti, ti, ti])),
      )
    ),
    test_case("a missing label", `Quick, () =>
      check(
        summary,
        "age missing",
        Some("name,age | name | age | "),
        shape(
          prod([lab("name", ts), lab("age", ti)]),
          prod([lab("name", ts)]),
        ),
      )
    ),
    test_case("an unexpected label with the same arity", `Quick, () =>
      check(
        summary,
        "c instead of b",
        Some("a,b | a,c | b | c"),
        shape(
          prod([lab("a", ti), lab("b", ti)]),
          prod([lab("a", ti), lab("c", ti)]),
        ),
      )
    ),
  ],
);

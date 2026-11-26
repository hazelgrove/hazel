open Alcotest;
open Haz3lcore;

/* Form labels that are a proper prefix of another form's label: the forms
   Segment.remold_tile can re-form an incomplete tile into. */
let prefix_forms = (): list(string) => {
  let labels =
    Form.forms
    |> List.map(~f=((_, def: Form.def)) => def.label)
    |> List.dedup_and_sort(~compare=List.compare(String.compare));
  let rec prefixes = (label: Label.t): list(Label.t) =>
    switch (List.rev(label)) {
    | []
    | [_] => []
    | [_, ...rest] =>
      let shorter = List.rev(rest);
      [shorter, ...prefixes(shorter)];
    };
  labels
  |> List.concat_map(~f=label =>
       prefixes(label)
       |> List.filter(~f=prefix =>
            !List.is_empty(Form.compound_defs(prefix))
          )
       |> List.map(~f=prefix =>
            String.concat(~sep=" ", label)
            ++ " -> "
            ++ String.concat(~sep=" ", prefix)
          )
     )
  |> List.dedup_and_sort(~compare=String.compare);
};

let tests = (
  "Form",
  [
    test_case(
      "the only forms a shorter form spells are the module and signature items",
      `Quick,
      () =>
      check(
        testable(Fmt.(list(string)), List.equal(String.equal)),
        "form labels that are prefixes of another form's label",
        [
          "let = -> let",
          "let = in -> let",
          "let = in -> let =",
          "module = -> module",
          "module = in -> module",
          "module = in -> module =",
          "type = in -> type =",
        ],
        prefix_forms(),
      )
    ),
  ],
);

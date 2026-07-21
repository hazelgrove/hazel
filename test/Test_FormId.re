/* Property tests proving the FormId classification layer
 * (Form.classify_label / label_of / mold_of / remold_candidates)
 * byte-equivalent to the existing label=>mold oracles
 * (Form.Molds.get / Form.Molds.try_get). See plans/tile-datatype.md
 * Phase 0c. */
open Alcotest;
open Haz3lcore;

let mold_testable: testable(Mold.t) =
  testable(Fmt.using(Mold.show, Fmt.string), Mold.equal);
let label_testable: testable(Label.t) =
  testable(Fmt.using(Label.show, Fmt.string), Label.equal);

/* All sorts, including Drv sorts. (Sort.all omits Mod/Sig/MPat.) */
let all_sorts: list(Sort.t) =
  [Sort.Any, Pat, Typ, TPat, Rul, Exp, Mod, Sig, MPat]
  @ List.map(s => Sort.Drv(s), Language.DrvSort.all);

let all_labels: list(Label.t) =
  Form.forms |> List.map(snd) |> List.map((f: Form.t) => f.label);

let token_corpus: list(Token.t) = [
  "x",
  "foo",
  "A",
  "Cons",
  "5",
  "0.5",
  "true",
  "\"str\"",
  "_",
  "le",
  "l",
  "i",
  "th",
  "els",
  "+",
  "->",
  "==>",
  "~~~",
  "$%^",
  "()",
  "[]",
  "in",
  "then",
];

let corpus_labels: list(Label.t) = List.map(t => [t], token_corpus);

let case_name = (sort: Sort.t, label: Label.t): string =>
  Sort.to_string(sort) ++ " / " ++ Label.show(label);

/* mold_of(classify_label(sort, label)) == Molds.get(sort, label)
   and label_of(classify_label(sort, label)) == label */
let check_classify = (sort: Sort.t, label: Label.t): unit => {
  let id = Form.classify_label(sort, label);
  check(
    mold_testable,
    "mold: " ++ case_name(sort, label),
    Form.Molds.get(sort, label),
    Form.mold_of(id),
  );
  check(
    label_testable,
    "label: " ++ case_name(sort, label),
    label,
    Form.label_of(id),
  );
};

/* remold_candidates(label, sort) mirrors Molds.try_get(sort, label):
   same molds (via mold_of), same order; None <-> [] */
let check_remold = (sort: Sort.t, label: Label.t): unit => {
  let expected =
    switch (Form.Molds.try_get(sort, label)) {
    | None => []
    | Some(molds) => molds
    };
  let actual = Form.remold_candidates(label, sort) |> List.map(Form.mold_of);
  check(
    list(mold_testable),
    "candidates: " ++ case_name(sort, label),
    expected,
    actual,
  );
};

let tests = (
  "FormId",
  [
    test_case("Form(cf) label/mold agree with Form.get", `Quick, () =>
      List.iter(
        cf => {
          let form = Form.get(cf);
          let name = Form.show_compound_form(cf);
          check(
            label_testable,
            name ++ " label",
            form.label,
            Form.label_of(FormId.Form(cf)),
          );
          check(
            mold_testable,
            name ++ " mold",
            form.mold,
            Form.mold_of(FormId.Form(cf)),
          );
        },
        Form.all_of_compound_form,
      )
    ),
    test_case(
      "classify_label equals Molds.get on all form labels x all sorts",
      `Quick,
      () =>
      List.iter(
        label => List.iter(sort => check_classify(sort, label), all_sorts),
        all_labels,
      )
    ),
    test_case(
      "classify_label equals Molds.get on token corpus x all sorts", `Quick, () =>
      List.iter(
        label => List.iter(sort => check_classify(sort, label), all_sorts),
        corpus_labels,
      )
    ),
    test_case(
      "remold_candidates equals Molds.try_get on labels x sorts", `Quick, () =>
      List.iter(
        label => List.iter(sort => check_remold(sort, label), all_sorts),
        all_labels @ corpus_labels,
      )
    ),
  ],
);

/* Property tests for the FormId classification layer
 * (Form.classify_label / label_of / mold_of / remold_candidates),
 * stated oracle-free against the form registry (Form.forms /
 * Form.get / Form.compound_defs). */
open Alcotest;
open Haz3lcore;

let mold_testable: testable(Mold.t) =
  testable(Fmt.using(Mold.show, Fmt.string), Mold.equal);
let label_testable: testable(Label.t) =
  testable(Fmt.using(Label.show, Fmt.string), Label.equal);
let form_id_testable: testable(Form.t) =
  testable(Fmt.using(Form.show, Fmt.string), Form.equal);
let sort_testable: testable(Sort.t) =
  testable(Fmt.using(Sort.show, Fmt.string), Sort.equal);
let compound_form_testable: testable(Form.compound_form) =
  testable(
    Fmt.using(Form.show_compound_form, Fmt.string),
    Form.equal_compound_form,
  );

/* All sorts, including Drv sorts. (Sort.all omits Mod/Sig/MPat.) */
let all_sorts: list(Sort.t) =
  [Sort.Any, Pat, Typ, TPat, Rul, Exp, Mod, Sig, MPat]
  @ List.map(s => Sort.Drv(s), Language.DrvSort.all);

let all_labels: list(Label.t) =
  Form.forms |> List.map(snd) |> List.map((f: Form.def) => f.label);

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

let is_compound_label = (label: Label.t): bool =>
  Form.compound_defs(label) != [];

/* classify_label is total and label-preserving; it picks the first
   remold candidate when one fits the sort, and otherwise falls back
   to Unsorted (registered labels) / Unmolded (unregistered tokens)
   with the Any-sorted fallback mold (bin for operator-shaped tokens,
   op otherwise). */
let check_classify = (sort: Sort.t, label: Label.t): unit => {
  let name = case_name(sort, label);
  let id = Form.classify_label(sort, label);
  check(label_testable, "label: " ++ name, label, Form.label_of(id));
  switch (Form.remold_candidates(label, sort)) {
  | [first, ..._] =>
    check(form_id_testable, "first candidate: " ++ name, first, id);
    check(sort_testable, "mold sort: " ++ name, sort, Form.mold_of(id).out);
  | [] =>
    let fallback_ok =
      switch (id) {
      | Compound(_)
      | Atom(_) => false
      | Unsorted(_) => is_compound_label(label)
      | Unmolded(_) => !is_compound_label(label)
      };
    check(bool, "fallback class: " ++ name, true, fallback_ok);
    let fallback_mold =
      switch (label) {
      | [t]
          when
            Token.is_potential_operator(t) && !Token.is_potential_operand(t) =>
        Mold.mk_bin(Precedence.max, Sort.Any, [])
      | _ => Mold.mk_op(Sort.Any, [])
      };
    check(
      mold_testable,
      "fallback mold: " ++ name,
      fallback_mold,
      Form.mold_of(id),
    );
  };
};

/* remold_candidates: every candidate fits the sort and spells the
   label; candidates are Compound/Atom only, atoms (single tokens)
   before compounds; the compound candidates appear in
   forms-declaration order. */
let check_remold = (sort: Sort.t, label: Label.t): unit => {
  let name = case_name(sort, label);
  let cands = Form.remold_candidates(label, sort);
  List.iter(
    c => {
      check(
        sort_testable,
        "candidate sort: " ++ name,
        sort,
        Form.mold_of(c).out,
      );
      check(
        label_testable,
        "candidate label: " ++ name,
        label,
        Form.label_of(c),
      );
    },
    cands,
  );
  let expected_compounds =
    Form.forms
    |> List.filter(((_, def): (Form.compound_form, Form.def)) =>
         def.label == label && def.mold.out == sort
       )
    |> List.map(fst);
  let actual_compounds =
    cands
    |> List.filter_map(
         fun
         | Form.Compound(cf) => Some(cf)
         | _ => None,
       );
  check(
    list(compound_form_testable),
    "compounds in declaration order: " ++ name,
    expected_compounds,
    actual_compounds,
  );
  let rec atoms_then_compounds = (cands, seen_compound) =>
    switch (cands) {
    | [] => true
    | [Form.Compound(_), ...tl] => atoms_then_compounds(tl, true)
    | [Form.Atom(_), ...tl] =>
      !seen_compound && atoms_then_compounds(tl, seen_compound)
    | [Form.Unsorted(_) | Form.Unmolded(_), ..._] => false
    };
  check(
    bool,
    "atoms precede compounds: " ++ name,
    true,
    atoms_then_compounds(cands, false),
  );
};

let tests = (
  "FormId",
  [
    test_case("Compound(cf) label/mold agree with Form.get", `Quick, () =>
      List.iter(
        cf => {
          let form = Form.get(cf);
          let name = Form.show_compound_form(cf);
          check(
            label_testable,
            name ++ " label",
            form.label,
            Form.label_of(Form.Compound(cf)),
          );
          check(
            mold_testable,
            name ++ " mold",
            form.mold,
            Form.mold_of(Form.Compound(cf)),
          );
        },
        Form.all_of_compound_form,
      )
    ),
    test_case(
      "classify_label on all form labels x all sorts: label-preserving, "
      ++ "first-candidate, never Unmolded",
      `Quick,
      () =>
      List.iter(
        label =>
          List.iter(
            sort => {
              check_classify(sort, label);
              let id = Form.classify_label(sort, label);
              check(
                bool,
                "registered label never Unmolded: " ++ case_name(sort, label),
                true,
                switch (id) {
                | Form.Unmolded(_) => false
                | _ => true
                },
              );
            },
            all_sorts,
          ),
        all_labels,
      )
    ),
    test_case(
      "classify_label on token corpus x all sorts: total, label-preserving, "
      ++ "sort-correct",
      `Quick,
      () =>
      List.iter(
        t =>
          List.iter(
            sort => {
              check_classify(sort, [t]);
              let id = Form.classify_label(sort, [t]);
              check(
                bool,
                "mold sort in {sort, Any}: " ++ case_name(sort, [t]),
                true,
                List.mem(Form.mold_of(id).out, [sort, Sort.Any]),
              );
              if (!is_compound_label([t])) {
                check(
                  bool,
                  "unregistered token never Compound/Unsorted: "
                  ++ case_name(sort, [t]),
                  true,
                  switch (id) {
                  | Form.Compound(_)
                  | Form.Unsorted(_) => false
                  | Form.Atom(_)
                  | Form.Unmolded(_) => true
                  },
                );
              };
            },
            all_sorts,
          ),
        token_corpus,
      )
    ),
    test_case(
      "remold_candidates on labels x sorts: sort-fit, label-preserving, "
      ++ "declaration order",
      `Quick,
      () =>
      List.iter(
        label => List.iter(sort => check_remold(sort, label), all_sorts),
        all_labels @ corpus_labels,
      )
    ),
  ],
);

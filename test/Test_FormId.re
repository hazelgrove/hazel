/* Property tests for the FormId v2 classification layer
 * (Form.classify_label / label_of / mold_of / remold_candidates and
 * the family table), stated oracle-free against the form registry
 * (Form.forms / Form.defs_of / Form.compound_defs). */
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
let family_testable: testable(Form.family) =
  testable(Fmt.using(Form.show_family, Fmt.string), Form.equal_family);

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

/* The outer-nib shape-role of a mold, precedence-erased. */
let shape_role = (m: Mold.t): (bool, bool) => {
  let (l, r) = m.nibs;
  let conv = (n: Nib.t) =>
    switch (n.shape) {
    | Convex => true
    | Concave(_) => false
    };
  (conv(l), conv(r));
};

/* family construction: the family key is exactly the quotient of the
   definition rows by (label, outer-nib shape-role). */
let check_family_grouping = (): unit =>
  List.iter(
    ((fam1, d1): (Form.family, Form.def)) =>
      List.iter(
        ((fam2, d2): (Form.family, Form.def)) => {
          let same_family = fam1 == fam2;
          let same_key =
            d1.label == d2.label
            && shape_role(d1.mold) == shape_role(d2.mold);
          check(
            bool,
            "grouping is (label, shape-role) quotient: "
            ++ Form.show_family(fam1)
            ++ " "
            ++ Label.show(d1.label)
            ++ " vs "
            ++ Form.show_family(fam2)
            ++ " "
            ++ Label.show(d2.label),
            same_key,
            same_family,
          );
        },
        Form.forms,
      ),
    Form.forms,
  );

/* family uniqueness: within a family, (out sort -> mold) is a
   function — rows sharing an out sort must have identical molds
   (the Dot family's duplicate Typ row — the historical
   DotTyp/ProdProjection pair — is the one such case). */
let check_family_uniqueness = (): unit =>
  List.iter(
    (fam: Form.family) => {
      let defs = Form.defs_of(fam);
      check(
        bool,
        "every family is inhabited: " ++ Form.show_family(fam),
        true,
        defs != [],
      );
      List.iter(
        (d1: Form.def) =>
          List.iter(
            (d2: Form.def) =>
              if (d1.mold.out == d2.mold.out) {
                check(
                  mold_testable,
                  "out->mold is a function in "
                  ++ Form.show_family(fam)
                  ++ " at "
                  ++ Sort.to_string(d1.mold.out),
                  d1.mold,
                  d2.mold,
                );
              },
            defs,
          ),
        defs,
      );
      List.iter(
        (d: Form.def) =>
          check(
            label_testable,
            "family label is row label: " ++ Form.show_family(fam),
            Form.label_of_family(fam),
            d.label,
          ),
        defs,
      );
    },
    Form.all_of_family,
  );

/* classify_label is total and label-preserving; it picks the first
   remold candidate when one fits the sort (storing that sort), and
   otherwise falls back to stored-sort Any with the Any-fallback mold
   (Parens: op wrapping one child; bin for operator-shaped tokens, op
   otherwise). It never emits TokInfix. */
let check_classify = (sort: Sort.t, label: Label.t): unit => {
  let name = case_name(sort, label);
  let (id, stored) = Form.classify_label(sort, label);
  check(label_testable, "label: " ++ name, label, Form.label_of(id));
  check(
    bool,
    "classify never emits TokInfix: " ++ name,
    true,
    switch (id) {
    | Form.TokInfix(_) => false
    | _ => true
    },
  );
  switch (Form.remold_candidates(label, sort)) {
  | [(first, first_sort), ..._] =>
    check(form_id_testable, "first candidate: " ++ name, first, id);
    check(sort_testable, "stored sort: " ++ name, first_sort, stored);
    check(
      sort_testable,
      "mold sort: " ++ name,
      sort,
      Form.mold_of(id, stored).out,
    );
  | [] =>
    check(sort_testable, "fallback stored sort: " ++ name, Sort.Any, stored);
    let fallback_ok =
      switch (id) {
      | Compound(_) => is_compound_label(label)
      | Tok(_) => !is_compound_label(label)
      | TokInfix(_) => false
      };
    check(bool, "fallback class: " ++ name, true, fallback_ok);
    let fallback_mold =
      switch (id, label) {
      | (Compound(Parens), _) => Mold.mk_op(Sort.Any, [Sort.Any])
      | (_, [t])
          when
            Token.is_potential_operator(t) && !Token.is_potential_operand(t) =>
        Mold.mk_bin(Precedence.max, Sort.Any, [])
      | _ => Mold.mk_op(Sort.Any, [])
      };
    check(
      mold_testable,
      "fallback mold: " ++ name,
      fallback_mold,
      Form.mold_of(id, stored),
    );
  };
};

/* remold_candidates: every candidate carries the queried sort, fits
   it, and spells the label; candidates are Compound/Tok/TokInfix
   only, atoms (single tokens) before compounds; the compound
   candidates are the families of the label's matching rows in
   priority order; TokInfix candidates appear exactly for
   InfixDelimiterPrefix tokens at its four sorts, with the
   concave-grout bin mold. */
let check_remold = (sort: Sort.t, label: Label.t): unit => {
  let name = case_name(sort, label);
  let cands = Form.remold_candidates(label, sort);
  List.iter(
    ((c, st)) => {
      check(sort_testable, "candidate sort: " ++ name, sort, st);
      check(
        sort_testable,
        "candidate mold sort: " ++ name,
        sort,
        Form.mold_of(c, st).out,
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
    |> List.filter(((_, def): (Form.family, Form.def)) =>
         def.label == label && def.mold.out == sort
       )
    |> List.map(fst);
  let actual_compounds =
    cands
    |> List.filter_map(
         fun
         | (Form.Compound(fam), _) => Some(fam)
         | _ => None,
       );
  check(
    list(family_testable),
    "compound families in priority order: " ++ name,
    expected_compounds,
    actual_compounds,
  );
  let rec atoms_then_compounds = (cands, seen_compound) =>
    switch (cands) {
    | [] => true
    | [(Form.Compound(_), _), ...tl] => atoms_then_compounds(tl, true)
    | [(Form.Tok(_) | Form.TokInfix(_), _), ...tl] =>
      !seen_compound && atoms_then_compounds(tl, seen_compound)
    };
  check(
    bool,
    "atoms precede compounds: " ++ name,
    true,
    atoms_then_compounds(cands, false),
  );
  let expected_tok_infix =
    switch (label) {
    | [t] =>
      Form.is_infix_delimiter_op_prefix(t)
      && List.mem(sort, [Sort.Exp, Pat, Typ, TPat])
    | _ => false
    };
  let tok_infix =
    cands
    |> List.filter(
         fun
         | (Form.TokInfix(_), _) => true
         | _ => false,
       );
  check(
    bool,
    "TokInfix candidate iff keyword-prefix at an IDP sort: " ++ name,
    expected_tok_infix,
    tok_infix != [],
  );
  List.iter(
    ((c, st)) =>
      check(
        mold_testable,
        "TokInfix mold is the concave-grout bin: " ++ name,
        Mold.mk_bin(Precedence.concave_grout, sort, []),
        Form.mold_of(c, st),
      ),
    tok_infix,
  );
};

let tests = (
  "FormId",
  [
    test_case(
      "family is the (label, shape-role) quotient of the definition rows",
      `Quick,
      check_family_grouping,
    ),
    test_case(
      "family uniqueness: (out sort -> mold) is a function; labels agree",
      `Quick,
      check_family_uniqueness,
    ),
    test_case(
      "Compound(family) label/mold agree with defs_of at each row sort",
      `Quick,
      () =>
      List.iter(
        (fam: Form.family) =>
          List.iter(
            (def: Form.def) => {
              let name = Form.show_family(fam);
              check(
                label_testable,
                name ++ " label",
                def.label,
                Form.label_of(Form.Compound(fam)),
              );
              check(
                mold_testable,
                name ++ " mold at " ++ Sort.to_string(def.mold.out),
                def.mold,
                Form.mold_of(Form.Compound(fam), def.mold.out),
              );
            },
            Form.defs_of(fam),
          ),
        Form.all_of_family,
      )
    ),
    test_case(
      "classify_label on all form labels x all sorts: label-preserving, "
      ++ "first-candidate, never the unregistered fallback",
      `Quick,
      () =>
      List.iter(
        label =>
          List.iter(
            sort => {
              check_classify(sort, label);
              let (id, stored) = Form.classify_label(sort, label);
              /* a registered label may classify as Tok when an atomic
                 class wins (e.g. "let" as a Var at Exp), but never as
                 the (Tok, Any) unregistered fallback */
              check(
                bool,
                "registered label never (Tok, Any): "
                ++ case_name(sort, label),
                true,
                switch (id, stored) {
                | (Form.Tok(_), Sort.Any) => false
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
              let (id, stored) = Form.classify_label(sort, [t]);
              check(
                bool,
                "mold sort in {sort, Any}: " ++ case_name(sort, [t]),
                true,
                List.mem(Form.mold_of(id, stored).out, [sort, Sort.Any]),
              );
              if (!is_compound_label([t])) {
                check(
                  bool,
                  "unregistered token never Compound: "
                  ++ case_name(sort, [t]),
                  true,
                  switch (id) {
                  | Form.Compound(_) => false
                  | Form.Tok(_)
                  | Form.TokInfix(_) => true
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
      ++ "priority order, TokInfix placement",
      `Quick,
      () =>
      List.iter(
        label => List.iter(sort => check_remold(sort, label), all_sorts),
        all_labels @ corpus_labels,
      )
    ),
  ],
);

open Alcotest;
open Language;

let testable_typ = testable(Fmt.using(Typ.show, Fmt.string), Typ.fast_equal);

[@deriving show({with_path: false})]
type issue =
  | Marks(list(Mark.t));

let source_equal = (a: Typ.source, b: Typ.source) =>
  Id.equal(a.id, b.id) && Typ.fast_equal(a.ty, b.ty);

let equal_partial_ap = (a: Mark.error_partial_ap, b: Mark.error_partial_ap) =>
  switch (a, b) {
  | (NoDeferredArgs, NoDeferredArgs) => true
  | (
      ArityMismatch({expected: e1, actual: a1}),
      ArityMismatch({expected: e2, actual: a2}),
    ) =>
    e1 == e2 && a1 == a2
  | _ => false
  };

let equal_builtin = (a: Mark.error_builtin, b: Mark.error_builtin) =>
  switch (a, b) {
  | (ToLvsMissingLabelsOnTuple(t1), ToLvsMissingLabelsOnTuple(t2)) =>
    Typ.fast_equal(t1, t2)
  | (ProjectLabelsMissingLabels(l1), ProjectLabelsMissingLabels(l2)) =>
    l1 == l2
  | (MissingLabels(l1), MissingLabels(l2)) => l1 == l2
  | (PivotLabelIsNotString(t1), PivotLabelIsNotString(t2)) =>
    Typ.fast_equal(t1, t2)
  | (ArgumentMustBeTuple, ArgumentMustBeTuple) => true
  | (ArgumentMustBeListOfTuples, ArgumentMustBeListOfTuples) => true
  | (AtLeast2Arguments, AtLeast2Arguments) => true
  | (Exactly2Arguments, Exactly2Arguments) => true
  | _ => false
  };

let rec equal_mark: (Mark.t, Mark.t) => bool =
  (a, b) =>
    switch (a, b) {
    | (NoMeet(j1, s1), NoMeet(j2, s2)) =>
      j1 == j2 && List.equal(source_equal, s1, s2)
    | (DuplicateLabel(l1, t1), DuplicateLabel(l2, t2)) =>
      l1 == l2 && Typ.fast_equal(t1, t2)
    | (CompareFun(t1), CompareFun(t2)) => Typ.fast_equal(t1, t2)
    | (DuplicateVar(n1, t1), DuplicateVar(n2, t2)) =>
      n1 == n2 && Typ.fast_equal(t1, t2)
    | (BadToken(s1), BadToken(s2)) => s1 == s2
    | (BadLabel(a1), BadLabel(a2)) => Any.fast_equal(a1, a2)
    | (InvalidLabel(l1, ls1), InvalidLabel(l2, ls2)) =>
      l1 == l2 && ls1 == ls2
    | (UnexpectedLabelSort(l1), UnexpectedLabelSort(l2)) => l1 == l2
    | (
        TupleLabelError({
          malformed_labels: m1,
          duplicate_labels: d1,
          invalid_labels: i1,
          typ: t1,
        }),
        TupleLabelError({
          malformed_labels: m2,
          duplicate_labels: d2,
          invalid_labels: i2,
          typ: t2,
        }),
      ) =>
      List.equal(Any.fast_equal, m1, m2)
      && d1 == d2
      && i1 == i2
      && Typ.fast_equal(t1, t2)
    | (IsMulti, IsMulti) => true
    | (FreeConstructor(c1), FreeConstructor(c2)) => Constructor.equal(c1, c2)
    | (ExplicitNonlabel, ExplicitNonlabel) => true
    | (
        ExpectationMismatch({ana: a1, syn: s1}),
        ExpectationMismatch({ana: a2, syn: s2}),
      ) =>
      Typ.fast_equal(a1, a2) && Typ.fast_equal(s1, s2)
    | (Free(v1), Free(v2)) => Var.equal(v1, v2)
    | (InexhaustiveMatch(ty1, ms1, ex1), InexhaustiveMatch(ty2, ms2, ex2)) =>
      Typ.fast_equal(ty1, ty2)
      && List.equal(equal_mark, ms1, ms2)
      && Any.fast_equal(ex1, ex2)
    | (IsDeferral(p1), IsDeferral(p2)) => p1 == p2
    | (IsBadPartialAp(e1), IsBadPartialAp(e2)) => equal_partial_ap(e1, e2)
    | (BuiltinError(b1), BuiltinError(b2)) => equal_builtin(b1, b2)
    | (
        InvalidUseMode({bad_typ: b1, inner_typ: i1}),
        InvalidUseMode({bad_typ: b2, inner_typ: i2}),
      ) =>
      Typ.fast_equal(b1, b2) && Typ.fast_equal(i1, i2)
    | (
        IsLivelitName({name: n1, exp_t: e1}),
        IsLivelitName({name: n2, exp_t: e2}),
      ) =>
      n1 == n2 && Typ.fast_equal(e1, e2)
    | (BadTrivAp(t1), BadTrivAp(t2)) => Typ.fast_equal(t1, t2)
    | (DotOperatorRequiresTuple, DotOperatorRequiresTuple) => true
    | (TupleExtensionRequiresTuples, TupleExtensionRequiresTuples) => true
    | (LabelNotFound(l1, ls1), LabelNotFound(l2, ls2)) =>
      l1 == l2 && ls1 == ls2
    | (ModuleMissingMembers(ns1), ModuleMissingMembers(ns2)) => ns1 == ns2
    | (
        ModuleMemberNotFound({name: n1, members: m1, type_member: t1}),
        ModuleMemberNotFound({name: n2, members: m2, type_member: t2}),
      ) =>
      n1 == n2 && m1 == m2 && t1 == t2
    | (
        ModuleTypeMemberNotFound({name: n1, members: m1, submodule: s1}),
        ModuleTypeMemberNotFound({name: n2, members: m2, submodule: s2}),
      ) =>
      n1 == n2 && m1 == m2 && s1 == s2
    | (
        TypWantModule({name: n1, typ: t1}),
        TypWantModule({name: n2, typ: t2}),
      ) =>
      n1 == n2 && Typ.fast_equal(t1, t2)
    | (
        ModuleTypeMemberMismatch({name: n1, expected: e1, actual: a1}),
        ModuleTypeMemberMismatch({name: n2, expected: e2, actual: a2}),
      ) =>
      n1 == n2 && Typ.fast_equal(e1, e2) && Typ.fast_equal(a1, a2)
    | (BadOperator(s1), BadOperator(s2)) => s1 == s2
    | (BadLivelitModel(t1), BadLivelitModel(t2)) => Typ.fast_equal(t1, t2)
    | (BadTheorem(t1), BadTheorem(t2)) => Typ.fast_equal(t1, t2)
    | (Redundant, Redundant) => true
    | (ExpectedConstructor, ExpectedConstructor) => true
    | (TypFreeTypeVariable(a), TypFreeTypeVariable(b)) => a == b
    | (TypDuplicateConstructor(c1), TypDuplicateConstructor(c2)) =>
      Constructor.equal(c1, c2)
    | (TypDuplicateLabels(ls1, t1), TypDuplicateLabels(ls2, t2)) =>
      ls1 == ls2 && Typ.fast_equal(t1, t2)
    | (TypWantTypeFoundAp, TypWantTypeFoundAp) => true
    | (TypWantLabel, TypWantLabel) => true
    | (TypWantProduct(t1), TypWantProduct(t2)) => Typ.fast_equal(t1, t2)
    | (TypWantConstructorFoundType(t1), TypWantConstructorFoundType(t2)) =>
      Typ.fast_equal(t1, t2)
    | (TypWantConstructorFoundAp, TypWantConstructorFoundAp) => true
    | (TypParseFailure, TypParseFailure) => true
    | (TPatShadowsType(s1, src1), TPatShadowsType(s2, src2)) =>
      s1 == s2 && src1 == src2
    | (TPatNotAVar(e1), TPatNotAVar(e2)) => e1 == e2
    | _ => false
    };

let equal_issue = (a: issue, b: issue): bool =>
  switch (a, b) {
  | (Marks(xs), Marks(ys)) => List.equal(equal_mark, xs, ys)
  };

let testable_issue: testable(issue) =
  testable(Fmt.using(show_issue, Fmt.string), equal_issue);

let statics = term =>
  fst(Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term));

/* Test-only helpers for inspecting the statics map. Kept here rather than in
   StaticsBase since nothing in production code consumes them. */
type errors_map = Id.Map.t(list(Mark.t));

let errors = (map: Statics.Map.t): list((Id.t, list(Mark.t))) =>
  Id.Map.fold(
    (id, info: Info.t, acc) =>
      switch (Info.marks_of(info)) {
      | [] => acc
      | ms => [(id, ms), ...acc]
      },
    map,
    [],
  );

let collect_errors = (map: Statics.Map.t): errors_map =>
  Id.Map.filter_map(
    (_: Uuidm.t, info: Info.t) =>
      switch (Info.marks_of(info)) {
      | [] => None
      | ms => Some(ms)
      },
    map,
  );

let parse_exp = (s: string) => {
  switch (Haz3lcore.Parser.to_term(s, ~root=Exp)) {
  | Some(e) => e
  | None => Alcotest.fail("Failed to parse expression: " ++ s)
  };
};

let annotate_static_errors = (exp: TermBase.exp_t, info_map: Statics.Map.t) => {
  Grammar.map_exp_annotation(
    ({ids, _}: IdTagged.IdTag.t) => {
      switch (Statics.Map.lookup(List.hd(ids), info_map)) {
      | Some(info) =>
        switch (Info.marks_of(info)) {
        | [] => None
        | ms => Some(Marks(ms))
        }
      | None =>
        Alcotest.fail("No info found for the id: " ++ Id.show(List.hd(ids)))
      }
    },
    exp,
  );
};

let annotated_exp: testable(Grammar.exp_t(option(issue))) =
  testable(
    Fmt.using([%derive.show: Grammar.exp_t(option(issue))], Fmt.string),
    Grammar.equal_exp_t(Option.equal(equal_issue)),
  );

let fresh = (exp: Grammar.exp_t(unit)): TermBase.exp_t => {
  Grammar.map_exp_annotation(
    (_annotation): IdTagged.IdTag.t => IdTagged.IdTag.mk_internal([Id.mk()]),
    exp,
  );
};

// Get the type from the statics
let type_of = (~static_map=?, f) => {
  let m =
    switch (static_map) {
    | Some(s) => s
    | None => statics(f)
    };
  Statics.Map.ty_of(IdTagged.rep_id(f), m);
};

let annotated_tree_test = (name, expected_type, expected_error_tree) => {
  let term = fresh(Grammar.map_exp_annotation(_ => (), expected_error_tree));
  let s = statics(term);
  let annotated: Grammar.exp_t(option(issue)) =
    annotate_static_errors(term, s);
  let typ = type_of(~static_map=s, term);
  Alcotest.check(annotated_exp, name, expected_error_tree, annotated);
  Alcotest.check(
    testable_typ,
    "Expected Type",
    expected_type,
    Option.get(typ),
  );
};

let inconsistent_typecheck = (name, exp) => {
  test_case(
    name,
    `Quick,
    () => {
      let s = statics(exp);

      let errors = List.map(ms => Marks(ms), List.map(snd, errors(s)));

      Alcotest.check(
        neg(list(testable_issue)),
        "Missing Static Errors",
        [],
        errors,
      );
    },
  );
};
let fully_consistent_typecheck =
    (~normalize=false, name, serialized, expected) => {
  test_case(
    name,
    `Quick,
    () => {
      let exp = parse_exp(serialized);
      let s = statics(exp);
      let errors = List.map(ms => Marks(ms), List.map(snd, errors(s)));
      let actual_type =
        type_of(~static_map=s, exp)
        |> Option.map(
             normalize
               ? Typ.normalize(Builtins.ctx_init(Some(Int))) : Fun.id,
           );
      Alcotest.check(list(testable_issue), "Static Errors", [], errors);
      Alcotest.check(
        Alcotest.option(testable_typ),
        serialized,
        expected,
        actual_type,
      );
    },
  );
};

let skip_known_bug = (message: string, expression: string) =>
  test_case("Known Bug: " ++ message, `Quick, () => {
    [@warning "-21"]
    {
      let uexp = parse_exp(expression);
      Alcotest.skip();
      let _ = statics(uexp);
      ();
    }
  });

// FactoryInfoError
module FIError =
  Grammar.Factory({
    type t = option(issue);
    let default_value = () => None;
  });
module FTemp =
  Grammar.Factory({
    type t = IdTagged.IdTag.t;
    let default_value = (): IdTagged.IdTag.t => IdTagged.IdTag.temp();
  });

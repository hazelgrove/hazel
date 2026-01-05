open Alcotest;
open Language;

let testable_typ = testable(Fmt.using(Typ.show, Fmt.string), Typ.fast_equal);

let eq_info_error_exp = (a: Info.error_exp, b: Info.error_exp) => {
  switch (a, b) {
  | (Common(DuplicateLabel(l, ty)), Common(DuplicateLabel(r, ty2))) =>
    l == r && Typ.fast_equal(ty, ty2)
  | (Common(NoType(BadLabel(a))), Common(NoType(BadLabel(b)))) =>
    Any.fast_equal(a, b)
  | (
      Common(NoType(InvalidLabel(a, ls))),
      Common(NoType(InvalidLabel(b, ls'))),
    ) =>
    a == b && ls == ls'
  | (
      Common(Inconsistent(Expectation({ana: a1, syn: a2}))),
      Common(Inconsistent(Expectation({ana: b1, syn: b2}))),
    ) =>
    Typ.fast_equal(a1, b1) && Typ.fast_equal(a2, b2)
  | (Common(TupleLabelError(err)), Common(TupleLabelError(err'))) =>
    List.equal(Any.fast_equal, err.malformed_labels, err'.malformed_labels)
    && List.equal(String.equal, err.duplicate_labels, err'.duplicate_labels)
    && List.equal(String.equal, err.invalid_labels, err'.invalid_labels)
    && Typ.fast_equal(err.typ, err'.typ)
  | (
      Common(NoType(FreeConstructor(a))),
      Common(NoType(FreeConstructor(b))),
    ) =>
    String.equal(a, b)
  | (FreeVariable(a), FreeVariable(b)) => String.equal(a, b)
  | _ =>
    Alcotest.fail(
      "Not implemented for "
      ++ Info.show_error_exp(a)
      ++ " and "
      ++ Info.show_error_exp(b),
    )
  };
};

let eq_info_error = (a: Info.error, b: Info.error) => {
  switch (a, b) {
  | (Exp(a), Exp(b)) => eq_info_error_exp(a, b)
  | _ =>
    Alcotest.fail(
      "Not implemented for "
      ++ Info.show_error(a)
      ++ " and "
      ++ Info.show_error(b),
    )
  };
};
let testable_info_error_exp =
  testable(Fmt.using(Info.show_error_exp, Fmt.string), Info.equal_error_exp);

let testable_error: testable(Info.error) =
  testable(Fmt.using(Info.show_error, Fmt.string), Info.equal_error);

let statics = Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)));

let parse_exp = (s: string) => {
  switch (Haz3lcore.Parser.to_term(s)) {
  | Some(e) => e
  | None => Alcotest.fail("Failed to parse expression: " ++ s)
  };
};

let annotate_static_errors = (exp: TermBase.exp_t, info_map: Statics.Map.t) => {
  Grammar.map_exp_annotation(
    ({ids, _}: IdTagged.IdTag.t) => {
      let new_info = Id.Map.find_opt(List.hd(ids), info_map);
      switch (new_info) {
      | Some(info) => Info.error_of(info)
      | None =>
        Alcotest.fail("No info found for the id: " ++ Id.show(List.hd(ids)))
      };
    },
    exp,
  );
};

let annotated_exp: testable(Grammar.exp_t(option(Info.error))) =
  testable(
    Fmt.using(
      [%derive.show: Grammar.exp_t(option(Info.error))],
      Fmt.string,
    ),
    Grammar.equal_exp_t(Option.equal(Info.equal_error)),
  );

let fresh = (exp: Grammar.exp_t(unit)): TermBase.exp_t => {
  Grammar.map_exp_annotation(
    (_annotation): IdTagged.IdTag.t => {
      let id = Id.mk();
      {ids: [id]};
    },
    exp,
  );
};

// Get the type from the statics
let type_of = (~static_map=?, f) => {
  IdTagged.rep_id(f)
  |> Id.Map.find_opt(
       _,
       switch (static_map) {
       | Some(s) => s
       | None => statics(f) |> fst
       },
     )
  |> Option.bind(
       _,
       fun
       | InfoExp(e) => Some(e.ty)
       | _ => None,
     );
};

let annotated_tree_test = (name, expected_type, expected_error_tree) => {
  let term = fresh(Grammar.map_exp_annotation(_ => (), expected_error_tree));
  let (s, _) = statics(term);
  let annotated: Grammar.exp_t(option(Info.error)) =
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
      let (s, _) = statics(exp);

      let errors = List.map(snd, Statics.Map.errors(s));

      Alcotest.check(
        neg(list(testable_error)),
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
      let (s, _) = statics(exp);
      let errors = List.map(snd, Statics.Map.errors(s));
      let actual_type =
        type_of(~static_map=s, exp)
        |> Option.map(
             normalize
               ? Typ.normalize(Builtins.ctx_init(Some(Int))) : Fun.id,
           );
      Alcotest.check(list(testable_error), "Static Errors", [], errors);
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
    type t = option(Info.error);
    let default_value = () => None;
  });
module FTemp =
  Grammar.Factory({
    type t = IdTagged.IdTag.t;
    let default_value = (): IdTagged.IdTag.t => {ids: [Id.invalid]};
  });

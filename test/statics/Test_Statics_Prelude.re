open Alcotest;
open Language;

let testable_typ = testable(Fmt.using(Typ.show, Fmt.string), Typ.fast_equal);

let testable_info_error_exp =
  testable(Fmt.using(Info.show_error_exp, Fmt.string), Info.equal_error_exp);

let testable_error: testable(Info.error) =
  testable(Fmt.using(Info.show_error, Fmt.string), Info.equal_error);

let statics = Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)));

let parse_exp = (s: string) => {
  switch (
    Haz3lcore.Parser.to_term(
      ~projector_init=Haz3lcore.Parser.default_projector_init,
      s,
    )
  ) {
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
       | None => statics(f)
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
  let s = statics(term);
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
      let s = statics(exp);

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
let fully_consistent_typecheck = (name, serialized, expected) => {
  test_case(
    name,
    `Quick,
    () => {
      let exp = parse_exp(serialized);
      let s = statics(exp);
      let errors = List.map(snd, Statics.Map.errors(s));
      let actual_type = type_of(~static_map=s, exp);
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

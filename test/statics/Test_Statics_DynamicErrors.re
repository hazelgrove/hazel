open Alcotest;
open Test_Statics_Prelude;
open Language;

/* Runtime errors (DynamicErrorHole) surface as Mark.DynamicError on the
   inner expression's info entry: printing strips the hole wrapper, so only
   the inner ids appear in rendered results and can carry the decoration. */

let wrap_dyn_err =
    (inner: TermBase.exp_t, err: InvalidOperationError.t): TermBase.exp_t => {
  term: DynamicErrorHole(inner, err),
  annotation: IdTagged.IdTag.mk_internal([Id.mk()]),
};

let tests = (
  "Statics.DynamicErrors",
  [
    test_case(
      "DynamicError mark lands on the inner expression",
      `Quick,
      () => {
        let inner = parse_exp("1 / 0");
        let exp = wrap_dyn_err(inner, DivideByZero);
        let s = statics(exp);
        let marks =
          switch (Statics.Map.lookup(IdTagged.rep_id(inner), s)) {
          | Some(info) => Info.marks_of(info)
          | None => Alcotest.fail("no info for inner expression")
          };
        check(
          testable_issue,
          "inner marks",
          Marks([DynamicError(DivideByZero)]),
          Marks(marks),
        );
      },
    ),
    test_case(
      "inner expression id is reported in error_ids",
      `Quick,
      () => {
        let inner = parse_exp("1 / 0");
        let exp = wrap_dyn_err(inner, DivideByZero);
        let s = statics(exp);
        let ids = Statics.Map.error_ids(s);
        check(
          bool,
          "error_ids contains inner rep_id",
          true,
          List.exists(Id.equal(IdTagged.rep_id(inner)), ids),
        );
      },
    ),
  ],
);

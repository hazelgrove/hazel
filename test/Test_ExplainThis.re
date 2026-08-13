open Language;

/* Property-based test ensuring ExplainThis never raises while producing
   documentation for any sub-term of an expression. The documentation for
   each form substitutes term ids into its explanation string via a format,
   so a mismatch between the number of `%s` placeholders and the number of
   supplied arguments crashes at runtime. This test guards against that. */

let globals = Web.Globals.Model.init(~settings=Web.Settings.Model.init, ());
let docs = Web.ExplainThisModel.init;

let statics = term =>
  fst(
    Statics.mk(
      CoreSettings.on,
      Builtins.ctx_init(Some(Operators.default_mode)),
      term,
    ),
  );

let qcheck_explainthis_does_not_crash =
  QCheck.Test.make(
    ~name="ExplainThis.get_doc does not crash",
    ~count=1000,
    QCheck_Util.arb_exp(~minimal_idents=true, 12),
    exp => {
    /* Statics failures are out of scope; we only assert that ExplainThis
       itself does not raise for any sub-term it is asked to document. */
    switch (statics(exp)) {
    | exception _ => true
    | info_map =>
      Id.Map.iter(
        (_id, info: Info.t) => {
          let _ =
            Web.ExplainThis.get_doc(
              ~globals,
              ~docs,
              Some(info),
              Web.ExplainThis.Colorings,
            );
          ();
        },
        info_map,
      );
      true;
    }
  });

/* The ExplainThis section title and the cursor inspector label both come
   from Info.cls_label, which must reflect the statics-re-kinded negation
   op rather than the user term's (always-Int) op. */
let unop_label = (program: string): option(string) => {
  let exp =
    switch (Haz3lcore.Parser.to_term(program, ~root=Exp)) {
    | Some(e) => e
    | None => Alcotest.fail("Failed to parse expression: " ++ program)
    };
  Id.Map.fold(
    (_id, info: Info.t, acc) =>
      switch (acc, Info.cls_of(info)) {
      | (None, Exp(UnOp(_))) => Some(Info.cls_label(info))
      | _ => acc
      },
    statics(exp),
    None,
  );
};

let negation_labels = () => {
  Alcotest.(check(option(string)))(
    "float negation label",
    Some("Float Negation"),
    unop_label("-1.5"),
  );
  Alcotest.(check(option(string)))(
    "integer negation label",
    Some("Integer Negation"),
    unop_label("-5"),
  );
};

let tests = (
  "ExplainThis",
  [
    QCheck_alcotest.to_alcotest(qcheck_explainthis_does_not_crash),
    Alcotest.test_case("negation labels re-kind by class", `Quick, () =>
      negation_labels()
    ),
  ],
);

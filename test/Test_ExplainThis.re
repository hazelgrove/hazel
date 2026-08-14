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

let tests = (
  "ExplainThis",
  [QCheck_alcotest.to_alcotest(qcheck_explainthis_does_not_crash)],
);

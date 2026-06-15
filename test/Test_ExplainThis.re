open Haz3lcore;
open Language;
open MenhirParser;

/* Property-based test ensuring ExplainThis never raises while producing
   documentation for any sub-term of an expression. The documentation for
   each form substitutes term ids into its explanation string via a format,
   so a mismatch between the number of `%s` placeholders and the number of
   supplied arguments crashes at runtime. This test guards against that. */

let globals = Web.Globals.Model.init();
let docs = Web.ExplainThisModel.init;

let arb_drv_exp = (~minimal_idents, size) => {
  open QCheck.Gen;
  let base = AST.gen_exp_sized(~minimal_idents, size);
  let to_core = menhir_exp =>
    Conversion.Exp.of_menhir_ast(menhir_exp)
    |> Grammar.map_exp_annotation(_ => IdTagged.IdTag.fresh());
  let show = exp =>
    switch (
      exp
      |> ExpToSegment.exp_to_segment(
           ~settings=ExpToSegment.Settings.editable(~inline=true),
           _,
         )
      |> Printer.of_segment(~holes="?", _)
    ) {
    | s => s
    | exception _ => "<unprintable expression>"
    };
  QCheck.make(~print=show, map(to_core, base));
};

let statics = term =>
  fst(Statics.mk(CoreSettings.on, Builtins.ctx_init(Some(Int)), term));

let qcheck_explainthis_does_not_crash =
  QCheck.Test.make(
    ~name="ExplainThis does not crash",
    ~count=1000,
    arb_drv_exp(~minimal_idents=true, 12),
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

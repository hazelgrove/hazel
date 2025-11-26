open Haz3lcore;

let qcheck_pads_typ_for_exp_to_segment =
  QCheck.Test.make(
    ~name="No ids are needed to be padded during ExpToSegment",
    ~count=1000,
    QCheck_Util.arb_typ(~minimal_idents=false, 30),
    typ => {
      let padded = PadIds.pad_typ_ids(typ);
      let _ =
        ExpToSegment.typ_to_segment(
          ~settings={
            inline: false,
            fold_case_clauses: false,
            fold_fn_bodies: `NoFold,
            hide_fixpoints: false,
            show_filters: true,
            show_unknown_as_hole: true,
            raise_if_padding: true // Will raise an exception if padding
          },
          padded,
        );
      Language.Equality.syntactic.typ(padded, PadIds.pad_typ_ids(padded));
    },
  );

let tests = [
  (
    "PadIds",
    [QCheck_alcotest.to_alcotest(qcheck_pads_typ_for_exp_to_segment)],
  ),
];

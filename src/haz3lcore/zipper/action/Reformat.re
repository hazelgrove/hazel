open Language;

let reformat = (_: Statics.Map.t, z: Zipper.t): option(Zipper.t) => {
  let term = MakeTerm.from_zip_for_sem(z).term;
  let seg =
    ExpToSegment.exp_to_segment(
      ~settings={
        inline: false,
        fold_case_clauses: false,
        fold_fn_bodies: false,
        hide_fixpoints: false,
        show_filters: true,
        show_unknown_as_hole: true,
      },
      term,
    );
  let z = Zipper.unzip(seg);
  Some(z);
};

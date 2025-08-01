open Language;

let introduce_labels = (statics: Statics.Map.t, z: Zipper.t) => {
  switch (Indicated.ci_of(z, statics)) {
  | None => None
  | Some(InfoExp({term, _})) =>
    let term =
      Exp.map_term(
        ~f_exp=
          (cont, exp) => {
            let id = Exp.rep_id(exp);
            switch (Id.Map.find_opt(id, statics)) {
            | Some(InfoExp({inferred_label: Some(label), term, _})) =>
              let label = Label(label) |> Exp.fresh;
              Exp.fresh(TupLabel(label, term));
            | _ => cont(exp)
            };
          },
        term,
      );
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
    let z = {
      ...z,
      selection: Selection.mk(seg),
    };
    Some(z);
  | _ => None
  };
};

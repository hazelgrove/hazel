// Rebuild a term with slice-omitted subterms replaced by holes.
let exp_hole = (e: Exp.t): Exp.t => {
  ...e,
  term: EmptyHole,
};
let pat_hole = (p: Pat.t): Pat.t => {
  ...p,
  term: EmptyHole,
};
let typ_hole = (t: Typ.t): Typ.t => {
  ...t,
  term: Unknown(Hole(EmptyHole)),
};
let tpat_hole = (tp: TPat.t): TPat.t => {
  ...tp,
  term: EmptyHole,
};
let mod_hole = (m: Mod.t): Mod.t => {
  ...m,
  term: EmptyHole,
};
let sig_hole = (s: Sig.t): Sig.t => {
  ...s,
  term: EmptyHole,
};

let rec reconstruct = (omitted: Id.Set.t, e: Exp.t): Exp.t =>
  Exp.map_term(
    ~f_exp=
      (continue, e) =>
        if (Id.Set.mem(Exp.rep_id(e), omitted)) {
          exp_hole(e);
        } else {
          switch (Exp.term_of(e)) {
          | Module(items) => {
              ...e,
              term: Module(List.map(reconstruct_mod(omitted), items)),
            }
          | _ => continue(e)
          };
        },
    ~f_pat=
      (continue, p) =>
        Id.Set.mem(Pat.rep_id(p), omitted) ? pat_hole(p) : continue(p),
    ~f_typ=
      (continue, t) =>
        if (Id.Set.mem(Typ.rep_id(t), omitted)) {
          typ_hole(t);
        } else {
          switch (Typ.term_of(t)) {
          | Sig(items) => {
              ...t,
              term: Sig(List.map(reconstruct_sig(omitted), items)),
            }
          | _ => continue(t)
          };
        },
    ~f_tpat=
      (continue, tp) =>
        Id.Set.mem(TPat.rep_id(tp), omitted)
          ? tpat_hole(tp) : continue(tp),
    e,
  )
and reconstruct_pat = (omitted: Id.Set.t, p: Pat.t): Pat.t =>
  Pat.map_term(
    ~f_pat=
      (continue, p) =>
        Id.Set.mem(Pat.rep_id(p), omitted) ? pat_hole(p) : continue(p),
    ~f_typ=(_, t) => reconstruct_typ(omitted, t),
    ~f_tpat=
      (continue, tp) =>
        Id.Set.mem(TPat.rep_id(tp), omitted)
          ? tpat_hole(tp) : continue(tp),
    p,
  )
and reconstruct_typ = (omitted: Id.Set.t, t: Typ.t): Typ.t =>
  Typ.map_term(
    ~f_exp=(_, e) => reconstruct(omitted, e),
    ~f_typ=
      (continue, t) =>
        if (Id.Set.mem(Typ.rep_id(t), omitted)) {
          typ_hole(t);
        } else {
          switch (Typ.term_of(t)) {
          | Sig(items) => {
              ...t,
              term: Sig(List.map(reconstruct_sig(omitted), items)),
            }
          | _ => continue(t)
          };
        },
    ~f_tpat=
      (continue, tp) =>
        Id.Set.mem(TPat.rep_id(tp), omitted)
          ? tpat_hole(tp) : continue(tp),
    t,
  )
and reconstruct_tpat = (omitted: Id.Set.t, tp: TPat.t): TPat.t =>
  TPat.map_term(
    ~f_tpat=
      (continue, tp) =>
        Id.Set.mem(TPat.rep_id(tp), omitted)
          ? tpat_hole(tp) : continue(tp),
    tp,
  )
and reconstruct_mod = (omitted: Id.Set.t, m: Mod.t): Mod.t =>
  if (Id.Set.mem(IdTagged.rep_id(m), omitted)) {
    mod_hole(m);
  } else {
    Mod.map_term(
      ~f_exp=(_, e) => reconstruct(omitted, e),
      ~f_pat=(_, p) => reconstruct_pat(omitted, p),
      ~f_typ=(_, t) => reconstruct_typ(omitted, t),
      ~f_tpat=(_, tp) => reconstruct_tpat(omitted, tp),
      m,
    );
  }
and reconstruct_sig = (omitted: Id.Set.t, s: Sig.t): Sig.t =>
  if (Id.Set.mem(IdTagged.rep_id(s), omitted)) {
    sig_hole(s);
  } else {
    Sig.map_term(
      ~f_pat=(_, p) => reconstruct_pat(omitted, p),
      ~f_typ=(_, t) => reconstruct_typ(omitted, t),
      ~f_tpat=(_, tp) => reconstruct_tpat(omitted, tp),
      s,
    );
  };

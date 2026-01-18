[@deriving (show({with_path: false}), sexp, yojson)]
type equiv('a) =
  | EquivCon(Prov.t, Prov.t)
  | DominateCon(Prov.t, 'a);
[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Typ(equiv(Typ.t))
  | TPat(equiv(TPat.t));

let terms_of_equiv = (equiv: Typ.equivalence) => {
  let Con(leftType, rightType) = equiv;
  (leftType |> Typ.term_of, rightType |> Typ.term_of);
};

let unfold_tpat_constramnot = (l_tpat: TPat.t, r_tpat: TPat.t) => {
  let (unwrapped_l_tpat, rewrap_ltpat) = IdTagged.unwrap(l_tpat);
  let (unwrapped_r_tpat, rewrap_rtpat) = IdTagged.unwrap(r_tpat);
  switch (unwrapped_l_tpat, unwrapped_r_tpat) {
  | (Unknown(p), Unknown(q)) =>
    if (Prov.is_identified(p) && Prov.is_identified(q)) {
      [TPat(EquivCon(p, q))];
    } else {
      [];
    }
  | (Unknown(p), t) =>
    if (Prov.is_identified(p)) {
      [TPat(DominateCon(p, t |> rewrap_rtpat))];
    } else {
      [];
    }
  | (t, Unknown(p)) =>
    if (Prov.is_identified(p)) {
      [TPat(DominateCon(p, t |> rewrap_ltpat))];
    } else {
      [];
    }
  | (Var(_), Var(_)) => []
  };
};

// precondition: recieves a consistent constramnot
// postondition: returns an equivalent list of canonical (left side is hole) constriants
let rec unfold_constramnot = (equiv: Typ.equivalence): list(t) => {
  let Con(left_equiv, right_equiv) = equiv;

  switch (terms_of_equiv(equiv)) {
  | (Parens(paren_ty), _) => unfold_constramnot(Con(paren_ty, right_equiv))
  | (_, Parens(paren_ty)) => unfold_constramnot(Con(left_equiv, paren_ty))
  // | (Unknown({term: Hole(EmptyHole), _}), _) => []
  // | (_, Unknown({term: Hole(EmptyHole), _})) => []
  | (Unknown(p), Unknown(q)) =>
    if (Prov.is_identified(p) && Prov.is_identified(q)) {
      [Typ(EquivCon(p, q))];
    } else {
      [];
    }
  | (Unknown(p), t) =>
    if (Prov.is_identified(p)) {
      [Typ(DominateCon(p, t |> Typ.temp))];
    } else {
      [];
    }
  | (t, Unknown(p)) =>
    if (Prov.is_identified(p)) {
      [Typ(DominateCon(p, t |> Typ.temp))];
    } else {
      [];
    }
  | (Arrow(l1, l2), Arrow(r1, r2)) =>
    unfold_constramnot(Con(l1, r1)) @ unfold_constramnot(Con(l2, r2))
  | (Prod(l_args), Prod(r_args)) =>
    unfold_constramnot_product(l_args, r_args)
  | (Label(_), Label(_)) => []
  | (TupLabel(l_label, l_typ), TupLabel(r_label, r_typ)) =>
    unfold_constramnot(Con(l_label, r_label))
    @ unfold_constramnot(Con(l_typ, r_typ))
  | (Atom(_), Atom(_)) => []
  | (Sum(_), Sum(_)) => []
  | (List(l), List(r)) => unfold_constramnot(Con(l, r))
  | (Var(_), Var(_)) => []
  | (Rec(_, l_ty), Rec(_, r_ty)) => unfold_constramnot(Con(l_ty, r_ty))
  | (Poly(l_tpat, l_ty), Poly(r_tpat, r_ty)) =>
    unfold_tpat_constramnot(l_tpat, r_tpat)
    @ unfold_constramnot(Con(l_ty, r_ty))
  | (ProdProjection(l1, l2), ProdProjection(r1, r2))
  | (ProdExtension(l1, l2), ProdExtension(r1, r2)) =>
    unfold_constramnot(Con(l1, r1)) @ unfold_constramnot(Con(l2, r2))
  | (Atom(_), _)
  | (Arrow(_), _)
  | (Var(_), _)
  | (Prod(_), _)
  | (Label(_), _)
  | (TupLabel(_), _)
  | (Sum(_), _)
  | (List(_), _)
  | (Rec(_), _)
  | (Poly(_), _)
  | (ProdExtension(_), _)
  | (ProdProjection(_), _)
  | (ExplicitNonlabel, _)
  | (ProofOf(_), _) => []
  };
}
and unfold_constramnot_product = (args1, args2): list(t) =>
  if (List.length(args1) == List.length(args2)) {
    List.fold_left2(
      (acc, t1, t2) => acc @ unfold_constramnot(Con(t1, t2)),
      [],
      args1,
      args2,
    );
  } else {
    [];
  };

let unfold_constramnots: list(Typ.equivalence) => list(t) =
  List.concat_map(unfold_constramnot);

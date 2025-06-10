open Util;
open OptUtil.Syntax;

let dhpat_extend_ctx = (dhpat: DHPat.t, ty: Typ.t, ctx: Ctx.t): option(Ctx.t) => {
  let rec dhpat_var_entry =
          (dhpat: DHPat.t, ty: Typ.t): option(list(Ctx.entry)) => {
    let ty' = ty;
    let ty =
      switch (ty.term) {
      | TupLabel(_, ty) => ty
      | _ => ty
      };
    switch (dhpat |> Pat.term_of) {
    | Var(name) =>
      let entry =
        Ctx.VarEntry({
          name,
          id: Id.invalid,
          typ: ty,
        });
      Some([entry]);
    | Label(name) =>
      Typ.equal(ty, Label(name) |> Typ.temp) ? Some([]) : None
    | TupLabel(_, dp1) =>
      switch (ty'.term) {
      | TupLabel(_, ty2)
          when
            LabeledTuple.has_same_labels(
              DHPat.match_tup_label(dhpat),
              Typ.match_tup_label(ty'),
            ) =>
        dhpat_var_entry(dp1, ty2)
      | TupLabel(_, _) => None
      | _ => dhpat_var_entry(dp1, ty)
      }
    | Tuple(l1) =>
      let (l1, ts) =
        Typ.matched_prod(ctx, l1, Pat.match_tup_label, ty, (name, b) =>
          TupLabel(Label(name) |> Pat.fresh, b) |> Pat.fresh
        );
      let* l =
        List.map2((dhp, typ) => {dhpat_var_entry(dhp, typ)}, l1, ts)
        |> OptUtil.sequence;
      Some(List.concat(l));
    | Cons(dhp1, dhp2) =>
      let* t = Typ.matched_list_strict(ctx, ty);
      let* l1 = dhpat_var_entry(dhp1, t);
      let* l2 = dhpat_var_entry(dhp2, List(t) |> Typ.temp);
      Some(l1 @ l2);
    | ListLit(l) =>
      let* t = Typ.matched_list_strict(ctx, ty);
      let* l =
        List.map(dhp => {dhpat_var_entry(dhp, t)}, l) |> OptUtil.sequence;
      Some(List.concat(l));
    | Ap({term: Constructor(name, _), _}, dhp) =>
      let* ctrs = Typ.get_sum_constructors(ctx, ty);
      let* typ = ConstructorMap.get_entry(name, ctrs);
      let* typ' = typ;
      dhpat_var_entry(dhp, typ');
    | Ap(_) => None
    | EmptyHole
    | Wild
    | Invalid(_)
    | MultiHole(_) => Some([])
    | Parens(dhp)
    | Probe(dhp, _) => dhpat_var_entry(dhp, ty)
    | Atom(c) =>
      Typ.equal(ty, Atom(Atom.cls_of_t(c)) |> Typ.temp) ? Some([]) : None
    | Constructor(_) => Some([]) // TODO: make this stricter
    | Cast(dhp, ty1, _) => dhpat_var_entry(dhp, ty1)
    };
  };
  let+ l = dhpat_var_entry(dhpat, ty);
  List.fold_left((ctx, entry) => Ctx.extend(ctx, entry), ctx, l);
};

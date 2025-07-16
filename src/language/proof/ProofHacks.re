open Util;
open OptUtil.Syntax;
// Find exp with id using ugly exception route

exception Found(Exp.t);

let find_exp_id = (id: Id.t, exp: Exp.t) =>
  switch (
    Exp.map_term(
      ~f_exp=
        (cont, exp) =>
          if (Exp.rep_id(exp) == id) {
            raise(Found(exp));
          } else {
            cont(exp);
          },
      exp,
    )
  ) {
  | exception (Found(x)) => Some(x)
  | _ => None
  };

let replace_exp_id = (id: Id.t, exp: Exp.t, new_exp: Exp.t) =>
  Exp.map_term(
    ~f_exp=
      (cont, exp) =>
        if (Exp.rep_id(exp) == id) {
          new_exp;
        } else {
          cont(exp);
        },
    exp,
  );

let rec exp_to_pat = (exp: Exp.t): Pat.t => {
  let term = exp |> Exp.term_of;
  let rewrap: Pat.term => Pat.t =
    term => {
      term,
      annotation: exp.annotation,
    };
  switch (term) {
  | Invalid(x) => rewrap(Invalid(x))
  | EmptyHole => rewrap(EmptyHole)
  | MultiHole(xs) => rewrap(MultiHole(xs))
  | Atom(Bool(b)) => rewrap(Atom(Bool(b)))
  | Atom(Int(i)) => rewrap(Atom(Int(i)))
  | Atom(Float(f)) => rewrap(Atom(Float(f)))
  | Atom(String(s)) => rewrap(Atom(String(s)))
  | ListLit(xs) => rewrap(ListLit(List.map(exp_to_pat, xs)))
  | Constructor(c, t) => rewrap(Constructor(c, t))
  | Cons(e1, e2) => rewrap(Cons(exp_to_pat(e1), exp_to_pat(e2)))
  | Var(x) => rewrap(Var(x))
  | Tuple(xs) => rewrap(Tuple(List.map(exp_to_pat, xs)))
  | Parens(e) => rewrap(Parens(exp_to_pat(e)))
  | Ap(_, e1, e2) => rewrap(Ap(exp_to_pat(e1), exp_to_pat(e2)))
  | Asc(e, t1) => rewrap(Asc(exp_to_pat(e), t1))
  | _ => MultiHole([Exp(exp)]) |> Pat.fresh
  };
};

let rec pat_to_exp = (pat: Pat.t): Exp.t => {
  let term = pat |> Pat.term_of;
  let rewrap: Exp.term => Exp.t =
    term => {
      term,
      annotation: pat.annotation,
    };
  switch (term) {
  | Invalid(x) => rewrap(Invalid(x))
  | EmptyHole => rewrap(EmptyHole)
  | MultiHole(xs) => rewrap(MultiHole(xs))
  | Wild => rewrap(Atom(Bool(true)))
  | Atom(a) => rewrap(Atom(a))
  | ListLit(xs) => rewrap(ListLit(List.map(pat_to_exp, xs)))
  | Constructor(c, t) => rewrap(Constructor(c, t))
  | Cons(e1, e2) => rewrap(Cons(pat_to_exp(e1), pat_to_exp(e2)))
  | Var(x) => rewrap(Var(x))
  | Tuple(xs) => rewrap(Tuple(List.map(pat_to_exp, xs)))
  | Parens(e) => rewrap(Parens(pat_to_exp(e)))
  | Ap(e1, e2) => rewrap(Ap(Forward, pat_to_exp(e1), pat_to_exp(e2)))
  | Asc(e, t1) => rewrap(Asc(pat_to_exp(e), t1))
  | Label(l) => rewrap(Label(l))
  | TupLabel(l, e) => rewrap(TupLabel(pat_to_exp(l), pat_to_exp(e)))
  | Probe(e, probe) => rewrap(Probe(pat_to_exp(e), probe))
  };
};

let add_wrapping_function = (~typ=?, pat: Pat.t): Exp.t => {
  Fun(pat, EmptyHole |> Exp.fresh, typ, None) |> Exp.fresh;
};

let rec remove_wrapping_function = (exp: Exp.t): Pat.t => {
  switch (exp |> Exp.term_of) {
  | Fun(p, _, _, _) => p
  | Asc(e, _) => remove_wrapping_function(e) // see https://github.com/hazelgrove/hazel/issues/1586
  | _ => MultiHole([Exp(exp)]) |> Pat.fresh
  };
};

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
          custom_statics: None,
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
    | Asc(dhp, ty1) => dhpat_var_entry(dhp, ty1)
    };
  };
  let+ l = dhpat_var_entry(dhpat, ty);
  List.fold_left((ctx, entry) => Ctx.extend(ctx, entry), ctx, l);
};

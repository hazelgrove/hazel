/*
   Matched type judgements (▷): extract expected shapes from types after WHNF.
   Prefer these over ad hoc `Typ.term_of` switches — parens, unknowns, SynSwitch.

   Every function returns a list of unification constraints alongside the
   extracted shape. When the input is an Unknown(SynSwitch) we delegate to
   the branch's `Typ.matched_*` helpers, which create fresh provenances for
   the inferred shape and emit Con(ty, ShapedType) constraints that the
   inference pipeline consumes.
 */

open Util;
open Either;
open Typ;

let internal = () => Unknown(Internal |> Prov.fresh) |> temp;

/* Arrow: returns Some((in, out, cons)) when ty matches Arrow shape or is a
   refinable Unknown; None otherwise. */
let rec arrow = (ctx, ty) =>
  switch (term_of(weak_head_normalize(ctx, ty))) {
  | Parens(ty) => arrow(ctx, ty)
  | Arrow(ty_in, ty_out) => Some((ty_in, ty_out, []))
  | Unknown({term: SynSwitch, _} as prov) =>
    Some(Typ.matched_arrow_of_prov(prov, ty))
  | _ => None
  };

let arrow_tolerant = (ctx, ty): (Typ.t, Typ.t, list(Typ.equivalence)) =>
  switch (arrow(ctx, ty)) {
  | Some(triple) => triple
  | None =>
    /* Unknown(p) when p is not SynSwitch: use that prov for refinement.
       Otherwise allocate a fresh Internal prov. */
    switch (term_of(weak_head_normalize(ctx, ty))) {
    | Unknown(prov) => Typ.matched_arrow_of_prov(prov, ty)
    | _ =>
      let prov = (Internal: TermBase.Prov.term) |> IdTagged.temp;
      Typ.matched_arrow_of_prov(prov, ty);
    }
  };

let rec poly_pair = (ctx, ty) =>
  switch (term_of(weak_head_normalize(ctx, ty))) {
  | Parens(ty) => poly_pair(ctx, ty)
  | Poly(t, ty) => Some((Some(t), ty, []))
  | Unknown({term: SynSwitch, _} as prov) =>
    Some(Typ.matched_poly_of_prov(prov, ty))
  | _ => None
  };

let poly_pair_tolerant =
    (ctx, ty): (option(TPat.t), Typ.t, list(Typ.equivalence)) =>
  switch (poly_pair(ctx, ty)) {
  | Some(triple) => triple
  | None =>
    switch (term_of(weak_head_normalize(ctx, ty))) {
    | Unknown(prov) => Typ.matched_poly_of_prov(prov, ty)
    | _ =>
      let prov = (Internal: TermBase.Prov.term) |> IdTagged.temp;
      Typ.matched_poly_of_prov(prov, ty);
    }
  };

/* prod_strict: (es', Some(tys), cons) on matched product or refinable
   Unknown; (es, None, []) otherwise. */
let rec prod_strict:
  type a.
    (Ctx.t, list(a), a => option((string, a)), Typ.t, (string, a) => a) =>
    (list(a), option(list(Typ.t)), list(Typ.equivalence)) =
  (ctx: Ctx.t, es, get_label_es, ty: Typ.t, constructor) => {
    switch (term_of(weak_head_normalize(ctx, ty))) {
    | Parens(ty) => prod_strict(ctx, es, get_label_es, ty, constructor)
    | Prod(tys: list(Typ.t)) =>
      if (List.length(es) != List.length(tys)) {
        (es, None, []);
      } else {
        (
          LabeledTuple.rearrange(
            match_tup_label,
            get_label_es,
            tys,
            es,
            constructor,
          ),
          Some(tys),
          [],
        );
      }
    | Unknown({term: SynSwitch, _} as prov) =>
      let (prod_provs, cons) = Typ.matched_prod_of_prov(prov, es, ty);
      (es, Some(prod_provs), cons);
    | _ => (es, None, [])
    };
  };

let prod = (ctx, es, get_label_es, ty, constructor) => {
  let (es', tys_opt, cons) =
    prod_strict(ctx, es, get_label_es, ty, constructor);
  switch (tys_opt) {
  | Some(tys) => (es', tys, cons)
  | None =>
    switch (term_of(weak_head_normalize(ctx, ty))) {
    | Unknown(prov) =>
      let (provs, c) = Typ.matched_prod_of_prov(prov, es, ty);
      (es', provs, c);
    | _ =>
      let prov = (Internal: TermBase.Prov.term) |> IdTagged.temp;
      let (provs, c) = Typ.matched_prod_of_prov(prov, es, ty);
      (es', provs, c);
    }
  };
};

let rec list_strict = (ctx, ty) =>
  switch (term_of(weak_head_normalize(ctx, ty))) {
  | Parens(ty) => list_strict(ctx, ty)
  | List(ty) => Some((ty, []))
  | Unknown({term: SynSwitch, _} as prov) =>
    Some(Typ.matched_list_hole_of_prov(prov, ty))
  | _ => None
  };

let list_tolerant = (ctx, ty): (Typ.t, list(Typ.equivalence)) =>
  switch (list_strict(ctx, ty)) {
  | Some(pair) => pair
  | None =>
    switch (term_of(weak_head_normalize(ctx, ty))) {
    | Unknown(prov) => Typ.matched_list_hole_of_prov(prov, ty)
    | _ =>
      let prov = (Internal: TermBase.Prov.term) |> IdTagged.temp;
      Typ.matched_list_hole_of_prov(prov, ty);
    }
  };

let rec args = (ctx, ty, arity): Either.t('a, int) => {
  switch (term_of(weak_head_normalize(ctx, ty))) {
  | Parens(ty) => args(ctx, ty, arity)
  | Prod(tys) when List.length(tys) == arity => L(tys)
  | Prod(tys) => R(List.length(tys))
  | _ when arity == 1 => L([ty])
  | Unknown(_) => L(List.init(arity, _ => internal()))
  | _ => R(1)
  };
};

let label = (ctx, ty): option((Typ.t, Typ.t, list(Typ.equivalence))) =>
  switch (term_of(weak_head_normalize(ctx, ty))) {
  | TupLabel({term: Label(ml), _}, ty) =>
    Some((Label(ml) |> temp, ty, []))
  | Unknown({term: SynSwitch, _}) =>
    /* Typ.matched_label handles SynSwitch internally and emits a TupLabel
       constraint refining the prov. */
    Typ.matched_label(ctx, ty)
  | _ => None
  };

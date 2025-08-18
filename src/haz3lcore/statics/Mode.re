open Util;

/* MODE.re

     This module defines the analytic type expectation imposed by a term's
      syntactic context.
   */

//maintain parentheses in slices
let of_parens = ids => Typ.(wrap_ana_slice(CodeSlice.of_ids(ids)));

// ty is Some if the expression is an annotated lambda
let of_arrow =
    (ids, ctx: Ctx.t, ana: Typ.t, ty: option(Typ.t)): (Typ.t, Typ.t) =>
  switch (ty) {
  | None => ana |> Typ.matched_arrow(ctx)
  | Some(ty') =>
    let (t1, t2) = ana |> Typ.matched_arrow(ctx);
    (Typ.join(ctx, t1, ty') |> Option.value(~default=ty'), t2)
    |> TupleUtil.map2(t => Typ.wrap_ana_slice(CodeSlice.of_ids(ids), t));
  };

let of_forall = (ctx: Ctx.t, name_opt: option(string), ana: Typ.t): Typ.t => {
  let (name_expected_opt, item) = Typ.matched_forall(ctx, ana);
  switch (name_opt, name_expected_opt) {
  | (Some(name), Some(name_expected)) =>
    Typ.subst(Var(name) |> Typ.temp_empty, name_expected, item)
  | _ => item
  };
};

let of_label = (ids, ana: Typ.t): option((Typ.t, Typ.t)) =>
  switch (ana) {
  | s when Typ.is_tuplabel(s, ~ignore_parens=false) =>
    let (label, val_ty) = Typ.untuplabel(s);
    switch (label |> Typ.term_of) {
    | Label(ana_label) =>
      Some((
        Typ.(
          Label(ana_label)
          |> from_ana_slice(CodeSlice.of_ids(ids))
          |> Typ.temp
        ),
        val_ty,
      ))
    | _ => None
    };
  | _ => None
  };

let of_prod =
    (
      ids,
      ctx: Ctx.t,
      ana: Typ.t,
      es: list('a),
      filt: 'a => option((string, 'a)),
      constructor: (string, 'a) => 'a,
    )
    : (list('a), list(Typ.t)) => {
  let (es, tys) = Typ.matched_prod(ctx, es, filt, ana, constructor);
  (es, tys |> List.map(Typ.(wrap_ana_slice(CodeSlice.of_ids(ids)))));
};

let of_cons_hd = (ids: list(Id.t), ctx: Ctx.t, ana: Typ.t): Typ.t =>
  Typ.(matched_list(ctx, ana) |> wrap_ana_slice(CodeSlice.of_ids(ids)));

let of_cons_tl = (ids: list(Id.t), ctx: Ctx.t, ana: Typ.t): Typ.t =>
  List(Typ.matched_list(ctx, ana))
  |> Typ.from_code_slices((
       // Re-add incremental slice of list, as we rewrap in a list: TODO - check this
       Typ.syn_code_slice_of(ana),
       CodeSlice.of_ids(ids),
     ))
  |> Typ.temp;

let of_list = (ids: list(Id.t), ctx: Ctx.t, ana: Typ.t): Typ.t =>
  Typ.(matched_list(ctx, ana) |> wrap_ana_slice(CodeSlice.of_ids(ids)));

let of_list_concat = (ids: list(Id.t), ctx: Ctx.t, ana: Typ.t): Typ.t =>
  List(Typ.matched_list(ctx, ana))
  |> Typ.from_code_slices((
       // Re-add incremental slice of list, as we rewrap in a list: TODO - check this
       Typ.syn_code_slice_of(ana),
       CodeSlice.of_ids(ids),
     ))
  |> Typ.temp;

let of_list_lit =
    (ids: list(Id.t), ctx: Ctx.t, length, ana: Typ.t): list(Typ.t) =>
  List.init(length, _ => of_list(ids, ctx, ana));

let of_deferred_ap_args = (length: int, ty_ins: list(Typ.t)): list(Typ.t) =>
  List.length(ty_ins) == length
    ? ty_ins
    : List.init(length, _ => (Unknown(Internal): Typ.term) |> Typ.temp_empty);

// Operations are analysed against the return type.
let of_op = (ids: list(Id.t), ty: Typ.term) =>
  ty |> Typ.from_ana_slice(CodeSlice.of_ids(ids)) |> Typ.temp;

let of_ann = (ids: list(Id.t), ty: Typ.t): Typ.t => {
  ty |> Typ.(wrap_ana_slice(CodeSlice.of_ids(ids)));
};

let of_ap_arg = of_ann;

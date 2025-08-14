open Util;
open OptUtil.Syntax;

/* MODE.re

     This module defines the analytic type expectation imposed by a term's
      syntactic context.
   */

//maintain parentheses in slices
let of_parens = ids => TypSlice.(wrap_incr(slice_of_ids(ids)));

// ty is Some if the expression is an annotated lambda
let of_arrow =
    (ids, ctx: Ctx.t, ana: TypSlice.t, ty: option(TypSlice.t)): (t, t) =>
  switch (ty) {
  | None => ana |> TypSlice.matched_arrow(ctx)
  | Some(ty') =>
    let (t1, t2) = ana |> TypSlice.matched_arrow(ctx);
    (TypSlice.join(ctx, t1, ty') |> Option.value(~default=ty'), t2)
    |> TupleUtil.map2(t => TypSlice.wrap_incr(TypSlice.slice_of_ids(ids), t));
  };

let of_forall =
    (ctx: Ctx.t, name_opt: option(string), ana: TypSlice.t): TypSlice.t => {
  let (name_expected_opt, item) = TypSlice.matched_forall(ctx, ana);
  switch (name_opt, name_expected_opt) {
  | (Some(name), Some(name_expected)) =>
    TypSlice.subst(`Typ(Var(name)) |> TypSlice.temp, name_expected, item)
  | _ => item
  };
};

let of_label = (ids, ana: TypSlice.t): option((TypSlice.t, TypSlice.t)) =>
  switch (ana) {
  | s when TypSlice.is_tuplabel(s, ~ignore_parens=false) =>
    let (label, val_ty) = TypSlice.untuplabel(s);
    switch (label |> TypSlice.typ_term_of) {
    | Label(ana_label) =>
      Some((
        TypSlice.(
          Label(ana_label)
          |> Typ.temp
          |> t_of_typ_t
          |> wrap_global(slice_of_ids(ids))
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
      ana: TypSlice.t,
      es: list('a),
      filt: 'a => option((string, 'a)),
      constructor: (string, 'a) => 'a,
    )
    : (list('a), list(TypSlice.t)) => {
  let (es, tys) = TypSlice.matched_prod(ctx, es, filt, ana, constructor);
  (es, tys |> List.map(TypSlice.(wrap_global(slice_of_ids(ids)))));
};

let of_cons_hd = (ids: list(Id.t), ctx: Ctx.t, ana: TypSlice.t): TypSlice.t =>
  TypSlice.(matched_list(ctx, ana) |> wrap_global(slice_of_ids(ids)));

// TODO: Slicing for syn case
let of_cons_tl = (ids: list(Id.t), ctx: Ctx.t, ana: TypSlice.t): TypSlice.t =>
  `SliceIncr((
    Slice(List(TypSlice.matched_list(ctx, ana))),
    TypSlice.(
      // Re-add incremental slice of list, as we rewrap in a list: TODO - check this
      ana |> term_of |> get_incr_slice_or_empty
    ),
  ))
  |> TypSlice.temp
  |> TypSlice.(wrap_global(slice_of_ids(ids)));

let of_list = (ids: list(Id.t), ctx: Ctx.t, ana: TypSlice.t): TypSlice.t =>
  TypSlice.(matched_list(ctx, ana) |> wrap_global(slice_of_ids(ids)));

// TODO: Slicing for syn case
let of_list_concat =
    (ids: list(Id.t), ctx: Ctx.t, ana: TypSlice.t): TypSlice.t =>
  `SliceIncr((
    Slice(List(TypSlice.matched_list(ctx, ana))),
    TypSlice.(
      // Re-add incremental slice of list, as we rewrap in a list
      ana |> term_of |> get_incr_slice_or_empty
    ),
  ))
  |> TypSlice.temp
  |> TypSlice.(wrap_global(slice_of_ids(ids)));

let of_list_lit =
    (ids: list(Id.t), ctx: Ctx.t, length, ana: TypSlice.t): list(TypSlice.t) =>
  List.init(length, _ => of_list(ids, ctx, ana));

let of_deferred_ap_args =
    (length: int, ty_ins: list(TypSlice.t)): list(TypSlice.t) =>
  List.length(ty_ins) == length
    ? ty_ins
    : List.init(length, _ =>
        (`Typ(Unknown(Internal)): TypSlice.term) |> TypSlice.temp
      );

// Operations are analysed against the return type.
let of_op = (ids: list(Id.t), ty: Typ.term) =>
  `SliceGlobal((`Typ(ty), TypSlice.slice_of_ids(ids))) |> TypSlice.temp;

let of_ann = (ids: list(Id.t), ty: TypSlice.t): t => {
  ty |> TypSlice.(wrap_global(slice_of_ids(ids)));
};

let of_ap_arg = of_ann;

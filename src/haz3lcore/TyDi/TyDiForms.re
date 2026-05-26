open Language;

/* This module generates TyDi suggestions which depend
 * neither on the typing context or the backpack */

let leading_expander = " ";

/* Specifies type information for syntactic forms. Could in principle be
 * derived by generating segments from Forms, parsing them to terms, and
 * running Statics, but for now, new forms e.g. operators must be added
 * below manually.  */
module Typ = {
  let unk: Typ.t = Unknown(Internal |> Prov.fresh) |> Typ.fresh;

  let of_const_mono_delim: list((Token.t, Typ.t)) = [
    ("true", Atom(Bool) |> Typ.fresh),
    ("false", Atom(Bool) |> Typ.fresh),
    //("[]", List(unk)), / *NOTE: would need to refactor buffer for this to show up */
    //("()", Prod([])), /* NOTE: would need to refactor buffer for this to show up */
    ("\"\"", Atom(String) |> Typ.fresh), /* NOTE: Irrelevent as second quote appears automatically */
    ("_", unk),
  ];

  /* Only need to add forms here if they have a non-trivial type */
  let of_leading_delim: list((Token.t, Typ.t)) = [
    ("fun" ++ leading_expander, Arrow(unk, unk) |> Typ.fresh),
    (
      "typfun" ++ leading_expander,
      Poly(Var("") |> TPat.fresh, unk) |> Typ.fresh,
    ),
    ("test" ++ leading_expander, Prod([]) |> Typ.fresh),
    ("of_jdmt" ++ leading_expander, unk),
    ("of_ctx" ++ leading_expander, unk),
    ("of_prop" ++ leading_expander, unk),
    ("of_alfa_exp" ++ leading_expander, unk),
    ("of_alfa_typ" ++ leading_expander, unk),
    ("of_alfa_pat" ++ leading_expander, unk),
    ("of_alfa_tpat" ++ leading_expander, unk),
  ];

  let of_infix_delim: list((Token.t, Typ.term)) = [
    ("|>", Unknown(Internal |> Prov.fresh)),
    (",", Prod([unk, unk])),
    ("::", List(unk)),
    ("@", List(unk)),
    (";", Unknown(Internal |> Prov.fresh)),
    ("&&", Atom(Bool)),
    ("\\/", Atom(Bool)),
    ("||", Atom(Bool)),
    ("==.", Atom(Bool)),
    ("==", Atom(Bool)),
    ("!", Atom(Bool)),
    ("!=", Atom(Bool)),
    ("!=.", Atom(Bool)),
    ("<", Atom(Bool)),
    (">", Atom(Bool)),
    ("<=", Atom(Bool)),
    (">=", Atom(Bool)),
    ("<.", Atom(Bool)),
    (">.", Atom(Bool)),
    ("<=.", Atom(Bool)),
    (">=.", Atom(Bool)),
    ("+", Atom(Int)),
    ("-", Atom(Int)),
    ("*", Atom(Int)),
    ("/", Atom(Int)),
    ("**", Atom(Int)),
    ("+.", Atom(Float)),
    ("-.", Atom(Float)),
    ("*.", Atom(Float)),
    ("/.", Atom(Float)),
    ("**.", Atom(Float)),
    ("++", Atom(String)),
  ];

  let expected: Info.t => Typ.t =
    fun
    | InfoExp({ana, _})
    | InfoPat({ana, _}) => ana
    | _ => Unknown(Internal |> Prov.fresh) |> Typ.fresh;

  let filter_by =
      (
        ctx: Ctx.t,
        expected_ty: Typ.t,
        self_tys: list((Token.t, Typ.t)),
        delims: list(string),
      )
      : list((Token.t, Typ.t)) =>
    List.filter_map(
      delim =>
        switch (List.assoc_opt(delim, self_tys)) {
        | _ when Form.is_annoying_delim(delim) => None
        | None => Some((delim, unk))
        | Some(self_ty) when Typ.is_consistent(ctx, expected_ty, self_ty) =>
          Some((delim, self_ty))
        | Some(_) => None
        },
      delims,
    );
};

/* Automatically collates most delimiters from Forms, notably all
 * mono delimiters, all infix operators, and all leading delimiters */
module Delims = {
  let leading = (sort: Sort.t): list(Token.t) =>
    Form.delims
    |> List.map(token => {
         let (lbl, _) = Form.Expansion.get(sort, token);
         switch (Form.Molds.try_get(sort, lbl)) {
         | None => []
         | Some(molds) =>
           molds
           |> List.filter_map((_: Mold.t) =>
                List.length(lbl) > 1 && token == List.hd(lbl)
                  ? Some(token ++ leading_expander) : None
              )
         };
       })
    |> List.flatten
    |> List.sort_uniq(compare);

  let leading_exp = leading(Exp);
  let leading_pat = leading(Pat);
  let leading_typ = leading(Typ);
  /* Drv sorts: at the mold level Drv(Jdmt)/Drv(Ctx)/Drv(Prop) all collapse
     to Drv(Exp) (see DrvSort.re on the "remolding issue"), so we reuse the
     Drv(Exp) delim list for all of them. */
  let leading_drv_exp = leading(Drv(Exp));
  let leading_drv_typ = leading(Drv(Typ));
  let leading_drv_pat = leading(Drv(Pat));
  let leading_drv_tpat = leading(Drv(TPat));

  let leading = (sort: Sort.t): list(string) =>
    switch (sort) {
    | Exp => leading_exp
    | Pat => leading_pat
    | Typ => leading_typ
    | Drv(Jdmt | Ctx | Prop | Exp) => leading_drv_exp
    | Drv(Typ) => leading_drv_typ
    | Drv(Pat) => leading_drv_pat
    | Drv(TPat) => leading_drv_tpat
    | _ => []
    };

  let infix = (sort: Sort.t): list(Token.t) =>
    Form.delims
    |> List.map(token => {
         List.filter_map(
           (m: Mold.t) =>
             m.out == sort && Mold.is_infix_op(m) ? Some(token) : None,
           switch (Form.Molds.compound([token])) {
           | Some(molds) => molds
           | None => []
           },
         )
       })
    |> List.flatten
    |> List.sort_uniq(compare);
  let infix_exp = infix(Exp);
  let infix_pat = infix(Pat);
  let infix_typ = infix(Typ);
  let infix_drv_exp = infix(Drv(Exp));
  let infix_drv_typ = infix(Drv(Typ));
  let infix_drv_pat = infix(Drv(Pat));
  let infix_drv_tpat = infix(Drv(TPat));
  let infix = (sort: Sort.t): list(string) =>
    switch (sort) {
    | Exp => infix_exp
    | Pat => infix_pat
    | Typ => infix_typ
    | Drv(Jdmt | Ctx | Prop | Exp) => infix_drv_exp
    | Drv(Typ) => infix_drv_typ
    | Drv(Pat) => infix_drv_pat
    | Drv(TPat) => infix_drv_tpat
    | _ => []
    };

  let const_mono = (sort: Sort.t): list(Token.t) =>
    Token.const_mono_delims
    |> List.map(token => {
         switch (Form.Molds.try_get(sort, [token])) {
         | None => []
         | Some(molds) =>
           molds
           |> List.filter_map((_: Mold.t) =>
                List.mem(token, Token.const_mono_delims) ? Some(token) : None
              )
         }
       })
    |> List.flatten
    |> List.sort_uniq(compare);

  /* base_typs (String, Int, Float, Bool, Nat, SInt) have Exp/Pat-sort
   * molds (as constructors) but no type entry in Typ.of_const_mono_delim.
   * Without an entry, filter_by assigns Unknown type, making them match
   * any expected type. Exclude them from Exp and Pat suggestions;
   * constructor suggestions come from TyDiCtx.bound_constructors instead.
   * They remain in Typ sort for type-position completion. */
  let const_mono_exp =
    const_mono(Exp) |> List.filter(t => !List.mem(t, Token.base_typs));
  let const_mono_pat =
    const_mono(Pat) |> List.filter(t => !List.mem(t, Token.base_typs));
  let const_mono_typ = const_mono(Typ);
  let const_mono_drv_exp = const_mono(Drv(Exp));
  let const_mono_drv_typ = const_mono(Drv(Typ));
  let const_mono_drv_pat = const_mono(Drv(Pat));
  let const_mono_drv_tpat = const_mono(Drv(TPat));

  let const_mono = (sort: Sort.t): list(string) =>
    switch (sort) {
    | Exp => const_mono_exp
    | Pat => const_mono_pat
    | Typ => const_mono_typ
    | Drv(Jdmt | Ctx | Prop | Exp) => const_mono_drv_exp
    | Drv(Typ) => const_mono_drv_typ
    | Drv(Pat) => const_mono_drv_pat
    | Drv(TPat) => const_mono_drv_tpat
    | _ => []
    };
};

let suggest_form =
    (ty_map, delims_of_sort, ci: Info.t): list(TyDiSuggestion.t) => {
  let sort = Info.sort_of(ci);
  let delims = delims_of_sort(sort);
  let filtered =
    Typ.filter_by(Info.ctx_of(ci), Typ.expected(ci), ty_map, delims);
  switch (sort) {
  | Exp =>
    List.map(
      ((content, ty)) =>
        TyDiSuggestion.{
          content,
          strategy: Exp(Common(NewForm(ty))),
        },
      filtered,
    )
  | Pat =>
    List.map(
      ((content, ty)) =>
        TyDiSuggestion.{
          content,
          strategy: Pat(Common(NewForm(ty))),
        },
      filtered,
    )
  | _ =>
    delims
    |> List.map(content =>
         TyDiSuggestion.{
           content,
           strategy: Typ(NewForm),
         }
       )
  };
};

let suggest_operator: Info.t => list(TyDiSuggestion.t) =
  suggest_form(
    List.map(((a, b)) => (a, IdTagged.fresh(b)), Typ.of_infix_delim),
    Delims.infix,
  );

let suggest_operand: Info.t => list(TyDiSuggestion.t) =
  suggest_form(Typ.of_const_mono_delim, Delims.const_mono);

let suggest_leading: Info.t => list(TyDiSuggestion.t) =
  suggest_form(Typ.of_leading_delim, Delims.leading);

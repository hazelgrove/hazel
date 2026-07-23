open Language;

/* This module generates TyDi suggestions which depend
 * neither on the typing context or the backpack */

let leading_expander = " ";

/* Automatically collates most delimiters from Forms, notably all
 * mono delimiters, all infix operators, and all leading delimiters */
module Delims = {
  let leading = (sort: Sort.t): list(Token.t) =>
    Form.delims
    |> List.filter_map(token => {
         let (lbl, _) = Form.Expansion.get(sort, token);
         Form.remold_candidates(lbl, sort) != []
         && List.length(lbl) > 1
         && token == List.hd(lbl)
           ? Some(token ++ leading_expander) : None;
       })
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

  /* compound forms only: single tokens registered merely as atoms
   * (e.g. infix-delimiter prefixes) are not infix suggestions */
  let infix = (sort: Sort.t): list(Token.t) =>
    Form.delims
    |> List.filter(token =>
         Form.compound_defs([token])
         |> List.exists(((_, m): (Form.family, Mold.t)) =>
              m.out == sort && Mold.is_infix_op(m)
            )
       )
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
    |> List.filter(token => Form.remold_candidates([token], sort) != [])
    |> List.sort_uniq(compare);

  /* base_typs (String, Int, Float, Bool, Nat, SInt) have Exp/Pat-sort
   * molds (as constructors) but derive Unknown self types (free
   * constructors), which would match any expected type. Exclude them
   * from Exp and Pat suggestions; constructor suggestions come from
   * TyDiCtx.bound_constructors instead. They remain in Typ sort for
   * type-position completion. */
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

/* Specifies type information for syntactic forms. All tables are
 * derived from the grammar at startup: each suggestible token is
 * parsed to a minimal term (infix operators pick up convex holes as
 * operands via regrout; leading delimiters expand to their completed
 * forms) and run through Statics in an empty context; the resulting
 * term's type is the entry. */
module Typ = {
  let unk: Typ.t = Unknown(Internal) |> Typ.fresh;

  /* Consumers ignore Unknown provenance (is_consistent); normalize
   * for stable, deterministic table entries */
  let normalize_unknowns: Typ.t => Typ.t =
    Typ.map_term(~f_typ=(continue, ty) =>
      switch (Typ.term_of(ty)) {
      | Unknown(_) => Typ.temp(Unknown(Internal))
      | _ => continue(ty)
      }
    );

  let derive_self_ty = (token: Token.t): option(Typ.t) =>
    try(
      switch (Parser.to_term(token, ~root=Exp)) {
      | None => None
      | Some(term) =>
        let (info_map, _) = Statics.mk(CoreSettings.on, Ctx.empty, term);
        switch (Id.Map.find_opt(Exp.rep_id(term), info_map)) {
        | Some(InfoExp({ty, _})) => Some(normalize_unknowns(ty))
        | _ => None
        };
      }
    ) {
    | _ => None
    };

  /* Deliberately untyped: forms whose minimal-form self type is
   * inconsistent with the instances they are used to build, so a
   * typed entry would suppress the suggestion exactly where it is
   * wanted. "=" (labeled-tuple element) and "{" (module literal)
   * derive singleton/empty products, inconsistent with the n-ary
   * products in use; "proof_object" derives ProofOf(<hole>), and
   * ProofOf consistency requires semantic exp equality. */
  let deliberately_untyped: list(Token.t) = [
    "=",
    "{" ++ leading_expander,
    "proof_object" ++ leading_expander,
  ];

  let derive_table = (tokens: list(Token.t)): list((Token.t, Typ.t)) =>
    tokens
    |> List.filter(t => !List.mem(t, deliberately_untyped))
    |> List.filter_map(t => derive_self_ty(t) |> Option.map(ty => (t, ty)));

  let of_const_mono_delim: list((Token.t, Typ.t)) =
    derive_table(
      List.sort_uniq(
        compare,
        Delims.const_mono(Exp) @ Delims.const_mono(Pat),
      ),
    );

  let of_infix_delim: list((Token.t, Typ.t)) =
    derive_table(
      List.sort_uniq(compare, Delims.infix(Exp) @ Delims.infix(Pat)),
    );

  /* Leading delimiters (with expander) parse to their completed forms,
   * so e.g. "fun " derives Arrow(?, ?) and "[ " derives [?]. Only the
   * Exp/Pat domains matter: suggest_form consults the table only for
   * those sorts. */
  let of_leading_delim: list((Token.t, Typ.t)) =
    derive_table(
      List.sort_uniq(compare, Delims.leading(Exp) @ Delims.leading(Pat)),
    );

  let expected: Info.t => Typ.t =
    fun
    | InfoExp({ana, _})
    | InfoPat({ana, _}) => ana
    | _ => Unknown(Internal) |> Typ.fresh;

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
  suggest_form(Typ.of_infix_delim, Delims.infix);

let suggest_operand: Info.t => list(TyDiSuggestion.t) =
  suggest_form(Typ.of_const_mono_delim, Delims.const_mono);

let suggest_leading: Info.t => list(TyDiSuggestion.t) =
  suggest_form(Typ.of_leading_delim, Delims.leading);

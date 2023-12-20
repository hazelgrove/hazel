open Util;
open OptUtil.Syntax;

/* This module generates TyDi suggestions which depend
 * neither on the typing context or the backpack */

/* Specifies type information for syntactic forms. Could in principle be
 * derived by generating segments from Forms, parsing them to terms, and
 * running Statics, but for now, new forms e.g. operators must be added
 * below manually.  */
module Typ = {
  let unk: Typ.t = Unknown(Internal);

  let of_abstract_mono_delim: list((Token.t, Typ.t)) = [
    ("~INTLIT~", Int),
    ("~FLOATLIT~", Float),
    ("~STRINGLIT~", String),
    ("~PATVAR~", unk),
    ("~TYPVAR~", unk),
    ("~CONSTRUCTOR~", unk) // for type definitions
  ];

  let of_const_mono_delim: list((Token.t, Typ.t)) = [
    ("true", Bool),
    ("false", Bool),
    ("[]", List(unk)), /*NOTE: would need to refactor buffer for this to show up */
    ("()", Prod([])), /* NOTE: would need to refactor buffer for this to show up */
    //("\"\"", String), /* NOTE: Irrelevent as second quote appears automatically */
    ("_", unk),
  ];

  let of_prefix_leading_delim: list((Token.t, Typ.t)) = [
    ("(", unk),
    ("case ", unk),
    ("if ", unk),
    ("let ", unk),
    ("type ", unk),
    ("[", List(unk)),
    ("test ", Prod([])),
    ("fun ", Arrow(unk, unk)),
  ];

  let of_postfix_leading_delim: list((Token.t, Typ.t)) = [
    ("(", unk), // TODO(andrew): do better. hack ap case, suggests too freely
    ("|", unk) //TODO(andrew): hack, for lsp case rules...
  ];

  let of_infix_delim: list((Token.t, Typ.t)) = [
    //(",", Prod([unk, unk])), /* NOTE: Current approach doesn't work for this, but irrelevant as 1-char */
    //TODO(andrew): with LSP commas now handled separately, see LSP
    ("::", List(unk)),
    (":", List(unk)), //TODO(andrew): hacky, for LSP
    ("@", List(unk)),
    (";", unk),
    ("&&", Bool),
    ("\\/", Bool),
    ("$==", Bool),
    ("==.", Bool),
    ("==", Bool),
    //("!", Bool), // maybe doesnt belong here? but blocks autocomplete of ! to !=
    ("!=", Bool),
    ("!=.", Bool),
    ("<", Bool),
    (">", Bool),
    ("<=", Bool),
    (">=", Bool),
    ("<.", Bool),
    (">.", Bool),
    ("<=.", Bool),
    (">=.", Bool),
    ("+", Int),
    ("-", Int),
    ("*", Int),
    ("/", Int),
    ("**", Int),
    ("+.", Float),
    ("-.", Float),
    ("*.", Float),
    ("/.", Float),
    ("**.", Float),
    ("++", String),
  ];

  let of_prefix_delim: list((Token.t, Typ.t)) = [("-", Int), ("!", Bool)];

  let expected: Info.t => Typ.t =
    fun
    | InfoExp({mode, _})
    | InfoPat({mode, _}) => Mode.ty_of(mode)
    | _ => Unknown(Internal);

  let filter_by =
      (
        ctx: Ctx.t,
        expected_ty: Typ.t,
        self_tys: list((Token.t, Typ.t)),
        delims: list(string),
      )
      : list((Token.t, Typ.t)) =>
    List.filter_map(
      delim => {
        let* self_ty = List.assoc_opt(delim, self_tys);
        Typ.is_consistent(ctx, expected_ty, self_ty)
          ? Some((delim, self_ty)) : None;
      },
      delims,
    );
};

/* Automatically collates most delimiters from Forms, notably all
 * mono delimiters, all infix operators, and all leading delimiters */
module Delims = {
  let is_const_mono_of = (sort, token, m: Mold.t) =>
    m.out == sort && List.mem(token, Form.const_mono_delims)
      ? Some(token) : None;

  let is_prefix_mono_of = (sort, token, m: Mold.t) =>
    Mold.is_prefix_op(m) && m.out == sort ? Some(token) : None;

  let is_infix_mono_of = (sort, token, m: Mold.t) =>
    Mold.is_infix_op(m) && m.out == sort ? Some(token) : None;

  let is_left_concave_poly_instant = (sort, token, m: Mold.t) =>
    Mold.is_left_concave(m) && fst(m.nibs).sort == sort
      ? Some(token) : None;

  let is_left_convex_poly_delayed = (sort, token, m: Mold.t) =>
    //HACK(andrew): addition of space to get autoexpand for tydi
    Mold.is_left_convex(m) && m.out == sort ? Some(token ++ " ") : None;

  let is_left_convex_poly_instant = (sort, token, m: Mold.t) =>
    //TODO(andrew): propogate this sort logic everywhere, replacing .out?
    Mold.is_left_convex(m) && fst(m.nibs).sort == sort ? Some(token) : None;

  let filter_mono = (f, sort: Sort.t, token: Token.t): list(string) =>
    List.filter_map(f(sort, token), Molds.get([token]));

  let filter_poly = (expander, f, sort: Sort.t, token: Token.t): list(string) =>
    switch (token |> expander |> fst) {
    | [hd, _, ..._] as lbl when token == hd =>
      List.filter_map(f(sort, token), Molds.get(lbl))
    | _ => []
    };

  let collate = (base: list(Token.t), filter, sort: Sort.t): list(Token.t) =>
    base
    |> List.map(filter(sort))
    |> List.flatten
    |> List.sort_uniq(compare);

  let left_convex_poly_delayed =
    collate(
      Form.delims,
      filter_poly(Molds.delayed_expansion, is_left_convex_poly_delayed),
    );
  let left_convex_poly_instant =
    collate(
      Form.delims,
      filter_poly(Molds.instant_expansion, is_left_convex_poly_instant),
    );
  let left_concave_poly_instant =
    collate(
      Form.delims,
      filter_poly(Molds.instant_expansion, is_left_concave_poly_instant),
    );
  let infix_mono = collate(Form.delims, filter_mono(is_infix_mono_of));
  let prefix_mono = collate(Form.delims, filter_mono(is_prefix_mono_of));
  let const_convex_mono =
    collate(Form.const_mono_delims, filter_mono(is_const_mono_of));

  let left_convex_poly_exp =
    left_convex_poly_instant(Exp) @ left_convex_poly_delayed(Exp);
  let left_convex_poly_pat =
    left_convex_poly_instant(Pat) @ left_convex_poly_delayed(Pat);
  let left_convex_poly_typ =
    left_convex_poly_instant(Typ) @ left_convex_poly_delayed(Typ);

  let left_convex_poly = (sort: Sort.t): list(string) =>
    switch (sort) {
    | Exp => left_convex_poly_exp
    | Pat => left_convex_poly_pat
    | Typ => left_convex_poly_typ
    | _ => []
    };

  let left_concave_poly_exp = left_concave_poly_instant(Exp);
  let left_concave_poly_pat = left_concave_poly_instant(Pat);
  let left_concave_poly_typ = left_concave_poly_instant(Typ);
  let left_concave_poly = (sort: Sort.t): list(string) =>
    switch (sort) {
    | Exp => left_concave_poly_exp
    | Pat => left_concave_poly_pat
    | Typ => left_concave_poly_typ
    | _ => []
    };

  let infix_mono_exp = infix_mono(Exp);
  let infix_mono_pat = infix_mono(Pat);
  let infix_mono_typ = infix_mono(Typ);
  let infix_mono = (sort: Sort.t): list(string) =>
    switch (sort) {
    | Exp => infix_mono_exp
    | Pat => infix_mono_pat
    | Typ => infix_mono_typ
    | _ => []
    };

  let prefix_mono_exp = prefix_mono(Exp);
  let prefix_mono_pat = prefix_mono(Pat);
  let prefix_mono_typ = prefix_mono(Typ);
  let prefix_mono = (sort: Sort.t): list(string) =>
    switch (sort) {
    | Exp => prefix_mono_exp
    | Pat => prefix_mono_pat
    | Typ => prefix_mono_typ
    | _ => []
    };

  let const_convex_mono_exp = const_convex_mono(Exp);
  let const_convex_mono_pat = const_convex_mono(Pat);
  let const_convex_mono_typ = const_convex_mono(Typ);
  let const_convex_mono = (sort: Sort.t): list(string) =>
    switch (sort) {
    | Exp => const_convex_mono_exp
    | Pat => const_convex_mono_pat
    | Typ => const_convex_mono_typ
    | _ => []
    };

  let abstract_convex_mono = (sort: Sort.t): list(string) =>
    switch (sort) {
    | Exp => ["~INTLIT~", "~FLOATLIT~", "~STRINGLIT~"]
    | Pat => ["~PATVAR~", "~INTLIT~", "~FLOATLIT~", "~STRINGLIT~"]
    | Typ => ["~CONSTRUCTOR~"]
    | TPat => ["~TYPVAR~"]
    | _ => []
    };
};

let sug' =
    (ty_map, delims_of_sort, sort: Sort.t, ctx: Ctx.t, ty_expected)
    : list(Suggestion.t) => {
  let delims = delims_of_sort(sort);
  let filtered = Typ.filter_by(ctx, ty_expected, ty_map, delims);
  switch (sort) {
  | Exp =>
    List.map(
      ((content, ty)) =>
        Suggestion.{content, strategy: Exp(Common(NewForm(ty)))},
      filtered,
    )
  | Pat =>
    List.map(
      ((content, ty)) =>
        Suggestion.{content, strategy: Pat(Common(NewForm(ty)))},
      filtered,
    )
  | _ =>
    delims
    |> List.map(content => Suggestion.{content, strategy: Typ(NewForm)})
  };
};

let suggest_form = (ty_map, delims_of_sort, ci: Info.t): list(Suggestion.t) =>
  sug'(
    ty_map,
    delims_of_sort,
    Info.sort_of(ci),
    Info.ctx_of(ci),
    Typ.expected(ci),
  );

let suggest_const_mono: Info.t => list(Suggestion.t) =
  suggest_form(Typ.of_const_mono_delim, Delims.const_convex_mono);

let suggest_abstract_mono: Info.t => list(Suggestion.t) =
  suggest_form(Typ.of_abstract_mono_delim, Delims.abstract_convex_mono);

let suggest_infix_mono: Info.t => list(Suggestion.t) =
  suggest_form(Typ.of_infix_delim, Delims.infix_mono);

let suggest_prefix_mono: Info.t => list(Suggestion.t) =
  suggest_form(Typ.of_prefix_delim, Delims.prefix_mono);

let suggest_prefix_leading: Info.t => list(Suggestion.t) =
  suggest_form(Typ.of_prefix_leading_delim, Delims.left_convex_poly);

let suggest_postfix_leading: Info.t => list(Suggestion.t) =
  suggest_form(Typ.of_postfix_leading_delim, Delims.left_concave_poly);

let suggest_all_ty_convex = (s: Sort.t, c: Ctx.t, ty): list(Suggestion.t) => {
  sug'(Typ.of_const_mono_delim, Delims.const_convex_mono, s, c, ty)
  @ sug'(Typ.of_abstract_mono_delim, Delims.abstract_convex_mono, s, c, ty)
  @ sug'(Typ.of_prefix_delim, Delims.prefix_mono, s, c, ty)
  @ sug'(Typ.of_prefix_leading_delim, Delims.left_convex_poly, s, c, ty);
};

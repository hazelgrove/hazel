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
  ];

  let of_const_mono_delim: list((Token.t, Typ.t)) = [
    ("true", Bool),
    ("false", Bool),
    ("[]", List(unk)), /*NOTE: would need to refactor buffer for this to show up */
    ("()", Prod([])), /* NOTE: would need to refactor buffer for this to show up */
    ("\"\"", String), /* NOTE: Irrelevent as second quote appears automatically */
    ("_", unk) //TODO: this shouldn't show up for exp?
  ];

  let of_prefix_leading_delim: list((Token.t, Typ.t)) = [
    ("case ", unk),
    ("fun ", Arrow(unk, unk)),
    ("if ", unk),
    ("let ", unk),
    ("test ", Prod([])),
    ("type ", unk),
    ("(", unk), //TODO(andrew): note this one has two possible left tips (parens vs app)
    ("[", List(unk)),
    ("|", unk) //TODO(andrew): hack, for lsp case rules...
  ];

  let of_postfix_leading_delim: list((Token.t, Typ.t)) = [
    ("(", unk), // hack? ap case. can do better for type logic?
    ("|", unk) //TODO(andrew): hack, for lsp case rules...
  ];

  let of_infix_delim: list((Token.t, Typ.t)) = [
    (",", Prod([unk, unk])), /* NOTE: Current approach doesn't work for this, but irrelevant as 1-char */
    ("::", List(unk)),
    (":", List(unk)), //TODO(andrew): hacky, for LSP
    ("@", List(unk)),
    ("|>", unk), //TODO: not actually in Forms
    ("||", Bool), //TODO: add to Forms
    //(";", unk), //TODO: do we want this?
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
    m.out == sort && Mold.is_prefix_op(m) ? Some(token) : None;

  let is_infix_mono_of = (sort, token, m: Mold.t) =>
    m.out == sort && Mold.is_infix_op(m) ? Some(token) : None;

  let is_postfix_instant = (sort, token, m: Mold.t) =>
    fst(m.nibs).shape != Convex && fst(m.nibs).sort == sort
      ? Some(token) : None;

  let is_prefix_delayed = (sort, token, m: Mold.t) =>
    //HACK(andrew): addition of space to get autoexpand for tydi
    fst(m.nibs).shape == Convex && m.out == sort ? Some(token ++ " ") : None;

  let is_prefix_instant = (sort, token, m: Mold.t) =>
    //TODO(andrew): propogate this sort logic everywhere
    fst(m.nibs).shape == Convex && fst(m.nibs).sort == sort
      ? Some(token) : None;

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

  let delayed_prefix_leading =
    collate(
      Form.delims,
      filter_poly(Molds.delayed_expansion, is_prefix_delayed),
    );
  let instant_prefix_leading =
    collate(
      Form.delims,
      filter_poly(Molds.instant_expansion, is_prefix_instant),
    );
  let instant_postfix_leading =
    collate(
      Form.delims,
      filter_poly(Molds.instant_expansion, is_postfix_instant),
    );
  let infix_mono = collate(Form.delims, filter_mono(is_infix_mono_of));
  let prefix_mono = collate(Form.delims, filter_mono(is_prefix_mono_of));
  let const_mono =
    collate(Form.const_mono_delims, filter_mono(is_const_mono_of));

  let prefix_leading_exp =
    instant_prefix_leading(Exp) @ delayed_prefix_leading(Exp);
  let prefix_leading_pat =
    instant_prefix_leading(Pat) @ delayed_prefix_leading(Pat);
  let prefix_leading_typ =
    instant_prefix_leading(Typ) @ delayed_prefix_leading(Typ);

  let prefix_leading = (sort: Sort.t): list(string) =>
    switch (sort) {
    | Exp => prefix_leading_exp
    | Pat => prefix_leading_pat
    | Typ => prefix_leading_typ
    | _ => []
    };

  let postfix_leading_exp = instant_postfix_leading(Exp);
  let postfix_leading_pat = instant_postfix_leading(Pat);
  let postfix_leading_typ = instant_postfix_leading(Typ);
  let postfix_leading = (sort: Sort.t): list(string) =>
    switch (sort) {
    | Exp => postfix_leading_exp
    | Pat => postfix_leading_pat
    | Typ => postfix_leading_typ
    | _ => []
    };

  let infix_exp = infix_mono(Exp);
  let infix_pat = infix_mono(Pat);
  let infix_typ = infix_mono(Typ);
  let infix = (sort: Sort.t): list(string) =>
    switch (sort) {
    | Exp => infix_exp
    | Pat => infix_pat
    | Typ => infix_typ
    | _ => []
    };

  let prefix_exp = prefix_mono(Exp);
  let prefix_pat = prefix_mono(Pat);
  let prefix_typ = prefix_mono(Typ);
  let prefix = (sort: Sort.t): list(string) =>
    switch (sort) {
    | Exp => prefix_exp
    | Pat => prefix_pat
    | Typ => prefix_typ
    | _ => []
    };

  let const_mono_exp = const_mono(Exp);
  let const_mono_pat = const_mono(Pat);
  let const_mono_typ = const_mono(Typ);
  let const_mono = (sort: Sort.t): list(string) =>
    switch (sort) {
    | Exp => const_mono_exp
    | Pat => const_mono_pat
    | Typ => const_mono_typ
    | _ => []
    };

  let abstract_mono = (sort: Sort.t): list(string) =>
    switch (sort) {
    | Exp => ["~INTLIT~", "~FLOATLIT~", "~STRINGLIT~"]
    | Pat => ["~PATVAR~", "~INTLIT~", "~FLOATLIT~", "~STRINGLIT~"]
    | Typ => []
    | TPat => ["~TYPVAR~"]
    | _ => []
    };
};

let suggest_form = (ty_map, delims_of_sort, ci: Info.t): list(Suggestion.t) => {
  let sort = Info.sort_of(ci);
  let delims = delims_of_sort(sort);
  let filtered =
    Typ.filter_by(Info.ctx_of(ci), Typ.expected(ci), ty_map, delims);
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

let suggest_const_mono: Info.t => list(Suggestion.t) =
  suggest_form(Typ.of_const_mono_delim, Delims.const_mono);

let suggest_abstract_mono: Info.t => list(Suggestion.t) =
  suggest_form(Typ.of_abstract_mono_delim, Delims.abstract_mono);

let suggest_infix_mono: Info.t => list(Suggestion.t) =
  suggest_form(Typ.of_infix_delim, Delims.infix);

let suggest_prefix_mono: Info.t => list(Suggestion.t) =
  suggest_form(Typ.of_prefix_delim, Delims.prefix);

let suggest_prefix_leading: Info.t => list(Suggestion.t) =
  suggest_form(Typ.of_prefix_leading_delim, Delims.prefix_leading);

let suggest_postfix_leading: Info.t => list(Suggestion.t) =
  suggest_form(Typ.of_postfix_leading_delim, Delims.postfix_leading);

open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type ancestors = list(Id.t);

// EXP

[@deriving (show({with_path: false}), sexp, yojson)]
type ana_exp =
  | Jdmt
  | Ctx
  | Prop
  | Exp;

let repr_list_ana_exp = (anas: list(ana_exp)): string =>
  anas |> List.map(show_ana_exp) |> String.concat(", ");

let ana_exp_match_sort = (ana: ana_exp, sort: DrvSort.t): bool =>
  switch (ana, sort) {
  | (Jdmt, Jdmt)
  | (Ctx, Ctx)
  | (Prop, Prop)
  | (Exp, Exp) => true
  | _ => false
  };

[@deriving (show({with_path: false}), sexp, yojson)]
type error_exp =
  | BadToken(Token.t)
  | MultiHole
  | NoJoin(ana_exp, list(ana_exp)) // expected, actuals
  | FreeVar
  | NotVar
  | VarNoJoin(ana_exp, Typ.t) // expected, actual
  | TupleNotStandard
  | CaseNotStandard;

[@deriving (show({with_path: false}), sexp, yojson)]
type status_exp =
  | NotInHole
  | InHole(error_exp);

[@deriving (show({with_path: false}), sexp, yojson)]
type exp = {
  term: Drv.Exp.t,
  cls: Cls.t,
  ancestors,
  status: status_exp,
};

// PAT

[@deriving (show({with_path: false}), sexp, yojson)]
type ana_pat =
  | Var
  | Cast_Var
  | Pair_Or_Case_Var
  | InjL
  | InjR;

[@deriving (show({with_path: false}), sexp, yojson)]
type error_pat =
  | BadToken(Token.t)
  | MultiHole
  | NoJoin(ana_pat, list(ana_pat)); // expected, actuals

[@deriving (show({with_path: false}), sexp, yojson)]
type status_pat =
  | NotInHole
  | InHole(error_pat);

[@deriving (show({with_path: false}), sexp, yojson)]
type pat = {
  term: Drv.Pat.t,
  cls: Cls.t,
  ancestors,
  status: status_pat,
};

// TYP

[@deriving (show({with_path: false}), sexp, yojson)]
type error_typ =
  | BadToken(Token.t)
  | MultiHole
  | FreeVar
  | NotVar
  | VarNoJoin(Typ.t); // actual

[@deriving (show({with_path: false}), sexp, yojson)]
type status_typ =
  | NotInHole
  | InHole(error_typ);

[@deriving (show({with_path: false}), sexp, yojson)]
type typ = {
  term: Drv.Typ.t,
  cls: Cls.t,
  ancestors,
  status: status_typ,
};

// TPAT

[@deriving (show({with_path: false}), sexp, yojson)]
type error_tpat =
  | BadToken(Token.t)
  | MultiHole;

[@deriving (show({with_path: false}), sexp, yojson)]
type status_tpat =
  | NotInHole
  | InHole(error_tpat);

[@deriving (show({with_path: false}), sexp, yojson)]
type tpat = {
  term: Drv.TPat.t,
  cls: Cls.t,
  ancestors,
  status: status_tpat,
};

// DRV

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Exp(exp)
  | Pat(pat)
  | Typ(typ)
  | TPat(tpat);

[@deriving (show({with_path: false}), sexp, yojson)]
type error =
  | Exp(error_exp)
  | Pat(error_pat)
  | Typ(error_typ)
  | TPat(error_tpat);

let sort_of: t => DrvSort.t =
  fun
  | Exp(_) => Exp
  | Pat(_) => Pat
  | Typ(_) => Typ
  | TPat(_) => TPat;

let cls_of: t => Cls.t =
  fun
  | Exp(exp) => exp.cls
  | Pat(pat) => pat.cls
  | Typ(typ) => typ.cls
  | TPat(tpat) => tpat.cls;

let id_of: t => Id.t =
  fun
  | Exp(exp) => Drv.Exp.rep_id(exp.term)
  | Pat(pat) => Drv.Pat.rep_id(pat.term)
  | Typ(typ) => Drv.Typ.rep_id(typ.term)
  | TPat(tpat) => Drv.TPat.rep_id(tpat.term);

let error_of: t => option(error) =
  fun
  | Exp({status: NotInHole, _})
  | Pat({status: NotInHole, _})
  | Typ({status: NotInHole, _})
  | TPat({status: NotInHole, _}) => None
  | Exp({status: InHole(err), _}) => Some(Exp(err))
  | Pat({status: InHole(err), _}) => Some(Pat(err))
  | Typ({status: InHole(err), _}) => Some(Typ(err))
  | TPat({status: InHole(err), _}) => Some(TPat(err));

[@deriving (show({with_path: false}), sexp, yojson)]
type status_drv =
  | Exp(status_exp)
  | Pat(status_pat)
  | Typ(status_typ)
  | TPat(status_tpat);

let anas_of_exp = (exp: Drv.Exp.t) =>
  switch (exp.term) {
  | Hole(_)
  | Abbr(_)
  | Parens(_) => [Jdmt, Ctx, Prop, Exp]
  | Var(_) => [Prop, Exp]
  | Val(_)
  | Eval(_)
  | Entail(_)
  | Consistent(_)
  | MatchedArrow(_)
  | MatchedProd(_)
  | MatchedSum(_) => [Jdmt]
  | Ctx(_)
  | Cons(_)
  | Concat(_) => [Ctx]
  | Type(_)
  | HasType(_)
  | Syn(_)
  | Ana(_)
  | And(_)
  | Or(_)
  | Impl(_)
  | Truth
  | Falsity => [Prop]
  | NumLit(_)
  | Neg(_)
  | Plus(_)
  | Minus(_)
  | Times(_)
  | Eq(_)
  | Lt(_)
  | Gt(_)
  | True
  | False
  | If(_)
  | Let(_)
  | Fix(_)
  | Fun(_)
  | Ap(_)
  | Triv
  | Tuple(_)
  | PrjL(_)
  | PrjR(_)
  | Case(_)
  | ExpHole
  | InjL(_)
  | InjR(_)
  | Roll(_)
  | Unroll(_) => [Exp]
  };

let status_exp = (exp: Drv.Exp.t, ~ana: ana_exp, ~is_var: bool): status_exp =>
  switch (exp.term) {
  | Hole(Invalid(token)) => InHole(BadToken(token))
  | Hole(MultiHole(_)) => InHole(MultiHole)
  | Hole(EmptyHole) => NotInHole
  | Tuple(es) when List.length(es) != 2 => InHole(TupleNotStandard)
  | Case(_, rs) when List.length(rs) != 2 => InHole(CaseNotStandard)
  | Var(_) when is_var => NotInHole
  | _ when is_var => InHole(NotVar)
  | _ when List.mem(ana, anas_of_exp(exp)) => NotInHole
  | _ => InHole(NoJoin(ana, anas_of_exp(exp)))
  };

let anas_of_pat = (pat: Drv.Pat.t) =>
  switch (pat.term) {
  | Hole(_)
  | Parens(_) => [Var, Cast_Var, Pair_Or_Case_Var, InjL, InjR]
  | Var(_) => [Var, Cast_Var, Pair_Or_Case_Var]
  | Cast(_) => [Cast_Var, Pair_Or_Case_Var]
  | Pair(_) => [Pair_Or_Case_Var]
  | InjL(_) => [InjL]
  | InjR(_) => [InjR]
  };

let status_pat = (pat: Drv.Pat.t, ~ana: ana_pat): status_pat =>
  switch (pat.term) {
  | Hole(Invalid(token)) => InHole(BadToken(token))
  | Hole(MultiHole(_)) => InHole(MultiHole)
  | Hole(EmptyHole) => NotInHole
  | _ when List.mem(ana, anas_of_pat(pat)) => NotInHole
  | _ => InHole(NoJoin(ana, anas_of_pat(pat)))
  };

let status_typ = (typ: Drv.Typ.t, ~is_var: bool): status_typ =>
  switch (typ.term) {
  | Hole(Invalid(token)) => InHole(BadToken(token))
  | Hole(MultiHole(_)) => InHole(MultiHole)
  | Var(_) when is_var => NotInHole
  | _ when is_var => InHole(NotVar)
  | _ => NotInHole
  };

let status_tpat = (tpat: Drv.TPat.t): status_tpat =>
  switch (tpat.term) {
  | Hole(Invalid(token)) => InHole(BadToken(token))
  | Hole(MultiHole(_)) => InHole(MultiHole)
  | _ => NotInHole
  };

let is_error = (ci: t): bool => {
  switch (ci) {
  | Exp({status: InHole(_), _})
  | Pat({status: InHole(_), _})
  | Typ({status: InHole(_), _})
  | TPat({status: InHole(_), _}) => true
  | Exp({status: NotInHole, _})
  | Pat({status: NotInHole, _})
  | Typ({status: NotInHole, _})
  | TPat({status: NotInHole, _}) => false
  };
};

let ancestors_of: t => ancestors =
  fun
  | Exp({ancestors, _})
  | Pat({ancestors, _})
  | Typ({ancestors, _})
  | TPat({ancestors, _}) => ancestors;

let derived_exp = (exp: Drv.Exp.t, ~ancestors, ~ana, ~is_var): exp => {
  let cls = Cls.Drv(Exp(Drv.Exp.cls_of_term(exp.term)));
  let status = status_exp(exp, ~ana, ~is_var);
  {term: exp, cls, status, ancestors};
};

let derived_pat = (pat: Drv.Pat.t, ~ancestors, ~ana): pat => {
  let cls = Cls.Drv(Pat(Drv.Pat.cls_of_term(pat.term)));
  let status = status_pat(pat, ~ana);
  {term: pat, cls, status, ancestors};
};

let derived_typ = (typ: Drv.Typ.t, ~ancestors, ~is_var): typ => {
  let cls = Cls.Drv(Typ(Drv.Typ.cls_of_term(typ.term)));
  let status = status_typ(typ, ~is_var);
  {term: typ, cls, status, ancestors};
};

let derived_tpat = (tpat: Drv.TPat.t, ~ancestors): tpat => {
  let cls = Cls.Drv(TPat(Drv.TPat.cls_of_term(tpat.term)));
  let status = status_tpat(tpat);
  {term: tpat, cls, status, ancestors};
};

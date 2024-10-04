open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type ancestors = list(Id.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type ok_common = unit;

[@deriving (show({with_path: false}), sexp, yojson)]
type error_common =
  | BadToken(Token.t)
  | MultiHole;

[@deriving (show({with_path: false}), sexp, yojson)]
type status_common =
  | NotInHole(ok_common)
  | InHole(error_common);

[@deriving (show({with_path: false}), sexp, yojson)]
type ty_merged =
  | Jdmt
  | Ctx
  | Prop
  | Exp
  | Arrow;

[@deriving (show({with_path: false}), sexp, yojson)]
type error_exp =
  | BadToken(Token.t)
  | MultiHole
  | NoJoin(ty_merged); // expected

[@deriving (show({with_path: false}), sexp, yojson)]
type status_exp =
  | NotInHole(ok_common)
  | InHole(error_exp);

[@deriving (show({with_path: false}), sexp, yojson)]
type exp = {
  term: Drv.Exp.t,
  cls: Cls.t,
  ancestors,
  status: status_exp,
  ty: ty_merged,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type pat_expect =
  | Any
  | Var
  | Cast_Var
  | Pair_Or_Case_Var
  | Ap_InjL
  | Ap_InjR
  | InjL
  | InjR;

[@deriving (show({with_path: false}), sexp, yojson)]
type error_pat =
  | BadToken(Token.t)
  | MultiHole
  | Expect(pat_expect);

[@deriving (show({with_path: false}), sexp, yojson)]
type status_pat =
  | NotInHole(ok_common)
  | InHole(error_pat);

[@deriving (show({with_path: false}), sexp, yojson)]
type pat = {
  term: Drv.Pat.t,
  cls: Cls.t,
  ancestors,
  status: status_pat,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type typ = {
  term: Drv.Typ.t,
  cls: Cls.t,
  ancestors,
  status: status_common,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type tpat = {
  term: Drv.TPat.t,
  cls: Cls.t,
  ancestors,
  status: status_common,
};

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
  | Typ(error_common)
  | TPat(error_common);

let sort_of: t => Sort.DrvSort.t =
  fun
  | Exp({ty: Jdmt, _}) => Jdmt
  | Exp({ty: Ctx, _}) => Ctx
  | Exp({ty: Prop, _}) => Prop
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
  | Exp({status: NotInHole(_), _})
  | Pat({status: NotInHole(_), _})
  | Typ({status: NotInHole(_), _})
  | TPat({status: NotInHole(_), _}) => None
  | Exp({status: InHole(err), _}) => Some(Exp(err))
  | Pat({status: InHole(err), _}) => Some(Pat(err))
  | Typ({status: InHole(err), _}) => Some(Typ(err))
  | TPat({status: InHole(err), _}) => Some(TPat(err));

[@deriving (show({with_path: false}), sexp, yojson)]
type status_drv =
  | Exp(status_exp)
  | Pat(status_pat)
  | Typ(status_common)
  | TPat(status_common);

let types_of_exp = (exp: Drv.Exp.t): list(ty_merged) =>
  switch (exp.term) {
  | Hole(_)
  | Abbr(_)
  | Parens(_) => [Jdmt, Ctx, Prop, Exp, Arrow]
  | Var(_) => [Prop, Exp, Arrow]
  | Tuple(es) when List.length(es) == 2 => [Exp]
  | Tuple(_) => []
  | Val(_)
  | Eval(_)
  | Entail(_) => [Jdmt]
  | Ctx(_)
  | Cons(_)
  | Concat(_) => [Ctx]
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
  | PrjL(_)
  | PrjR(_)
  | Case(_) => [Exp, Arrow]
  | InjL
  | InjR
  | Roll
  | Unroll => [Arrow]
  };

let status_exp = (exp: Drv.Exp.t, ty: ty_merged): status_exp =>
  switch (exp.term) {
  | Hole(Invalid(token)) => InHole(BadToken(token))
  | Hole(MultiHole(_)) => InHole(MultiHole)
  | _ when !List.mem(ty, types_of_exp(exp)) => InHole(NoJoin(ty))
  | _ => NotInHole()
  };

let status_pat = (pat: Drv.Pat.t): status_pat =>
  switch (pat.term) {
  | Hole(Invalid(token)) => InHole(BadToken(token))
  | Hole(MultiHole(_)) => InHole(MultiHole)
  | _ => NotInHole()
  };

let status_typ = (typ: Drv.Typ.t): status_common =>
  switch (typ.term) {
  | Hole(Invalid(token)) => InHole(BadToken(token))
  | Hole(MultiHole(_)) => InHole(MultiHole)
  | _ => NotInHole()
  };

let status_tpat = (tpat: Drv.TPat.t): status_common =>
  switch (tpat.term) {
  | Hole(Invalid(token)) => InHole(BadToken(token))
  | Hole(MultiHole(_)) => InHole(MultiHole)
  | _ => NotInHole()
  };

let is_error = (ci: t): bool => {
  switch (ci) {
  | Exp({status: InHole(_), _})
  | Pat({status: InHole(_), _})
  | Typ({status: InHole(_), _})
  | TPat({status: InHole(_), _}) => true
  | Exp({status: NotInHole(_), _})
  | Pat({status: NotInHole(_), _})
  | Typ({status: NotInHole(_), _})
  | TPat({status: NotInHole(_), _}) => false
  };
};

let ancestors_of: t => ancestors =
  fun
  | Exp({ancestors, _})
  | Pat({ancestors, _})
  | Typ({ancestors, _})
  | TPat({ancestors, _}) => ancestors;

let derived_exp = (exp: Drv.Exp.t, ~ancestors, ~ty): exp => {
  let cls = Cls.Drv(Exp(Drv.Exp.cls_of_term(exp.term)));
  let status = status_exp(exp, ty);
  {term: exp, cls, status, ancestors, ty};
};

let derived_pat = (pat: Drv.Pat.t, ~ancestors): pat => {
  let cls = Cls.Drv(Pat(Drv.Pat.cls_of_term(pat.term)));
  let status = status_pat(pat);
  {term: pat, cls, status, ancestors};
};

let derived_typ = (typ: Drv.Typ.t, ~ancestors): typ => {
  let cls = Cls.Drv(Typ(Drv.Typ.cls_of_term(typ.term)));
  let status = status_typ(typ);
  {term: typ, cls, status, ancestors};
};

let derived_tpat = (tpat: Drv.TPat.t, ~ancestors): tpat => {
  let cls = Cls.Drv(TPat(Drv.TPat.cls_of_term(tpat.term)));
  let status = status_tpat(tpat);
  {term: tpat, cls, status, ancestors};
};

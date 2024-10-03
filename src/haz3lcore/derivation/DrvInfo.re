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
type jdmt = {
  term: Drv.Jdmt.t,
  cls: Cls.t,
  ancestors,
  status: status_common,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type ctxt = {
  term: Drv.Ctxt.t,
  cls: Cls.t,
  ancestors,
  status: status_common,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type error_prop =
  | BadToken(Token.t)
  | MultiHole
  | NotAllowTuple;

[@deriving (show({with_path: false}), sexp, yojson)]
type status_prop =
  | NotInHole(ok_common)
  | InHole(error_prop);

[@deriving (show({with_path: false}), sexp, yojson)]
type prop = {
  term: Drv.Prop.t,
  cls: Cls.t,
  ancestors,
  status: status_prop,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type error_exp =
  | BadToken(Token.t)
  | MultiHole
  | NotAllowSingle;

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
  | Jdmt(jdmt)
  | Ctxt(ctxt)
  | Prop(prop)
  | Exp(exp)
  | Pat(pat)
  | Typ(typ)
  | TPat(tpat);

[@deriving (show({with_path: false}), sexp, yojson)]
type error =
  | Jdmt(error_common)
  | Ctxt(error_common)
  | Prop(error_prop)
  | Exp(error_exp)
  | Pat(error_pat)
  | Typ(error_common)
  | TPat(error_common);

let sort_of: t => Sort.DrvSort.t =
  fun
  | Jdmt(_) => Jdmt
  | Ctxt(_) => Ctxt
  | Prop(_) => Prop
  | Exp(_) => Exp
  | Pat(_) => Pat
  | Typ(_) => Typ
  | TPat(_) => TPat;

let cls_of: t => Cls.t =
  fun
  | Jdmt(jdmt) => jdmt.cls
  | Ctxt(ctxt) => ctxt.cls
  | Prop(prop) => prop.cls
  | Exp(exp) => exp.cls
  | Pat(pat) => pat.cls
  | Typ(typ) => typ.cls
  | TPat(tpat) => tpat.cls;

let id_of: t => Id.t =
  fun
  | Jdmt(jdmt) => Drv.Jdmt.rep_id(jdmt.term)
  | Ctxt(ctxt) => Drv.Ctxt.rep_id(ctxt.term)
  | Prop(prop) => Drv.Prop.rep_id(prop.term)
  | Exp(exp) => Drv.Exp.rep_id(exp.term)
  | Pat(pat) => Drv.Pat.rep_id(pat.term)
  | Typ(typ) => Drv.Typ.rep_id(typ.term)
  | TPat(tpat) => Drv.TPat.rep_id(tpat.term);

let error_of: t => option(error) =
  fun
  | Jdmt({status: NotInHole(_), _})
  | Ctxt({status: NotInHole(_), _})
  | Prop({status: NotInHole(_), _})
  | Exp({status: NotInHole(_), _})
  | Pat({status: NotInHole(_), _})
  | Typ({status: NotInHole(_), _})
  | TPat({status: NotInHole(_), _}) => None
  | Jdmt({status: InHole(err), _}) => Some(Jdmt(err))
  | Ctxt({status: InHole(err), _}) => Some(Ctxt(err))
  | Prop({status: InHole(err), _}) => Some(Prop(err))
  | Exp({status: InHole(err), _}) => Some(Exp(err))
  | Pat({status: InHole(err), _}) => Some(Pat(err))
  | Typ({status: InHole(err), _}) => Some(Typ(err))
  | TPat({status: InHole(err), _}) => Some(TPat(err));

[@deriving (show({with_path: false}), sexp, yojson)]
type status_drv =
  | Jdmt(status_common)
  | Ctxt(status_common)
  | Prop(status_prop)
  | Exp(status_exp)
  | Pat(status_pat)
  | Typ(status_common)
  | TPat(status_common);

let status_jdmt = (jdmt: Drv.Jdmt.t): status_common =>
  switch (jdmt.term) {
  | Hole(Invalid(token)) => InHole(BadToken(token))
  | Hole(MultiHole(_)) => InHole(MultiHole)
  | _ => NotInHole()
  };

let status_ctxt = (ctxt: Drv.Ctxt.t): status_common =>
  switch (ctxt.term) {
  | Hole(Invalid(token)) => InHole(BadToken(token))
  | Hole(MultiHole(_)) => InHole(MultiHole)
  | _ => NotInHole()
  };

let status_prop = (prop: Drv.Prop.t): status_prop =>
  switch (prop.term) {
  | Hole(Invalid(token)) => InHole(BadToken(token))
  | Hole(MultiHole(_)) => InHole(MultiHole)
  | _ => NotInHole()
  };

let status_exp = (exp: Drv.Exp.t): status_exp =>
  switch (exp.term) {
  | Hole(Invalid(token)) => InHole(BadToken(token))
  | Hole(MultiHole(_)) => InHole(MultiHole)
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

let status_drv = (drv: Drv.t): status_drv =>
  switch (drv) {
  | Jdmt(term) => Jdmt(status_jdmt(term))
  | Ctxt(term) => Ctxt(status_ctxt(term))
  | Prop(term) => Prop(status_prop(term))
  | Exp(term) => Exp(status_exp(term))
  | Pat(term) => Pat(status_pat(term))
  | Typ(term) => Typ(status_typ(term))
  | TPat(term) => TPat(status_tpat(term))
  };

let is_error = (ci: t): bool => {
  switch (ci) {
  | Jdmt({status: InHole(_), _})
  | Ctxt({status: InHole(_), _})
  | Prop({status: InHole(_), _})
  | Exp({status: InHole(_), _})
  | Pat({status: InHole(_), _})
  | Typ({status: InHole(_), _})
  | TPat({status: InHole(_), _}) => true
  | Jdmt({status: NotInHole(_), _})
  | Ctxt({status: NotInHole(_), _})
  | Prop({status: NotInHole(_), _})
  | Exp({status: NotInHole(_), _})
  | Pat({status: NotInHole(_), _})
  | Typ({status: NotInHole(_), _})
  | TPat({status: NotInHole(_), _}) => false
  };
};

let ancestors_of: t => ancestors =
  fun
  | Jdmt({ancestors, _})
  | Ctxt({ancestors, _})
  | Prop({ancestors, _})
  | Exp({ancestors, _})
  | Pat({ancestors, _})
  | Typ({ancestors, _})
  | TPat({ancestors, _}) => ancestors;

let derived_jdmt = (jdmt: Drv.Jdmt.t, ~ancestors): jdmt => {
  let cls = Cls.Drv(Jdmt(Drv.Jdmt.cls_of_term(jdmt.term)));
  let status = status_jdmt(jdmt);
  {term: jdmt, cls, status, ancestors};
};

let derived_ctxt = (ctxt: Drv.Ctxt.t, ~ancestors): ctxt => {
  let cls = Cls.Drv(Ctxt(Drv.Ctxt.cls_of_term(ctxt.term)));
  let status = status_ctxt(ctxt);
  {term: ctxt, cls, status, ancestors};
};

let derived_prop = (prop: Drv.Prop.t, ~ancestors): prop => {
  let cls = Cls.Drv(Prop(Drv.Prop.cls_of_term(prop.term)));
  let status = status_prop(prop);
  {term: prop, cls, status, ancestors};
};

let derived_exp = (exp: Drv.Exp.t, ~ancestors): exp => {
  let cls = Cls.Drv(Exp(Drv.Exp.cls_of_term(exp.term)));
  let status = status_exp(exp);
  {term: exp, cls, status, ancestors};
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

let derived_drv = (drv: Drv.t, ~ancestors): t => {
  switch (drv) {
  | Jdmt(jdmt) => Jdmt(derived_jdmt(jdmt, ~ancestors))
  | Ctxt(ctxt) => Ctxt(derived_ctxt(ctxt, ~ancestors))
  | Prop(prop) => Prop(derived_prop(prop, ~ancestors))
  | Exp(exp) => Exp(derived_exp(exp, ~ancestors))
  | Pat(pat) => Pat(derived_pat(pat, ~ancestors))
  | Typ(typ) => Typ(derived_typ(typ, ~ancestors))
  | TPat(tpat) => TPat(derived_tpat(tpat, ~ancestors))
  };
};

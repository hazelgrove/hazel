open Sexplib.Std;
open Util;
open OptUtil.Syntax;
open Term;

/* INFO.re

   This module defines the cursor INFO data structure, which is used
   to represent the static information associated with a term in the
   AST. This includes the term itself, and information related to
   typing and syntax, included erroneous states.

   Each term is assigned a STATUS, which is directly used to determine
   the message displayed to the user in CursorInspector.re. Each sort
   has its own status datatype, which is divided into OK states (not
   in error holes) and ERROR states (in error holes).

   Regardless of errors, every expression & pattern term is ultimately
   assigned a FIXED TYPE, which is the type of the term after hole
   fixing; that is, all otherwise ill-typed terms are considered to
   be 'wrapped in non-empty holes', i.e. assigned an Unknown type.

   Fixed types are determined by reconcilling two sources of type
   information: the SELF (Self.re), representing the type information
   derivable from a term in isolation, and the MODE (Mode.re),
   representing the expected type information imposed by the surrounding
   syntactic context. A successful reconcilliation results in an OK
   status; otherwise, an ERROR status, but in both cases, a fixed type
   is determined.

   */

/* The ids of a term's ancestors in the AST */
[@deriving (show({with_path: false}), sexp, yojson)]
type ancestors = list(Id.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type error_inconsistent =
  /* Self type (syn) inconsistent with expected type (ana) */
  | Expectation({
      ana: Typ.t,
      syn: Typ.t,
    })
  /* Inconsistent match or listlit */
  | Internal(list(Typ.t))
  /* Bad function position */
  | WithArrow(Typ.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type error_no_type =
  /* Invalid expression token, treated as hole */
  | BadToken(Token.t)
  /* Empty application of function with inconsistent type */
  | BadTrivAp(Typ.t)
  /* Sum constructor neiter bound nor in ana type */
  | FreeConstructor(Constructor.t);

/* Errors which can apply to either expression or patterns */
[@deriving (show({with_path: false}), sexp, yojson)]
type error_common =
  /* Underdetermined: No type can be assigned */
  | NoType(error_no_type)
  /* Overdetermined: Conflicting type expectations */
  | Inconsistent(error_inconsistent);

[@deriving (show({with_path: false}), sexp, yojson)]
type error_exp =
  | FreeVariable(Var.t) /* Unbound variable (not in typing context) */
  | InexhaustiveMatch(option(error_common))
  | Common(error_common);

[@deriving (show({with_path: false}), sexp, yojson)]
type error_pat =
  | ExpectedConstructor /* Only construtors can be applied */
  | Common(error_common);

[@deriving (show({with_path: false}), sexp, yojson)]
type ok_ana =
  /* The expected (ana) type and the self (syn) type are
     consistent, as witnessed by their joint type (join) */
  | Consistent({
      ana: Typ.t,
      syn: Typ.t,
      join: Typ.t,
    })
  /* A match expression or list literal which, in synthetic position,
     would be marked as internally inconsistent, but is considered
     fine as the expected type provides a consistent lower bound
     (often Unknown) for the types of the branches/elements */
  | InternallyInconsistent({
      ana: Typ.t,
      nojoin: list(Typ.t),
    });

[@deriving (show({with_path: false}), sexp, yojson)]
type ok_common =
  | Syn(Typ.t)
  | Ana(ok_ana);

[@deriving (show({with_path: false}), sexp, yojson)]
type ok_exp = ok_common;

[@deriving (show({with_path: false}), sexp, yojson)]
type ok_pat = ok_common;

[@deriving (show({with_path: false}), sexp, yojson)]
type status_common =
  | InHole(error_common)
  | NotInHole(ok_common);

[@deriving (show({with_path: false}), sexp, yojson)]
type status_exp =
  | InHole(error_exp)
  | NotInHole(ok_exp);

[@deriving (show({with_path: false}), sexp, yojson)]
type status_pat =
  | InHole(error_pat)
  | NotInHole(ok_pat);

[@deriving (show({with_path: false}), sexp, yojson)]
type status_variant =
  | Unique
  | Duplicate;

/* Expectation imposed on a type by the parent form.
   TODO: This is fundamentally syntactic and should
   eventually be reimplemeted via a seperate sort */
[@deriving (show({with_path: false}), sexp, yojson)]
type typ_expects =
  | TypeExpected
  | ConstructorExpected(status_variant, Typ.t)
  | VariantExpected(status_variant, Typ.t);

/* Type term errors
   TODO: The three additional errors statuses
   are fundamentally syntactic and should when
   possible be reimplemeted via a seperate sort */
[@deriving (show({with_path: false}), sexp, yojson)]
type error_typ =
  | BadToken(Token.t) /* Invalid token, treated as type hole */
  | FreeTypeVariable(TypVar.t) /* Free type variable */
  | DuplicateConstructor(Constructor.t) /* Duplicate ctr in same sum */
  | WantTypeFoundAp
  | WantConstructorFoundType(Typ.t)
  | WantConstructorFoundAp;

/* Type ok statuses for cursor inspector */
[@deriving (show({with_path: false}), sexp, yojson)]
type ok_typ =
  | Variant(Constructor.t, Typ.t)
  | VariantIncomplete(Typ.t)
  | TypeAlias(TypVar.t, Typ.t)
  | Type(Typ.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type status_typ =
  | InHole(error_typ)
  | NotInHole(ok_typ);

[@deriving (show({with_path: false}), sexp, yojson)]
type type_var_err =
  | Other
  | NotCapitalized;

/* Type pattern term errors */
[@deriving (show({with_path: false}), sexp, yojson)]
type error_tpat =
  | ShadowsType(TypVar.t)
  | NotAVar(type_var_err);

/* Type pattern ok statuses for cursor inspector */
[@deriving (show({with_path: false}), sexp, yojson)]
type ok_tpat =
  | Empty
  | Var(TypVar.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type status_tpat =
  | NotInHole(ok_tpat)
  | InHole(error_tpat);

[@deriving (show({with_path: false}), sexp, yojson)]
type exp = {
  term: UExp.t, /* The term under consideration */
  ancestors, /* Ascending list of containing term ids */
  ctx: Ctx.t, /* Typing context for the term */
  mode: Mode.t, /* Parental type expectations  */
  self: Self.exp, /* Expectation-independent type info */
  co_ctx: CoCtx.t, /* Locally free variables */
  cls: Term.Cls.t, /* DERIVED: Syntax class (i.e. form name) */
  status: status_exp, /* DERIVED: Ok/Error statuses for display */
  ty: Typ.t /* DERIVED: Type after nonempty hole fixing */
};

[@deriving (show({with_path: false}), sexp, yojson)]
type pat = {
  term: UPat.t,
  ancestors,
  ctx: Ctx.t,
  co_ctx: CoCtx.t,
  mode: Mode.t,
  self: Self.pat,
  cls: Term.Cls.t,
  status: status_pat,
  ty: Typ.t,
  constraint_: Constraint.t,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type typ = {
  term: UTyp.t,
  ancestors,
  ctx: Ctx.t,
  expects: typ_expects,
  cls: Term.Cls.t,
  status: status_typ,
  ty: Typ.t,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type tpat = {
  term: UTPat.t,
  ancestors,
  ctx: Ctx.t,
  cls: Term.Cls.t,
  status: status_tpat,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type secondary = {
  id: Id.t, // Id of term static info is sourced from
  cls: Term.Cls.t, // Cls of secondary, not source term
  sort: Sort.t, // from source term
  ctx: Ctx.t // from source term
};

/* The static information collated for each term */
[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | InfoExp(exp)
  | InfoPat(pat)
  | InfoTyp(typ)
  | InfoTPat(tpat)
  | Secondary(secondary);

[@deriving (show({with_path: false}), sexp, yojson)]
type error =
  | Exp(error_exp)
  | Pat(error_pat)
  | Typ(error_typ)
  | TPat(error_tpat);

let sort_of: t => Sort.t =
  fun
  | InfoExp(_) => Exp
  | InfoPat(_) => Pat
  | InfoTyp(_) => Typ
  | InfoTPat(_) => TPat
  | Secondary(s) => s.sort;

let cls_of: t => Cls.t =
  fun
  | InfoExp({cls, _})
  | InfoPat({cls, _})
  | InfoTyp({cls, _})
  | InfoTPat({cls, _})
  | Secondary({cls, _}) => cls;

let ctx_of: t => Ctx.t =
  fun
  | InfoExp({ctx, _})
  | InfoPat({ctx, _})
  | InfoTyp({ctx, _})
  | InfoTPat({ctx, _})
  | Secondary({ctx, _}) => ctx;

let ancestors_of: t => ancestors =
  fun
  | InfoExp({ancestors, _})
  | InfoPat({ancestors, _})
  | InfoTyp({ancestors, _})
  | InfoTPat({ancestors, _}) => ancestors
  | Secondary(_) => []; //TODO

let id_of: t => Id.t =
  fun
  | InfoExp(i) => Term.UExp.rep_id(i.term)
  | InfoPat(i) => Term.UPat.rep_id(i.term)
  | InfoTyp(i) => Term.UTyp.rep_id(i.term)
  | InfoTPat(i) => Term.UTPat.rep_id(i.term)
  | Secondary(s) => s.id;

let error_of: t => option(error) =
  fun
  | InfoExp({status: NotInHole(_), _})
  | InfoPat({status: NotInHole(_), _})
  | InfoTyp({status: NotInHole(_), _})
  | InfoTPat({status: NotInHole(_), _}) => None
  | InfoExp({status: InHole(err), _}) => Some(Exp(err))
  | InfoPat({status: InHole(err), _}) => Some(Pat(err))
  | InfoTyp({status: InHole(err), _}) => Some(Typ(err))
  | InfoTPat({status: InHole(err), _}) => Some(TPat(err))
  | Secondary(_) => None;

let exp_co_ctx: exp => CoCtx.t = ({co_ctx, _}) => co_ctx;
let exp_ty: exp => Typ.t = ({ty, _}) => ty;
let pat_ctx: pat => Ctx.t = ({ctx, _}) => ctx;
let pat_ty: pat => Typ.t = ({ty, _}) => ty;
let pat_constraint: pat => Constraint.t = ({constraint_, _}) => constraint_;

let rec status_common =
        (ctx: Ctx.t, mode: Mode.t, self: Self.t): status_common =>
  switch (self, mode) {
  | (Just(syn), Syn) => NotInHole(Syn(syn))
  | (Just(syn), Ana(ana)) =>
    switch (Typ.join_fix(ctx, ana, syn)) {
    | None => InHole(Inconsistent(Expectation({syn, ana})))
    | Some(join) => NotInHole(Ana(Consistent({ana, syn, join})))
    }
  | (Just(syn), SynFun) =>
    switch (
      Typ.join_fix(ctx, Arrow(Unknown(Internal), Unknown(Internal)), syn)
    ) {
    | None => InHole(Inconsistent(WithArrow(syn)))
    | Some(_) => NotInHole(Syn(syn))
    }
  | (IsConstructor({name, syn_ty}), _) =>
    /* If a ctr is being analyzed against (an arrow type returning)
       a sum type having that ctr as a variant, its self type is
       considered to be determined by the sum type; otherwise,
       check the context for the ctr's type */
    switch (Mode.ctr_ana_typ(ctx, mode, name), syn_ty) {
    | (Some(ana_ty), _) => status_common(ctx, mode, Just(ana_ty))
    | (_, Some(syn_ty)) => status_common(ctx, mode, Just(syn_ty))
    | _ => InHole(NoType(FreeConstructor(name)))
    }
  | (BadToken(name), _) => InHole(NoType(BadToken(name)))
  | (BadTrivAp(ty), _) => InHole(NoType(BadTrivAp(ty)))
  | (IsMulti, _) => NotInHole(Syn(Unknown(Internal)))
  | (NoJoin(wrap, tys), Ana(ana)) =>
    let syn: Typ.t = Self.join_of(wrap, Unknown(Internal));
    switch (Typ.join_fix(ctx, ana, syn)) {
    | None => InHole(Inconsistent(Expectation({ana, syn})))
    | Some(_) =>
      NotInHole(
        Ana(InternallyInconsistent({ana, nojoin: Typ.of_source(tys)})),
      )
    };
  | (NoJoin(_, tys), Syn | SynFun) =>
    InHole(Inconsistent(Internal(Typ.of_source(tys))))
  };

let status_pat = (ctx: Ctx.t, mode: Mode.t, self: Self.pat): status_pat =>
  switch (mode, self) {
  | (Syn | Ana(_), Common(self_pat))
  | (SynFun, Common(IsConstructor(_) as self_pat)) =>
    /* Little bit of a hack. Anything other than a bound ctr will, in
       function position, have SynFun mode (see Typ.ap_mode). Since we
       are prohibiting non-ctrs in ctr applications in patterns for now,
       we catch them here, diverting to an ExpectedConstructor error. But we
       avoid capturing the second case above, as these will ultimately
       get a (more precise) unbound ctr  via status_common */
    switch (status_common(ctx, mode, self_pat)) {
    | NotInHole(ok_exp) => NotInHole(ok_exp)
    | InHole(err_pat) => InHole(Common(err_pat))
    }
  | (SynFun, _) => InHole(ExpectedConstructor)
  };

/* Determines whether an expression or pattern is in an error hole,
   depending on the mode, which represents the expectations of the
   surrounding syntactic context, and the self which represents the
   makeup of the expression / pattern itself. */
let rec status_exp = (ctx: Ctx.t, mode: Mode.t, self: Self.exp): status_exp =>
  switch (self, mode) {
  | (Free(name), _) => InHole(FreeVariable(name))
  | (InexhaustiveMatch(self), _) =>
    let additional_err =
      switch (status_exp(ctx, mode, self)) {
      | InHole(Common(Inconsistent(Internal(_)) as inconsistent_err)) =>
        Some(inconsistent_err)
      | NotInHole(_)
      | InHole(Common(Inconsistent(Expectation(_) | WithArrow(_)))) => None /* Type checking should fail and these errors would be nullified */
      | InHole(Common(NoType(_)))
      | InHole(FreeVariable(_) | InexhaustiveMatch(_)) =>
        failwith("InHole(InexhaustiveMatch(impossible_err))")
      };
    InHole(InexhaustiveMatch(additional_err));
  | (Common(self_pat), _) =>
    switch (status_common(ctx, mode, self_pat)) {
    | NotInHole(ok_exp) => NotInHole(ok_exp)
    | InHole(err_pat) => InHole(Common(err_pat))
    }
  };

/* This logic determines whether a type should be put
   in a hole or not. It's mostly syntactic, determining
   the proper placement of sum type variants and ctrs;
   this should be reimplemented in the future as a
   separate sort. It also determines semantic properties
   such as whether or not a type variable reference is
   free, and whether a ctr name is a dupe. */
let status_typ =
    (ctx: Ctx.t, expects: typ_expects, term: TermBase.UTyp.t, ty: Typ.t)
    : status_typ =>
  switch (term.term) {
  | Invalid(token) => InHole(BadToken(token))
  | EmptyHole => NotInHole(Type(ty))
  | Var(name)
  | Constructor(name) =>
    switch (expects) {
    | VariantExpected(Unique, sum_ty)
    | ConstructorExpected(Unique, sum_ty) =>
      NotInHole(Variant(name, sum_ty))
    | VariantExpected(Duplicate, _)
    | ConstructorExpected(Duplicate, _) =>
      InHole(DuplicateConstructor(name))
    | TypeExpected =>
      switch (Ctx.is_alias(ctx, name)) {
      | false => InHole(FreeTypeVariable(name))
      | true => NotInHole(TypeAlias(name, Typ.weak_head_normalize(ctx, ty)))
      }
    }
  | Ap(t1, t2) =>
    switch (expects) {
    | VariantExpected(status_variant, ty_variant) =>
      let ty_in = UTyp.to_typ(ctx, t2);
      switch (status_variant, t1.term) {
      | (Unique, Var(name) | Constructor(name)) =>
        NotInHole(Variant(name, Arrow(ty_in, ty_variant)))
      | _ => NotInHole(VariantIncomplete(Arrow(ty_in, ty_variant)))
      };
    | ConstructorExpected(_) => InHole(WantConstructorFoundAp)
    | TypeExpected => InHole(WantTypeFoundAp)
    }
  | _ =>
    switch (expects) {
    | TypeExpected => NotInHole(Type(ty))
    | ConstructorExpected(_)
    | VariantExpected(_) => InHole(WantConstructorFoundType(ty))
    }
  };

let status_tpat = (ctx: Ctx.t, utpat: UTPat.t): status_tpat =>
  switch (utpat.term) {
  | EmptyHole => NotInHole(Empty)
  | Var(name)
      when Form.is_base_typ(name) || Ctx.lookup_alias(ctx, name) != None =>
    InHole(ShadowsType(name))
  | Var(name) => NotInHole(Var(name))
  | Invalid(_) => InHole(NotAVar(NotCapitalized))
  | MultiHole(_) => InHole(NotAVar(Other))
  };

/* Determines whether any term is in an error hole. */
let is_error = (ci: t): bool => {
  switch (ci) {
  | InfoExp({mode, self, ctx, _}) =>
    switch (status_exp(ctx, mode, self)) {
    | InHole(_) => true
    | NotInHole(_) => false
    }
  | InfoPat({mode, self, ctx, _}) =>
    switch (status_pat(ctx, mode, self)) {
    | InHole(_) => true
    | NotInHole(_) => false
    }
  | InfoTyp({expects, ctx, term, ty, _}) =>
    switch (status_typ(ctx, expects, term, ty)) {
    | InHole(_) => true
    | NotInHole(_) => false
    }
  | InfoTPat({term, ctx, _}) =>
    switch (status_tpat(ctx, term)) {
    | InHole(_) => true
    | NotInHole(_) => false
    }
  | Secondary(_) => false
  };
};

/* Determined the type of an expression or pattern 'after hole fixing';
   that is, all ill-typed terms are considered to be 'wrapped in
   non-empty holes', i.e. assigned Unknown type. */
let fixed_typ_ok: ok_pat => Typ.t =
  fun
  | Syn(syn) => syn
  | Ana(Consistent({join, _})) => join
  | Ana(InternallyInconsistent({ana, _})) => ana;

let fixed_typ_pat = (ctx, mode: Mode.t, self: Self.pat): Typ.t =>
  switch (status_pat(ctx, mode, self)) {
  | InHole(_) => Unknown(Internal)
  | NotInHole(ok) => fixed_typ_ok(ok)
  };

let fixed_constraint_pat =
    (ctx, mode: Mode.t, self: Self.pat, constraint_: Constraint.t)
    : Constraint.t =>
  switch (fixed_typ_pat(ctx, mode, self)) {
  | Unknown(_) => Constraint.Hole
  | _ => constraint_
  };

let fixed_typ_exp = (ctx, mode: Mode.t, self: Self.exp): Typ.t =>
  switch (status_exp(ctx, mode, self)) {
  | InHole(_) => Unknown(Internal)
  | NotInHole(ok) => fixed_typ_ok(ok)
  };

/* Add derivable attributes for expression terms */
let derived_exp =
    (~uexp: UExp.t, ~ctx, ~mode, ~ancestors, ~self, ~co_ctx): exp => {
  let cls = Cls.Exp(UExp.cls_of_term(uexp.term));
  let status = status_exp(ctx, mode, self);
  let ty = fixed_typ_exp(ctx, mode, self);
  {cls, self, ty, mode, status, ctx, co_ctx, ancestors, term: uexp};
};

/* Add derivable attributes for pattern terms */
let derived_pat =
    (~upat: UPat.t, ~ctx, ~co_ctx, ~mode, ~ancestors, ~self, ~constraint_)
    : pat => {
  let cls = Cls.Pat(UPat.cls_of_term(upat.term));
  let status = status_pat(ctx, mode, self);
  let ty = fixed_typ_pat(ctx, mode, self);
  let constraint_ = fixed_constraint_pat(ctx, mode, self, constraint_);
  {
    cls,
    self,
    mode,
    ty,
    status,
    ctx,
    co_ctx,
    ancestors,
    term: upat,
    constraint_,
  };
};

/* Add derivable attributes for types */
let derived_typ = (~utyp: UTyp.t, ~ctx, ~ancestors, ~expects): typ => {
  let cls: Cls.t =
    /* Hack to improve CI display */
    switch (expects, UTyp.cls_of_term(utyp.term)) {
    | (VariantExpected(_), Var) => Cls.Typ(Constructor)
    | (_, cls) => Cls.Typ(cls)
    };
  let ty = UTyp.to_typ(ctx, utyp);
  let status = status_typ(ctx, expects, utyp, ty);
  {cls, ctx, ancestors, status, expects, ty, term: utyp};
};

/* Add derivable attributes for type patterns */
let derived_tpat = (~utpat: UTPat.t, ~ctx, ~ancestors): tpat => {
  let cls = Cls.TPat(UTPat.cls_of_term(utpat.term));
  let status = status_tpat(ctx, utpat);
  {cls, ancestors, status, ctx, term: utpat};
};

/* If the info represents some kind of name binding which
   exists in the context, return the id where the binding occurs */
let get_binding_site = (info: t): option(Id.t) => {
  switch (info) {
  | InfoExp({term: {term: Var(name), _}, ctx, _}) =>
    let+ entry = Ctx.lookup_var(ctx, name);
    entry.id;
  | InfoExp({term: {term: Constructor(name), _}, ctx, _})
  | InfoPat({term: {term: Constructor(name), _}, ctx, _}) =>
    let+ entry = Ctx.lookup_ctr(ctx, name);
    entry.id;
  | InfoTyp({term: {term: Var(name), _}, ctx, _}) =>
    let+ entry = Ctx.lookup_tvar(ctx, name);
    entry.id;
  | _ => None
  };
};

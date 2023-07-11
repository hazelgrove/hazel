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

/* Common errors which can apply to either expression or patterns */
[@deriving (show({with_path: false}), sexp, yojson)]
type error_common =
  | BadToken(Token.t) /* Invalid expression token, treated as hole */
  | FreeTag /* Sum constructor neiter bound nor in ana type */
  | InconsistentWithArrow(Typ.t) /* Bad function position */
  | SynInconsistentBranches(list(Typ.t)) /* Inconsistent match or listlit */
  | TypeInconsistent({
      ana: Typ.t,
      syn: Typ.t,
    });

/* Expression term errors */
[@deriving (show({with_path: false}), sexp, yojson)]
type error_exp =
  | FreeVariable
  | Common(error_common);

/* Pattern term errors */
[@deriving (show({with_path: false}), sexp, yojson)]
type error_pat =
  | ExpectedTag
  | ExpectedModule(string)
  | Common(error_common);

/* Common ok statuses. The third represents the possibility of a
   match or list literal which has inconsisent branches. This is
   fine since the branches are in analytic position, but we may
   want to warn about this inconsistency in the cursor inspector */
[@deriving (show({with_path: false}), sexp, yojson)]
type ok_common =
  | SynConsistent(Typ.t)
  | AnaConsistent({
      ana: Typ.t,
      syn: Typ.t,
      join: Typ.t,
    })
  | AnaInternalInconsistent({
      ana: Typ.t,
      nojoin: list(Typ.t),
    });

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
  | TagExpected(status_variant, Typ.t)
  | VariantExpected(status_variant, Typ.t)
  | ModuleExpected
  | AnaTypeExpected(Typ.t);

/* Type term errors
   TODO: The three additional errors statuses
   are fundamentally syntactic and should when
   possible be reimplemeted via a seperate sort */
[@deriving (show({with_path: false}), sexp, yojson)]
type error_typ =
  | BadToken(Token.t) /* Invalid token, treated as type hole */
  | FreeTypeVar(TypVar.t) /* Free type variable */
  | DuplicateTag(Tag.t) /* Duplicate tag in same sum */
  | WantTypeFoundAp
  | WantTagFoundType(Typ.t)
  | WantTagFoundAp
  | FreeTypeMember(Token.t)
  | WantModule
  | InconsistentMember({
      ana: Typ.t,
      syn: Typ.t,
    });

/* Type ok statuses for cursor inspector */
[@deriving (show({with_path: false}), sexp, yojson)]
type ok_typ =
  | Variant(Tag.t, Typ.t)
  | VariantIncomplete(Typ.t)
  | TypeAlias(TypVar.t, Typ.t)
  | Module(Tag.t, Ctx.t)
  | Type(Typ.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type status_typ =
  | InHole(error_typ)
  | NotInHole(ok_typ);

/* Type pattern term errors */
[@deriving (show({with_path: false}), sexp, yojson)]
type error_tpat =
  | ShadowsType(TypVar.t)
  | NotAVar;

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
  mode: Mode.t, /* Parental type expectations; see Mode.re */
  self: Self.exp, /* Expectation-independent type info; see Self.re */
  co_ctx: CoCtx.t, /* Locally unbound variables; see CoCtx.re */
  cls: UExp.cls, /* derived */
  status: status_exp, /* derived: cursor inspector display */
  ty: Typ.t /* derived: type after hole fixing */
};

[@deriving (show({with_path: false}), sexp, yojson)]
type pat = {
  term: UPat.t,
  ancestors,
  ctx: Ctx.t,
  mode: Mode.t,
  self: Self.pat,
  cls: UPat.cls, /* derived */
  status: status_pat, /* derived: cursor inspector display */
  ty: Typ.t /* derived: type after hole fixing */
};

[@deriving (show({with_path: false}), sexp, yojson)]
type typ = {
  term: UTyp.t,
  ancestors,
  ctx: Ctx.t,
  expects: typ_expects,
  cls: UTyp.cls, /* derived */
  status: status_typ, /* derived: cursor inspector display */
  ty: Typ.t /* derived: represented type */
};

[@deriving (show({with_path: false}), sexp, yojson)]
type tpat = {
  term: UTPat.t,
  ancestors,
  ctx: Ctx.t,
  cls: UTPat.cls, /* derived */
  status: status_tpat /* derived : cursor inspector display */
};

/* The static information collated for each term */
[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | InfoExp(exp)
  | InfoPat(pat)
  | InfoTyp(typ)
  | InfoTPat(tpat);

let ctx_of: t => Ctx.t =
  fun
  | InfoExp({ctx, _}) => ctx
  | InfoPat({ctx, _}) => ctx
  | InfoTyp({ctx, _}) => ctx
  | InfoTPat({ctx, _}) => ctx;

let exp_co_ctx: exp => CoCtx.t = ({co_ctx, _}) => co_ctx;
let exp_ty: exp => Typ.t = ({ty, _}) => ty;
let pat_ctx: pat => Ctx.t = ({ctx, _}) => ctx;
let pat_ty: pat => Typ.t = ({ty, _}) => ty;

let rec status_common =
        (ctx: Ctx.t, mode: Mode.t, self: Self.t): status_common =>
  switch (self, mode) {
  | (Just(syn), Syn) => NotInHole(SynConsistent(syn))
  | (Just(syn), Ana(ana)) =>
    switch (Typ.join(ctx, ana, syn)) {
    | None => InHole(TypeInconsistent({syn, ana}))
    | Some(join) => NotInHole(AnaConsistent({ana, syn, join}))
    }
  | (Just(syn), SynFun) =>
    switch (
      Typ.join(ctx, Arrow(Unknown(Internal), Unknown(Internal)), syn)
    ) {
    | None => InHole(InconsistentWithArrow(syn))
    | Some(_) => NotInHole(SynConsistent(syn))
    }
  | (IsTag({name, syn_ty}), _) =>
    /* If a tag is being analyzed against (an arrow type returning)
       a sum type having that tag as a variant, its self type is
       considered to be determined by the sum type; otherwise,
       check the context for the tag's type */
    switch (Mode.tag_ana_typ(ctx, mode, name), syn_ty) {
    | (Some(ana_ty), _) => status_common(ctx, mode, Just(ana_ty))
    | (_, Some(syn_ty)) => status_common(ctx, mode, Just(syn_ty))
    | _ => InHole(FreeTag)
    }
  | (BadToken(name), _) => InHole(BadToken(name))
  | (IsMulti, _) => NotInHole(SynConsistent(Unknown(Internal)))
  | (NoJoin(tys), Ana(ana)) =>
    NotInHole(AnaInternalInconsistent({ana, nojoin: Typ.of_source(tys)}))
  | (NoJoin(tys), Syn | SynFun) =>
    InHole(SynInconsistentBranches(Typ.of_source(tys)))
  };

let status_pat = (ctx: Ctx.t, mode: Mode.t, self: Self.pat): status_pat =>
  switch (mode, self) {
  | (Ana(Module(_)), Common(BadToken(name))) =>
    InHole(ExpectedModule(name))
  | (Syn | Ana(_), Common(self_pat))
  | (SynFun, Common(IsTag(_) as self_pat)) =>
    /* Little bit of a hack. Anything other than a bound tag will, in
       function position, have SynFun mode (see Typ.ap_mode). Since we
       are prohibiting non-tags in tag applications in patterns for now,
       we catch them here, diverting to an ExpectedTag error. But we
       avoid capturing the second case above, as these will ultimately
       get a (more precise) unbound tag  via status_common */
    switch (status_common(ctx, mode, self_pat)) {
    | NotInHole(ok_exp) => NotInHole(ok_exp)
    | InHole(err_pat) => InHole(Common(err_pat))
    }
  | (SynFun, _) => InHole(ExpectedTag)
  };

/* Determines whether an expression or pattern is in an error hole,
   depending on the mode, which represents the expectations of the
   surrounding syntactic context, and the self which represents the
   makeup of the expression / pattern itself. */
let status_exp = (ctx: Ctx.t, mode: Mode.t, self: Self.exp): status_exp =>
  switch (self, mode) {
  | (FreeVar, _) => InHole(FreeVariable)
  | (Common(self_pat), _) =>
    switch (status_common(ctx, mode, self_pat)) {
    | NotInHole(ok_exp) => NotInHole(ok_exp)
    | InHole(err_pat) => InHole(Common(err_pat))
    }
  };

/* This logic determines whether a type should be put
   in a hole or not. It's mostly syntactic, determining
   the proper placement of sum type variants and tags;
   this should be reimplemented in the future as a
   separate sort. It also determines semantic properties
   such as whether or not a type variable reference is
   free, and whether a tag name is a dupe. */
let status_typ =
    (ctx: Ctx.t, expects: typ_expects, term: TermBase.UTyp.t, ty: Typ.t)
    : status_typ =>
  switch (term.term) {
  | Invalid(token) => InHole(BadToken(token))
  | EmptyHole => NotInHole(Type(ty))
  | Var(name)
  | Tag(name) =>
    switch (expects) {
    | VariantExpected(Unique, sum_ty)
    | TagExpected(Unique, sum_ty) => NotInHole(Variant(name, sum_ty))
    | VariantExpected(Duplicate, _)
    | TagExpected(Duplicate, _) => InHole(DuplicateTag(name))
    | ModuleExpected =>
      switch (Module.get_module("", ctx, term)) {
      | Some((name, Some(inner_ctx))) => NotInHole(Module(name, inner_ctx))
      | _ => InHole(WantModule)
      }

    | TypeExpected =>
      switch (Ctx.is_alias(ctx, name)) {
      | false => InHole(FreeTypeVar(name))
      | true => NotInHole(TypeAlias(name, Typ.weak_head_normalize(ctx, ty)))
      }
    | AnaTypeExpected(ana) =>
      switch (Typ.join(ctx, ana, ty)) {
      | None => InHole(InconsistentMember({ana, syn: ty}))
      | Some(join) => NotInHole(Type(join))
      }
    }
  | Ap(t1, t2) =>
    switch (expects) {
    | VariantExpected(status_variant, ty_variant) =>
      let ty_in = UTyp.to_typ(ctx, t2);
      switch (status_variant, t1.term) {
      | (Unique, Var(name) | Tag(name)) =>
        NotInHole(Variant(name, Arrow(ty_in, ty_variant)))
      | _ => NotInHole(VariantIncomplete(Arrow(ty_in, ty_variant)))
      };
    | TagExpected(_) => InHole(WantTagFoundAp)
    | ModuleExpected => InHole(WantModule)
    | TypeExpected => InHole(WantTypeFoundAp)
    | AnaTypeExpected(ana) =>
      switch (Typ.join(ctx, ana, ty)) {
      | None => InHole(InconsistentMember({ana, syn: ty}))
      | Some(join) => NotInHole(Type(join))
      }
    }
  | Dot(_, _) =>
    switch (expects, ty) {
    | (ModuleExpected, _) =>
      switch (Module.get_module("", ctx, term)) {
      | Some((name, Some(inner_ctx))) => NotInHole(Module(name, inner_ctx))
      | _ => InHole(WantModule)
      }
    | (_, Member(name, Typ.Unknown(Internal))) =>
      InHole(FreeTypeMember(name))
    | (_, Member(name, ty)) => NotInHole(TypeAlias(name, ty))
    | _ => InHole(BadToken("")) // Shouldn't reach
    }

  | _ =>
    switch (expects) {
    | TypeExpected => NotInHole(Type(ty))
    | TagExpected(_)
    | VariantExpected(_) => InHole(WantTagFoundType(ty))
    | ModuleExpected => InHole(WantModule)
    | AnaTypeExpected(ana) =>
      switch (Typ.join(ctx, ana, ty)) {
      | None => InHole(InconsistentMember({ana, syn: ty}))
      | Some(join) => NotInHole(Type(join))
      }
    }
  };

let status_tpat = (ctx: Ctx.t, utpat: UTPat.t): status_tpat =>
  switch (utpat.term) {
  | EmptyHole => NotInHole(Empty)
  | Var(name)
      when Form.is_base_typ(name) || Ctx.lookup_alias(ctx, name) != None =>
    InHole(ShadowsType(name))
  | Var(name) => NotInHole(Var(name))
  | Invalid(_)
  | MultiHole(_) => InHole(NotAVar)
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
  };
};

/* Determined the type of an expression or pattern 'after hole fixing';
   that is, all ill-typed terms are considered to be 'wrapped in
   non-empty holes', i.e. assigned Unknown type. */
let fixed_typ_ok: ok_pat => Typ.t =
  fun
  | SynConsistent(syn) => syn
  | AnaConsistent({join, _}) => join
  | AnaInternalInconsistent({ana, _}) => ana;

let fixed_typ_pat = (ctx, mode: Mode.t, self: Self.pat): Typ.t =>
  switch (status_pat(ctx, mode, self)) {
  | InHole(_) => Unknown(Internal)
  | NotInHole(ok) => fixed_typ_ok(ok)
  };

let fixed_typ_exp = (ctx, mode: Mode.t, self: Self.exp): Typ.t =>
  switch (status_exp(ctx, mode, self)) {
  | InHole(_) => Unknown(Internal)
  | NotInHole(ok) => fixed_typ_ok(ok)
  };

/* Add derivable attributes for expression terms */
let derived_exp =
    (~uexp: UExp.t, ~ctx, ~mode, ~ancestors, ~self, ~co_ctx): exp => {
  let cls = UExp.cls_of_term(uexp.term);
  let status = status_exp(ctx, mode, self);
  let ty = fixed_typ_exp(ctx, mode, self);
  {cls, self, ty, mode, status, ctx, co_ctx, ancestors, term: uexp};
};

/* Add derivable attributes for pattern terms */
let derived_pat = (~upat: UPat.t, ~ctx, ~mode, ~ancestors, ~self): pat => {
  let cls = UPat.cls_of_term(upat.term);
  let status = status_pat(ctx, mode, self);
  let ty = fixed_typ_pat(ctx, mode, self);
  {cls, self, mode, ty, status, ctx, ancestors, term: upat};
};

/* Add derivable attributes for types */
let derived_typ = (~utyp: UTyp.t, ~ctx, ~ancestors, ~expects): typ => {
  let cls: UTyp.cls =
    /* Hack to improve CI display */
    switch (expects, UTyp.cls_of_term(utyp.term)) {
    | (VariantExpected(_), Var) => Tag
    | (_, cls) => cls
    };
  let ty = UTyp.to_typ(ctx, utyp);
  let status = status_typ(ctx, expects, utyp, ty);
  {cls, ctx, ancestors, status, expects, ty, term: utyp};
};

/* Add derivable attributes for type patterns */
let derived_tpat = (~utpat: UTPat.t, ~ctx, ~ancestors): tpat => {
  let cls = UTPat.cls_of_term(utpat.term);
  let status = status_tpat(ctx, utpat);
  {cls, ancestors, status, ctx, term: utpat};
};

/* If the info represents some kind of name binding which
   exists in the context, return the id where the binding occurs */
let get_binding_site = (info: t): option(Id.t) => {
  switch (info) {
  | InfoExp({term: {term: Var(name) | Tag(name), _}, ctx, _})
  | InfoPat({term: {term: Tag(name), _}, ctx, _})
  | InfoTyp({term: {term: Var(name), _}, ctx, _}) =>
    let+ entry = Ctx.lookup(ctx, name);
    Ctx.get_id(entry);
  | _ => None
  };
};

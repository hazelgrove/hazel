open Sexplib.Std;
open Util.OptUtil.Syntax;
open Term;

/* The ids of a term's ancestors in the AST */
[@deriving (show({with_path: false}), sexp, yojson)]
type ancestors = list(Id.t);

/* Hazel type annotated with a relevant source location.
   Currently used to track match branches for inconsistent
   branches errors, but could perhaps be used more broadly
   for type debugging UI. */
[@deriving (show({with_path: false}), sexp, yojson)]
type source = {
  id: int,
  ty: Typ.t,
};

/* The common (synthetic) type information derivable from pattern
   or expression terms in isolation, using the typing context but
   not the syntactic context i.e. typing mode */
[@deriving (show({with_path: false}), sexp, yojson)]
type self_common =
  | Just(Typ.t) /* Just a regular type */
  | NoJoin(list(source)) /* Inconsistent types for e.g match, listlits */
  | BadToken(Token.t) /* Invalid expression token, treated as hole */
  | IsMulti /* Multihole, treated as hole */
  | IsTag({
      name: Token.t,
      syn_ty: option(Typ.t),
    }); /* Tags have special ana logic */

/* The self for expressions could also be a free variable */
[@deriving (show({with_path: false}), sexp, yojson)]
type self_exp =
  | FreeVar
  | Common(self_common);

[@deriving (show({with_path: false}), sexp, yojson)]
type self_pat =
  | Common(self_common);

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
  | VariantExpected(status_variant, Typ.t);

/* Type term errors
   TODO: The three additional errors statuses
   are fundamentally syntactic and should when
   possible be reimplemeted via a seperate sort */
[@deriving (show({with_path: false}), sexp, yojson)]
type error_typ =
  | BadToken(Token.t) /* Invalid token, treated as type hole */
  | FreeTypeVar(Token.t) /* Free type variable */
  | DuplicateTag(Token.t) /* Duplicate tag in same sum */
  | WantTypeFoundAp
  | WantTagFoundType(Typ.t)
  | WantTagFoundAp;

/* Type ok statuses for cursor inspector */
[@deriving (show({with_path: false}), sexp, yojson)]
type ok_typ =
  | Variant(Token.t, Typ.t)
  | VariantIncomplete(Typ.t)
  | TypeAlias(Token.t, Typ.t)
  | Type(Typ.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type status_typ =
  | InHole(error_typ)
  | NotInHole(ok_typ);

/* Type pattern term errors */
[@deriving (show({with_path: false}), sexp, yojson)]
type error_tpat =
  | ShadowsType(Token.t)
  | NotAVar;

/* Type pattern ok statuses for cursor inspector */
[@deriving (show({with_path: false}), sexp, yojson)]
type ok_tpat =
  | Empty
  | Var(Token.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type status_tpat =
  | NotInHole(ok_tpat)
  | InHole(error_tpat);

[@deriving (show({with_path: false}), sexp, yojson)]
type exp = {
  term: UExp.t,
  ancestors,
  ctx: Ctx.t,
  mode: Typ.mode,
  self: self_exp,
  free: Ctx.co, /* _Locally_ unbound variables */
  cls: UExp.cls, /* derived */
  status: status_exp, /* derived: cursor inspector */
  ty: Typ.t /* derived: type after hole fixing */
};

[@deriving (show({with_path: false}), sexp, yojson)]
type pat = {
  term: UPat.t,
  ancestors,
  ctx: Ctx.t,
  mode: Typ.mode,
  self: self_pat,
  cls: UPat.cls, /* derived */
  status: status_pat, /* derived: cursor inspector */
  ty: Typ.t /* derived: type after hole fixing */
};

[@deriving (show({with_path: false}), sexp, yojson)]
type typ = {
  term: UTyp.t,
  ancestors,
  ctx: Ctx.t,
  expects: typ_expects,
  cls: UTyp.cls, /* derived */
  status: status_typ, /* derived: cursor inspector */
  ty: Typ.t /* derived: represented type */
};

[@deriving (show({with_path: false}), sexp, yojson)]
type tpat = {
  term: UTPat.t,
  ancestors,
  ctx: Ctx.t,
  cls: UTPat.cls, /* derived: from term */
  status: status_tpat /* derived : cursor inspector */
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

let exp_free: exp => Ctx.co = ({free, _}) => free;
let exp_ty: exp => Typ.t = ({ty, _}) => ty;
let pat_ctx: pat => Ctx.t = ({ctx, _}) => ctx;
let pat_ty: pat => Typ.t = ({ty, _}) => ty;

/* Strip location information from a list of sources */
let source_tys = List.map((source: source) => source.ty);

let rec status_common =
        (ctx: Ctx.t, mode: Typ.mode, self: self_common): status_common =>
  switch (self, mode) {
  | (BadToken(name), Syn | SynFun | Ana(_)) => InHole(BadToken(name))
  | (IsMulti, Syn | SynFun | Ana(_)) =>
    NotInHole(SynConsistent(Unknown(Internal)))
  | (Just(ty), Syn) => NotInHole(SynConsistent(ty))
  | (Just(ty), SynFun) =>
    switch (Typ.join(ctx, Arrow(Unknown(Internal), Unknown(Internal)), ty)) {
    | Some(_) => NotInHole(SynConsistent(ty))
    | None => InHole(InconsistentWithArrow(ty))
    }
  | (Just(syn), Ana(ana)) =>
    switch (Typ.join(ctx, ana, syn)) {
    | None => InHole(TypeInconsistent({syn, ana}))
    | Some(join) => NotInHole(AnaConsistent({ana, syn, join}))
    }
  | (IsTag({name, syn_ty}), _) =>
    /* If a tag is being analyzed against (an arrow type returning)
       a sum type having that tag as a variant, its self type is
       considered to be determined by the sum type; otherwise,
       check the context for the tag's type */
    switch (Typ.tag_ana_typ(ctx, mode, name), syn_ty) {
    | (Some(ana_ty), _) => status_common(ctx, mode, Just(ana_ty))
    | (_, Some(syn_ty)) => status_common(ctx, mode, Just(syn_ty))
    | _ => InHole(FreeTag)
    }
  | (NoJoin(tys), Syn | SynFun) =>
    InHole(SynInconsistentBranches(source_tys(tys)))
  | (NoJoin(tys), Ana(ana)) =>
    NotInHole(AnaInternalInconsistent({ana, nojoin: source_tys(tys)}))
  };

let status_pat = (ctx: Ctx.t, mode: Typ.mode, self: self_pat): status_pat =>
  switch (self, mode) {
  | (Common(self_pat), _) =>
    switch (status_common(ctx, mode, self_pat)) {
    | NotInHole(ok_exp) => NotInHole(ok_exp)
    | InHole(err_pat) => InHole(Common(err_pat))
    }
  };

/* Determines whether an expression or pattern is in an error hole,
   depending on the mode, which represents the expectations of the
   surrounding syntactic context, and the self which represents the
   makeup of the expression / pattern itself. */
let status_exp = (ctx: Ctx.t, mode: Typ.mode, self: self_exp): status_exp =>
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
    | TypeExpected =>
      switch (Ctx.is_alias(ctx, name)) {
      | false => InHole(FreeTypeVar(name))
      | true => NotInHole(TypeAlias(name, Typ.normalize_shallow(ctx, ty)))
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
    | TypeExpected => InHole(WantTypeFoundAp)
    }
  | _ =>
    switch (expects) {
    | TypeExpected => NotInHole(Type(ty))
    | TagExpected(_)
    | VariantExpected(_) => InHole(WantTagFoundType(ty))
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

/* Determined the type of an expression or pattern 'after hole wrapping';
   that is, all ill-typed terms are considered to be 'wrapped in
   non-empty holes', i.e. assigned Unknown type. */
let typ_ok: ok_pat => Typ.t =
  fun
  | SynConsistent(syn) => syn
  | AnaConsistent({join, _}) => join
  | AnaInternalInconsistent({ana, _}) => ana;

let ty_after_fix_pat = (ctx, mode: Typ.mode, self: self_pat): Typ.t =>
  switch (status_pat(ctx, mode, self)) {
  | InHole(_) => Unknown(Internal)
  | NotInHole(ok) => typ_ok(ok)
  };
let ty_after_fix_exp = (ctx, mode: Typ.mode, self: self_exp): Typ.t =>
  switch (status_exp(ctx, mode, self)) {
  | InHole(_) => Unknown(Internal)
  | NotInHole(ok) => typ_ok(ok)
  };

/* Add derivable attributes for expression terms */
let derived_exp = (~uexp: UExp.t, ~ctx, ~mode, ~ancestors, ~self, ~free): exp => {
  let cls = UExp.cls_of_term(uexp.term);
  let status = status_exp(ctx, mode, self);
  let ty = ty_after_fix_exp(ctx, mode, self);
  {cls, self, ty, mode, status, ctx, free, ancestors, term: uexp};
};

/* Add derivable attributes for pattern terms */
let derived_pat = (~upat: UPat.t, ~ctx, ~mode, ~ancestors, ~self): pat => {
  let cls = UPat.cls_of_term(upat.term);
  let status = status_pat(ctx, mode, self);
  let ty = ty_after_fix_pat(ctx, mode, self);
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

/* The self of a var depends on the ctx; if the
   lookup fails, it is a free variable */
let self_var = (ctx: Ctx.t, name: Token.t): self_exp =>
  switch (Ctx.lookup_var(ctx, name)) {
  | None => FreeVar
  | Some(var) => Common(Just(var.typ))
  };

/* The self of a tag depends on the ctx, but a
   lookup failure doesn't necessarily means its
   free; it may be given a type analytically */
let self_tag = (ctx: Ctx.t, name: Token.t): self_common =>
  IsTag({
    name,
    syn_ty:
      switch (Ctx.lookup_tag(ctx, name)) {
      | None => None
      | Some({typ, _}) => Some(typ)
      },
  });

/* The self assigned to things like cases and list literals
   which can have internal type inconsistencies. */
let join =
    (wrap: Typ.t => Typ.t, tys: list(Typ.t), ids: list(Id.t), ctx: Ctx.t)
    : self_common =>
  switch (Typ.join_all(ctx, tys)) {
  | None => NoJoin(List.map2((id, ty) => {id, ty}, ids, tys))
  | Some(ty) => Just(wrap(ty))
  };

/* What the type would be if the position had been
   synthetic, so no hole fixing. Returns none if
   there's no applicable synthetic rule. */
let typ_of_self_common: (Ctx.t, self_common) => option(Typ.t) =
  _ctx =>
    fun
    | Just(typ) => Some(typ)
    | IsTag({syn_ty, _}) => syn_ty
    | BadToken(_)
    | IsMulti
    | NoJoin(_) => None;

let typ_of_self_exp: (Ctx.t, self_exp) => option(Typ.t) =
  ctx =>
    fun
    | FreeVar => None
    | Common(self) => typ_of_self_common(ctx, self);

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

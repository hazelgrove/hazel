open Sexplib.Std;

/* Expressions are assigned a mode (reflecting the static expectations
   if any of their syntactic parent), a self (reflecting what their
   statics would be in isolation), a context (variables in scope), and
   free (variables occuring free in the expression. */
[@deriving (show({with_path: false}), sexp, yojson)]
type info_exp = {
  cls: Term.UExp.cls,
  term: Term.UExp.t,
  mode: Typ.mode,
  self: Typ.self,
  ctx: Ctx.t,
  free: Ctx.co,
};

/* Patterns are assigned a mode (reflecting the static expectations
   if any of their syntactic parent) and a self (reflecting what their
   statics would be in isolation), a context (variables in scope) */
[@deriving (show({with_path: false}), sexp, yojson)]
type info_pat = {
  cls: Term.UPat.cls,
  term: Term.UPat.t,
  mode: Typ.mode,
  self: Typ.self,
  ctx: Ctx.t // TODO: detect in-pattern shadowing
};

/* A type can be either valid or a free type variable.
   The additional errors statuses are fundamentally
   syntactic and should eventually be reimplemeted
   via a seperate sort */
[@deriving (show({with_path: false}), sexp, yojson)]
type status_typ =
  | Ok(Typ.t)
  | FreeTypeVar
  | DuplicateTag
  | ApOutsideSum
  | TagExpected(Typ.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type status_variant =
  | Unique
  | Duplicate;

[@deriving (show({with_path: false}), sexp, yojson)]
type typ_mode =
  | TypeExpected
  | TagExpected(status_variant)
  | VariantExpected(status_variant);

[@deriving (show({with_path: false}), sexp, yojson)]
type info_typ = {
  cls: Term.UTyp.cls,
  term: Term.UTyp.t,
  mode: typ_mode,
  ctx: Ctx.t,
  status: status_typ,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type info_rul = {
  cls: Term.URul.cls,
  term: Term.UExp.t,
};

/* Either a type pattern is a valid name or it's an error */
[@deriving (show({with_path: false}), sexp, yojson)]
type status_tpat =
  | Ok
  | NotAName;

[@deriving (show({with_path: false}), sexp, yojson)]
type info_tpat = {
  cls: Term.UTPat.cls,
  term: Term.UTPat.t,
  status: status_tpat,
};

/* The Info aka Cursorinfo assigned to each subterm. */
[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Invalid(TermBase.parse_flag)
  | InfoExp(info_exp)
  | InfoPat(info_pat)
  | InfoTyp(info_typ)
  | InfoRul(info_rul)
  | InfoTPat(info_tpat);

/* Static error classes */

let status_tpat = (utpat: Term.UTPat.t): status_tpat =>
  switch (utpat.term) {
  | Var(_) => Ok
  | _ => NotAName
  };

[@deriving (show({with_path: false}), sexp, yojson)]
type error =
  | Self(Typ.self_error)
  | SynInconsistentBranches(list(Typ.t))
  | TypeInconsistent(Typ.t, Typ.t);

/* Statics non-error classes */
[@deriving (show({with_path: false}), sexp, yojson)]
type happy =
  | SynConsistent(Typ.t)
  | AnaConsistent(Typ.t, Typ.t, Typ.t) //ana, syn, join
  | AnaInternalInconsistent(Typ.t, list(Typ.t)) // ana, branches
  | AnaExternalInconsistent(Typ.t, Typ.t); // ana, syn

/* The error status which 'wraps' each term. */
[@deriving (show({with_path: false}), sexp, yojson)]
type error_status =
  | InHole(error)
  | NotInHole(happy);

/* Determines whether an expression or pattern is in an error hole,
   depending on the mode, which represents the expectations of the
   surrounding syntactic context, and the self which represents the
   makeup of the expression / pattern itself. */
let error_status = (ctx: Ctx.t, mode: Typ.mode, self: Typ.self): error_status =>
  switch (mode, self) {
  | (SynFun, Just(ty)) =>
    switch (Typ.join(ctx, Arrow(Unknown(Internal), Unknown(Internal)), ty)) {
    | None => InHole(Self(NoFun(ty)))
    | Some(_) => NotInHole(SynConsistent(ty))
    }
  | (SynFun, Joined(_wrap, tys_syn)) =>
    let tys_syn = Typ.source_tys(tys_syn);
    switch (Typ.join_all(ctx, tys_syn)) {
    | None => InHole(SynInconsistentBranches(tys_syn))
    | Some(ty_joined) =>
      switch (
        Typ.join(
          ctx,
          Arrow(Unknown(Internal), Unknown(Internal)),
          ty_joined,
        )
      ) {
      | None => InHole(Self(NoFun(ty_joined)))
      | Some(_) => NotInHole(SynConsistent(ty_joined))
      }
    };
  | (Syn | SynFun | Ana(_), Self(Multi)) =>
    NotInHole(SynConsistent(Unknown(Internal)))
  | (Syn | SynFun | Ana(_), Self(err)) => InHole(Self(err))

  | (Syn, Just(ty)) => NotInHole(SynConsistent(ty))
  | (Syn, Joined(wrap, tys_syn)) =>
    let tys_syn = Typ.source_tys(tys_syn);
    switch (Typ.join_all(ctx, tys_syn)) {
    | None => InHole(SynInconsistentBranches(tys_syn))
    | Some(ty_joined) => NotInHole(SynConsistent(wrap(ty_joined)))
    };
  | (Ana(ty_ana), Just(ty_syn)) =>
    switch (Typ.join(ctx, ty_ana, ty_syn)) {
    | None => InHole(TypeInconsistent(ty_syn, ty_ana))
    | Some(ty_join) => NotInHole(AnaConsistent(ty_ana, ty_syn, ty_join))
    }
  | (Ana(ty_ana), Joined(wrap, tys_syn)) =>
    switch (Typ.join_all(ctx, Typ.source_tys(tys_syn))) {
    | Some(ty_syn) =>
      let ty_syn = wrap(ty_syn);
      switch (Typ.join(ctx, ty_syn, ty_ana)) {
      | None => NotInHole(AnaExternalInconsistent(ty_ana, ty_syn))
      | Some(ty_join) => NotInHole(AnaConsistent(ty_ana, ty_syn, ty_join))
      };
    | None =>
      NotInHole(AnaInternalInconsistent(ty_ana, Typ.source_tys(tys_syn)))
    }
  };

/* Determines whether any term is in an error hole. Currently types cannot
   be in error, and Invalids (things to which Term was unable to assign a
   parse) are always in error. The error status of expressions and patterns
   are determined by error_status above. */
let is_error = (ci: t): bool => {
  switch (ci) {
  | Invalid(Secondary) => false
  | Invalid(_) => true
  | InfoExp({mode, self, ctx, _})
  | InfoPat({mode, self, ctx, _}) =>
    switch (error_status(ctx, mode, self)) {
    | InHole(_) => true
    | NotInHole(_) => false
    }
  | InfoTyp({status, _}) =>
    switch (status) {
    | Ok(_) => false
    | _ => true
    }
  | InfoTPat({status, _}) => status != Ok
  | InfoRul(_) => false
  };
};

/* Determined the type of an expression or pattern 'after hole wrapping';
   that is, all ill-typed terms are considered to be 'wrapped in
   non-empty holes', i.e. assigned Unknown type. */
let typ_after_fix = (ctx, mode: Typ.mode, self: Typ.self): Typ.t =>
  switch (error_status(ctx, mode, self)) {
  | InHole(_) => Unknown(Internal)
  | NotInHole(SynConsistent(t)) => t
  | NotInHole(AnaConsistent(_, _, ty_join)) => ty_join
  | NotInHole(AnaExternalInconsistent(ty_ana, _)) => ty_ana
  | NotInHole(AnaInternalInconsistent(ty_ana, _)) => ty_ana
  };

let typ_of_self = (ctx: Ctx.t): (Typ.self => Typ.t) =>
  fun
  | Just(t) => t
  | Joined(wrap, ss) =>
    switch (ss |> List.map((s: Typ.source) => s.ty) |> Typ.join_all(ctx)) {
    | None => Unknown(Internal)
    | Some(t) => wrap(t)
    }
  | Self(_) => Unknown(Internal);

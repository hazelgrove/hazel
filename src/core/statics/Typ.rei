/** Type brands for statically determining the index positioning scheme of a de Bruijn index */
module Pos: {
  /** Absolute positioning */
  [@deriving (sexp, yojson)]
  [@sexp.opaque]
  [@yojson.opaqe]
  type absolute;

  /** Relative positioning */
  [@deriving (sexp, yojson)]
  [@sexp.opaque]
  [@yojson.opaqe]
  type relative;
};

module Idx: {
  /** An explicit representation of a De Bruijn Index */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type s('pos) = pri int;

  let equal: (s('pos), s('pos)) => bool;
  /* let increment: t('pos) => t('pos); */
  /* let decrement: t('pos) => t('pos); */

  /* let shift: (~above: int, ~amount: int, t('idx)) => t('idx); */

  /** Absolutely-positioned indices */
  module Abs: {
    type rel := s(Pos.relative);
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = s(Pos.absolute);
    let of_int: int => t;
    let to_int: t => int;
    let to_rel: (~offset: int=?, t) => rel;
  };

  /** Relatively-positioned indices */
  module Rel: {
    type abs := s(Pos.absolute);
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = s(Pos.relative);
    let of_int: int => t;
    let to_int: t => int;
    let to_abs: (~offset: int=?, t) => abs;
  };
};

/** A typing context reference */
module Ref: {
  type peers := list(string);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type s('pos) = {
    /** A de Bruijn index */
    index: Idx.s('pos),
    /** The length of the context when the reference was constructed */
    stamp: int,
    /** The names of bindings preceding this one in the context when the reference was constructed */
    predecessors: peers,
    /** The names of bindings succeeding this one in the context when the reference was constructed */
    successors: peers,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = s(Pos.absolute);

  let abs: (Idx.Abs.t, ~predecessors: peers=?, ~successors: peers=?, int) => t;

  let equiv: (t, t) => bool;
};

/** Types with holes and type variables */
module Typ_syntax: {
  /** TYPE_PROVENANCE: From whence does an unknown type originate?
     Is it generated from an unannotated pattern variable (SynSwitch),
     a pattern variable annotated with a type hole (TypeHole), or
     generated by an internal judgement (Internal)? */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type type_provenance =
    | SynSwitch
    | TypeHole
    | Internal;

  /** TYP.T: Hazel types */
  [@deriving (sexp, yojson)]
  type t('pos) =
    | /** An unknown type */
      Unknown(type_provenance)
    | /** Integers */
      Int
    | /** Floating-point numbers */
      Float
    | /** Booleans */
      Bool
    | /** Lists */
      List(t('pos))
    | /** Functions */
      Arrow(t('pos), t('pos))
    /* Tuples */
    | Prod(list(t('pos)))
    | TyVar(Ref.s('pos), TyVar.t);

  /** Changes indices from absolute to relative positioning */
  let to_rel: (~offset: int=?, t(Pos.absolute)) => t(Pos.relative);

  /** Changes indices from relative to absolute positioning */
  let to_abs: (~offset: int=?, t(Pos.relative)) => t(Pos.absolute);

  /** How type provenance information should be collated when
   joining unknown types. This probably requires more thought,
      but right now TypeHole strictly predominates over Internal
      which strictly predominates over SynSwitch. */
  let join_type_provenance:
    (type_provenance, type_provenance) => type_provenance;
};

module Kind_syntax: {
  [@deriving (sexp, yojson)]
  type s('pos) =
    | /** A type of unknown kind */
      Unknown
    | /** An abstract type */
      Abstract
    | /** An equivalence class of types */
      Singleton(Typ_syntax.t('pos));

  /** Changes indices from absolute to relative positioning */
  let to_rel: (~offset: int=?, s(Pos.absolute)) => s(Pos.relative);

  /** Changes indices from relative to absolute positioning */
  let to_abs: (~offset: int=?, s(Pos.relative)) => s(Pos.absolute);
};

/** An integrated typing context for type variables and expression variables */
module rec Ctx: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type binding;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = list(binding);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type var_entry = {
    id: Id.t,
    name: Var.t,
    typ: Typ.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type tyvar_entry = {
    id: Id.t,
    name: TyVar.t,
    kind: Kind.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type entry =
    | VarEntry(var_entry)
    | TyVarEntry(tyvar_entry);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type coentry = {
    id: Id.t,
    mode: Typ.mode,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type co = VarMap.t_(list(coentry));

  let subtract: (t, co) => co;
  let union: list(co) => co;

  let binding_name: binding => string;
  let empty: unit => t;

  let to_list:
    t => (list((Ref.t, var_entry)), list((Ref.t, tyvar_entry)));

  /** Returns a context consisting of the given entries. */
  let of_entries: list(entry) => t;

  /** Returns the (sanitized) bindings of a given context. */
  let entries: t => list(entry);

  /** Returns the number of binding in the given context */
  let length: t => int;

  /** Converts a reference between two different versions of a context.

     WARNING: Result is only valid if the given context is the same context used
              to construct the reference, or an extension of it.
     WARNING: Crashes if the given context is empty.
     WARNING: Crashes if the given reference is out of bounds.
   */
  let rescope: (t, Ref.t) => Ref.t;

  /* Type Variables */

  /** Returns a reference and a record describing each type variable bound in
     the given context. */
  let tyvars: t => list((Ref.t, tyvar_entry));

  /** Returns a record describing the referenced type variable. */
  let tyvar: (t, Ref.t) => option(tyvar_entry);

  /** Returns a reference and a record describing the named type variable. */
  let tyvar_named: (t, TyVar.t) => option((Ref.t, tyvar_entry));

  /** Binds a type variable. */
  let add_tyvar: (t, tyvar_entry) => t;

  /** [reduce_tyvars(new_ctx, old_ctx, ty)] replaces any type variables bound by
     [new_ctx] but not by [old_ctx] in [ty] with equivalent types that have no
     new type variables.

     WARNING: result only valid if the contexts are the same or one is an
     extension of the other.
   */
  let reduce_tyvars: (t, t, Typ.t) => Typ.t;

  /* Expression Variables */

  /** Returns a reference and a record describing each expression variable bound
   in the given context. */
  let vars: t => list((Ref.t, var_entry));

  /** Returns the name of the referenced expression variable. */
  let var: (t, Ref.t) => option(var_entry);

  /** Returns a reference and a record describing the named expression variable. */
  let var_named: (t, Var.t) => option((Ref.t, var_entry));

  /** Binds an expression variable. */
  let add_var: (t, var_entry) => t;
  /* [@deriving (show({with_path: false}), sexp, yojson)] */
  /* type co_item = { */
  /*   id: Id.t, */
  /*   mode: Typ.mode, */
  /* }; */
  /* [@deriving (show({with_path: false}), sexp, yojson)] */
  /* type co_entry = list(co_item); */
  /* [@deriving (show({with_path: false}), sexp, yojson)] */
  /* type co = VarMap.t_(co_entry); */
  /* let empty: t; */
  /* let subtract: (t, co) => co; */
  /* let union: list(co) => co; */
}

and Kind: {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Kind_syntax.s(Pos.absolute);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type source = {
    id: int,
    ty: t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type self =
    | Just(t)
    /* | Joined(list(source)) */
    | Free;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type mode =
    | Syn
    | Ana(t);

  /** Produces a [Typ] belonging to the given [Kind]. */
  let to_typ: t => Typ.t;

  let unknown: unit => t;
  let abstract: unit => t;
  let singleton: Typ.t => t;
}

and Typ: {
  /** An (opaque / abstract) Hazel type.

     @see Hazel PHI 9 */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = pri Typ_syntax.t(Pos.absolute);

  /** SOURCE: Hazel type annotated with a relevant source location.
   Currently used to track match branches for inconsistent
   branches errors, but could perhaps be used more broadly
   for type debugging UI. */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type source = {
    id: int,
    ty: t,
  };

  /** SELF: The (synthetic) type information derivable from a term
   in isolation, using the typing context but not the syntactic
   context. This can either be Free (no type, in the case of
   free variables), Joined (a list of types, possibly
   inconsistent, generated by branching forms like ifs,
   matches, and list literals), or Just a regular type. */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type self =
    | Just(t)
    | Joined(list(source))
    | Multi
    | Free;

  /** MODE: The (analytic) type information derived from a term's
   syntactic context. This can either Syn (no type expectation),
   or Ana (a type expectation). It is conjectured [citation needed]
   that the Syn mode is functionally indistinguishable from
   Ana(Unknown(SynSwitch)), and that this type is thus vestigial. */
  [@deriving (show({with_path: false}), sexp, yojson)]
  type mode =
    | Syn
    | Ana(t);

  /** Returns the underlying AST of a [Typ]. */
  let to_syntax: t => Typ_syntax.t(Pos.absolute);

  /** Returns a [Typ] with the given underlying AST. */
  let of_syntax: Typ_syntax.t(Pos.absolute) => t;

  /** Scope preserving cross-context index shifting. */
  let rescope: (Ctx.t, t) => t;

  /* Type Constructor Functions */

  let unknown: Typ_syntax.type_provenance => t;
  let int: unit => t;
  let float: unit => t;
  let bool: unit => t;
  let list: t => t;
  let arrow: (t, t) => t;
  let product: list(t) => t;

  /* Type Constructor Predicates */

  let is_unknown: t => bool;
  let is_unknown_synswitch: t => bool;

  let is_int: t => bool;
  let is_float: t => bool;
  let is_tyvar: t => bool;

  /* Type Variables */

  /** Type variable substitution.  */
  let subst_tyvars: (Ctx.t, t, list((Ref.t, t))) => t;
};

include
   (module type of Typ) with
    type t = Typ.t and
    type source = Typ.source and
    type self = Typ.self and
    type mode = Typ.mode;

/** Strip location information from a list of sources */
let source_tys: list(source) => list(t);

/** Context-sensitive type consistency.

   Two types are consistent when each pair of coinciding subterms satisfies one of the following properties:

   - They are structurally equal, up to holes and type variables.
   - At least one is a type variable hole or a type variable of kind [Hole] or [Type].
   - If one is a type variable of kind [S], the other is consistent with its underlying type.
   - If both are type variables of kind [S], their underlying types are consistent. */
let consistent: (Ctx.t, t, t) => bool;

/** Context-sensitive type equivalence.

   Two types are equivalent when each pair of coinciding subterms satisfies one of the following properties:

   - They are structurally equal, up to type variables and type variable holes.
   - If one is a type variable hole, the other is a type variable hole with the same id. */
let equivalent: (Ctx.t, t, t) => bool;

/** A [Typ] is complete when it has no holes. */
let complete: t => bool;

/* Type Variables */

let tyvar: (Ctx.t, Idx.Abs.t, TyVar.t) => t;
let tyvar_ref: t => option(Ref.t);
let tyvar_name: t => option(TyVar.t);

/** Type variable substitution.  */
let subst_tyvars: (Ctx.t, t, list((Ref.t, t))) => t;

/* Joins */

/** Lattice join on types. This is a LUB join in the hazel2
   sense in that any type dominates Unknown */
let join: (Ctx.t, t, t) => option(t);

let join_all: (Ctx.t, list(t)) => option(t);

let join_or_fst: (Ctx.t, t, t) => t;

/** MATCHED JUDGEMENTS: Note that matched judgements work
   a bit different than hazel2 here since hole fixing is
   implicit. Somebody should check that what I'm doing
   here actually makes sense -Andrew */
let matched_arrow: t => (t, t);

let matched_arrow_mode: mode => (mode, mode);

let matched_prod_mode: (mode, int) => list(mode);

let matched_list: t => t;

let matched_list_mode: mode => mode;

let matched_list_lit_mode: (mode, int) => list(mode);

let ap_mode: mode;

/* Typ Normalization */

/** A normalized [Typ]. */
[@deriving sexp]
type normalized = Typ_syntax.t(Pos.absolute);

/** Coerces a normalized [Typ] to an ordinary [Typ]. */
let of_normalized: normalized => t;

/** Normalizes a [Typ].

   Replaces every type variable of kind [Singleton] with its (recursively normalized) underlying type. */
let normalize: (Ctx.t, t) => normalized;

/* Properties of Normalized Types */

/** Normalized [Typ] consistency.

   WARNING: This function assumes all type variables are of kind [Unknown] or [Abstract]. */
let normalized_consistent: (normalized, normalized) => bool;

/** Normalized [Typ] equivalence.

   WARNING: This function assumes all type variables are of kind [Unknown] or [Abstract]. */
let normalized_equivalent: (normalized, normalized) => bool;

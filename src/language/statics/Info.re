open Util;
open OptUtil.Syntax;

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

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type error_inconsistent =
  /* Self type (syn) inconsistent with expected type (ana) */
  | Expectation({
      ana: Typ.t,
      syn: Typ.t,
    })
  /* Inconsistent match or listlit */
  | Internal(list(Typ.t))
  /* Bad type equality due to function inside */
  | CompareFun(Typ.t);

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type error_no_type =
  /* Invalid expression token, treated as hole */
  | BadToken(string)
  /* Sum constructor neiter bound nor in ana type */
  | FreeConstructor(Constructor.t)
  /* Sort error used as label in tuple */
  | BadLabel(Any.t)
  /* Invalid label in tuple */
  | InvalidLabel(LabeledTuple.label, list(LabeledTuple.label))
  | UnexpectedLabelSort(LabeledTuple.label) /* A Label is present but not expected */;

/* Errors which can apply to either expression or patterns */
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type error_common =
  /* Underdetermined: No type can be assigned */
  | NoType(error_no_type)
  /* Overdetermined: Conflicting type expectations */
  | Inconsistent(error_inconsistent)
  /* The error on a specific duplicate label */
  | DuplicateLabel(LabeledTuple.label, Typ.t)
  | DuplicateVar(string, Typ.t)
  /* Tuple/TupLabel contains malformed labels, duplicate labels, and/or invalid labels */
  | TupleLabelError({
      malformed_labels: list(Any.t),
      duplicate_labels: list(LabeledTuple.label),
      invalid_labels: list(LabeledTuple.label),
      typ: Typ.t,
    });

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type error_exp =
  | FreeVariable(Var.t) /* Unbound variable (not in typing context) */
  | InexhaustiveMatch(
      option(error_common),
      [@equal Any.fast_equal] Grammar.any_t(IdTagged.IdTag.t),
    )
  | UnusedDeferral
  | BadPartialAp(Self.error_partial_ap)
  | Common(error_common)
  | BuiltinError(Self.error_builtin)
  | InvalidUseMode({
      bad_typ: Typ.t,
      inner_typ: Typ.t,
    })
  /* Livelit name not bound in ctx */
  | UnboundLivelit(string)
  /* Empty application of function with inconsistent type */
  | BadTrivAp(Typ.t)
  /* Dot Operator is ill-formed */
  | DotOperatorRequiresTuple
  /* Tuple extension requires both arguments to be tuples */
  | TupleExtensionRequiresTuples
  /* Invalid operator for current use mode, treated as hole */
  | BadOperator(string)
  /* Label not found in tuple for dot operator */
  | LabelNotFound(LabeledTuple.label, list(LabeledTuple.label))
  /* Bad Livelit model */
  | BadLivelitModel(Typ.t)
  /* Bad Theorem */
  | BadTheorem(Typ.t);

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type error_pat =
  | ExpectedConstructor /* Only construtors can be applied */
  | Redundant(option(error_pat))
  | Common(error_common);

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type ok_ana =
  /* The expected (ana) type and the self (syn) type are
     consistent, as witnessed by their meet type */
  | Consistent({
      ana: Typ.t,
      syn: Typ.t,
      meet: Typ.t,
    })
  /* A match expression or list literal which, in synthetic position,
     would be marked as internally inconsistent, but is considered
     fine as the expected type provides a consistent lower bound
     (often Unknown) for the types of the branches/elements */
  | InternallyInconsistent({
      ana: Typ.t,
      nomeet: list(Typ.t),
    });

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type ok_common =
  | Syn(Typ.t)
  | Ana(ok_ana);

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type ok_exp =
  | AnaDeferralConsistent(Typ.t)
  | Common(ok_common);

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type ok_pat = ok_common;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type status_common =
  | InHole(error_common)
  | NotInHole(ok_common);

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type status_exp =
  | InHole(error_exp)
  | NotInHole(ok_exp);

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type status_pat =
  | InHole(error_pat)
  | NotInHole(ok_pat);

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type status_variant =
  | Unique
  | Duplicate;

/* Expectation imposed on a type by the parent form.
   TODO: This is fundamentally syntactic and should
   eventually be reimplemeted via a seperate sort */
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type typ_expects =
  | TypeExpected
  | LabelExpected(status_variant, list(LabeledTuple.label)) // list of duplicate labels
  | LabelProjectionExpected(option(list(LabeledTuple.label))) // list of labels that exist on the product that is being projected
  | ProductExpected // Expects a product type (e.g. for product extension)
  | ConstructorExpected(status_variant, Typ.t)
  | VariantExpected(status_variant, Typ.t);

/* Type term errors
   TODO: The three additional errors statuses
   are fundamentally syntactic and should when
   possible be reimplemeted via a seperate sort */
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type error_typ =
  | BadToken(string) /* Invalid token, treated as type hole */
  | FreeTypeVariable(string) /* Free type variable */
  | DuplicateConstructor(Constructor.t) /* Duplicate ctr in same sum */
  | DuplicateLabels(list(LabeledTuple.label), Typ.t)
  | Duplicate(LabeledTuple.label, Typ.t)
  | WantTypeFoundAp
  | WantLabel
  | InvalidLabel(LabeledTuple.label, list(LabeledTuple.label))
  | WantProduct(Typ.t)
  | WantConstructorFoundType(Typ.t)
  | WantConstructorFoundAp
  | ParseFailure;

// Not an error mark on this type but on one of its children so we can not give a normalized type
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type underdetermined_typ =
  | ProdExtensionUnderdetermined(list(Typ.t))
  | ProdProjectionMissingLabel(LabeledTuple.label, list(LabeledTuple.label))
  | ProdProjectionBadArgs({
      product: option(Typ.t),
      label: option(Typ.t),
    });

/* Type ok statuses for cursor inspector */
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type ok_typ =
  | Variant(Constructor.t, Typ.t)
  | TypeAlias(string, Typ.t)
  | WHNormalizedTo({
      unnormalized: Typ.t,
      whnormalized: Typ.t,
    })
  | Type(Typ.t)
  | EmptyLabel
  | TypeUnderdetermined(underdetermined_typ);

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type status_typ =
  | InHole(error_typ)
  | NotInHole(ok_typ);

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type type_var_err =
  | Other
  | NotCapitalized;

/* What are we shadowing? */
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type shadow_src =
  | BaseTyp
  | TyAlias
  | TyVar;

/* Type pattern term errors */
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type error_tpat =
  | ShadowsType(string, shadow_src)
  | NotAVar(type_var_err);

/* Type pattern ok statuses for cursor inspector */
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type ok_tpat =
  | Empty
  | Var(string);

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type status_tpat =
  | NotInHole(ok_tpat)
  | InHole(error_tpat);

[@deriving (show({with_path: false}), sexp, yojson)]
type label_inference('a) =
  | SingletonLabelInference({
      label: LabeledTuple.label,
      pre_labeled_info: 'a,
    })
  | MultiLabelInference({
      reordered: bool,
      introduced_labels: list(LabeledTuple.label),
    });

[@deriving (show({with_path: false}), sexp, yojson)]
type exp = {
  term: Exp.t, /* The term under consideration */
  ancestors, /* Ascending list of containing term ids */
  ctx: Ctx.t, /* Typing context for the term */
  ana: Typ.t, /* Parental type expectations  */
  self: Self.exp, /* Expectation-independent type info */
  co_ctx: CoCtx.t, /* Locally free variables */
  cls: Cls.t, /* DERIVED: Syntax class (i.e. form name) */
  status: status_exp, /* DERIVED: Ok/Error statuses for display */
  ty: Typ.t, /* DERIVED: Type after nonempty hole fixing */
  label_inference: option(label_inference(exp)), /* Label inference information for the tuple */
  inferred_label: option(LabeledTuple.label), /* Inferred label for an expression within the tuple */
  label_sort: bool /* When in the position of a label */
};

[@deriving (show({with_path: false}), sexp, yojson)]
type pat = {
  term: Pat.t,
  ancestors,
  ctx: Ctx.t,
  co_ctx: CoCtx.t,
  prev_synswitch: option(Typ.t), // If a pattern is first synthesized, then analysed, the initial syn is stored here.
  ana: Typ.t,
  self: Self.pat,
  cls: Cls.t,
  status: status_pat,
  ty: Typ.t,
  constraint_: Coverage.Constraint.t,
  label_inference: option(label_inference(pat)),
  inferred_label: option(LabeledTuple.label),
  label_sort: bool /* When in the position of a label */
};

[@deriving (show({with_path: false}), sexp, yojson)]
type typ = {
  term: Typ.t,
  ancestors,
  ctx: Ctx.t,
  expects: typ_expects,
  cls: Cls.t,
  status: status_typ,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type tpat = {
  term: TPat.t,
  ancestors,
  ctx: Ctx.t,
  cls: Cls.t,
  status: status_tpat,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type secondary = {
  id: Id.t, // Id of term static info is sourced from
  cls: Cls.t, // Cls of secondary, not source term
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

[@deriving (show({with_path: false}), sexp, yojson, eq)]
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

let any_of: t => option(Any.t) =
  fun
  | InfoExp({term, _}) => Some(Exp(term))
  | InfoPat({term, _}) => Some(Pat(term))
  | InfoTyp({term, _}) => Some(Typ(term))
  | InfoTPat({term, _}) => Some(TPat(term))
  | Secondary(_) => None;

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

let parent_id_of: t => option(Id.t) =
  info => info |> ancestors_of |> ListUtil.hd_opt;

let id_of: t => Id.t =
  fun
  | InfoExp(i) => Exp.rep_id(i.term)
  | InfoPat(i) => Pat.rep_id(i.term)
  | InfoTyp(i) => Typ.rep_id(i.term)
  | InfoTPat(i) => TPat.rep_id(i.term)
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

/* A term is "typable" if it can meaningfully be assigned a type and will
   have a runtime value. This includes expressions and patterns, but excludes
   types, type patterns, and certain expression forms (deferrals, labels,
   type aliases) that don't produce useful values for probing/statics display. */
let is_typable_term: option(t) => bool =
  fun
  | Some(InfoExp({term: {term: Deferral(_) | Label(_) | TyAlias(_), _}, _})) =>
    false
  | Some(InfoTyp(_) | InfoTPat(_) | Secondary(_)) => false
  | Some(InfoExp(_) | InfoPat(_)) => true
  | None => false;

let exp_co_ctx: exp => CoCtx.t = ({co_ctx, _}) => co_ctx;
let exp_ty: exp => Typ.t = ({ty, _}) => ty;
let pat_ctx: pat => Ctx.t = ({ctx, _}) => ctx;
let pat_ty: pat => Typ.t = ({ty, _}) => ty;
let pat_constraint: pat => Coverage.Constraint.t =
  ({constraint_, _}) => constraint_;

let rec status_common =
        (ctx: Ctx.t, ty_ana: Typ.t, self: Self.t): status_common =>
  switch (self, ty_ana) {
  | (_, {term: TupLabel({term: ExplicitNonlabel, _}, ana_inner), _}) =>
    status_common(ctx, ana_inner, self)
  | (Just(ty), {term: Unknown(SynSwitch), _}) => NotInHole(Syn(ty))
  | (Just(syn), ana) =>
    switch (
      Typ.meet(
        ctx,
        ana,
        syn /* Note: the ordering of ana, syn matters */
      )
    ) {
    | None =>
      switch (ana.term, syn.term) {
      | (Label(_), _) =>
        InHole(
          Inconsistent(
            Expectation({
              ana,
              syn,
            }),
          ),
        )
      | _ =>
        InHole(
          Inconsistent(
            Expectation({
              ana,
              syn,
            }),
          ),
        )
      }
    | Some(meet) =>
      NotInHole(
        Ana(
          Consistent({
            ana,
            syn,
            meet,
          }),
        ),
      )
    }
  | (CompareFun(ty), _) => InHole(Inconsistent(CompareFun(ty)))
  | (FreeConstructor(name), _) => InHole(NoType(FreeConstructor(name)))
  | (BadToken(name), _) => InHole(NoType(BadToken(name)))
  | (BadLabel(label), _) => InHole(NoType(BadLabel(label)))
  | (ExplicitNonlabel, _) => NotInHole(Syn(Unknown(Internal) |> Typ.temp))
  | (UnexpectedLabelSort(label), _) =>
    InHole(NoType(UnexpectedLabelSort(label)))
  | (InvalidLabel(label, expected_labels), _) =>
    InHole(NoType(InvalidLabel(label, expected_labels)))
  | (
      TupleLabelError({
        malformed_labels,
        duplicate_labels,
        invalid_labels,
        typ,
      }),
      _,
    ) =>
    InHole(
      TupleLabelError({
        malformed_labels,
        duplicate_labels,
        invalid_labels,
        typ,
      }),
    )
  | (DuplicateLabel(lab, Just(ty)), _) => InHole(DuplicateLabel(lab, ty))
  | (DuplicateVar(lab, Just(ty)), _) => InHole(DuplicateVar(lab, ty))
  | (DuplicateLabel(lab, _), _) =>
    InHole(DuplicateLabel(lab, Unknown(Internal) |> Typ.temp))
  | (DuplicateVar(lab, _), _) =>
    InHole(DuplicateVar(lab, Unknown(Internal) |> Typ.temp))
  | (IsMulti, _) => NotInHole(Syn(Unknown(Internal) |> Typ.temp))
  | (NoMeet(PolyEq, tys), _)
  | (NoMeet(_, tys), {term: Unknown(SynSwitch), _}) =>
    InHole(Inconsistent(Internal(Typ.of_source(tys))))
  | (NoMeet(wrap, tys), ana) =>
    let syn: Typ.t = Self.meet_of(wrap, Unknown(Internal) |> Typ.temp);
    switch (Typ.meet(ctx, ana, syn)) {
    | None =>
      switch (ana.term, syn.term) {
      | (Label(_), Label(_)) =>
        InHole(
          Inconsistent(
            Expectation({
              ana,
              syn,
            }),
          ),
        )
      | (Label(_), _) => InHole(NoType(BadLabel(Typ(syn))))
      | _ =>
        InHole(
          Inconsistent(
            Expectation({
              ana,
              syn,
            }),
          ),
        )
      }
    | Some(_) =>
      NotInHole(
        Ana(
          InternallyInconsistent({
            ana,
            nomeet: Typ.of_source(tys),
          }),
        ),
      )
    };
  };

let rec status_pat = (ctx: Ctx.t, ty_ana: Typ.t, self: Self.pat): status_pat =>
  switch (self) {
  | Redundant(self) =>
    let additional_err =
      switch (status_pat(ctx, ty_ana, self)) {
      | InHole(
          Common(
            Inconsistent(Internal(_) | Expectation(_) | CompareFun(_)) |
            NoType(_) |
            DuplicateLabel(_) |
            DuplicateVar(_) |
            TupleLabelError(_),
          ) as err,
        ) =>
        Some(err)
      | NotInHole(_) => None
      | InHole(ExpectedConstructor | Redundant(_)) =>
        // ExpectedConstructor cannot be a reason to hole-wrap the entire pattern
        failwith("InHole(Redundant(impossible_err))")
      };
    InHole(Redundant(additional_err));
  | Common(FreeConstructor(name)) =>
    InHole(Common(NoType(FreeConstructor(name))))
  | ExpectedConstructor(_) => InHole(ExpectedConstructor)
  | Common(self_pat) =>
    switch (status_common(ctx, ty_ana, self_pat)) {
    | NotInHole(ok_pat) => NotInHole(ok_pat)
    | InHole(err_pat) => InHole(Common(err_pat))
    }
  };

/* Determines whether an expression or pattern is in an error hole,
   depending on the mode, which represents the expectations of the
   surrounding syntactic context, and the self which represents the
   makeup of the expression / pattern itself. */
let rec status_exp = (ctx: Ctx.t, ty_ana, self: Self.exp): status_exp =>
  switch (self) {
  | Free(name) => InHole(FreeVariable(name))
  | InexhaustiveMatch(self, example) =>
    let additional_err =
      switch (status_exp(ctx, ty_ana, self)) {
      | InHole(Common(Inconsistent(Internal(_)) as inconsistent_err)) =>
        Some(inconsistent_err)
      | NotInHole(_)
      | InHole(Common(Inconsistent(Expectation(_) | CompareFun(_)))) => None /* Type checking should fail and these errors would be nullified */
      | InHole(Common(NoType(_)))
      | InHole(Common(TupleLabelError(_)))
      | InHole(Common(DuplicateLabel(_)))
      | InHole(Common(DuplicateVar(_)))
      | InHole(TupleExtensionRequiresTuples)
      | InHole(
          FreeVariable(_) | InexhaustiveMatch(_) | UnusedDeferral |
          BadPartialAp(_) |
          InvalidUseMode(_) |
          UnboundLivelit(_) |
          BadTrivAp(_) |
          DotOperatorRequiresTuple |
          BadLivelitModel(_) |
          BadOperator(_) |
          LabelNotFound(_) |
          BadTheorem(_) |
          BuiltinError(_),
        ) =>
        failwith("InHole(InexhaustiveMatch(impossible_err))")
      };
    InHole(InexhaustiveMatch(additional_err, example));
  | IsDeferral(InAp) => NotInHole(AnaDeferralConsistent(ty_ana))
  | IsDeferral(_) => InHole(UnusedDeferral)
  | IsBadPartialAp(_ as info) => InHole(BadPartialAp(info))
  | BuiltinError(err) => InHole(BuiltinError(err))
  | InvalidUseMode({inner_typ, bad_typ}) =>
    InHole(
      InvalidUseMode({
        inner_typ,
        bad_typ,
      }),
    )
  | DotOperatorRequiresTuple => InHole(DotOperatorRequiresTuple)
  | IsLivelitName({name, _}) =>
    let ll = Ctx.lookup_livelit(ctx, name);
    switch (ll) {
    | None => InHole(UnboundLivelit(name))
    | Some(_livelit) =>
      NotInHole(Common(Syn(Unknown(Internal) |> Typ.temp)))
    };
  | TupleExtensionRequiresTuples => InHole(TupleExtensionRequiresTuples)
  | BadTrivAp(ty) => InHole(BadTrivAp(ty))
  | BadOperator(op) => InHole(BadOperator(op))
  | LabelNotFound(label, labels) => InHole(LabelNotFound(label, labels))
  | BadLivelitModel(typ) => InHole(BadLivelitModel(typ))
  | Common(self_exp) =>
    switch (status_common(ctx, ty_ana, self_exp)) {
    | NotInHole(ok_exp) => NotInHole(Common(ok_exp))
    | InHole(err_exp) => InHole(Common(err_exp))
    }
  | BadTheorem(typ) => InHole(BadTheorem(typ))
  };

/* This logic determines whether a type should be put
   in a hole or not. It's mostly syntactic, determining
   the proper placement of sum type variants and ctrs;
   this should be reimplemented in the future as a
   separate sort. It also determines semantic properties
   such as whether or not a type variable reference is
   free, and whether a ctr name is a dupe. */
let rec status_typ = (ctx: Ctx.t, expects: typ_expects, ty: Typ.t): status_typ => {
  switch (expects, ty.term) {
  | (_, Unknown(Hole(Invalid(token)))) => InHole(BadToken(token))
  | (LabelExpected(_), Unknown(Hole(EmptyHole))) => NotInHole(EmptyLabel)
  | (LabelProjectionExpected(_), Unknown(Hole(EmptyHole))) =>
    NotInHole(EmptyLabel)
  | (TypeExpected | ProductExpected, ProdProjection(pty, l)) =>
    switch (Typ.weak_head_normalize(ctx, pty), l.term) {
    | ({term: Prod(tys), _}, Label(l)) =>
      switch (Typ.project_type(tys, l)) {
      | Some(ty') =>
        NotInHole(
          WHNormalizedTo({
            unnormalized: ty,
            whnormalized: ty',
          }),
        )
      | None =>
        NotInHole(
          TypeUnderdetermined(
            ProdProjectionMissingLabel(
              l,
              List.filter_map(
                t => Typ.match_tup_label(t) |> Option.map(fst),
                tys,
              ),
            ),
          ),
        )
      }
    | (t1, _) =>
      NotInHole(
        TypeUnderdetermined(
          ProdProjectionBadArgs({
            product:
              switch (t1.term) {
              | Prod(_) => None
              | _ => Some(Typ.weak_head_normalize(ctx, ty))
              },
            label:
              switch (l.term) {
              | Label(_) => None
              | _ => Some(l)
              },
          }),
        ),
      )
    }
  | (TypeExpected | ProductExpected, ProdExtension(t1, t2)) =>
    switch (
      Typ.weak_head_normalize(ctx, t1).term,
      Typ.weak_head_normalize(ctx, t2).term,
    ) {
    | (Prod(t1s), Prod(t2s)) =>
      NotInHole(
        WHNormalizedTo({
          unnormalized: ty,
          whnormalized: Typ.product_extension(t1s, t2s) |> Typ.fresh,
        }),
      )
    | (Prod(_), _) =>
      NotInHole(TypeUnderdetermined(ProdExtensionUnderdetermined([t2])))
    | (_, Prod(_)) =>
      NotInHole(TypeUnderdetermined(ProdExtensionUnderdetermined([t1])))
    | _ =>
      NotInHole(TypeUnderdetermined(ProdExtensionUnderdetermined([t1, t2])))
    }
  | (ProductExpected, _) =>
    switch (Typ.weak_head_normalize(ctx, ty)) {
    | {term: Prod(_), _} as ty => NotInHole(Type(ty))
    | ty => InHole(WantProduct(ty))
    }
  | (_, Unknown(Hole(EmptyHole))) => NotInHole(Type(ty))
  | (_, Unknown(Hole(MultiHole(_tms)))) => InHole(ParseFailure)
  | (VariantExpected(Unique, sum_ty), Var(name))
  | (ConstructorExpected(Unique, sum_ty), Var(name)) =>
    NotInHole(Variant(name, sum_ty))
  | (VariantExpected(Duplicate, _), Var(name))
  | (ConstructorExpected(Duplicate, _), Var(name)) =>
    InHole(DuplicateConstructor(name))
  | (TypeExpected, Var(name)) =>
    switch (Ctx.is_alias(ctx, name)) {
    | false =>
      switch (Ctx.is_abstract(ctx, name)) {
      | false => InHole(FreeTypeVariable(name))
      | true => NotInHole(Type(Var(name) |> Typ.temp))
      }
    | true => NotInHole(TypeAlias(name, Typ.weak_head_normalize(ctx, ty)))
    }
  | (TypeExpected, Label(_))
  | (LabelExpected(Unique, _), Label(_)) => NotInHole(Type(ty))
  | (LabelExpected(Duplicate, dupes), Label(name)) =>
    List.exists(l => name == l, dupes)
      ? InHole(Duplicate(name, ty)) : InHole(WantLabel)
  | (LabelProjectionExpected(Some(labels)), Label(name)) =>
    List.mem(name, labels)
      ? NotInHole(Type(ty)) : InHole(InvalidLabel(name, labels))
  | (LabelProjectionExpected(None), Label(_)) =>
    NotInHole(Type(Unknown(Internal) |> Typ.temp)) // Unknown type because the product is unknown
  | (ConstructorExpected(_), Label(_))
  | (VariantExpected(_), Label(_)) => InHole(WantConstructorFoundType(ty))

  | (LabelExpected(_), _)
  | (LabelProjectionExpected(_), _) => InHole(WantLabel)
  | (ConstructorExpected(_), _)
  | (VariantExpected(_), _) => InHole(WantConstructorFoundType(ty))
  | (_, Parens(t)) => status_typ(ctx, expects, t)
  | (TypeExpected, _) => NotInHole(Type(ty))
  };
};

let status_tpat = (ctx: Ctx.t, utpat: TPat.t): status_tpat =>
  switch (utpat.term) {
  | EmptyHole => NotInHole(Empty)
  | Var(name) when Ctx.shadows_typ(ctx, name) =>
    let f = src => InHole(ShadowsType(name, src));
    if (Ctx.is_base_typ(name)) {
      f(BaseTyp);
    } else if (Ctx.is_alias(ctx, name)) {
      f(TyAlias);
    } else {
      f(TyVar);
    };
  | Var(name) => NotInHole(Var(name))
  | Invalid(_) => InHole(NotAVar(NotCapitalized))
  | MultiHole(_) => InHole(NotAVar(Other))
  };

/* Determines whether any term is in an error hole. */
let is_error = (ci: t): bool => {
  switch (ci) {
  | InfoExp({status, _}) =>
    switch (status) {
    | InHole(_) => true
    | NotInHole(_) => false
    }
  | InfoPat({status, _}) =>
    switch (status) {
    | InHole(_) => true
    | NotInHole(_) => false
    }
  | InfoTyp({status, _}) =>
    switch (status) {
    | InHole(_) => true
    | NotInHole(_) => false
    }
  | InfoTPat({status, _}) =>
    switch (status) {
    | InHole(_) => true
    | NotInHole(_) => false
    }
  | Secondary(_) => false
  };
};

/* Determined the type of an expression or pattern 'after hole fixing';
   that is, some ill-typed terms are considered to be 'wrapped in
   non-empty holes', i.e. assigned Unknown type. */
let fixed_typ_ok: ok_pat => Typ.t =
  fun
  | Syn(syn) => syn
  | Ana(Consistent({meet, _})) => meet
  | Ana(InternallyInconsistent({ana, _})) => ana;

let fixed_typ_err_common: (error_common, Typ.t) => Typ.t =
  (err, ana) => {
    let typ_or_ana = ty =>
      if (Typ.is_syn_plus(ana)) {
        ty;
      } else {
        ana;
      };
    switch (err) {
    | NoType(FreeConstructor(c)) =>
      typ_or_ana(
        Sum([
          ConstructorMap.Variant(
            c,
            ConstructorMap.mk_variant_ann(~ids=[Id.invalid], ()),
            None,
          ),
          ConstructorMap.BadEntry(Unknown(Internal) |> Typ.temp),
        ])
        |> Typ.temp,
      )
    | NoType(BadToken(_))
    | NoType(BadLabel(_))
    | NoType(InvalidLabel(_))
    | NoType(UnexpectedLabelSort(_)) => Unknown(Internal) |> Typ.temp
    | TupleLabelError({typ, _})
    | DuplicateLabel(_, typ)
    | DuplicateVar(_, typ) => typ_or_ana(typ)
    | Inconsistent(Expectation({ana, _})) => ana
    | Inconsistent(Internal(_)) => Unknown(Internal) |> Typ.temp // Should this be some sort of meet?
    | Inconsistent(CompareFun(_)) => typ_or_ana(Atom(Bool) |> Typ.temp)
    };
  };

let fixed_typ_err: (error_exp, Typ.t) => Typ.t =
  (err, ana) =>
    switch (err) {
    | UnboundLivelit(_)
    | FreeVariable(_)
    | UnusedDeferral
    | BadPartialAp(_)
    | InexhaustiveMatch(_)
    | DotOperatorRequiresTuple
    | BadOperator(_)
    | LabelNotFound(_, _)
    | TupleExtensionRequiresTuples
    | BuiltinError(
        ProjectLabelsMissingLabels(_) | MissingLabels(_) |
        PivotLabelIsNotString(_),
      )
    | BuiltinError(
        ArgumentMustBeTuple | ArgumentMustBeListOfTuples | AtLeast2Arguments |
        Exactly2Arguments,
      )
    | BadTrivAp(_) =>
      if (Typ.is_syn_plus(ana)) {
        Unknown(Internal) |> Typ.temp;
      } else {
        ana;
      }
    | BuiltinError(ToLvsMissingLabelsOnTuple(ty)) => ty
    | Common(err) => fixed_typ_err_common(err, ana)
    | InvalidUseMode({inner_typ, _}) => inner_typ
    | BadLivelitModel(ana)
    | BadTheorem(ana) => ana
    };

let fixed_typ_err_pat: (error_pat, Typ.t) => Typ.t =
  (err, ana) =>
    switch (err) {
    | ExpectedConstructor
    | Redundant(_) => Unknown(Internal) |> Typ.temp
    | Common(err) => fixed_typ_err_common(err, ana)
    };

let fixed_typ_pat = (ctx, ty_ana: Typ.t, self: Self.pat): Typ.t => {
  // TODO: get rid of unwrapping (probably by changing the implementation of error_exp.Redundant)
  let self =
    switch (self) {
    | Redundant(self) => self
    | _ => self
    };
  switch (status_pat(ctx, ty_ana, self)) {
  | InHole(err) => fixed_typ_err_pat(err, ty_ana)
  | NotInHole(ok) => fixed_typ_ok(ok)
  };
};

let fixed_typ_exp = (ctx, ty_ana: Typ.t, self: Self.exp): Typ.t =>
  switch (status_exp(ctx, ty_ana, self)) {
  | InHole(err) => fixed_typ_err(err, ty_ana)
  | NotInHole(AnaDeferralConsistent(ana)) => ana
  | NotInHole(Common(ok)) => fixed_typ_ok(ok)
  };

let derived_dynamic_self_common =
    (
      ~dynamics: DynamicStatics.Map.t,
      ~calculate_dynamic_type: Exp.t => option(Typ.t),
      ~ctx,
      ~term_id: Id.t,
      ~self_common: Self.t,
    )
    : Self.t => {
  switch (self_common) {
  | Just(t) when Typ.count_unknowns(t) > 0 =>
    switch (DynamicStatics.Map.lookup(term_id, dynamics)) {
    | None => self_common
    | Some([]) => self_common
    | Some(entry) =>
      let exps = List.map((s: DynamicStatics.sample) => s.exp, entry);
      let dyn_typs = OptUtil.traverse(calculate_dynamic_type, exps);
      let dyn_typ =
        Option.bind(
          dyn_typs,
          Typ.meet_all(~empty=Unknown(Internal) |> Typ.temp, ctx),
        );
      Just(dyn_typ |> Option.value(~default=Unknown(Internal) |> Typ.temp));
    }
  | _ => self_common
  };
};

let derived_dynamic_self_exp =
    (
      ~dynamics: DynamicStatics.Map.t,
      ~calculate_dynamic_type: Exp.t => option(Typ.t),
      ~ctx,
      ~uexp: Exp.t,
      ~self: Self.exp,
    )
    : Self.exp => {
  switch (self) {
  | Common(self_common) =>
    Common(
      derived_dynamic_self_common(
        ~dynamics,
        ~calculate_dynamic_type,
        ~ctx,
        ~term_id=uexp |> Exp.rep_id,
        ~self_common,
      ),
    )
  | _ => self
  };
};

/* Add derivable attributes for expression terms */
let derived_exp =
    (
      ~calculate_dynamic_type: Exp.t => option(Typ.t),
      ~dynamics: DynamicStatics.Map.t,
      ~uexp: Exp.t,
      ~ctx,
      ~ana,
      ~ancestors,
      ~self: Self.exp,
      ~co_ctx,
      ~label_inference: option(label_inference(exp)),
      ~inferred_label: option(LabeledTuple.label),
      ~label_sort,
    )
    : exp => {
  let self: Self.exp =
    derived_dynamic_self_exp(
      ~calculate_dynamic_type,
      ~dynamics,
      ~ctx,
      ~uexp,
      ~self,
    );

  let cls = Cls.Exp(Exp.cls_of_term(uexp.term));
  let status = status_exp(ctx, ana, self);

  let ty = fixed_typ_exp(ctx, ana, self);
  {
    cls,
    self,
    ty,
    ana,
    status,
    ctx,
    co_ctx,
    ancestors,
    term: uexp,
    label_inference,
    inferred_label,
    label_sort,
  };
};

/* Add derivable attributes for pattern terms */
let derived_dynamic_self_pat =
    (
      ~dynamics: DynamicStatics.Map.t,
      ~calculate_dynamic_type: Exp.t => option(Typ.t),
      ~ctx,
      ~upat: Pat.t,
      ~self: Self.pat,
    )
    : Self.pat => {
  switch (self) {
  | Common(self_common) =>
    Common(
      derived_dynamic_self_common(
        ~dynamics,
        ~calculate_dynamic_type,
        ~ctx,
        ~term_id=upat |> Pat.rep_id,
        ~self_common,
      ),
    )
  | _ => self
  };
};
let derived_pat =
    (
      ~dynamics: DynamicStatics.Map.t,
      ~calculate_dynamic_type: Exp.t => option(Typ.t),
      ~upat: Pat.t,
      ~ctx,
      ~co_ctx,
      ~prev_synswitch,
      ~ana,
      ~ancestors,
      ~self: Self.pat,
      ~constraint_,
      ~label_inference,
      ~inferred_label,
      ~label_sort,
    )
    : pat => {
  let self: Self.pat =
    derived_dynamic_self_pat(
      ~calculate_dynamic_type,
      ~dynamics,
      ~ctx,
      ~upat,
      ~self,
    );

  let cls = Cls.Pat(Pat.cls_of_term(upat.term));
  let status = status_pat(ctx, ana, self);
  let ty = fixed_typ_pat(ctx, ana, self);

  // replace constraints with Hole if this info has an error
  let constraint_': Coverage.Constraint.t =
    switch (constraint_, status) {
    | (Coverage.Constraint.Hole(_), _) => constraint_
    | (_, InHole(_)) => Hole(Some(constraint_))
    | (_, NotInHole(_)) => constraint_
    };

  {
    cls,
    self,
    prev_synswitch,
    ana,
    ty,
    status,
    ctx,
    co_ctx,
    ancestors,
    term: upat,
    constraint_: constraint_',
    label_inference,
    inferred_label,
    label_sort,
  };
};

/* Add derivable attributes for types */
let derived_typ = (~utyp: Typ.t, ~ctx, ~ancestors, ~expects): typ => {
  let cls: Cls.t =
    /* Hack to improve CI display */
    switch (expects, Typ.cls_of_term(utyp.term)) {
    | (VariantExpected(_) | ConstructorExpected(_), Var) =>
      Cls.Typ(Constructor)
    | (_, cls) => Cls.Typ(cls)
    };
  let status = status_typ(ctx, expects, utyp);
  {
    cls,
    ctx,
    ancestors,
    status,
    expects,
    term: utyp,
  };
};

/* Add derivable attributes for type patterns */
let derived_tpat = (~utpat: TPat.t, ~ctx, ~ancestors): tpat => {
  let cls = Cls.TPat(TPat.cls_of_term(utpat.term));
  let status = status_tpat(ctx, utpat);
  {
    cls,
    ancestors,
    status,
    ctx,
    term: utpat,
  };
};

/* If the info represents some kind of name binding which
   exists in the context, return the id where the binding occurs */
let get_binding_site = (info: t): option(Id.t) => {
  switch (info) {
  | InfoExp({term: {term: Var(name), _}, ctx, _}) =>
    let* entry = Ctx.lookup_var(ctx, name);
    entry.id == Id.invalid ? None : Some(entry.id);
  | InfoExp({term: {term: Constructor(name, _), _}, ctx, _})
  | InfoPat({term: {term: Constructor(name, _), _}, ctx, _}) =>
    let* entry = Ctx.lookup_ctr(ctx, name);
    entry.id == Id.invalid ? None : Some(entry.id);
  | InfoTyp({term: {term: Var(name), _}, ctx, _}) =>
    let* id = Ctx.lookup_tvar_id(ctx, name);
    id == Id.invalid ? None : Some(id);
  | _ => None
  };
};

let typ_is_constructor_expected = t =>
  switch (t) {
  | {expects: ConstructorExpected(_) | VariantExpected(_), _} => true
  | _ => false
  };

let rec pre_labeled_info = (info: t): t =>
  switch (info) {
  | InfoExp({
      label_inference:
        Some(SingletonLabelInference({pre_labeled_info: pli, _})),
      _,
    }) =>
    pre_labeled_info(InfoExp(pli))
  | InfoPat({
      label_inference:
        Some(SingletonLabelInference({pre_labeled_info: pli, _})),
      _,
    }) =>
    pre_labeled_info(InfoPat(pli))
  | _ => info
  };

// We should just carry this through the rearranging in statics rather than recomputing it
let derive_label_inference_info = (original_labels, new_labels) => {
  let introduced_labels =
    List.filter(
      l => !List.mem(l, List.filter_map(Fun.id, original_labels)),
      List.filter_map(Fun.id, new_labels),
    );
  let reordered =
    !
      List.equal(
        (a, b) => {
          switch (a, b) {
          | (Some(a), Some(b)) => a == b
          | (Some(a), None) => List.mem(a, introduced_labels) // If we introduce a label, we don't consider it reordered
          | (None, Some(_)) => false
          | (None, None) => true
          }
        },
        new_labels,
        original_labels,
      );

  MultiLabelInference({
    reordered,
    introduced_labels,
  });
};

let is_label = (info: t): bool =>
  switch (info) {
  | InfoTyp({status: NotInHole(EmptyLabel), _})
  | InfoTyp({term: {term: Label(_), _}, _})
  | InfoExp({term: {term: Label(_), _}, _})
  | InfoPat({term: {term: Label(_), _}, _})
  | InfoPat({label_sort: true, _})
  | InfoExp({label_sort: true, _}) => true
  | _ => false
  };

/* Extract the projector kind from an info, if it represents a projector term */
let projector_kind_of = (info: t): option(ProjectorKind.t) =>
  switch (info) {
  | InfoExp({term: {term: Projector({kind, _}, _), _}, _}) => Some(kind)
  | InfoPat({term: {term: Projector({kind, _}, _), _}, _}) => Some(kind)
  | InfoTyp({term: {term: Projector({kind, _}, _), _}, _}) => Some(kind)
  | _ => None
  };

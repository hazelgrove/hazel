open Util;

/* MARK.re — error marks from statics (unified Mark.t).
   Statics passes elab_syn_ty + list(t); principal type is elab_syn_ty, not carried on marks. */

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type meet_type =
  | Id
  | List
  | PolyEq;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type error_partial_ap =
  | NoDeferredArgs
  | ArityMismatch({
      expected: int,
      actual: int,
    });

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type error_builtin =
  | ToLvsMissingLabelsOnTuple(Typ.t)
  | ProjectLabelsMissingLabels(list(string))
  | MissingLabels(list(string))
  | PivotLabelIsNotString(Typ.t)
  | ArgumentMustBeTuple
  | ArgumentMustBeListOfTuples
  | AtLeast2Arguments
  | Exactly2Arguments;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type tpat_shadow_src =
  | BaseTyp
  | TyAlias
  | TyVar;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type tpat_var_err =
  | Other
  | NotCapitalized;

/* NOTE: Declaration order is load-bearing.
   The priority of a mark (for cursor inspector / error printer selection)
   is determined by its position in this type: earlier-declared constructors
   have higher priority. The `compare` function below uses
   `Variants.to_rank` (derived by ppx_variants_conv), which returns the
   zero-based declaration index — so reordering this type reorders priority.
   Do not reorder without understanding the impact on error selection. */
/* Which side of a failed comparison an escaped abstract type was on: the
   type required here, the type the expression has, or both, as two different
   escapes of the same path, which is what two calls of one function
   produce. */
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type escaped_side =
  | Required
  | Supplied
  | TwoDifferent;

[@deriving (show({with_path: false}), sexp, yojson, variants)]
type t =
  | BuiltinError(error_builtin)
  | Free(Var.t)
  | IsBadPartialAp(error_partial_ap)
  | InexhaustiveMatch(Typ.t, list(t), Grammar.any_t(IdTagged.IdTag.t))
  | InvalidUseMode({
      bad_typ: Typ.t,
      inner_typ: Typ.t,
    })
  | BadTrivAp(Typ.t)
  | DotOperatorRequiresTuple
  | TupleExtensionRequiresTuples
  | LabelNotFound(LabeledTuple.label, list(LabeledTuple.label))
  | ModuleMissingMembers(list(Var.t))
  /* `M.y` where the module has no value member y: on the label, while the
     dot carries a message. [type_member]: y is one of its type members. */
  | ModuleMemberNotFound({
      name: Var.t,
      members: list(Var.t),
      type_member: bool,
    })
  /* A signature declares member [name] more than once; on the later item.
     Values and modules share a namespace, type members have their own. */
  | SigDuplicateMember({
      name: Var.t,
      type_member: bool,
    })
  | BadOperator(string)
  | BadLivelitModel(Typ.t)
  | BadTheorem(Typ.t)
  | IsLivelitName({
      name: string,
      exp_t: Typ.t,
    })
  | ExpectationMismatch({
      ana: Typ.t,
      syn: Typ.t,
    })
  | ModuleTypeMemberMismatch({
      name: Var.t,
      expected: Typ.t,
      actual: Typ.t,
    })
  /* An abstract type that escaped the scope of the module it came from
     ([path], the projection it was written as) meets a type it cannot be
     consistent with. */
  | EscapedType({
      path: string,
      side: escaped_side,
    })
  | BadToken(string)
  | BadLabel(Any.t)
  | InvalidLabel(LabeledTuple.label, list(LabeledTuple.label))
  | UnexpectedLabelSort(LabeledTuple.label)
  | ExplicitNonlabel
  | TPatShadowsType(string, tpat_shadow_src)
  | TPatNotAVar(tpat_var_err)
  | TypFreeTypeVariable(string)
  | TypDuplicateConstructor(Constructor.t)
  | TypDuplicateLabels(list(LabeledTuple.label), Typ.t)
  | TypWantTypeFoundAp
  | TypWantLabel
  | TypWantProduct(Typ.t)
  /* `M.T` where the module has no type member T, or ([submodule]) `M.P.T`
     where it has no sub-module P: on the label. */
  | ModuleTypeMemberNotFound({
      name: Var.t,
      members: list(Var.t),
      submodule: bool,
    })
  /* `m.T` where m is a value that is not a module. */
  | TypWantModule({
      name: Var.t,
      typ: Typ.t,
    })
  /* `S.T` where S is a signature alias and T is abstract in it: no module is
     named, so there is no T to name. */
  | TypAbstractMemberOfSignature(Var.t)
  | TypWantConstructorFoundType(Typ.t)
  | TypWantConstructorFoundAp
  | TypParseFailure
  | TupleLabelError({
      malformed_labels: list(Any.t),
      duplicate_labels: list(LabeledTuple.label),
      invalid_labels: list(LabeledTuple.label),
      typ: Typ.t,
    })
  | IsDeferral(Exp.deferral_position)
  | FreeConstructor(Constructor.t)
  | CompareFun(Typ.t)
  | NoMeet(meet_type, list(Typ.source))
  | Redundant
  | ExpectedConstructor
  | IsMulti
  | DuplicateLabel(LabeledTuple.label, Typ.t)
  | DuplicateVar(string, Typ.t);

/* Declaration-order tag index, derived by ppx_variants_conv. */
let compare = (a: t, b: t): int =>
  Int.compare(Variants.to_rank(a), Variants.to_rank(b));

/* Earliest-declared variant wins (highest priority). */
let highest = (marks: list(t)): option(t) =>
  switch (marks) {
  | [] => None
  | [h, ...tl] =>
    Some(
      List.fold_left(
        (best, cur) => compare(cur, best) < 0 ? cur : best,
        h,
        tl,
      ),
    )
  };

/* Determines whether a set of marks represents a syntax error (bad token or
   parse failure) as opposed to a static type error. Dispatches on sort because
   Exp/Pat look at the highest-ranked mark, while Typ looks for any BadToken or
   TypParseFailure anywhere in the list. */
let is_syntax_error = (sort: Sort.t, marks: list(t)): bool =>
  switch (sort) {
  | Exp
  | Pat =>
    switch (highest(marks)) {
    | Some(BadToken(_) | IsMulti) => true
    | _ => false
    }
  | Typ =>
    List.exists(
      fun
      | BadToken(_)
      | TypParseFailure => true
      | _ => false,
      marks,
    )
  | _ => false
  };

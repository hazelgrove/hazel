open Util;

/* MARK.re — error marks from statics (unified Mark.t).
   Statics passes syn_ty + list(t); principal type is syn_ty, not carried on marks. */

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

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  /* “Core” marks (meet / label / hole shape); shared by exp and pat statics */
  | NoMeet(meet_type, list(Typ.source))
  | DuplicateLabel(LabeledTuple.label, Typ.t)
  | CompareFun(Typ.t)
  | DuplicateVar(string, Typ.t)
  | BadToken(string)
  | BadLabel(Any.t)
  | InvalidLabel(LabeledTuple.label, list(LabeledTuple.label))
  | UnexpectedLabelSort(LabeledTuple.label)
  | TupleLabelError({
      malformed_labels: list(Any.t),
      duplicate_labels: list(LabeledTuple.label),
      invalid_labels: list(LabeledTuple.label),
      typ: Typ.t,
    })
  | IsMulti
  | FreeConstructor(Constructor.t)
  | ExplicitNonlabel
  | ExpectationMismatch({
      ana: Typ.t,
      syn: Typ.t,
    })
  /* Expression-only */
  | Free(Var.t)
  | InexhaustiveMatch(Typ.t, list(t), Grammar.any_t(IdTagged.IdTag.t))
  | IsDeferral(Exp.deferral_position)
  | IsBadPartialAp(error_partial_ap)
  | BuiltinError(error_builtin)
  | InvalidUseMode({
      bad_typ: Typ.t,
      inner_typ: Typ.t,
    })
  | IsLivelitName({
      name: string,
      exp_t: Typ.t,
    })
  | BadTrivAp(Typ.t)
  | DotOperatorRequiresTuple
  | TupleExtensionRequiresTuples
  | LabelNotFound(LabeledTuple.label, list(LabeledTuple.label))
  | BadOperator(string)
  | BadLivelitModel(Typ.t)
  | BadTheorem(Typ.t)
  /* Pattern-only */
  | Redundant
  | ExpectedConstructor
  /* Type position (InfoTyp) — from Statics.derive_typ_status */
  | TypFreeTypeVariable(string)
  | TypDuplicateConstructor(Constructor.t)
  | TypDuplicateLabels(list(LabeledTuple.label), Typ.t)
  | TypWantTypeFoundAp
  | TypWantLabel
  | TypWantProduct(Typ.t)
  | TypWantConstructorFoundType(Typ.t)
  | TypWantConstructorFoundAp
  | TypParseFailure
  /* TPat position (InfoTPat) — mirror Message.error_tpat */
  | TPatShadowsType(string, tpat_shadow_src)
  | TPatNotAVar(tpat_var_err);

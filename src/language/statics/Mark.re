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
  | BadToken(string)
  | BadLabel(Any.t)
  | InvalidLabel(LabeledTuple.label, list(LabeledTuple.label))
  | UnexpectedLabelSort(LabeledTuple.label)
  | ExplicitNonlabel
  | TPatShadowsType(string, tpat_shadow_src)
  | TPatNotAVar(tpat_var_err)
  /* The `T(a, b)` parameter-list form (a `TPat.Param`) is only valid
     as the outermost tpat of a type alias declaration:
     `type T(a, b) = ...`. It is rejected as a sub-tpat (e.g.
     `type T(A(a)) = ...`) and as the binder of `poly`, `typfun`,
     `typlam`, or `rec` (e.g. `poly A(a) -> ...`). The associated
     string is the head name (or `"?"` if the head is a hole). */
  | TPatParamNotAtAliasHead(string)
  | TypFreeTypeVariable(string)
  | TypKindMismatch({
      expected: TypKind.t,
      actual: TypKind.t,
    })
  | TypParamApplyNonArrowKind(TypKind.t)
  | TypParamApplyArityMismatch({
      callee: Typ.t,
      callee_kind: TypKind.t,
      expected: int,
      actual: int,
    })
  /* Value-level type abstraction (`abs`) applied with the wrong
     number of type arguments at `e@<T>`. `expected` is the number
     of binders on the function's `Poly` type (with multi-binder
     `Poly(TPat.Tuple([…]), …)` flattened via `TPat.binders_of`);
     `actual` is the number of types provided in the application's
     `TypTuple` (or 1 for a single-arg `e@<T>`). The mark sits on
     the `TypAp` node and the result type is `Unknown` (so the
     surrounding application doesn't surface a downstream
     mismatch on `e`'s arguments using free type variables that
     leaked from a partially-substituted body). */
  | TypAbsApplyArityMismatch({
      expected: int,
      actual: int,
    })
  | TypDuplicateConstructor(Constructor.t)
  | TypDuplicateLabels(list(LabeledTuple.label), Typ.t)
  | TypWantTypeFoundAp
  | TypWantLabel
  | TypWantProduct(Typ.t)
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

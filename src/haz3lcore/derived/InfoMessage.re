/**
 * InfoMessage: Module for generating and formatting informational messages from static analysis.
 *
 * This module processes static analysis results (Info.t) and converts them into structured
 * messages that can be displayed in the UI or printed as strings. It handles both error
 * conditions and success/informational states.
 *
 * Used by:
 * - CursorInspector: For displaying real-time feedback in the editor UI
 * - ErrorPrint: For generating string representations of static errors
 */
open Util;
open Language;

type fragment =
  | Text(string)
  | Code(string)
  | Type(Typ.t)
  | Term(Any.t)
  | Label(string);

type message = {
  is_error: bool,
  fragments: list(fragment),
};

let build_common_err =
    (
      ~introduced_labels: list(LabeledTuple.label),
      ~lifted_ty: option(Typ.t),
      ~inferred_label: option(LabeledTuple.label),
      cls: Cls.t,
      err: Info.error_common,
    )
    : list(fragment) => {
  let fragments =
    (
      switch (err) {
      | NoType(BadToken(token)) =>
        switch (Token.bad_token_cls(token)) {
        | BadInt => [Text("Integer is too large or too small")]
        | Other => [
            Text(Printf.sprintf("\"%s\" isn't a valid token", token)),
          ]
        }
      | NoType(BadLabel(label)) => [
          Text("Malformed Label: "),
          Term(label),
        ]
      | NoType(FreeConstructor(name)) => [Code(name), Text("not found")]

      | NoType(InvalidLabel(name, expected_labels)) =>
        switch (expected_labels) {
        | [] => [
            Text("Invalid label: "),
            Label(name),
            Text(". No labels were expected."),
          ]
        | _ => [
            Text("Invalid label: "),
            Label(name),
            Text(" is not part of the expected labels: "),
            ...ListUtil.join(
                 Text(", "),
                 List.map(s => Code(s), expected_labels),
               ),
          ]
        }
      | NoType(UnexpectedLabelSort(name)) => [
          Text("Label "),
          Label(name),
          Text(" is here, but another sort is expected."),
        ]

      | TupleLabelError({
          malformed_labels,
          duplicate_labels,
          invalid_labels,
          _,
        }) =>
        (
          List.is_empty(malformed_labels)
            ? []
            : [
              Text("Malformed labels: "),
              ...ListUtil.join(
                   Text(", "),
                   List.map(t => Term(t), malformed_labels),
                 ),
            ]
        )
        @ (
          List.is_empty(duplicate_labels)
            ? []
            : [
              Text("Duplicate labels: "),
              ...ListUtil.join(
                   Text(", "),
                   List.map(s => Code(s), duplicate_labels),
                 ),
            ]
        )
        @ (
          List.is_empty(invalid_labels)
            ? []
            : [
              Text("Invalid labels: "),
              ...ListUtil.join(
                   Text(", "),
                   List.map(s => Code(s), invalid_labels),
                 ),
            ]
        )
      | DuplicateLabel(name, _) => [Text("Duplicate Label:"), Label(name)]
      | Inconsistent(CompareFun(ty)) => [
          Text("values cannot be compared:"),
          Type(ty),
        ]
      | Inconsistent(WithArrow(typ)) => [
          Text(":"),
          Type(typ),
          Text("inconsistent with arrow type"),
        ]
      | Inconsistent(Expectation({ana, syn})) =>
        switch (syn.term, ana.term) {
        | (Label(syn_l), Label(an_label)) => [
            Code(syn_l),
            Text("but expected label"),
            Code(an_label),
          ]
        | _ =>
          [
            Text(":"),
            Type(syn),
            Text("inconsistent with expected type"),
            Type(ana),
          ]
          @ (
            switch (lifted_ty) {
            | None => []
            | Some(lifted) => [Text(" lifted to"), Type(lifted)]
            }
          )
          @ (
            switch (introduced_labels) {
            | [] => []
            | [a] => [Text("after automatically added label "), Code(a)]
            | _ => [
                Text("after automatically added labels "),
                ...ListUtil.join(
                     Text(", "),
                     List.map(s => Code(s), introduced_labels),
                   ),
              ]
            }
          )
        }
      | Inconsistent(Internal(tys)) => [
          Text(
            (
              switch (cls) {
              | Exp(Match | If) => "Branches"
              | Exp(ListLit)
              | Pat(ListLit) => "Elements"
              | Exp(ListConcat)
              | Exp(BinOp(Poly(_))) => "Operands"
              | cls =>
                failwith(
                  "elements_noun: " ++ Cls.show(cls) ++ " cls has no elements",
                )
              }
            )
            ++ " have inconsistent types:",
          ),
          ...ListUtil.join_map(Text(","), ty => [Type(ty)], tys),
        ]
      }
    )
    @ (
      switch (inferred_label) {
      | None => []
      | Some(l) => [Text(" for label "), Label(l)]
      }
    );
  fragments;
};

let build_common_ok =
    (
      ~reordered: bool,
      ~introduced_labels: list(LabeledTuple.label),
      ~lifted_ty: option(Typ.t),
      ~inferred_label: option(LabeledTuple.label),
      ~label_sort: bool,
      cls: Cls.t,
      ok: Info.ok_common,
    )
    : list(fragment) => {
  let fragments =
    (
      switch (cls, ok) {
      | (Pat(EmptyHole), _) when label_sort => []
      | (Exp(EmptyHole), _) when label_sort => []
      | (Pat(ExplicitNonlabel), _) when label_sort => [
          Text("Explicitly unlabeled entry"),
        ]
      | (Exp(ExplicitNonlabel), _) when label_sort => [
          Text("Explicitly unlabeled entry"),
        ]
      | (Exp(MultiHole) | Pat(MultiHole), _) => [
          Text("Expecting operator or delimiter"),
        ]
      | (Exp(EmptyHole), Syn(_)) => [Text("Fillable by any expression")]
      | (Pat(EmptyHole), Syn(_)) => [Text("Fillable by any pattern")]
      | (Exp(EmptyHole), Ana(Consistent({ana, _}))) => [
          Text("Fillable by any expression of type"),
          Type(ana),
        ]
      | (Pat(EmptyHole), Ana(Consistent({ana, _}))) => [
          Text("Fillable by any pattern of type"),
          Type(ana),
        ]
      | (_, Syn(syn)) =>
        switch (syn.term) {
        | Label(l) => [Label(l)]
        | _ => [Text(":"), Type(syn)]
        }
      | (Pat(Var) | Pat(Wild), Ana(Consistent({ana, _}))) => [
          Text(":"),
          Type(ana),
        ]
      | (_, Ana(Consistent({ana, syn, _})))
          when Equality.semantic.typ(ana, syn) =>
        switch (syn.term) {
        | Label(l) => [Label(l), Text(" is a valid label")]
        | _ =>
          [Text(":"), Type(syn)]
          @ [Text("equals expected type")]
          @ (
            switch (lifted_ty) {
            | None => []
            | Some(lifted) => [Text(" lifted to"), Type(lifted)]
            }
          )
          @ (
            switch (introduced_labels) {
            | [] => []
            | [a] => [Text("by automatically adding label "), Code(a)]
            | _ => [
                Text("by automatically adding labels "),
                ...ListUtil.join_map(
                     Text(", "),
                     s => [Code(s)],
                     introduced_labels,
                   ),
              ]
            }
          )
          @ (
            switch (reordered) {
            | false => []
            | true => [Text(" after reordering by labels ")]
            }
          )
        }
      | (_, Ana(Consistent({ana, syn, _}))) =>
        (
          switch (syn.term) {
          | Label(l) => [Code(l), Text(" is a valid label")]
          | _ => [
              Text(":"),
              Type(syn),
              Text("consistent with expected type"),
            ]
          }
        )
        @ [Type(ana)]
        @ (
          switch (lifted_ty) {
          | None => []
          | Some(lifted) => [Text(" lifted to"), Type(lifted)]
          }
        )
        @ (
          switch (introduced_labels) {
          | [] => []
          | [a] => [Text("by automatically adding label "), Code(a)]
          | _ => [
              Text("by automatically adding labels "),
              ...ListUtil.join_map(
                   Text(", "),
                   s => [Code(s)],
                   introduced_labels,
                 ),
            ]
          }
        )
        @ (
          switch (reordered) {
          | false => []
          | true => [Text(" after reordering by labels ")]
          }
        )
      | (_, Ana(InternallyInconsistent({ana, nojoin: tys}))) =>
        [
          Text(
            (
              switch (cls) {
              | Exp(Match | If) => "Branches"
              | Exp(ListLit)
              | Pat(ListLit) => "Elements"
              | Exp(ListConcat)
              | Exp(BinOp(Poly(_))) => "Operands"
              | cls =>
                failwith(
                  "elements_noun: " ++ Cls.show(cls) ++ " cls has no elements",
                )
              }
            )
            ++ " have inconsistent types:",
          ),
          ...ListUtil.join_map(Text(", "), ty => [Type(ty)], tys),
        ]
        @ [Text("but consistent with expected"), Type(ana)]
      }
    )
    @ (
      switch (inferred_label) {
      | None => []
      | Some(l) => [Text(" for label "), Label(l)]
      }
    );
  fragments;
};
let build_underdetermined_typ =
    (underdetermined: Info.underdetermined_typ): list(fragment) => {
  switch (underdetermined) {
  | ProdExtensionUnderdetermined(tys) => [
      Text("Cannot determine type of product extension with argument types:"),
      ...ListUtil.join(Text(","), List.map(x => Type(x), tys)),
    ]
  | ProdProjectionMissingLabel(label, labels) => [
      Text("Cannot project label "),
      Label(label),
      Text(". Valid labels are: "),
      ...List.map(x => Label(x), labels),
    ]
  | ProdProjectionBadArgs({product, label}) =>
    let product_error =
      switch (product) {
      | Some(ty) => [Text("type"), Type(ty), Text("is not a tuple type")]
      | None => []
      };
    let label_error =
      switch (label) {
      | Some(ty) => [
          Text("label"),
          Type(ty),
          Text("is not a valid label: "),
        ]
      | None => []
      };

    [Text("Cannot determine projected type because ")]
    @ (
      ListUtil.join(
        [Text(" and ")],
        [product_error, label_error] |> List.filter(x => x != []),
      )
      |> List.concat
    );
  };
};

let build_typ_ok = (cls: Cls.t, ok: Info.ok_typ): list(fragment) => {
  let fragments =
    switch (ok) {
    | EmptyLabel => []
    | Type(_) when cls == Typ(EmptyHole) => [Text("Fillable by any type")]
    | Type(ty) =>
      [Type(ty)]
      @ (
        switch (cls) {
        | Typ(Label) => []
        | _ => [Text("is a type")]
        }
      )

    | TypeAlias(name, ty_lookup) => [
        Type(Var(name) |> Typ.fresh),
        Text("is an alias for"),
        Type(ty_lookup),
      ]
    | Variant(name, sum_ty) => [
        Type(Var(name) |> Typ.fresh),
        Text("is a sum type constuctor of type"),
        Type(sum_ty),
      ]
    | VariantIncomplete(sum_ty) => [
        Text("An incomplete sum type constuctor of type"),
        Type(sum_ty),
      ]
    | WHNormalizedTo({unnormalized, whnormalized}) => [
        Type(unnormalized),
        Text("is equal to"),
        Type(whnormalized),
      ]
    | TypeUnderdetermined(ty) => build_underdetermined_typ(ty)
    };
  fragments;
};

let build_typ_err = (ok: Info.error_typ): list(fragment) => {
  let fragments =
    switch (ok) {
    | FreeTypeVariable(name) => [
        Type(Var(name) |> Typ.fresh),
        Text("not found"),
      ]
    | BadToken(token) => [Code(token), Text("not a type or type operator")]
    | WantConstructorFoundAp
    | WantConstructorFoundType(_) => [Text("Expected a constructor")]
    | WantTypeFoundAp => [Text("Must be part of a sum type")]
    | WantLabel => [Text("Expect a valid label")]
    | DuplicateLabels(labels, _) => [
        Text("Duplicate labels within tuple: "),
        ...List.map(s => Label(s), labels),
      ]
    | Duplicate(name, _) => [Text("Duplicate Label: "), Label(name)]
    | DuplicateConstructor(name) => [
        Type(Var(name) |> Typ.fresh),
        Text("already used in this sum"),
      ]
    | ParseFailure => [Text("Parse failure")]
    | InvalidLabel(name, expected_labels) =>
      switch (expected_labels) {
      | [] => [
          Text("Invalid label: "),
          Label(name),
          Text(". No labels were expected."),
        ]
      | _ => [
          Text("Invalid label: "),
          Label(name),
          Text(" is not part of the expected labels: "),
          ...List.map(x => Label(x), expected_labels),
        ]
      }

    | WantProduct(ty) => [
        Text("Expected a tuple type, found type"),
        Type(ty),
      ]
    };
  fragments;
};

let build_exp_message = (info: Info.exp): message => {
  let introduced_labels =
    switch (info.label_inference) {
    | Some(MultiLabelInference({introduced_labels, _})) => introduced_labels
    | Some(SingletonLabelInference({label, pre_labeled_info})) =>
      let rec f = (info: option(Info.exp)): list(string) =>
        switch (Option.bind(info, i => i.label_inference)) {
        | Some(MultiLabelInference({introduced_labels, _})) => introduced_labels
        | Some(SingletonLabelInference({label, pre_labeled_info})) =>
          [label] @ f(Some(pre_labeled_info))
        | _ => []
        };
      [label] @ f(Some(pre_labeled_info));
    | _ => []
    };
  let reordered =
    switch (info.label_inference) {
    | Some(MultiLabelInference({reordered, _})) => reordered
    | _ => false
    };
  let lifted_ty =
    switch (info.label_inference) {
    | Some(SingletonLabelInference(_)) => Some(info.ty)
    | _ => None
    };
  let inferred_label = info.inferred_label;
  let cls = info.cls;
  let status = info.status;
  let fragments =
    switch (status) {
    | InHole(FreeVariable(name)) => [Code(name), Text("not found")]
    | InHole(InexhaustiveMatch(additional_err, example)) =>
      let cls_str = Cls.show(cls);
      switch (additional_err) {
      | None => [
          Text(
            cls_str ++ " is inexhaustive. An example of a missing pattern is ",
          ),
          Term(example),
        ]
      | Some(err) =>
        let cls_str = String.uncapitalize_ascii(cls_str);
        build_common_err(
          ~introduced_labels,
          ~lifted_ty,
          ~inferred_label,
          cls,
          err,
        )
        @ [Text("; "), Text(cls_str ++ " is inexhaustive")];
      };
    | InHole(UnusedDeferral) => [
        Text("Deferral must appear as a function argument"),
      ]
    | InHole(BadPartialAp(NoDeferredArgs)) => [
        Text("Expected at least one non-deferred argument"),
      ]
    | InHole(BadPartialAp(ArityMismatch({expected, actual}))) => [
        Text(
          "Arity mismatch: expected "
          ++ string_of_int(expected)
          ++ " argument"
          ++ (expected == 1 ? "" : "s")
          ++ ", got "
          ++ string_of_int(actual)
          ++ " arguments",
        ),
      ]
    | InHole(BuiltinError(e)) =>
      switch (e) {
      | MissingLabels(labels) => [
          Text("Labels not present in tuple: "),
          ...List.map(s => Label(s), labels),
        ]
      | ToLvsMissingLabelsOnTuple(_) => [
          Text(
            "All entries in the argument must have labels, but some were not provided",
          ),
        ]
      | ProjectLabelsMissingLabels(labels) => [
          Text("Projected tuple does not have the following labels: "),
          ...List.map(s => Label(s), labels),
        ]
      | ArgumentMustBeTuple => [Text("Argument must be a tuple")]
      | AtLeast2Arguments => [Text("Must have 2 or more direct arguments")]
      | Exactly2Arguments => [Text("Must have exactly 2 direct arguments")]
      | ArgumentMustBeListOfTuples => [
          Text("First argument must be a list of labeled tuples"),
        ]
      | PivotLabelIsNotString(ty) => [
          Text("Pivot column must be a string, but got: "),
          Type(ty),
        ]
      }
    | InHole(InvalidUseMode({bad_typ, _})) => [
        Text("Cannot use type "),
        Type(bad_typ),
        Text(" for number operators and literals."),
      ]
    | InHole(BadTrivAp(ty)) => [
        Text("Function argument type"),
        Type(ty),
        Text("inconsistent with"),
        Type(Prod([]) |> Typ.fresh),
      ]
    | InHole(TupleExtensionRequiresTuples) => [
        Text("Tuple extension requires tuple"),
      ]
    | InHole(DotOperatorRequiresTuple) => [
        Text("Requires tuple for first argument"),
      ]
    | InHole(Common(error)) =>
      build_common_err(
        ~introduced_labels,
        ~lifted_ty,
        ~inferred_label,
        cls,
        error,
      )
    | InHole(UnboundLivelit(name)) => [
        Text("Livelit with name"),
        Code(name),
        Text("not found, and also, it's a livelit"),
      ]
    | InHole(BadOperator(msg)) => [Text("Invalid operator: "), Text(msg)]
    | InHole(LabelNotFound(name, labels)) => [
        Text("Label "),
        Label(name),
        Text(" not found in tuple's labels: "),
        ...ListUtil.join_map(Text(", "), s => [Label(s)], labels),
      ]
    | InHole(BadLivelitModel(_)) => [Text("Bad internal livelit model")]
    | NotInHole(AnaDeferralConsistent(ana)) => [
        Text("Expecting type"),
        Type(ana),
      ]
    | NotInHole(Common(ok)) =>
      build_common_ok(
        ~reordered,
        ~introduced_labels,
        ~lifted_ty,
        ~inferred_label,
        ~label_sort=info.label_sort,
        cls,
        ok,
      )
    };
  {
    is_error:
      switch (status) {
      | InHole(_) => true
      | NotInHole(_) => false
      },
    fragments,
  };
};

let build_pat_message = (info: Info.pat): message => {
  let lifted_ty =
    switch (info.label_inference) {
    | Some(SingletonLabelInference(_)) => Some(info.ty)
    | _ => None
    };
  let inferred_label = info.inferred_label;
  let introduced_labels =
    switch (info.label_inference) {
    | Some(MultiLabelInference({introduced_labels, _})) => introduced_labels
    | Some(SingletonLabelInference({label, pre_labeled_info})) =>
      let rec f = (info: option(Info.pat)): list(string) =>
        switch (Option.bind(info, i => i.label_inference)) {
        | Some(MultiLabelInference({introduced_labels, _})) => introduced_labels
        | Some(SingletonLabelInference({label, pre_labeled_info})) =>
          [label] @ f(Some(pre_labeled_info))
        | _ => []
        };
      [label] @ f(Some(pre_labeled_info));
    | _ => []
    };
  let cls = info.cls;
  let status = info.status;
  let fragments =
    switch (status) {
    | InHole(ExpectedConstructor) => [Text("Expected a constructor")]
    | InHole(Redundant(additional_err)) =>
      switch (additional_err) {
      | None => [Text("Pattern is redundant")]
      | Some(err) =>
        let additional_fragments =
          switch (err) {
          | Common(error) =>
            build_common_err(
              ~introduced_labels,
              ~lifted_ty,
              ~inferred_label,
              cls,
              error,
            )
          | ExpectedConstructor => [Text("Expected a constructor")]
          | Redundant(_) => [Text("Redundant")]
          };
        additional_fragments @ [Text("; pattern is redundant")];
      }
    | InHole(Common(error)) =>
      build_common_err(
        ~introduced_labels,
        ~lifted_ty,
        ~inferred_label,
        cls,
        error,
      )
    | NotInHole(ok) =>
      build_common_ok(
        ~reordered=
          switch (info.label_inference) {
          | Some(MultiLabelInference({reordered, _})) => reordered
          | _ => false
          },
        ~introduced_labels,
        ~lifted_ty,
        ~inferred_label,
        ~label_sort=info.label_sort,
        cls,
        ok,
      )
    };
  {
    is_error:
      switch (status) {
      | InHole(_) => true
      | NotInHole(_) => false
      },
    fragments,
  };
};

let build_typ_message = (info: Info.typ): message => {
  let cls = info.cls;
  let status = info.status;
  let fragments =
    switch (status) {
    | NotInHole(ok) => build_typ_ok(cls, ok)
    | InHole(err) => build_typ_err(err)
    };
  {
    is_error:
      switch (status) {
      | InHole(_) => true
      | NotInHole(_) => false
      },
    fragments,
  };
};

let build_tpat_message = (info: Info.tpat): message => {
  let status = info.status;
  let fragments =
    switch (status) {
    | NotInHole(Empty) => [Text("Fillable with a new alias")]
    | NotInHole(Var(_)) => [Term(TPat(info.term))]
    | InHole(NotAVar(NotCapitalized)) => [
        Text("Must begin with a capital letter"),
      ]
    | InHole(NotAVar(_)) => [Text("Expected an alias")]
    | InHole(ShadowsType(name, BaseTyp)) => [
        Text("Can't shadow base type"),
        Type(Var(name) |> Typ.fresh),
      ]
    | InHole(ShadowsType(name, TyAlias)) => [
        Text("Can't shadow existing alias"),
        Type(Var(name) |> Typ.fresh),
      ]
    | InHole(ShadowsType(name, TyVar)) => [
        Text("Can't shadow existing type variable"),
        Type(Var(name) |> Typ.fresh),
      ]
    };
  {
    is_error:
      switch (status) {
      | InHole(_) => true
      | NotInHole(_) => false
      },
    fragments,
  };
};

let build_message = (info: Info.t): message =>
  switch (info) {
  | InfoExp(info) => build_exp_message(info)
  | InfoPat(info) => build_pat_message(info)
  | InfoTyp(info) => build_typ_message(info)
  | InfoTPat(info) => build_tpat_message(info)
  | Secondary(_) => {
      is_error: false,
      fragments: [],
    }
  };

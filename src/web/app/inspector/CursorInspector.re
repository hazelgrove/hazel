open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;
open Util;
open Language;

let errc = "error";
let warnc = "warning";
let okc = "ok";
let div_err = div(~attrs=[clss(["status", errc])]);
let div_ok = div(~attrs=[clss(["status", okc])]);
let div_warn = div(~attrs=[clss(["status", warnc])]);
let code_box_container = x =>
  div(~attrs=[clss(["code-box-container"])], [x]);
/* When true, prefixes type displays with ":" (e.g. ": Int").
   Appropriate in the cursor inspector but not in the error sidebar. */
let colon_prefix = show_type_colon => show_type_colon ? [text(":")] : [];

let code = (code: string): Node.t =>
  div(~attrs=[clss(["code"])], [text(code)]);

let label_view = (label: string): Node.t =>
  div(
    ~attrs=[clss(["code"])],
    [text(Haz3lcore.Token.quote_label_when_necessary(label))],
  );

let cls_view = (ci: Info.t): Node.t => {
  let cls = ci |> Info.cls_of;
  let cls_text =
    switch (Info.projector_kind_of(ci)) {
    | Some(kind) => "Projector (" ++ ProjectorKind.show(kind) ++ ")"
    | None =>
      switch (cls) {
      | Typ(EmptyHole)
      | Exp(EmptyHole)
      | Pat(EmptyHole) => Info.is_label(ci) ? "Label Hole" : Cls.show(cls)
      | _ => Info.cls_label(ci)
      }
    };

  div(~attrs=[clss(["syntax-class"])], [text(cls_text)]);
};

let ctx_toggle = (~globals: Globals.t): Node.t =>
  div(
    ~attrs=[
      Attr.on_click(_ => globals.inject_global(Set(ContextInspector))),
      clss(
        ["gamma"] @ (globals.settings.context_inspector ? ["visible"] : []),
      ),
    ],
    [Icons.gamma],
    //[text("Γ")],
  );

let term_view = (~globals: Globals.t, ~force_error=false, ci) => {
  /* Drv(_) sorts have verbose type-level names like "DrvJdmt"/"DrvProp"
     via Sort.to_string (needed for pretty-printing `DrvQuoteTy`). For the
     inspector header we prefer the terse form ("Jdmt", "Prop", ...),
     keeping the ALFA prefix for object-language sorts. */
  let sort_text =
    Info.is_label(ci)
      ? "Label"
      : (
        switch (Info.sort_of(ci)) {
        | Drv(s) => DrvSort.to_string_short(s)
        | s => Sort.to_string(s)
        }
      );
  let sort_class = Info.is_label(ci) ? "Label" : ci |> Info.class_of;
  div(
    ~attrs=[
      clss(
        ["ci-header", sort_class]
        @ (
          force_error || Info.is_error(ci)
            ? [errc]
            : Info.is_warning(ci) && globals.settings.core.display_warnings
                ? [warnc] : [okc]
        ),
      ),
    ],
    [
      ctx_toggle(~globals),
      div(~attrs=[clss(["term-tag"])], [text(sort_text)]),
      div(~attrs=[clss(["divider"])], [text("/")]),
      cls_view(ci),
    ],
  );
};

let elements_noun: Cls.t => string =
  fun
  | Exp(Match | If) => "Branches"
  | Exp(ListLit)
  | Pat(ListLit) => "Elements"
  | Exp(ListConcat)
  | Exp(BinOp(Poly(_))) => "Operands"
  | _ => "Sub-expressions";

let code_view_settings: Haz3lcore.ExpToSegment.Settings.t = {
  secondary: AutoFormat,
  parenthesization: Defensive,
  label_format: QuoteWhenNecessary,
  inline: true,
  fold_case_clauses: false,
  fold_fn_bodies: `NoFold,
  hide_fixpoints: false,
  show_ascriptions: true,
  show_filters: false,
  show_unknown_as_hole: true,
  hole_tiles: false,
  project_tables: false,
};

let view_any = (~globals, any: Any.t) =>
  any
  |> CodeViewable.view_any(~globals, ~settings=code_view_settings)
  |> code_box_container;

let view_type = (~globals, typ: Typ.t) =>
  typ
  |> CodeViewable.view_typ(~globals, ~settings=code_view_settings)
  |> code_box_container;

let core_mark_err_view =
    (
      ~globals,
      ~show_type_colon=true,
      ~introduced_labels: list(LabeledTuple.label),
      ~lifted_ty: option(Typ.t),
      ~inferred_label: option(LabeledTuple.label),
      ~ctx: Ctx.t,
      ~ana: Typ.t,
      cls: Cls.t,
      m: Mark.t,
    ) => {
  let view_type = view_type(~globals);
  let view_any = view_any(~globals);
  let ana = Statics.ana_skip_explicit_nonlabel(ana);
  let expectation_view = (~ana: Typ.t, ~syn: Typ.t) =>
    switch (syn.term, ana.term) {
    | (Label(syn_l), Label(an_label)) => [
        code(syn_l),
        text("but expected label"),
        code(an_label),
      ]
    | _ =>
      colon_prefix(show_type_colon)
      @ [
        view_type(syn) |> code_box_container,
        text("inconsistent with expected type"),
        view_type(ana) |> code_box_container,
      ]
      @ (
        switch (lifted_ty) {
        | None => []
        | Some(lifted) => [text(" lifted to"), view_type(lifted)]
        }
      )
      @ (
        switch (introduced_labels) {
        | [] => []
        | [a] => [text("after automatically added label "), code(a)]
        | _ => [
            text("after automatically added labels "),
            ...ListUtil.join(text(","), List.map(code, introduced_labels)),
          ]
        }
      )
    };
  (
    switch (m) {
    | BadToken(token) =>
      switch (Haz3lcore.Token.bad_token_cls(token)) {
      | BadInt => [text("Integer is too large or too small")]
      | Other => [text(Printf.sprintf("\"%s\" isn't a valid token", token))]
      }
    | BadLabel(label) => [text("Malformed Label: "), view_any(label)]
    | FreeConstructor(name) => [code(name), text("not found")]

    | InvalidLabel(name, expected_labels) =>
      switch (expected_labels) {
      | [] => [
          text("Invalid label: "),
          label_view(name),
          text(". No labels were expected."),
        ]
      | _ => [
          text("Invalid label: "),
          label_view(name),
          text(" is not part of the expected labels: "),
          ...List.map(code, expected_labels),
        ]
      }
    | UnexpectedLabelSort(name) => [
        text("Label "),
        label_view(name),
        text(" is here, but another sort is expected."),
      ]
    | IsMulti => [text("Broken expression")]

    | TupleLabelError({malformed_labels, duplicate_labels, invalid_labels, _}) =>
      (
        List.is_empty(malformed_labels)
          ? []
          : [
            text("Malformed labels: "),
            ...List.map(view_any, malformed_labels),
          ]
      )
      @ (
        List.is_empty(duplicate_labels)
          ? []
          : [
            text("Duplicate labels: "),
            ...List.map(code, duplicate_labels),
          ]
      )
      @ (
        List.is_empty(invalid_labels)
          ? []
          : [text("Invalid labels: "), ...List.map(code, invalid_labels)]
      )
    | DuplicateVar(name, _) => [text("Duplicate Variable:"), code(name)]
    | DuplicateLabel(name, _) => [
        text("Duplicate Label:"),
        label_view(name),
      ]
    | CompareFun(ty) => [text("values cannot be compared:"), view_type(ty)]
    | ExpectationMismatch({ana, syn}) => expectation_view(~ana, ~syn)
    | NoMeet(PolyEq, tys)
    | NoMeet(_, tys) when ana.term == Unknown(SynSwitch) => [
        text(elements_noun(cls) ++ " have inconsistent types:"),
        ...ListUtil.join(
             text(","),
             List.map(view_type, Typ.of_source(tys)),
           ),
      ]
    | NoMeet(wrap, _) =>
      let syn: Typ.t = SynTy.meet_of(wrap, Unknown(Internal) |> Typ.temp);
      switch (Typ.meet(ctx, ana, syn)) {
      | Some(_) => [text("Type error")]
      | None =>
        switch (ana.term, syn.term) {
        | (Label(_), _) => [text("Malformed Label: "), view_any(Typ(syn))]
        | _ => expectation_view(~ana, ~syn)
        }
      };
    | ExplicitNonlabel => [text("Type error")]
    | Free(_)
    | InexhaustiveMatch(_)
    | IsDeferral(_)
    | IsBadPartialAp(_)
    | BuiltinError(_)
    | InvalidUseMode(_)
    | IsLivelitName(_)
    | BadTrivAp(_)
    | DotOperatorRequiresTuple
    | TupleExtensionRequiresTuples
    | LabelNotFound(_)
    | ModuleMissingMembers(_)
    | ModuleExtraMembers(_)
    | ModuleMemberNotFound(_)
    | ModuleTypeMemberMismatch(_)
    | BadOperator(_)
    | BadLivelitModel(_)
    | BadTheorem(_)
    | Redundant
    | ExpectedConstructor
    | TypFreeTypeVariable(_)
    | TypDuplicateConstructor(_)
    | TypDuplicateLabels(_, _)
    | TypWantTypeFoundAp
    | TypWantLabel
    | TypWantProduct(_)
    | ModuleTypeMemberNotFound(_)
    | TypWantModule(_)
    | TypWantConstructorFoundType(_)
    | TypWantConstructorFoundAp
    | TypParseFailure
    | TPatShadowsType(_)
    | TPatNotAVar(_) => [text("Type error")]
    }
  )
  @ (
    switch (inferred_label) {
    | None => []
    | Some(l) => [text(" for label "), label_view(l)]
    }
  );
};

let common_warn_view = (warning: Warning.t) => {
  switch (warning) {
  | WarningPat(UnusedVar(name)) => [
      text("Warning: Variable"),
      code(name),
      text("is unused."),
    ]
  | None => []
  };
};
let common_ok_view =
    (
      ~globals,
      ~show_type_colon=true,
      ~reordered: bool,
      ~introduced_labels: list(LabeledTuple.label),
      ~lifted_ty: option(Typ.t),
      ~inferred_label: option(LabeledTuple.label),
      ~label_sort: bool,
      cls: Cls.t,
      ok: Message.ok_common,
    ) => {
  let view_type = view_type(~globals);
  (
    switch (cls, ok) {
    | (Pat(EmptyHole), _) when label_sort => []
    | (Exp(EmptyHole), _) when label_sort => []
    | (Pat(ExplicitNonlabel), _) when label_sort => [
        text("Explicitly unlabeled entry"),
      ]
    | (Exp(ExplicitNonlabel), _) when label_sort => [
        text("Explicitly unlabeled entry"),
      ]
    | (Exp(MultiHole) | Pat(MultiHole), _) => [
        text("Expecting operator or delimiter"),
      ]
    | (Exp(EmptyHole), Syn(_)) => [text("Fillable by any expression")]
    | (Pat(EmptyHole), Syn(_)) => [text("Fillable by any pattern")]
    | (Exp(EmptyHole), Ana(Consistent({ana, _}))) => [
        text("Fillable by any expression of type"),
        view_type(ana),
      ]
    | (Pat(EmptyHole), Ana(Consistent({ana, _}))) => [
        text("Fillable by any pattern of type"),
        view_type(ana),
      ]
    | (_, Syn(syn)) =>
      switch (syn.term) {
      | Label(l) => [label_view(l)]
      | _ => colon_prefix(show_type_colon) @ [view_type(syn)]
      }
    | (Pat(Var) | Pat(Wild) | Pat(ApFunc), Ana(Consistent({ana, _}))) =>
      /* Pat(ApFunc) is only produced by the `let f(args) = ...` function
         sugar (see FunctionSugar.re), where it denotes the function binder
         as a whole. Render it the same way as a plain variable binder. */
      colon_prefix(show_type_colon) @ [view_type(ana)]
    | (_, Ana(Consistent({ana, syn, _})))
        when Equality.semantic.typ(ana, syn) =>
      switch (syn.term) {
      | Label(l) => [label_view(l), text(" is a valid label")]
      | _ =>
        colon_prefix(show_type_colon)
        @ [view_type(syn)]
        @ [text("equals expected type")]
        @ (
          switch (lifted_ty) {
          | None => []
          | Some(lifted) => [text(" lifted to"), view_type(lifted)]
          }
        )
        @ (
          switch (introduced_labels) {
          | [] => []
          | [a] => [text("by automatically adding label "), label_view(a)]
          | _ => [
              text("by automatically adding labels "),
              ...ListUtil.join(
                   text(","),
                   List.map(label_view, introduced_labels),
                 ),
            ]
          }
        )
        @ (
          switch (reordered) {
          | false => []
          | true => [text(" after reordering by labels ")]
          }
        )
      }
    | (_, Ana(Consistent({ana, syn, _}))) =>
      (
        switch (syn.term) {
        | Label(l) => [code(l), text(" is a valid label")]
        | _ =>
          colon_prefix(show_type_colon)
          @ [view_type(syn), text("consistent with expected type")]
        }
      )
      @ [view_type(ana)]
      @ (
        switch (lifted_ty) {
        | None => []
        | Some(lifted) => [text(" lifted to"), view_type(lifted)]
        }
      )
      @ (
        switch (introduced_labels) {
        | [] => []
        | [a] => [text("by automatically adding label "), label_view(a)]
        | _ => [
            text("by automatically adding labels "),
            ...ListUtil.join(
                 text(","),
                 List.map(label_view, introduced_labels),
               ),
          ]
        }
      )
      @ (
        switch (reordered) {
        | false => []
        | true => [text(" after reordering by labels ")]
        }
      )
    | (_, Ana(InternallyInconsistent({ana, nomeet: tys}))) =>
      [
        text(elements_noun(cls) ++ " have inconsistent types:"),
        ...ListUtil.join(text(","), List.map(view_type, tys)),
      ]
      @ [text("but consistent with expected"), view_type(ana)]
    }
  )
  @ (
    switch (inferred_label) {
    | None => []
    | Some(l) => [text(" for label "), label_view(l)]
    }
  );
};

let underdetermined_typ_view =
    (~globals, underdetermined: Message.underdetermined_typ) => {
  let view_type = view_type(~globals);
  switch (underdetermined) {
  | ProdExtensionUnderdetermined(tys) => [
      text("Cannot determine type of product extension with argument types:"),
      ...ListUtil.join(text(","), List.map(view_type, tys)),
    ]
  | ProdProjectionMissingLabel(label, labels) => [
      text("Cannot project label "),
      label_view(label),
      text(". Valid labels are: "),
      ...List.map(code, labels),
    ]
  | ModuleTypeMemberMissing(name, members) =>
    [text("Module has no type member "), label_view(name)]
    @ (
      switch (members) {
      | [] => [text("; it has no type members")]
      | _ => [
          text("; its type members are "),
          ...ListUtil.join(text(", "), List.map(label_view, members)),
        ]
      }
    )
  | ProdProjectionBadArgs({product, label}) =>
    let product_error =
      switch (product) {
      | Some(ty) => [
          text("type"),
          view_type(ty),
          text("is not a tuple type"),
        ]
      | None => []
      };
    let label_error =
      switch (label) {
      | Some(ty) => [
          text("label"),
          view_type(ty),
          text("is not a valid label: "),
        ]
      | None => []
      };

    [text("Cannot determine projected type because ")]
    @ (
      ListUtil.join(
        [text(" and ")],
        [product_error, label_error] |> List.filter(x => x != []),
      )
      |> List.concat
    );
  };
};

let typ_ok_view = (~globals, cls: Cls.t, ok: Message.ok_typ) => {
  let view_type = view_type(~globals);
  switch (ok) {
  | EmptyLabel => []
  | Type(_) when cls == Typ(EmptyHole) => [text("Fillable by any type")]
  | Type(ty) =>
    [view_type(ty)]
    @ (
      switch (cls) {
      | Typ(Label) => []
      | _ => [text("is a type")]
      }
    )

  | TypeAlias(name, ty_lookup) => [
      view_type(Var(name) |> Typ.fresh),
      text("is equal to"),
      view_type(ty_lookup),
    ]
  | WHNormalizedTo({unnormalized, whnormalized}) => [
      view_type(unnormalized),
      text("is equal to"),
      view_type(whnormalized),
    ]
  | Variant(name, sum_ty) => [
      view_type(Var(name) |> Typ.fresh),
      text("is a sum type constuctor of type"),
      view_type(sum_ty),
    ]
  | TypeUnderdetermined(underdetermined) =>
    underdetermined_typ_view(~globals, underdetermined)
  };
};

let typ_mark_err_view = (~globals, m: Mark.t) => {
  let view_type = view_type(~globals);
  switch (m) {
  | TypFreeTypeVariable(name) => [
      view_type(Var(name) |> Typ.fresh),
      text("not found"),
    ]
  | BadToken(token) => [code(token), text("not a type or type operator")]
  | TypWantConstructorFoundAp
  | TypWantConstructorFoundType(_) => [text("Expected a constructor")]
  | TypWantTypeFoundAp => [text("Must be part of a sum type")]
  | TypWantLabel => [text("Expect a valid label")]
  | InvalidLabel(name, expected_labels) =>
    switch (expected_labels) {
    | [] => [
        text("Member "),
        label_view(name),
        text(" not found — no members available"),
      ]
    | _ => [
        text("Member "),
        label_view(name),
        text(" not found. Available: "),
        text(String.concat(", ", expected_labels)),
      ]
    }
  | TypDuplicateLabels(labels, _) => [
      text("Duplicate labels within tuple: "),
      ...List.map(label_view, labels),
    ]
  | DuplicateLabel(name, _) => [
      text("Duplicate Label: "),
      label_view(name),
    ]
  | TypDuplicateConstructor(name) => [
      view_type(Var(name) |> Typ.fresh),
      text("already used in this sum"),
    ]
  | TypParseFailure => [text("Parse failure")]
  | TypWantProduct(ty) => [
      text("Expected a module or tuple type, found type"),
      view_type(ty),
    ]
  | ModuleTypeMemberNotFound({name, members, submodule}) =>
    let what = submodule ? "sub-module" : "type member";
    [text("Module has no " ++ what ++ " "), label_view(name)]
    @ (
      switch (members) {
      | [] => [text("; it has no " ++ what ++ "s")]
      | _ => [
          text("; its " ++ what ++ "s are "),
          ...ListUtil.join(text(", "), List.map(label_view, members)),
        ]
      }
    );
  | TypWantModule({name, typ}) => [
      code(name),
      text("is a value of type"),
      view_type(typ),
      text(", not a module"),
    ]
  | _ => [text("Type error")]
  };
};

let rec automatic_inserted_labels_exp =
        (info: option(Info.exp)): list(string) =>
  switch (Option.bind(info, i => i.label_inference)) {
  | Some(MultiLabelInference({introduced_labels, _})) => introduced_labels
  | Some(SingletonLabelInference({label, pre_labeled_info})) =>
    [label] @ automatic_inserted_labels_exp(Some(pre_labeled_info))
  | _ => []
  };

let rec automatic_inserted_labels_pat =
        (info: option(Info.pat)): list(string) =>
  switch (Option.bind(info, i => i.label_inference)) {
  | Some(MultiLabelInference({introduced_labels, _})) => introduced_labels
  | Some(SingletonLabelInference({label, pre_labeled_info})) =>
    [label] @ automatic_inserted_labels_pat(Some(pre_labeled_info))
  | _ => []
  };

let exp_mark_err_view =
    (~globals, ~show_type_colon=true, cls: Cls.t, m: Mark.t, info: Info.exp) => {
  let introduced_labels =
    switch (info.label_inference) {
    | Some(MultiLabelInference({introduced_labels, _})) => introduced_labels
    | Some(SingletonLabelInference({label, pre_labeled_info})) =>
      [label] @ automatic_inserted_labels_exp(Some(pre_labeled_info))
    | _ => []
    };
  let lifted_ty =
    switch (info.label_inference) {
    | Some(SingletonLabelInference(_)) => Some(info.ty)
    | _ => None
    };
  let inferred_label = info.inferred_label;
  let view_type = view_type(~globals);
  let view_any = view_any(~globals);
  let ctx = info.ctx;
  let ana = info.ana;
  let common_from_core = () =>
    div_err(
      core_mark_err_view(
        ~globals,
        ~show_type_colon,
        ~introduced_labels,
        ~lifted_ty,
        ~inferred_label,
        ~ctx,
        ~ana,
        cls,
        m,
      ),
    );
  switch (m) {
  | Free(name) => div_err([code(name), text("not found")])
  | InexhaustiveMatch(_, inner_marks, example) =>
    let cls_str = Cls.show(cls);
    let additional =
      switch (
        Mark.highest(inner_marks),
        Statics.ana_skip_explicit_nonlabel(ana).term,
      ) {
      | (Some(NoMeet(PolyEq, tys)), _) => Some(Typ.of_source(tys))
      | (Some(NoMeet(_, tys)), Unknown(SynSwitch)) =>
        Some(Typ.of_source(tys))
      | _ => None
      };
    switch (additional) {
    | None =>
      div_err([
        text(
          cls_str ++ " is inexhaustive. An example of a missing pattern is ",
        ),
        view_any(example),
      ])
    | Some(tys) =>
      let cls_str = String.uncapitalize_ascii(cls_str);
      div_err([
        div_err([
          text(elements_noun(cls) ++ " have inconsistent types:"),
          ...ListUtil.join(text(","), List.map(view_type, tys)),
        ])
        |> code_box_container,
        text(
          "; "
          ++ cls_str
          ++ " is inexhaustive. An example of a missing pattern is ",
        ),
        view_any(example),
      ]);
    };
  | IsDeferral(InAp) =>
    div_err([
      text("(internal) deferral in application is not an error mark"),
    ])
  | IsDeferral(_) =>
    div_err([text("Deferral must appear as a function argument")])
  | IsBadPartialAp(NoDeferredArgs) =>
    div_err([text("Expected at least one non-deferred argument")])
  | IsBadPartialAp(ArityMismatch({expected, actual})) =>
    div_err([
      text(
        "Arity mismatch: expected "
        ++ string_of_int(expected)
        ++ " argument"
        ++ (expected == 1 ? "" : "s")
        ++ ", got "
        ++ string_of_int(actual)
        ++ " arguments",
      ),
    ])
  | BuiltinError(e) =>
    switch (e) {
    | MissingLabels(labels) =>
      div_err([
        text("Labels not present in tuple: "),
        ...List.map(label_view, labels),
      ])
    | ToLvsMissingLabelsOnTuple(_) =>
      div_err([
        text(
          "All entries in the argument must have labels, but some were not provided",
        ),
      ])
    | ProjectLabelsMissingLabels(labels) =>
      div_err([
        text("Projected tuple does not have the following labels: "),
        ...List.map(label_view, labels),
      ])
    | ArgumentMustBeTuple => div_err([text("Argument must be a tuple")])
    | AtLeast2Arguments =>
      div_err([text("Must have 2 or more direct arguments")])
    | Exactly2Arguments =>
      div_err([text("Must have exactly 2 direct arguments")])
    | ArgumentMustBeListOfTuples =>
      div_err([text("First argument must be a list of labeled tuples")])
    | PivotLabelIsNotString(ty) =>
      div_err([
        text("Pivot column must be a string, but got: "),
        view_type(ty),
      ])
    }
  | InvalidUseMode({bad_typ, _}) =>
    div_err([
      text("Cannot use type "),
      view_type(bad_typ) |> code_box_container,
      text(" for number operators and literals."),
    ])
  | BadTrivAp(ty) =>
    div_err([
      text("Function argument type"),
      view_type(ty),
      text("inconsistent with"),
      view_type(Prod([]) |> Typ.fresh),
    ])
  | TupleExtensionRequiresTuples =>
    div_err([text("Tuple extension requires tuple")])
  | DotOperatorRequiresTuple =>
    div_err([text("Requires a module or tuple for the first argument")])
  | IsLivelitName({name, _}) =>
    switch (Ctx.lookup_livelit(ctx, name)) {
    | None =>
      div_err([
        text("Livelit with name"),
        code(name),
        text("not found, and also, it's a livelit"),
      ])
    | Some(_) =>
      div_err([text("(internal) livelit should not surface as error")])
    }
  | BadOperator(msg) => div_err([text("Invalid operator: "), text(msg)])
  | LabelNotFound(name, labels) =>
    div_err([
      text("Label "),
      label_view(name),
      text(" not found in tuple's labels: "),
      ...List.map(label_view, labels),
    ])
  | ModuleMissingMembers(names) =>
    div_err([
      text("Module is missing members required by its signature: "),
      ...ListUtil.join(text(", "), List.map(code, names)),
    ])
  | ModuleMemberNotFound({name, members, type_member}) =>
    if (type_member) {
      div_err([
        code(name),
        text(
          " is a type member of the module, not a value; use it in a type position",
        ),
      ]);
    } else {
      div_err(
        [text("Module has no member "), code(name)]
        @ (
          switch (members) {
          | [] => [text("; it has no members")]
          | _ => [
              text("; its members are "),
              ...ListUtil.join(text(", "), List.map(code, members)),
            ]
          }
        ),
      );
    }
  | ModuleExtraMembers(names) =>
    div_err([
      text("Module has members its signature does not declare: "),
      ...ListUtil.join(text(", "), List.map(code, names)),
    ])
  | ModuleTypeMemberMismatch({name, expected, actual}) =>
    div_err([
      text("Type member "),
      code(name),
      text(" is defined as "),
      view_type(actual),
      text(" but its signature declares "),
      view_type(expected),
    ])
  | BadLivelitModel(_) => div_err([text("Bad internal livelit model")])
  | BadTheorem(typ) =>
    div_err([
      text("Theorem pattern is not of the form p : t, got "),
      view_type(typ),
    ])
  | TypFreeTypeVariable(_)
  | TypDuplicateConstructor(_)
  | TypDuplicateLabels(_, _)
  | TypWantTypeFoundAp
  | TypWantLabel
  | TypWantProduct(_)
  | ModuleTypeMemberNotFound(_)
  | TypWantModule(_)
  | TypWantConstructorFoundType(_)
  | TypWantConstructorFoundAp
  | TypParseFailure
  | TPatShadowsType(_)
  | TPatNotAVar(_) =>
    div_err([text("(internal) typ/tpat mark on expression")])
  | Redundant
  | ExpectedConstructor =>
    div_err([text("(internal) pattern-only mark on expression")])
  | FreeConstructor(_)
  | BadToken(_)
  | BadLabel(_)
  | ExplicitNonlabel
  | UnexpectedLabelSort(_)
  | InvalidLabel(_, _)
  | TupleLabelError(_)
  | IsMulti
  | DuplicateLabel(_, _)
  | DuplicateVar(_, _)
  | ExpectationMismatch(_)
  | NoMeet(_)
  | CompareFun(_) => common_from_core()
  };
};

let exp_view =
    (
      ~globals,
      ~show_type_colon=true,
      cls: Cls.t,
      message: Message.t,
      info: Info.exp,
    ) => {
  let introduced_labels =
    switch (info.label_inference) {
    | Some(MultiLabelInference({introduced_labels, _})) => introduced_labels
    | Some(SingletonLabelInference({label, pre_labeled_info})) =>
      [label] @ automatic_inserted_labels_exp(Some(pre_labeled_info))
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
  let marks = info.marks;
  switch (marks != []) {
  | false =>
    switch (message) {
    | Exp(Default) =>
      div_ok(
        common_ok_view(
          ~globals,
          ~show_type_colon,
          ~lifted_ty,
          ~reordered,
          ~introduced_labels,
          ~inferred_label,
          ~label_sort=info.label_sort,
          cls,
          Message.Syn(info.elab_syn_ty),
        ),
      )
    | Exp(AnaDeferralConsistent(ana)) =>
      div_ok([text("Expecting type"), view_type(~globals, ana)])
    | Exp(ModuleMemberNotFound({name, members})) =>
      div_ok(
        [text("Module has no member "), code(name)]
        @ (
          switch (members) {
          | [] => [text("; it has no members")]
          | _ => [
              text("; its members are "),
              ...ListUtil.join(text(", "), List.map(code, members)),
            ]
          }
        ),
      )
    | Exp(Common(ok)) =>
      div_ok(
        common_ok_view(
          ~globals,
          ~show_type_colon,
          ~lifted_ty,
          ~reordered,
          ~introduced_labels,
          ~inferred_label,
          ~label_sort=info.label_sort,
          cls,
          ok,
        ),
      )
    | Pat(_)
    | TypOk(_)
    | TPatOk(_) =>
      failwith("CursorInspector.exp_view: expected Message.Exp(...)")
    }
  | true =>
    switch (Mark.highest(marks)) {
    | Some(m) => exp_mark_err_view(~globals, ~show_type_colon, cls, m, info)
    | None =>
      div_err([
        text("(internal) expression marks indicate error but no syn mark"),
      ])
    }
  };
};

let pat_marks_err_view =
    (
      ~globals,
      ~show_type_colon=true,
      cls: Cls.t,
      marks: list(Mark.t),
      info: Info.pat,
    ) => {
  let ctx = info.ctx;
  let ana = info.ana;
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
      [label] @ automatic_inserted_labels_pat(Some(pre_labeled_info))
    | _ => []
    };

  switch (marks) {
  | [Redundant, ...tl] =>
    let additional = Mark.highest(tl);
    switch (additional) {
    | None => div_err([text("Pattern is redundant")])
    | Some(m) =>
      div_err([
        div_err(
          core_mark_err_view(
            ~globals,
            ~show_type_colon,
            ~inferred_label,
            ~introduced_labels,
            ~lifted_ty,
            ~ctx,
            ~ana,
            cls,
            m,
          ),
        )
        |> code_box_container,
        text("; pattern is redundant"),
      ])
    };
  | [ExpectedConstructor, ..._] => div_err([text("Expected a constructor")])
  | _ =>
    switch (Mark.highest(marks)) {
    | None => div_err([text("(internal) pattern error but no pat syn mark")])
    | Some(m) =>
      div_err(
        core_mark_err_view(
          ~globals,
          ~show_type_colon,
          ~inferred_label,
          ~introduced_labels,
          ~lifted_ty,
          ~ctx,
          ~ana,
          cls,
          m,
        ),
      )
    }
  };
};

let pat_view =
    (
      ~globals,
      ~show_type_colon=true,
      cls: Cls.t,
      message: Message.t,
      info: Info.pat,
    ) => {
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
      [label] @ automatic_inserted_labels_pat(Some(pre_labeled_info))
    | _ => []
    };

  let marks = info.marks;
  marks != []
    ? pat_marks_err_view(~globals, ~show_type_colon, cls, marks, info)
    : {
      let ok =
        switch (message) {
        | Pat(Default) => Message.Syn(info.elab_syn_ty)
        | Pat(Common(ok)) => ok
        | Exp(_)
        | TypOk(_)
        | TPatOk(_) =>
          failwith("CursorInspector.pat_view: expected Message.Pat(...)")
        };
      let ok_view =
        common_ok_view(
          ~globals,
          ~show_type_colon,
          ~lifted_ty,
          ~reordered=
            switch (info.label_inference) {
            | Some(MultiLabelInference({reordered, _})) => reordered
            | _ => false
            },
          ~introduced_labels,
          ~inferred_label,
          ~label_sort=info.label_sort,
          cls,
          ok,
        );
      switch (info.warnings) {
      | [Pat(UnusedVar(name))] =>
        if (globals.settings.core.display_warnings) {
          div_warn(common_warn_view(WarningPat(UnusedVar(name))));
        } else {
          div_ok(ok_view);
        }
      | _ => div_ok(ok_view)
      };
    };
};

let typ_view =
    (
      ~globals,
      cls: Cls.t,
      ~marks: list(Mark.t),
      ~message: option(Message.t),
    )
    : Node.t =>
  switch (marks) {
  | [] =>
    switch (message) {
    | Some(TypOk(o)) => div_ok(typ_ok_view(~globals, cls, o))
    | Some(Pat(_) | Exp(_) | TPatOk(_)) =>
      div_err([text("(internal) expected TypOk")])
    | None => div_err([text("(internal) missing type ok payload")])
    }
  | ms =>
    switch (Mark.highest(ms)) {
    | Some(m) => div_err(typ_mark_err_view(~globals, m))
    | None => div_err([text("(internal) missing type mark")])
    }
  };

let tpat_view =
    (~globals, _: Cls.t, ~marks: list(Mark.t), ~message: option(Message.t))
    : Node.t => {
  let view_type = view_type(~globals);
  switch (marks) {
  | [] =>
    switch (message) {
    | Some(TPatOk(Message.Empty)) =>
      div_ok([text("Fillable with a new alias")])
    | Some(TPatOk(Var(name))) =>
      div_ok([ContextInspector.alias_view(name)])
    | Some(Pat(_) | Exp(_) | TypOk(_)) =>
      div_err([text("(internal) expected TPatOk")])
    | None => div_err([text("(internal) missing tpat ok payload")])
    }
  | ms =>
    switch (Mark.highest(ms)) {
    | None => div_err([text("(internal) missing type pattern mark")])
    | Some(m) =>
      switch (m) {
      | TPatNotAVar(NotCapitalized) =>
        div_err([text("Must begin with a capital letter")])
      | TPatNotAVar(Other) => div_err([text("Expected an alias")])
      | TPatShadowsType(name, BaseTyp) =>
        div_err([
          text("Can't shadow base type"),
          view_type(Var(name) |> Typ.fresh),
        ])
      | TPatShadowsType(name, TyAlias) =>
        div_err([
          text("Can't shadow existing alias"),
          view_type(Var(name) |> Typ.fresh),
        ])
      | TPatShadowsType(name, TyVar) =>
        div_err([
          text("Can't shadow existing type variable"),
          view_type(Var(name) |> Typ.fresh),
        ])
      | _ => div_err([text("Type pattern error")])
      }
    }
  };
};

let view_of_info = (~globals, ci): list(Node.t) => {
  let wrapper = status_view => [term_view(~globals, ci), status_view];
  switch (ci) {
  | Secondary(_) => wrapper(div([]))
  | InfoMod({cls, _}) => wrapper(div_ok([text(cls |> Cls.show)]))
  | InfoSig({cls, _}) => wrapper(div_ok([text(cls |> Cls.show)]))
  | InfoMPat({cls, _}) => wrapper(div_ok([text(cls |> Cls.show)]))
  | InfoExp({cls, message, _} as ie) =>
    wrapper(exp_view(~globals, cls, message, ie))
  | InfoPat({cls, message, _} as ip) =>
    wrapper(pat_view(~globals, cls, message, ip))
  | InfoTyp({cls, marks, message, _}) =>
    wrapper(typ_view(~globals, cls, ~marks, ~message))
  | InfoTPat({cls, marks, message, _}) =>
    wrapper(tpat_view(~globals, cls, ~marks, ~message))
  | InfoDrv(ci) => wrapper(DrvCursorInspector.drv_view(~globals, ci))
  };
};

let inspector_view = (~globals: Globals.t, ci): Node.t =>
  div(
    ~attrs=[
      Attr.id("cursor-inspector"),
      clss([
        Info.is_error(ci)
          ? errc
          : Info.is_warning(ci) && globals.settings.core.display_warnings
              ? warnc : okc,
      ]),
    ],
    view_of_info(~globals, ci),
  );

let projector_error_inspector =
    (
      ~globals: Globals.t,
      ci: Language.Info.t,
      err: Haz3lcore.ProjectorBase.error,
    ) =>
  div(
    ~attrs=[Attr.id("cursor-inspector"), clss([errc])],
    [
      term_view(~globals, ~force_error=true, ci),
      div_err([text(err.message)]),
    ],
  );

let view = (~globals: Globals.t, cursor: Cursor.cursor(Editors.Update.t)) => {
  let bar_view = div(~attrs=[Attr.id("bottom-bar")]);
  let err_view = err =>
    bar_view([
      div(
        ~attrs=[Attr.id("cursor-inspector"), clss(["no-info"])],
        [div(~attrs=[clss(["icon"])], [Icons.magnify]), text(err)],
      ),
    ]);
  /* Look up projector error for the indicated piece */
  let projector_err =
    switch (cursor.indicated_piece, cursor.editor) {
    | (Some(Projector({id, kind, _})), Some(editor)) =>
      switch (Id.Map.find_opt(id, editor.syntax.projector_errors)) {
      | Some(err) => Some((kind, err))
      | None => None
      }
    | _ => None
    };
  switch (cursor.info) {
  | _ when !globals.settings.core.statics => div_empty
  | None => err_view("Whitespace or Comment")
  | Some(ci) =>
    /* Show projector error instead of normal status,
     * unless there's a statics error (which takes priority) */
    switch (projector_err) {
    | Some((_, err)) when !Info.is_error(ci) =>
      bar_view([projector_error_inspector(~globals, ci, err)])
    | _ => bar_view([inspector_view(~globals, ci)])
    }
  };
};

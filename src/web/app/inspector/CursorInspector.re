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

let code = (code: string): Node.t =>
  div(~attrs=[clss(["code"])], [text(code)]);

let cls_view = (ci: Info.t): Node.t => {
  let cls = ci |> Info.cls_of;

  div(
    ~attrs=[clss(["syntax-class"])],
    [
      text(
        switch (cls) {
        | Typ(EmptyHole)
        | Exp(EmptyHole)
        | Pat(EmptyHole) => Info.is_label(ci) ? "Label Hole" : Cls.show(cls)
        | cls => cls |> Cls.show
        },
      ),
    ],
  );
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

let term_view = (~globals: Globals.t, ci) => {
  let sort = Info.is_label(ci) ? "Label" : ci |> Info.sort_of |> Sort.show;

  div(
    ~attrs=[
      clss(
        ["ci-header", sort]
        @ (
          Info.is_error(ci)
            ? [errc]
            : Info.is_warning(ci) && globals.settings.core.display_warnings
                ? [warnc] : [okc]
        ),
      ),
    ],
    [
      ctx_toggle(~globals),
      div(~attrs=[clss(["term-tag"])], [text(sort)]),
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
  | Exp(BinOp(Poly(Equals))) => "Operands"
  | cls =>
    failwith("elements_noun: " ++ Cls.show(cls) ++ " cls has no elements");

let code_view_settings: Haz3lcore.ExpToSegment.Settings.t = {
  inline: true,
  fold_case_clauses: false,
  fold_fn_bodies: false,
  hide_fixpoints: false,
  show_filters: false,
  show_unknown_as_hole: true,
};

let view_any = (~globals, any: Term.Any.t) =>
  any
  |> CodeViewable.view_any(
       ~globals,
       ~settings=code_view_settings,
       ~shape_map=Haz3lcore.ProjectorCore.Shape.Map.empty // assume no projectors
     )
  |> code_box_container;

let view_type = (~globals, typ: Typ.t) =>
  typ
  |> CodeViewable.view_typ(~globals, ~settings=code_view_settings)
  |> code_box_container;

let common_err_view =
    (
      ~globals,
      ~introduced_labels: list(LabeledTuple.label),
      ~lifted_ty: option(Typ.t),
      ~inferred_label: option(LabeledTuple.label),
      cls: Cls.t,
      err: Info.error_common,
    ) => {
  let view_type = view_type(~globals);
  let view_any = view_any(~globals);
  (
    switch (err) {
    | NoType(BadToken(token)) =>
      switch (Haz3lcore.Form.bad_token_cls(token)) {
      | BadInt => [text("Integer is too large or too small")]
      | Other => [text(Printf.sprintf("\"%s\" isn't a valid token", token))]
      }
    | NoType(BadLabel(label)) => [
        text("Malformed Label: "),
        view_any(label),
      ]
    | NoType(FreeConstructor(name)) => [code(name), text("not found")]

    | NoType(InvalidLabel(name)) => [text("Invalid label:"), code(name)]
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
    | DuplicateLabel(name, _) => [text("Duplicate Label:"), code(name)]
    | Inconsistent(CompareFun(ty)) => [
        text("values cannot be compared:"),
        view_type(ty),
      ]
    | Inconsistent(WithArrow(typ)) => [
        text(":"),
        view_type(typ) |> code_box_container,
        text("inconsistent with arrow type"),
      ]
    | Inconsistent(Expectation({ana, syn})) =>
      switch (syn.term, ana.term) {
      | (Label(syn_l), Label(an_label)) => [
          code(syn_l),
          text("but expected label"),
          code(an_label),
        ]
      | _ =>
        [
          text(":"),
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
              ...ListUtil.join(
                   text(","),
                   List.map(code, introduced_labels),
                 ),
            ]
          }
        )
      }
    | Inconsistent(Internal(tys)) => [
        text(elements_noun(cls) ++ " have inconsistent types:"),
        ...ListUtil.join(text(","), List.map(view_type, tys)),
      ]
    }
  )
  @ (
    switch (inferred_label) {
    | None => []
    | Some(l) => [text(" for label "), code(l)]
    }
  );
};

let common_warn_view = (warning: Warning.t) => {
  switch (warning) {
  | WarningPat(w) =>
    switch (w) {
    | UnusedVar(name) => [
        text("Warning: Variable"),
        code(name),
        text("is unused."),
      ]
    | _ => [text("Warning: " ++ Warning.show(warning))]
    }
  | WarningExp(_)
  | WarningTyp(_)
  | WarningTPat(_) => [text("Warning: " ++ Warning.show(warning))]
  | None => []
  };
};
let common_ok_view =
    (
      ~globals,
      ~reordered: bool,
      ~introduced_labels: list(LabeledTuple.label),
      ~lifted_ty: option(Typ.t),
      ~inferred_label: option(LabeledTuple.label),
      ~label_sort: bool,
      cls: Cls.t,
      ok: Info.ok_common,
    ) => {
  let view_type = view_type(~globals);
  (
    switch (cls, ok) {
    | (Pat(EmptyHole), _) when label_sort => []
    | (Exp(EmptyHole), _) when label_sort => []
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
      | Label(l) => [code(l)]
      | _ => [text(":"), view_type(syn)]
      }
    | (Pat(Var) | Pat(Wild), Ana(Consistent({ana, _}))) => [
        text(":"),
        view_type(ana),
      ]
    | (_, Ana(Consistent({ana, syn, _})))
        when Typ.fast_equal(~alpha_equivalence=false, ana, syn) =>
      switch (syn.term) {
      | Label(l) => [code(l), text(" is a valid label")]
      | _ =>
        [text(":"), view_type(syn)]
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
          | [a] => [text("by automatically adding label "), code(a)]
          | _ => [
              text("by automatically adding labels "),
              ...ListUtil.join(
                   text(","),
                   List.map(code, introduced_labels),
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
        | _ => [
            text(":"),
            view_type(syn),
            text("consistent with expected type"),
          ]
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
        | [a] => [text("by automatically adding label "), code(a)]
        | _ => [
            text("by automatically adding labels "),
            ...ListUtil.join(text(","), List.map(code, introduced_labels)),
          ]
        }
      )
      @ (
        switch (reordered) {
        | false => []
        | true => [text(" after reordering by labels ")]
        }
      )
    | (_, Ana(InternallyInconsistent({ana, nojoin: tys}))) =>
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
    | Some(l) => [text(" for label "), code(l)]
    }
  );
};

let typ_ok_view = (~globals, cls: Cls.t, ok: Info.ok_typ) => {
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
      text("is an alias for"),
      view_type(ty_lookup),
    ]
  | Variant(name, sum_ty) => [
      view_type(Var(name) |> Typ.fresh),
      text("is a sum type constuctor of type"),
      view_type(sum_ty),
    ]
  | VariantIncomplete(sum_ty) => [
      text("An incomplete sum type constuctor of type"),
      view_type(sum_ty),
    ]
  };
};

let typ_err_view = (~globals, ok: Info.error_typ) => {
  let view_type = view_type(~globals);
  switch (ok) {
  | FreeTypeVariable(name) => [
      view_type(Var(name) |> Typ.fresh),
      text("not found"),
    ]
  | BadToken(token) => [code(token), text("not a type or type operator")]
  | WantConstructorFoundAp
  | WantConstructorFoundType(_) => [text("Expected a constructor")]
  | WantTypeFoundAp => [text("Must be part of a sum type")]
  | WantTuple => [text("Expect a valid tuple")]
  | WantLabel => [text("Expect a valid label")]
  | DuplicateLabels(labels, _) => [
      text("Duplicate labels within tuple: "),
      ...List.map(code, labels),
    ]
  | Duplicate(name, _) => [text("Duplicate Label: "), code(name)]
  | DuplicateConstructor(name) => [
      view_type(Var(name) |> Typ.fresh),
      text("already used in this sum"),
    ]
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

let rec exp_view =
        (~globals, cls: Cls.t, status: Info.status_exp, info: Info.exp) => {
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
  let view_type = view_type(~globals);
  switch (status) {
  | InHole(FreeVariable(name)) => div_err([code(name), text("not found")])
  | InHole(InexhaustiveMatch(additional_err)) =>
    let cls_str = Cls.show(cls);
    switch (additional_err) {
    | None => div_err([text(cls_str ++ " is inexhaustive")])
    | Some(err) =>
      let cls_str = String.uncapitalize_ascii(cls_str);
      div_err([
        exp_view(~globals, cls, InHole(Common(err)), info)
        |> code_box_container,
        text("; " ++ cls_str ++ " is inexhaustive"),
      ]);
    };
  | InHole(UnusedDeferral) =>
    div_err([text("Deferral must appear as a function argument")])
  | InHole(BadPartialAp(NoDeferredArgs)) =>
    div_err([text("Expected at least one non-deferred argument")])
  | InHole(BadPartialAp(ArityMismatch({expected, actual}))) =>
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
  | InHole(InvalidUseMode({bad_typ, _})) =>
    div_err([
      text("Cannot use type "),
      view_type(bad_typ) |> code_box_container,
      text(" for number operators and literals."),
    ])
  | InHole(BadTrivAp(ty)) =>
    div_err([
      text("Function argument type"),
      view_type(ty),
      text("inconsistent with"),
      view_type(Prod([]) |> Typ.fresh),
    ])
  | InHole(WantTuple) =>
    div_err([text("Requires tuple for first argument")])
  | InHole(Common(error)) =>
    div_err(
      common_err_view(
        ~globals,
        ~introduced_labels,
        ~lifted_ty,
        ~inferred_label,
        cls,
        error,
      ),
    )
  | InHole(UnboundLivelit(name)) =>
    div_err([
      text("Livelit with name"),
      code(name),
      text("not found, and also, it's a livelit"),
    ])
  | InHole(BadOperator(msg)) =>
    div_err([text("Invalid operator: "), text(msg)])
  | InHole(LabelNotFound(name, labels)) =>
    div_err([
      text("Label "),
      code(name),
      text(" not found in tuple's labels: "),
      ...List.map(code, labels),
    ])
  | InHole(BadLivelitModel(_)) =>
    div_err([text("Bad internal livelit model")])
  | NotInHole(AnaDeferralConsistent(ana)) =>
    div_ok([text("Expecting type"), view_type(ana)])
  | NotInHole(Common(ok)) =>
    div_ok(
      common_ok_view(
        ~globals,
        ~lifted_ty,
        ~reordered,
        ~introduced_labels,
        ~inferred_label,
        ~label_sort=info.label_sort,
        cls,
        ok,
      ),
    )
  };
};

let rec pat_view =
        (~globals, cls: Cls.t, status: Info.status_pat, info: Info.pat) => {
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

  switch (status) {
  | InHole(ExpectedConstructor) => div_err([text("Expected a constructor")])
  | InHole(Redundant(additional_err)) =>
    switch (additional_err) {
    | None => div_err([text("Pattern is redundant")])
    | Some(err) =>
      div_err([
        pat_view(~globals, cls, InHole(err), info) |> code_box_container,
        text("; pattern is redundant"),
      ])
    }
  | InHole(Common(error)) =>
    div_err(
      common_err_view(
        ~globals,
        ~inferred_label,
        ~introduced_labels,
        ~lifted_ty,
        cls,
        error,
      ),
    )
  | NotInHole(ok) =>
    let ok_view =
      common_ok_view(
        ~globals,
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
    switch (info.warning) {
    | WarningPat(_)
    | WarningExp(_)
    | WarningTyp(_)
    | WarningTPat(_) =>
      if (globals.settings.core.display_warnings) {
        div_warn(common_warn_view(info.warning));
      } else {
        div_ok(ok_view);
      }
    | _ => div_ok(ok_view)
    };
  };
};

let typ_view = (~globals, cls: Cls.t, status: Info.status_typ) =>
  switch (status) {
  | NotInHole(ok) => div_ok(typ_ok_view(~globals, cls, ok))
  | InHole(err) => div_err(typ_err_view(~globals, err))
  };

let tpat_view = (~globals, _: Cls.t, status: Info.status_tpat) => {
  let view_type = view_type(~globals);
  switch (status) {
  | NotInHole(Empty) => div_ok([text("Fillable with a new alias")])
  | NotInHole(Var(name)) => div_ok([ContextInspector.alias_view(name)])
  | InHole(NotAVar(NotCapitalized)) =>
    div_err([text("Must begin with a capital letter")])
  | InHole(NotAVar(_)) => div_err([text("Expected an alias")])
  | InHole(ShadowsType(name, BaseTyp)) =>
    div_err([
      text("Can't shadow base type"),
      view_type(Var(name) |> Typ.fresh),
    ])
  | InHole(ShadowsType(name, TyAlias)) =>
    div_err([
      text("Can't shadow existing alias"),
      view_type(Var(name) |> Typ.fresh),
    ])
  | InHole(ShadowsType(name, TyVar)) =>
    div_err([
      text("Can't shadow existing type variable"),
      view_type(Var(name) |> Typ.fresh),
    ])
  };
};

let secondary_view = (cls: Cls.t) => div_ok([text(cls |> Cls.show)]);

let view_of_info = (~globals, ci): list(Node.t) => {
  let wrapper = status_view => [term_view(~globals, ci), status_view];
  switch (ci) {
  | Secondary(_) => wrapper(div([]))
  | InfoExp({cls, status, _} as ie) =>
    wrapper(exp_view(~globals, cls, status, ie))
  | InfoPat({cls, status, _} as ip) =>
    wrapper(pat_view(~globals, cls, status, ip))
  | InfoTyp({cls, status, _}) => wrapper(typ_view(~globals, cls, status))
  | InfoTPat({cls, status, _}) => wrapper(tpat_view(~globals, cls, status))
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

let view =
    (
      ~globals: Globals.t,
      ~inject: Editors.Update.t => 'a,
      cursor: Cursor.cursor(Editors.Update.t),
    ) => {
  let bar_view = div(~attrs=[Attr.id("bottom-bar")]);
  let err_view = err =>
    bar_view([
      div(
        ~attrs=[Attr.id("cursor-inspector"), clss(["no-info"])],
        [div(~attrs=[clss(["icon"])], [Icons.magnify]), text(err)],
      ),
    ]);
  switch (cursor.info) {
  | _ when !globals.settings.core.statics => div_empty
  | None => err_view("Whitespace or Comment")
  | Some(ci) =>
    bar_view([
      inspector_view(~globals, ci),
      ProjectorPanel.view(
        ~inject=
          a =>
            cursor.editor_action(Project(a))
            |> Option.map(inject)
            |> Option.value(~default=Ui_effect.Ignore),
        cursor,
      ),
    ])
  };
};

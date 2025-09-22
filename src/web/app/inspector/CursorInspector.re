open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;
open Util;
open Language;
open Haz3lcore;
open ErrorMessage;

let errc = "error";
let okc = "ok";
let div_err = div(~attrs=[clss(["status", errc])]);
let div_ok = div(~attrs=[clss(["status", okc])]);
let code_box_container = x =>
  div(~attrs=[clss(["code-box-container"])], [x]);

let code = (code: string): Node.t =>
  div(~attrs=[clss(["code"])], [text(code)]);

let label_view = (label: string): Node.t =>
  div(
    ~attrs=[clss(["code"])],
    [text(Haz3lcore.Token.quote_label_when_necessary(label))],
  );

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
      clss(["ci-header", sort] @ (Info.is_error(ci) ? [errc] : [okc])),
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
  | Exp(BinOp(Poly(_))) => "Operands"
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
  |> CodeViewable.view_any(~globals, ~settings=code_view_settings)
  |> code_box_container;

let view_type = (~globals, typ: Typ.t) =>
  typ
  |> CodeViewable.view_typ(~globals, ~settings=code_view_settings)
  |> code_box_container;

let render_ui = (~globals, fragments) =>
  List.map(
    fun
    | Text(s) => text(s)
    | Code(s) => code(s)
    | Type(ty) => view_type(~globals, ty)
    | Term(term) => view_any(~globals, term)
    | Label(s) => label_view(s),
    fragments,
  );

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
      switch (Haz3lcore.Token.bad_token_cls(token)) {
      | BadInt => [text("Integer is too large or too small")]
      | Other => [text(Printf.sprintf("\"%s\" isn't a valid token", token))]
      }
    | NoType(BadLabel(label)) => [
        text("Malformed Label: "),
        view_any(label),
      ]
    | NoType(FreeConstructor(name)) => [code(name), text("not found")]

    | NoType(InvalidLabel(name, expected_labels)) =>
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
    | NoType(UnexpectedLabelSort(name)) => [
        text("Label "),
        label_view(name),
        text(" is here, but another sort is expected."),
      ]

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
    | DuplicateLabel(name, _) => [
        text("Duplicate Label:"),
        label_view(name),
      ]
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
    | Some(l) => [text(" for label "), label_view(l)]
    }
  );
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
      | Label(l) => [label_view(l)]
      | _ => [text(":"), view_type(syn)]
      }
    | (Pat(Var) | Pat(Wild), Ana(Consistent({ana, _}))) => [
        text(":"),
        view_type(ana),
      ]
    | (_, Ana(Consistent({ana, syn, _})))
        when Typ.fast_equal(~alpha_equivalence=false, ana, syn) =>
      switch (syn.term) {
      | Label(l) => [label_view(l), text(" is a valid label")]
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
    | Some(l) => [text(" for label "), label_view(l)]
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
  | WantLabel => [text("Expect a valid label")]
  | DuplicateLabels(labels, _) => [
      text("Duplicate labels within tuple: "),
      ...List.map(label_view, labels),
    ]
  | Duplicate(name, _) => [text("Duplicate Label: "), label_view(name)]
  | DuplicateConstructor(name) => [
      view_type(Var(name) |> Typ.fresh),
      text("already used in this sum"),
    ]
  | ParseFailure => [text("Parse failure")]
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

let exp_view = (~globals, info: Info.exp) => {
  let msg = build_exp_message(info);
  let content = render_ui(~globals, msg.fragments);
  if (msg.is_error) {
    div_err(content);
  } else {
    div_ok(content);
  };
};

let pat_view = (~globals, info: Info.pat) => {
  let msg = build_pat_message(info);
  let content = render_ui(~globals, msg.fragments);
  if (msg.is_error) {
    div_err(content);
  } else {
    div_ok(content);
  };
};

let typ_view = (~globals, info: Info.typ) => {
  let msg = build_typ_message(info);
  let content = render_ui(~globals, msg.fragments);
  if (msg.is_error) {
    div_err(content);
  } else {
    div_ok(content);
  };
};

let tpat_view = (~globals, info: Info.tpat) => {
  let msg = build_tpat_message(info);
  let content = render_ui(~globals, msg.fragments);
  if (msg.is_error) {
    div_err(content);
  } else {
    div_ok(content);
  };
};

let secondary_view = (cls: Cls.t) => div_ok([text(cls |> Cls.show)]);

let view_of_info = (~globals, ci): list(Node.t) => {
  let wrapper = status_view => [term_view(~globals, ci), status_view];
  switch (ci) {
  | Secondary(_) => wrapper(div([]))
  | InfoExp(ie) => wrapper(exp_view(~globals, ie))
  | InfoPat(ip) => wrapper(pat_view(~globals, ip))
  | InfoTyp(it) => wrapper(typ_view(~globals, it))
  | InfoTPat(it) => wrapper(tpat_view(~globals, it))
  };
};

let inspector_view = (~globals, ci): Node.t =>
  div(
    ~attrs=[
      Attr.id("cursor-inspector"),
      clss([Info.is_error(ci) ? errc : okc]),
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

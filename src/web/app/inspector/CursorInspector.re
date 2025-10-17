open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;
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

let code_view_settings: Haz3lcore.ExpToSegment.Settings.t = {
  inline: true,
  fold_case_clauses: false,
  fold_fn_bodies: false,
  hide_fixpoints: false,
  show_filters: false,
  show_unknown_as_hole: true,
};

let view_any = (~globals, any: Any.t) =>
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

let make_status_view = (~globals, msg: ErrorMessage.message) => {
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
  | InfoExp(ie) =>
    wrapper(make_status_view(~globals, build_exp_message(ie)))
  | InfoPat(ip) =>
    wrapper(make_status_view(~globals, build_pat_message(ip)))
  | InfoTyp(it) =>
    wrapper(make_status_view(~globals, build_typ_message(it)))
  | InfoTPat(it) =>
    wrapper(make_status_view(~globals, build_tpat_message(it)))
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

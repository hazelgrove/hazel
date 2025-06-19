open Virtual_dom.Vdom;
open Node;
open ProjectorBase;
open ProjectorInterface;
open Util.OptUtil.Syntax;
open Language;

/* =========== HELPERS ============ */

let expected_ty = (info: option(Info.t)): option(Typ.t) =>
  switch (info) {
  | Some(InfoExp({ana, _}))
  | Some(InfoPat({ana, _})) => Some(ana)
  | _ => None
  };

let self_ty = (info: option(Info.t)): option(Typ.t) =>
  switch (info) {
  | Some(InfoExp({self, ctx, _})) => Self.typ_of_exp(ctx, self)
  | Some(InfoPat({self, ctx, _})) => Self.typ_of_pat(ctx, self)
  | _ => None
  };

let totalize_ty = (expected_ty: option(Typ.t)): Typ.t =>
  switch (expected_ty) {
  | Some(expected_ty) => expected_ty
  | None => Typ.fresh(Unknown(Internal))
  };

/* =========== MODEL =========== */

[@deriving (show({with_path: false}), sexp, yojson)]
type mode =
  | Expected
  | Self;

[@deriving (show({with_path: false}), sexp, yojson)]
type model('ed) = (mode, 'ed);

let init =
    (~copy_ed as _, any: Term.Any.t, ed: unit => option('ed))
    : option(model('ed)) => {
  switch (any) {
  | Exp(_)
  | Pat(_) =>
    let* ed = ed();
    Some((Expected, ed));
  | Any () =>
    let* ed = ed();
    Some((Expected, ed)); /* Grout don't have sorts rn */
  | _ => None
  };
};

/* =========== UPDATE =========== */

[@deriving (show({with_path: false}), sexp, yojson)]
type action('ed_a) =
  | ToggleDisplay;

let update =
    (~update_ed as _, ~common as _, ~sort as _, _, model, a: action('ed_a)) =>
  switch (a, model) {
  | (ToggleDisplay, (Expected, m)) => (Self, m)
  | (ToggleDisplay, (Self, m)) => (Expected, m)
  };

let mk_term = (~mk_term_ed, ~sort, ~prev as _, (mode, ed)) => {
  let (ed', term) = mk_term_ed(~sort, ed);
  ((mode, ed'), term);
};

let calculate = (~calculate_ed, ~common, (mode, ed): model('ed)) => {
  let ed' = calculate_ed(~common, ed);
  (mode, ed');
};

/* =========== FOCUS =========== */

[@deriving (show({with_path: false}), sexp, yojson)]
type focus('ed_f) =
  |;

/* =========== VIEW =========== */

let display_ty = (model, statics): option(Typ.t) =>
  switch (model) {
  | _ when expected_ty(statics) |> totalize_ty |> Typ.is_syn =>
    statics |> self_ty
  | Self => statics |> self_ty
  | Expected => statics |> expected_ty
  };

let display_mode = (model: model('ed), statics: option(Info.t)): string =>
  switch (model) {
  | _ when self_ty(statics) == expected_ty(statics) => "⇔"
  | _ when expected_ty(statics) |> totalize_ty |> Typ.is_syn => "⇒"
  | (Self, _) => "⇒"
  | (Expected, _) => "⇐"
  };

let mode_view = (model, info) =>
  div(
    ~attrs=[Attr.classes(["mode"])],
    [text(display_mode(model, info))],
  );

let typ_view = (model, info: info, view_any: Any.t => Node.t) => {
  let typ = display_ty(model, info.statics) |> totalize_ty;
  div(~attrs=[Attr.classes(["type-cell"])], [Typ(typ) |> view_any]);
};

let placeholder = (~ed_size: 'a => Util.Point.t, (_, ed), _info) =>
  Util.ProjectorShape.inline(3 + ed_size(ed).row);
let icon = div(~attrs=[Attr.classes(["icon"])], []);

let view =
    (
      ~common,
      ~ed_str,
      ~view_ed: (~sort: Sort.t, ~background: bool=?, 'ed_m) => Node.t,
      ~view_editable as _,
      ~enter_ed as _,
      ~mk_ed,
      ~mk_term_ed,
      ~calculate_ed,
      ~local,
      ~parent as _,
      ~focus as _,
      ~focussed as _,
      model,
      info,
    ) => {
  let view_any = x =>
    x
    |> mk_ed
    |> mk_term_ed(~sort=Any.sort(x))
    |> fst
    |> calculate_ed(
         ~common={
                   ...common,
                   statics: CachedStatics.empty,
                   dynamics: Dynamics.Map.empty,
                 }: ProjectorInterface.common,
       )
    |> view_ed(~sort=Any.sort(x));
  View.{
    inline:
      div(
        ~attrs=[
          Attr.classes(["main"]),
          Attr.on_double_click(_ => local(ToggleDisplay)),
        ],
        [model |> snd |> ed_str |> text, icon],
      ),
    offside:
      Some(
        div(
          ~attrs=[Attr.classes(["offside"])],
          [
            mode_view(model, info.statics),
            typ_view(fst(model), info, view_any),
          ],
        ),
      ),
    overlay: None,
    enter_left: None,
    enter_right: None,
  };
};

let get_cursor_info =
    (
      ~get_cursor_info_ed as _,
      ~common as _,
      ~inject as _: action('a) => Ui_effect.t(unit),
      ~read_only as _,
      _model,
      _focus,
    ) => Cursor.empty;

let methods = {
  init,
  dynamics: false,
  view,
  placeholder,
  update,
  mk_term,
  calculate,
  get_cursor_info,
  sexp_of_model,
  model_of_sexp,
  yojson_of_model,
  model_of_yojson,
  sexp_of_action,
  action_of_sexp,
  yojson_of_action,
  action_of_yojson,
  sexp_of_focus,
  focus_of_sexp,
  yojson_of_focus,
  focus_of_yojson,
};

module M =
       (Editor: ProjectorInterface.EDITOR)

         : (
           ProjectorInterface.PROJECTOR with
             type model' = model(Editor.model) and
             type action' = action(Editor.action) and
             type focus' = focus(Editor.focus) and
             type editor_model = Editor.model
       ) => {
  type editor_model = Editor.model;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type model' = model(Editor.model);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action' = action(Editor.action);
  [@deriving (show({with_path: false}), sexp, yojson)]
  type focus' = focus(Editor.focus);

  let mk = (any, ed) => init(~copy_ed=Editor.Model.copy, any, ed);
  let dynamics = false;
  let placeholder = (model, info) =>
    placeholder(~ed_size=Editor.View.get_dimensions, model, info);
  let update = (~common, ~sort, info, model, a) =>
    update(~update_ed=Editor.Update.update, ~common, ~sort, info, model, a);
  let mk_term = (~sort, ~prev, model) =>
    mk_term(~mk_term_ed=Editor.Update.make_term, ~sort, ~prev, model);
  let calculate = (~common, model) =>
    calculate(~calculate_ed=Editor.Update.calculate, ~common, model);
  let get_cursor_info = (~common, ~inject, ~read_only, model, focus) =>
    get_cursor_info(
      ~get_cursor_info_ed=Editor.Focus.get_cursor_info,
      ~common,
      ~inject,
      ~read_only,
      model,
      focus,
    );

  let view = (~common, ~local, ~parent, ~focus, ~focussed, model, info) =>
    view(
      ~common,
      ~ed_str=Editor.View.print_string,
      ~view_ed=
        Editor.View.view(
          ~font_metrics=common.font_metrics,
          ~secondary_icons=common.secondary_icons,
        ),
      ~view_editable=Editor.View.view_editable,
      ~enter_ed=Editor.Focus.enter,
      ~mk_ed=Editor.Model.mk,
      ~mk_term_ed=Editor.Update.make_term,
      ~calculate_ed=Editor.Update.calculate,
      ~local,
      ~parent,
      ~focus,
      ~focussed,
      model,
      info,
    );
};

open Virtual_dom.Vdom;
open Node;
open ProjectorInterface;
open Util.OptUtil.Syntax;
open Language;

[@deriving (show({with_path: false}), sexp, yojson)]
type mode =
  | Expected
  | Self;

[@deriving (show({with_path: false}), sexp, yojson)]
type model('ed) = (mode, 'ed);

[@deriving (show({with_path: false}), sexp, yojson)]
type action('ed_a) =
  | ToggleDisplay;

[@deriving (show({with_path: false}), sexp, yojson)]
type focus('ed_f) =
  |;

module M =
       (Editor: EditorInterface.EDITOR)

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

  let typ_view = (model, statics, view_any: Any.t => Node.t) => {
    let typ = display_ty(model, statics) |> totalize_ty;
    div(~attrs=[Attr.classes(["type-cell"])], [Typ(typ) |> view_any]);
  };

  let icon = div(~attrs=[Attr.classes(["icon"])], []);

  /* =========== PROJECTOR INTERFACE IMPLEMENTATION =========== */

  let mk =
      (any: Term.Any.t, ed: unit => option(Editor.model))
      : option(model(Editor.model)) => {
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

  let dynamics = false;

  let placeholder = (~common as _, ~id as _, model) =>
    Util.ProjectorShape.inline(
      3 + Editor.View.get_dimensions(snd(model)).row,
    );

  let update = (~common as _, ~sort as _, ~id as _, model, a) =>
    switch (a, model) {
    | (ToggleDisplay, (Expected, m)) => (Self, m)
    | (ToggleDisplay, (Self, m)) => (Expected, m)
    };

  let mk_term = (~sort, ~prev as _, (mode, ed)) => {
    let (ed', term) = Editor.Update.make_term(~sort, ed);
    ((mode, ed'), term);
  };

  let calculate = (~common, (mode, ed)) => {
    let ed' = Editor.Update.calculate(~common, ed);
    (mode, ed');
  };

  let get_cursor_info =
      (
        ~common as _,
        ~inject as _,
        ~take_focus as _,
        ~read_only as _,
        _model,
        _focus,
      ) => Cursor.empty;

  let view =
      (
        ~common: Common.t,
        ~inject,
        ~escape as _,
        ~take_focus as _,
        ~focus as _,
        ~id,
        model,
      ) => {
    let statics = Statics.Map.lookup(id, common.statics.info_map);
    let view_any = x =>
      x
      |> Editor.Model.mk
      |> Editor.Update.make_term(~sort=Any.sort(x))
      |> fst
      |> Editor.Update.calculate(
           ~common={
                     ...common,
                     statics: CachedStatics.empty,
                     dynamics: Dynamics.Map.empty,
                   }: Common.t,
         )
      |> Editor.View.view(~common, ~mode=ReadOnly, ~sort=Any.sort(x));

    View.{
      inline:
        div(
          ~attrs=[
            Attr.classes(["main"]),
            Attr.on_double_click(_ => inject(ToggleDisplay)),
          ],
          [model |> snd |> Editor.View.print_string |> text, icon],
        ),
      offside:
        Some(
          div(
            ~attrs=[Attr.classes(["offside"])],
            [
              mode_view(model, statics),
              typ_view(fst(model), statics, view_any),
            ],
          ),
        ),
      overlay: None,
      enter_left: None,
      enter_right: None,
    };
  };
};

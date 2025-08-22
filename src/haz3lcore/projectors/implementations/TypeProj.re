open Virtual_dom.Vdom;
open Node;
open ProjectorBase;
open Language;
open Util;

let expected_ty = (info: option(Info.t)): option(Typ.t) =>
  switch (info) {
  | Some(InfoExp({ana, _}))
  | Some(InfoPat({ana, _})) => Some(ana)
  | _ => None
  };

let self_ty = (info: option(Info.t)): option(Typ.t) =>
  switch (info) {
  | Some(InfoExp({self, _})) => Self.typ_of_exp(self)
  | Some(InfoPat({self, _})) => Self.typ_of_pat(self)
  | _ => None
  };

let totalize_ty = (expected_ty: option(Typ.t)): Typ.t =>
  switch (expected_ty) {
  | Some(expected_ty) => expected_ty
  | None => Typ.fresh(Unknown(Internal))
  };

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model =
    | Expected
    | Self;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    | ToggleDisplay;

  let init = (any: Term.Any.t): option(model) => {
    switch (any) {
    | Exp(_)
    | Pat(_) => Some(Expected)
    | Any () => Some(Expected) /* Grout don't have sorts rn */
    | _ => None
    };
  };

  let dynamics = false;
  let focusable = Focusable.non;

  let display_ty = (model, statics): option(Typ.t) =>
    switch (model) {
    | _ when expected_ty(statics) |> totalize_ty |> Typ.is_syn =>
      statics |> self_ty
    | Self => statics |> self_ty
    | Expected => statics |> expected_ty
    };

  let display_mode = (model: model, statics: option(Language.Info.t)): string =>
    switch (model) {
    | _ when self_ty(statics) == expected_ty(statics) => "⇔"
    | _ when expected_ty(statics) |> totalize_ty |> Typ.is_syn => "⇒"
    | Self => "⇒"
    | Expected => "⇐"
    };

  let mode_view = (model, info) =>
    div(
      ~attrs=[Attr.classes(["mode"])],
      [text(display_mode(model, info))],
    );

  let typ_view = (model, info: info, utility, view_seg: View.seg) => {
    let typ = display_ty(model, info.statics) |> totalize_ty;
    div(
      ~attrs=[Attr.classes(["type-cell"])],
      [Typ(typ) |> utility.term_to_seg |> view_seg(Sort.Typ)],
    );
  };

  let update = (model, _, a: action) =>
    switch (a, model) {
    | (ToggleDisplay, Expected) => Self
    | (ToggleDisplay, Self) => Expected
    };

  let syntax_str = (info: info) => {
    let max_len = 30;
    let seg = Segment.unparenthesize(info.syntax);
    let str = info.utility.seg_to_string(seg);
    let str = StringUtil.replace(StringUtil.regexp({|\n|}), str, " ");
    String.length(str) > max_len
      ? String.sub(str, 0, max_len) ++ "..." : str;
  };

  let placeholder = (_m, info) =>
    ProjectorCore.Shape.inline(3 + String.length(syntax_str(info)));

  let syntax_view = (info: info) => info |> syntax_str |> text;

  let icon = div(~attrs=[Attr.classes(["icon"])], []);

  let view = (model, info, ~local, ~parent as _, ~view_seg) =>
    View.{
      inline:
        div(
          ~attrs=[
            Attr.classes(["main"]),
            Attr.on_double_click(_ => local(ToggleDisplay)),
          ],
          [syntax_view(info), icon],
        ),
      offside:
        Some(
          div(
            ~attrs=[Attr.classes(["offside"])],
            [
              mode_view(model, info.statics),
              typ_view(model, info, info.utility, view_seg),
            ],
          ),
        ),
      overlay: None,
    };
};

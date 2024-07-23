open Virtual_dom.Vdom;
open Node;
open ProjectorBase;

let mode = (info: option(Info.t)): option(Mode.t) =>
  switch (info) {
  | Some(InfoExp({mode, _}))
  | Some(InfoPat({mode, _})) => Some(mode)
  | _ => None
  };

let expected_ty = (info: option(Info.t)): option(Typ.t) =>
  switch (mode(info)) {
  | Some(mode) => Some(Mode.ty_of(mode))
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
  | None => Unknown(Internal)
  };

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model =
    | Expected
    | Self;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    | ToggleDisplay;

  let init = Expected;

  let can_project = (p: Piece.t): bool => {
    switch (Piece.sort(p)) {
    | (Exp | Pat, _) => true
    | _ when Piece.is_grout(p) => true /* Grout don't have sorts rn */
    | _ => false
    };
  };

  let can_focus = false;

  let display_ty = (model, info) =>
    switch (model) {
    | _ when mode(info) == Some(Syn) => info |> self_ty
    | Self => info |> self_ty
    | Expected => info |> expected_ty
    };

  let display_mode = (model, info): string =>
    switch (model) {
    | _ when mode(info) == Some(Syn) => "⇐"
    | Self => "⇐"
    | Expected => "⇒"
    };

  let display = (model, info) =>
    display_ty(model, info) |> totalize_ty |> Typ.pretty_print;

  let placeholder = (model, info: ProjectorBase.info) =>
    Inline((display(model, info.ci) |> String.length) + 5);

  let update = (model, a: action) =>
    switch (a, model) {
    | (ToggleDisplay, Expected) => Self
    | (ToggleDisplay, Self) => Expected
    };

  let view = (model, ~info: ProjectorBase.info, ~local, ~parent as _) =>
    div(
      ~attrs=[
        Attr.class_("info"),
        Attr.on_mousedown(_ => local(ToggleDisplay)),
      ],
      [
        text("⋱ " ++ display_mode(model, info.ci) ++ " "),
        div(
          ~attrs=[Attr.class_("type")],
          [text(display(model, info.ci))],
        ),
      ],
    );
  let focus = _ => ();
};

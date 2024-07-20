open Virtual_dom.Vdom;
open Node;
open ProjectorBase;

type inner_action +=
  | ToggleDisplay;

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

let display_ty = (expected_ty: option(Typ.t)): Typ.t =>
  switch (expected_ty) {
  | Some(expected_ty) => expected_ty
  | None => Unknown(Internal)
  };

let syn_display = info =>
  "⋱ ⇒ " ++ (info |> self_ty |> display_ty |> Typ.pretty_print);

let ana_display = info =>
  "⋱ ⇐ " ++ (info |> expected_ty |> display_ty |> Typ.pretty_print);

module M: CoreInner = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model =
    | Expected
    | Self;

  let init = Expected;

  let can_project = (p: Piece.t): bool => {
    switch (Piece.sort(p)) {
    | (Exp | Pat, _) => true
    | _ when Piece.is_grout(p) => true /* Grout don't have sorts rn */
    | _ => false
    };
  };

  let display = (model, info) =>
    switch (model) {
    | _ when mode(info) == Some(Syn) => syn_display(info)
    | Self => syn_display(info)
    | Expected => ana_display(info)
    };

  let placeholder = (model, info: ProjectorBase.info) =>
    Inline((display(model, info.ci) |> String.length) - 2);

  let update = (model, a: inner_action) =>
    switch (a, model) {
    | (ToggleDisplay, Expected) => Self
    | (ToggleDisplay, Self) => Expected
    | _ => model
    };

  let view = (model, ~info: ProjectorBase.info, ~inject) =>
    div(
      ~attrs=[Attr.on_mousedown(_ => inject(UpdateModel(ToggleDisplay)))],
      [text(display(model, info.ci))],
    );
  let activate = _ => ();
};

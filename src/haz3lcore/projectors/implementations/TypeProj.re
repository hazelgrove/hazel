open ProjectorBase;
open Language;

let expected_ty = (info: option(Info.t)): option(Typ.t) =>
  switch (info) {
  | Some(InfoExp({ana, _}))
  | Some(InfoPat({ana, _})) => Some(ana)
  | _ => None
  };

let self_ty = (info: option(Info.t)): option(Typ.t) =>
  switch (info) {
  | Some(InfoExp({elab_syn_ty, _}))
  | Some(InfoPat({elab_syn_ty, _})) => Some(elab_syn_ty)
  | _ => None
  };

let totalize_ty = (expected_ty: option(Typ.t)): Typ.t =>
  switch (expected_ty) {
  | Some(expected_ty) => expected_ty
  | None => Typ.fresh(Unknown(Internal))
  };

/* The model and its display logic live at file level (outside the
   sealed module below) so that alternative view backends (e.g. the
   TUI) can reuse the projector's semantics without going through the
   Vdom view. */
[@deriving (show({with_path: false}), sexp, yojson)]
type display =
  | Expected
  | Self;

[@deriving (show({with_path: false}), sexp, yojson)]
type type_action =
  | ToggleDisplay;

let display_ty = (model: display, statics): option(Typ.t) =>
  switch (model) {
  | _ when expected_ty(statics) |> totalize_ty |> Typ.is_syn =>
    statics |> self_ty
  | Self => statics |> self_ty
  | Expected => statics |> expected_ty
  };

let display_mode = (model: display, statics: option(Info.t)): string =>
  switch (model) {
  | _ when self_ty(statics) == expected_ty(statics) => "⇔"
  | _ when expected_ty(statics) |> totalize_ty |> Typ.is_syn => "⇒"
  | Self => "⇒"
  | Expected => "⇐"
  };

module M: Projector with type model = display and type action = type_action = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = display;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = type_action;

  let init = (any: Any.t): option(model) => {
    switch (any) {
    | Exp(_)
    | Pat(_) => Some(Expected)
    | Any () => Some(Expected) /* Grout don't have sorts rn */
    | _ => None
    };
  };

  let dynamics = false;
  let elaborate_syntax = false;

  let update = (model, _, a: action) =>
    switch (a, model) {
    | (ToggleDisplay, Expected) => Self
    | (ToggleDisplay, Self) => Expected
    };

  let placeholder = (_, _) => ProjectorCore.Shape.default;
  let error = (_, _): option(ProjectorBase.error) => None;
  let initialize = None;
};

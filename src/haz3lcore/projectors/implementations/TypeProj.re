open Virtual_dom.Vdom;
open Node;
open ProjectorBase;

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

let init = (any: Term.Any.t, ed: unit => 'ed): option(model('ed)) => {
  switch (any) {
  | Exp(_)
  | Pat(_) => Some((Expected, ed()))
  | Any () => Some((Expected, ed())) /* Grout don't have sorts rn */
  | _ => None
  };
};

/* =========== UPDATE =========== */

[@deriving (show({with_path: false}), sexp, yojson)]
type action =
  | ToggleDisplay;

let update = (model, _, a: action) =>
  switch (a, model) {
  | (ToggleDisplay, Expected) => Self
  | (ToggleDisplay, Self) => Expected
  };

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

let placeholder = (~ed_length, (_, ed), info) =>
  ProjectorShape.inline(3 + ed_length(ed));
let icon = div(~attrs=[Attr.classes(["icon"])], []);

let view = (~ed_str, model, info, ~local, ~parent as _, ~view_any) =>
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
  };

let mk_term = mk_term_default;

let methods = {
  init,
  focusable: Focusable.non,
  dynamics: false,
  view,
  placeholder,
  update,
  mk_term,
};

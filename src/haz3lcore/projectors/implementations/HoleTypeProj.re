open Virtual_dom.Vdom;
open Node;
open ProjectorBase;
open Language;
open Util;

let totalize_ty = (expected_ty: option(Typ.t)): Typ.t =>
  switch (expected_ty) {
  | Some(expected_ty) => expected_ty
  | None => Typ.fresh(Unknown(Internal |> Prov.fresh))
  };

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model =
    | Hole;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    | NoAction;

  let init = (any: Term.Any.t): option(model) => {
    switch (any) {
    | Typ({term: Unknown(_), _}) => Some(Hole)
    | _ => Some(Hole)
    };
  };

  let dynamics = false;
  let focusable = Focusable.non;

  let display_ty = (inference): option(Typ.t) =>
    switch (inference) {
    | Some(inf) => Some(Inference.typ_of_solution(inf) |> Typ.temp)
    | None => None
    };

  let typ_view = (info: info, utility, view_seg: View.seg) => {
    let typ = display_ty(info.inference) |> totalize_ty;
    div(
      ~attrs=[Attr.classes(["type-cell"])],
      [Typ(typ) |> utility.term_to_seg |> view_seg(Sort.Typ)],
    );
  };

  let update = (model, _, a: action) =>
    switch (a, model) {
    | (NoAction, Hole) => Hole
    };

  let placeholder = (_m, info) =>
    ProjectorCore.Shape.inline(
      1
      + String.length(
          Typ(display_ty(info.inference) |> totalize_ty)
          |> info.utility.term_to_seg
          |> info.utility.seg_to_string,
        ),
    );

  // let icon = div(~attrs=[Attr.classes(["icon"])], []);

  let view = (_, info, ~local as _, ~parent as _, ~view_seg) =>
    View.{
      inline:
        div(
          ~attrs=[Attr.classes(["main"])],
          [typ_view(info, info.utility, view_seg)],
        ),
      offside: None,
      overlay: None,
    };
};

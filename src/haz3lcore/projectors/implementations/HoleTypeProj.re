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
    | Typ(int);

  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    | MoveCursor(int);

  let init = (any: Any.t): option(model) => {
    switch (any) {
    | Typ({term: Unknown(_), _}) => Some(Typ(0))
    | Any(_) => Some(Typ(0))
    | _ => None
    };
  };

  let dynamics = false;
  let focusable = Focusable.non;

  let nav_bar_view = local => {
    let nav_arrow = (offset: int): Node.t =>
      Node.div(
        ~attrs=[
          Attr.classes(["nav-arrow"]),
          Attr.on_click(_ => local(MoveCursor(offset))),
        ],
        [],
      );
    div(
      ~attrs=[Attr.classes(["nav-bar"])],
      [nav_arrow(-1), nav_arrow(1)],
    );
  };

  let display_ty = (model: model, inference): option(Typ.t) => {
    switch (model) {
    | Typ(0) =>
      switch (inference) {
      | Some(Single(t)) => Some(t)
      | Some(Many(_))
      | None => None
      }
    | Typ(i) =>
      switch (inference) {
      | Some(Single(t)) => Some(t)
      | Some(Many(tys)) => List.nth_opt(Lazy.force(tys), i - 1)
      | None => None
      }
    };
  };

  let typ_view = (model: model, info: info, utility, view_seg: View.seg) => {
    let typ = display_ty(model, info.inference) |> totalize_ty;
    div([
      // ~attrs=[Attr.classes(["type-cell"])],
      Typ(typ) |> utility.term_to_seg |> view_seg(Sort.Typ),
    ]);
  };

  let update = (model, info, a: action) =>
    switch (a, model) {
    | (MoveCursor(offset), Typ(i)) =>
      switch (info.inference) {
      | Some(sol_count) =>
        let num_tys =
          switch (sol_count) {
          | Single(_) => 1
          | Many(inf) => List.length(Lazy.force(inf))
          };
        let next_typ_index = (i + offset) mod num_tys;
        Typ(next_typ_index <= 0 ? num_tys : next_typ_index);
      | None => Typ(0)
      }
    };

  let has_single_type = (info: info): bool =>
    switch (info.inference) {
    | Some(Single(_)) => true
    | Some(Many(_))
    | None => false
    };

  let placeholder = (model, info) =>
    ProjectorCore.Shape.inline(
      (has_single_type(info) ? 1 : 3)
      + String.length(
          Typ(display_ty(model, info.inference) |> totalize_ty)
          |> info.utility.term_to_seg
          |> info.utility.seg_to_string,
        ),
    );

  // let icon = div(~attrs=[Attr.classes(["icon"])], []);

  let view = ({model, info, local, view_seg, _}: View.args(model, action)) =>
    View.{
      inline:
        div(
          ~attrs=[Attr.classes(["main"])],
          (has_single_type(info) ? [] : [nav_bar_view(local)])
          @ [typ_view(model, info, info.utility, view_seg)],
        ),
      offside: None,
      overlay: None,
    };
};

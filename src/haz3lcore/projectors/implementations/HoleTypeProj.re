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
    | NextTyp;

  let init = (any: Term.Any.t): option(model) => {
    switch (any) {
    | Typ({term: Unknown(_), _}) => Some(Typ(0))
    | _ => Some(Typ(0))
    };
  };

  let dynamics = false;
  let focusable = Focusable.non;

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
    div(
      ~attrs=[Attr.classes(["type-cell"])],
      [Typ(typ) |> utility.term_to_seg |> view_seg(Sort.Typ)],
    );
  };

  let update = (model, info, a: action) =>
    switch (a, model) {
    | (NextTyp, Typ(0)) => Typ(1)
    | (NextTyp, Typ(i)) =>
      switch (info.inference) {
      | Some(inf) =>
        let num_tys =
          switch (inf) {
          | Single(_) => 1
          | Many(inf) => List.length(Lazy.force(inf))
          };
        Typ(i mod num_tys + 1);
      | None => Typ(0)
      }
    };

  let placeholder = (model, info) =>
    ProjectorCore.Shape.inline(
      1
      + String.length(
          Typ(display_ty(model, info.inference) |> totalize_ty)
          |> info.utility.term_to_seg
          |> info.utility.seg_to_string,
        ),
    );

  // let icon = div(~attrs=[Attr.classes(["icon"])], []);

  let view = (model, info, ~local, ~parent as _, ~view_seg) =>
    View.{
      inline:
        div(
          ~attrs=[
            Attr.classes(["main"]),
            Attr.on_click(_ => local(NextTyp)),
          ],
          [typ_view(model, info, info.utility, view_seg)],
        ),
      offside: None,
      overlay: None,
    };
};

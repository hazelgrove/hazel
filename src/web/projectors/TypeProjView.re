open Virtual_dom.Vdom;
open Node;
open Haz3lcore;
open ProjectorBase;
open ProjectorViewBase;

let mode_view = (model, info) =>
  div(
    ~attrs=[Attr.classes(["mode"])],
    [text(TypeProj.display_mode(model, info))],
  );

let typ_view = (model, info: info, utility, view_seg: View.seg) => {
  let typ = TypeProj.display_ty(model, info.statics) |> TypeProj.totalize_ty;
  div(
    ~attrs=[Attr.classes(["type-cell"])],
    [
      Typ(typ)
      |> utility.term_to_seg(~inline=true)
      |> view_seg(~single_line=true, Sort.Typ),
    ],
  );
};

module V: ProjectorView = {
  module L = TypeProj.M;

  let focusable = Focusable.non;

  let view =
      ({model, info, local, view_seg, _}: View.args(L.model, L.action)) =>
    View.{
      inline: div([]),
      offside:
        Some(
          div(
            ~attrs=[
              Attr.id(Id.cls(info.id)),
              Attr.tabindex(0),
              Attr.classes(["offside"]),
              Attr.on_double_click(_ => local(TypeProj.ToggleDisplay)),
            ],
            [
              mode_view(model, info.statics),
              typ_view(model, info, info.utility, view_seg),
            ],
          ),
        ),
      overlay: None,
      error: false,
    };
};

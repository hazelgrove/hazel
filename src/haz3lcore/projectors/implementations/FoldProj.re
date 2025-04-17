open Util;
open ProjectorBase;
open Virtual_dom.Vdom;
open Node;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = ProjectorCore.Kind.fold_model;

module M: Projector with type model = t = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = t;
  let kind = ProjectorCore.Kind.Fold;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;

  let init = _ => Some({text: "⋱"}: t);

  let focusable = Focusable.non;
  let dynamics = false;

  let placeholder = (m: t, _) =>
    ProjectorShape.inline(m.text == "⋱" ? 2 : m.text |> String.length);
  let update = (m, _, _) => m;

  let hover_view = (view_seg: View.seg('p), info: info('p)) => {
    let seg = Segment.unparenthesize(info.syntax);
    let sort = Segment.sort_of(Segment.skel(seg), seg);
    div(
      ~attrs=[Attr.class_("hover-view")],
      [
        view_seg(~background=true, sort, Segment.unparenthesize(info.syntax)),
      ],
    );
  };

  let view = (m: model, info: info('p), ~local as _, ~parent, ~view_seg) =>
    ProjectorBase.View.mk(
      div(
        ~attrs=[Attr.on_double_click(_ => parent(Remove))],
        [text(m.text), hover_view(view_seg, info)],
      ),
    );
};

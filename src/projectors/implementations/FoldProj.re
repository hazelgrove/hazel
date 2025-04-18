open Util;
open ProjectorBase;
open Virtual_dom.Vdom;
open Node;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = ProjectorCore.Kind.fold_model;

module Make: Projector =
  (
    Syntax: {
      type segment;
      let unparenthesize: segment => segment;
      let sort_of: segment => Semantics.Sort.t;
    },
  ) => {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type model = t;
    let kind = ProjectorCore.Kind.Fold;
    [@deriving (show({with_path: false}), sexp, yojson)]
    type action = unit;

    let init = _ => Some({text: "⋱"}: t);

    let focusable = Focusable.non;
    let dynamics = false;

    let placeholder = (m: t, _) =>
      ProjectorCore.Shape.inline(
        m.text == "⋱" ? 2 : m.text |> String.length,
      );
    let update = (m, _, _) => m;

    let hover_view = (view_seg: View.seg('s), info: info('s)) => {
      let seg = Syntax.unparenthesize(info.syntax);
      let sort = Syntax.sort_of(seg);
      div(
        ~attrs=[Attr.class_("hover-view")],
        [
          view_seg(
            ~background=true,
            sort,
            Syntax.unparenthesize(info.syntax),
          ),
        ],
      );
    };

    let view = (m: model, info, ~local as _, ~parent, ~view_seg) =>
      ProjectorBase.View.mk(
        div(
          ~attrs=[Attr.on_double_click(_ => parent(Remove))],
          [text(m.text), hover_view(view_seg, info)],
        ),
      );
  };

open Util;
open ProjectorBase;
open Virtual_dom.Vdom;
open Node;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  [@default "⋱"]
  text: string,
  expanded: bool,
};

let default: t = {
  text: "⋱",
  expanded: false,
};

let t_of_sexp = (sexp: Sexplib.Sexp.t): t =>
  switch (t_of_sexp(sexp)) {
  | exception _ => default
  | t => t
  };

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = t;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    | Toggle;

  let init = _ => Some(default);

  let focusable = Focusable.non;
  let dynamics = false;

  let placeholder = (m, _) =>
    ProjectorCore.Shape.inline(m.text == "⋱" ? 2 : m.text |> String.length);
  let update = (m, _, _) => {
    ...m,
    expanded: !m.expanded,
  };

  let hover_view = (view_seg: View.seg, info: info) => {
    let seg = Segment.unparenthesize(info.syntax);
    let sort = Segment.sort_of(Segment.skel(seg), seg);
    div(
      ~attrs=[Attr.class_("hover-view")],
      [
        view_seg(~background=true, sort, Segment.unparenthesize(info.syntax)),
      ],
    );
  };

  let view = (m: model, info, ~local, ~parent, ~view_seg) =>
    ProjectorBase.View.mk(
      div(
        ~attrs=[
          Attr.on_click(_ => local(Toggle)),
          Attr.on_double_click(_ => parent(Remove)),
        ],
        [text(m.text)] @ (m.expanded ? [hover_view(view_seg, info)] : []),
      ),
    );
};

open Util;
open ProjectorBase;
open Virtual_dom.Vdom;
open Node;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  [@default "⋱"]
  text: string,
  expanded: bool,
  always_render: bool,
};

let default: t = {
  text: "⋱",
  expanded: false,
  always_render: false,
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

  let hover_view = (view_seg: View.seg, m, info: info) => {
    let seg = Segment.unparenthesize(info.syntax);
    let sort = Segment.sort_of(Segment.skel(seg), seg);
    div(
      ~attrs=[
        Attr.classes(
          ["hover-view"]
          @ (
            m.always_render
              ? ["always-render"] : m.expanded ? [] : ["collapsed"]
          ),
        ),
      ],
      [
        view_seg(~background=true, sort, Segment.unparenthesize(info.syntax)),
      ],
    );
  };

  let view =
      ({model, info, local, view_seg, status, _}: View.args(model, action)) =>
    ProjectorBase.View.mk(
      if (model.always_render) {
        /* Always render mode: Use checkbox hack for CSS-only toggle */
        let checkbox_id = "fold-toggle-" ++ Id.to_string(info.id);
        label(
          ~attrs=[
            Attr.create("for", checkbox_id),
            Attr.classes(["fold-always-render"]),
          ],
          [
            input(
              ~attrs=[
                Attr.create("type", "checkbox"),
                Attr.id(checkbox_id),
                Attr.classes(["fold-toggle-checkbox"]),
                Attr.create("style", "display: none;"),
              ],
              (),
            ),
            text(model.text),
            hover_view(view_seg, model, info),
          ],
        );
      } else {
        div(
          ~attrs=[
            Attr.on_double_click(_ =>
              status.indication != None ? local(Toggle) : Ui_effect.Ignore
            ),
          ],
          [text(model.text)]
          @ (model.expanded ? [hover_view(view_seg, model, info)] : []),
        );
      },
    );
};

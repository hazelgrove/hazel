open Virtual_dom.Vdom;
open Node;
open Haz3lcore;
open ProjectorBase;
open ProjectorViewBase;

let hover_view = (view_seg: View.seg, m: FoldProj.t, info: info) => {
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
    [view_seg(~background=true, sort, Segment.unparenthesize(info.syntax))],
  );
};

module V: ProjectorView = {
  module L = FoldProj.M;

  let focusable = Focusable.non;

  let view =
      (
        {model, info, local, view_seg, status, _}:
          View.args(L.model, L.action),
      ) =>
    View.mk(
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
              status.indication != None
                ? local(FoldProj.Toggle) : Ui_effect.Ignore
            ),
          ],
          [text(model.text)]
          @ (model.expanded ? [hover_view(view_seg, model, info)] : []),
        );
      },
    );
};

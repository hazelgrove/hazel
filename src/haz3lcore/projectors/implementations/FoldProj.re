open Util;
open ProjectorBase;
open Virtual_dom.Vdom;
open Node;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  [@default "⋱"]
  text: string,
};

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = t;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;

  let init = {text: "⋱"};

  let can_project = (_, any: Any.t) =>
    switch (any) {
    | TPat(_) =>
      /* Because TPat has no parentheses, the current parenthesis-based approach
       * causes them to break when unwrapped in MakeTerm. In the absence of a more
       * robust approach, we currently prohibit folding them */
      false
    | Typ(_) =>
      /* While types do have parentheses, sum type constructor definitions are
       * implemented in a bespoke way which breaks if they are parenthesized.
       * Easier to just prohibit folding types for now. */
      false
    | _ => true
    };
  let focus = _ => ();
  let can_focus = false;
  let dynamics = false;

  let placeholder = (m, _) =>
    ProjectorCore.inline(m.text == "⋱" ? 2 : m.text |> String.length);
  let update = (m, _, _) => m;

  let hover_view = (view_seg: view_seg, info: info) =>
    div(
      ~attrs=[Attr.class_("hover-view")],
      switch (Segment.unparenthesize(info.syntax)) {
      | Some(seg) => [view_seg(~background=true, Exp, seg)]
      | None => []
      },
    );

  let view = (m: model, info, ~local as _, ~parent, ~view_seg) =>
    div(
      ~attrs=[Attr.on_double_click(_ => parent(Remove))],
      [text(m.text), hover_view(view_seg, info)],
    );

  let offside_view = Option.None;
  let overlay_view = Option.None;
  let underlay_view = Option.None;
};

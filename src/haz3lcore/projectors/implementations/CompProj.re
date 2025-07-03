open Util;
open ProjectorBase;
open Virtual_dom.Vdom;
open Node;

let segment_of = (any: Language.Any.t): option(string) =>
  switch (any) {
  | Exp({term: Atom(String(s)), _}) =>
    Some(StringUtil.unescape_linebreaks(s))
  | _ => None
  };

module M: Projector = {
  // Describes whether this code is old, or newly suggested by the agent.
  [@deriving (show({with_path: false}), sexp, yojson)]
  type generation =
    | Previous
    | Incoming;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = {
    previous_code: Language.Any.t,
    incoming_code: Language.Any.t,
    // todo: add children
    generation,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    // Toggles between previous and incoming code projections
    | SwitchGeneration;
  // // Accepts the currently displayed generation as the new code
  // // This effectively removes the projector and applies its content to the code
  // | Accept;  let init: Any.t => option(model);

  let init = (code: Language.Any.t) => {
    Some({
      previous_code: code,
      incoming_code: code,
      generation: Incoming,
    });
  };

  let focusable = Focusable.non;
  let dynamics = false;

  let placeholder = (m, _) =>
    ProjectorCore.Shape.inline(m.generation == Incoming ? 2 : 3);
  let update = (m, _, _) => m;

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

  let view = (m: model, info, ~local as _, ~parent, ~view_seg) =>
    ProjectorBase.View.mk(
      div(
        ~attrs=[Attr.on_double_click(_ => parent(Remove))],
        [text("test"), hover_view(view_seg, info)],
      ),
    );
};

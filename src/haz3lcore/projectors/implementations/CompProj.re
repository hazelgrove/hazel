open Util;
open ProjectorBase;
open Virtual_dom.Vdom;
open Node;

let seg_to_str = (info: info): string =>
  Segment.to_string(~holes=None, info.syntax);

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

  let placeholder = (_, info: info) => {
    let str = info |> seg_to_str;
    let lines = StringUtil.to_lines(str);
    let max_line_width =
      List.fold_left(max, 0, lines |> List.map(String.length));
    ProjectorCore.Shape.{
      vertical: Block(List.length(lines) - 1),
      horizontal: max_line_width,
    };
  };

  let update = (m, _, _) => m;

  let view = (_: model, info, ~local as _, ~parent, ~view_seg: View.seg) =>
    View.mk(
      {
        let seg = Segment.unparenthesize(info.syntax);
        let sort = Segment.sort_of(Segment.skel(seg), seg);
        div(
          ~attrs=[Attr.class_("comp-view")],
          [view_seg(~background=true, sort, info.syntax)],
        );
      },
    );
};

open Util;
open ProjectorBase;
open Virtual_dom.Vdom;
open Node;
open Language;

let seg_to_str = (info: info): string =>
  Segment.to_string(~holes=None, info.syntax);

module M: Projector = {
  // Describes whether this code is old, or newly suggested by the whont.
  [@deriving (show({with_path: false}), sexp, yojson)]
  type who =
    | Previous
    | Incoming;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = {
    previous_code: Language.Any.t,
    incoming_code: Language.Any.t,
    // todo: add children
    who,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type action =
    // Toggles between previous and incoming code projections
    | SwitchWho;
  // // Accepts the currently displayed who as the new code
  // // This effectively removes the projector and applies its content to the code
  // | Accept;  let init: Any.t => option(model);

  let init = (any: Language.Any.t): option(model) => {
    switch (any) {
    | Exp(exp) =>
      switch (Exp.term_of(exp)) {
      | Let(_, _, _) => None
      | _ =>
        Some({
          previous_code: any,
          incoming_code: any,
          who: Incoming,
        })
      }
    | _ => None
    };
  };

  // let paint = (info: info, model: model) => {
  //   let seg = Segment.unparenthesize(info.syntax);
  //   let sort = Segment.sort_of(Segment.skel(seg), seg);
  //   view_seg(~background=true, sort, info.utility.term_to_seg(model.previous_code))
  // };

  let focus_keyboard = (id: Id.t, d: Direction.t) => {
    (); // Focus the projector when keyboard navigation enters it
      // You can add custom logic here for cursor positioning
  };

  let focus_pointer = (id: Id.t) => {
    (); // Focus the projector when clicked
      // You can add custom logic here for cursor positioning
  };

  let focusable =
    Focusable.{
      pointer: Some(focus_pointer),
      keyboard: Some(focus_keyboard),
    };
  let dynamics = false;

  let placeholder = (_, info: info) => {
    let str = info |> seg_to_str;
    let lines = StringUtil.to_lines(str);
    ProjectorCore.Shape.{
      vertical: Block(List.length(lines) - 1),
      horizontal: 0,
    };
  };

  let update = (m, info: info, action) => {
    switch (action) {
    | SwitchWho => {
        ...m,
        who:
          switch (m.who) {
          | Previous => Incoming
          | Incoming => Previous
          },
      }
    };
  };

  let view = (model: model, info, ~local, ~parent, ~view_seg: View.seg) => {
    View.{
      inline: {
        let on_mousedown = evt =>
          switch (Js_of_ocaml.Js.Unsafe.coerce(evt)##.detail == 2) {
          | _ when Js_of_ocaml.Js.to_bool(evt##.shiftKey) => local(SwitchWho)
          | _ => local(SwitchWho)
          };

        let seg = Segment.unparenthesize(info.syntax);
        let sort = Segment.sort_of(Segment.skel(seg), seg);

        div(
          ~attrs=[
            Attr.class_("comp-view"),
            Attr.on_mousedown(on_mousedown),
          ],
          [
            switch (model.who) {
            | Previous =>
              view_seg(
                ~background=true,
                sort,
                info.utility.term_to_seg(model.previous_code),
              )
            | Incoming =>
              view_seg(
                ~background=true,
                sort,
                info.utility.term_to_seg(model.incoming_code),
              )
            },
          ],
        );
      },
      offside: None,
      overlay: None,
    };
  };
};

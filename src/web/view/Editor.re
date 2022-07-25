open Virtual_dom.Vdom;
open Node;
open Core;

let view =
    (
      ~font_metrics,
      ~show_backpack_targets,
      ~zipper: Zipper.t,
      ~settings: Model.settings,
    )
    : Node.t => {
  let sel_seg = Zipper.zip(zipper);
  let unsel_seg = Zipper.unselect_and_zip(zipper);
  let map = Measured.of_segment(unsel_seg);
  module Deco =
    Deco.Deco({
      let font_metrics = font_metrics;
      let map = map;
      let show_backpack_targets = show_backpack_targets;
    });

  let ci =
    switch (zipper |> Indicated.index) {
    | Some(index) =>
      let (_, _, info_map) =
        zipper |> Term.of_zipper |> Statics.uexp_to_info_map;
      Id.Map.find_opt(index, info_map);
    | None => None
    };
  let ci_text =
    switch (ci) {
    | None => "No index or info"
    | Some(ci) => Statics.show_info(ci)
    };
  let ci_is_wrapped =
    switch (ci) {
    | None => "??"
    | Some(ci) => Statics.show_error_status(Statics.error_status(ci))
    };
  //TODO(andrew): new div name/class
  div(
    [Attr.class_("code"), Attr.id("under-the-rail")],
    [Code.view(~font_metrics, ~sel_seg, ~unsel_seg, ~map, ~settings)]
    @ Deco.all(zipper, sel_seg)
    //@ [text(zipper |> Term.of_zipper |> Term.show_uexp)]
    @ [text(ci_is_wrapped)]
    @ [text(ci_text)],
  );
};

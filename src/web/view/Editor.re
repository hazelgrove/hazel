open Virtual_dom.Vdom;
open Node;
open Core;

module Memo = Core_kernel.Memo;

let evaluate = Memo.general(~cache_size_bound=1000, Evaluator.evaluate);

let get_result_str = (elab): string => {
  print_endline("elaboration:");
  print_endline(Sexplib.Sexp.to_string_hum(DHExp.sexp_of_t(elab)));
  switch (elab |> evaluate) {
  | BoxedValue(d) =>
    let res_string = Sexplib.Sexp.to_string_hum(DHExp.sexp_of_t(d));
    print_endline("boxedval:");
    print_endline(res_string);
    res_string;
  | Indet(d) =>
    let res_string = Sexplib.Sexp.to_string_hum(DHExp.sexp_of_t(d));
    print_endline("indet:");
    print_endline(res_string);
    res_string;
  | exception _ => "EXCEPTION"
  };
};

let view =
    (
      ~inject,
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
  let term = zipper |> Term.of_zipper;
  let (_, _, info_map) = term |> Statics.uexp_to_info_map;
  let ci =
    switch (zipper |> Indicated.index) {
    | Some(index) => Id.Map.find_opt(index, info_map)
    | None => None
    };
  let ci_text =
    switch (ci) {
    | None => "No index or info"
    | Some(ci) => Statics.show_info(ci)
    };
  let ci_is_wrapped =
    switch (ci) {
    | Some(InfoPat({mode, self, _}))
    | Some(InfoExp({mode, self, _})) =>
      Statics.show_error_status(Statics.error_status(mode, self))
    | _ => "??"
    };
  let res_text =
    switch (term |> Elaborator.uexp_elab(info_map)) {
    | Elaborates(d, _, _) => get_result_str(d)
    | _ => "NO ELABORATION"
    };
  //TODO(andrew): new div name/class
  div(
    Attr.[
      id("under-the-rail"),
      class_("code"),
      on_click(e => {
        let rect =
          JSUtil.force_get_elem_by_id("under-the-rail")##getBoundingClientRect;
        let target_x = float_of_int(e##.clientX);
        let target_y = float_of_int(e##.clientY);
        let target =
          Measured.{
            row:
              Float.to_int(
                (target_y -. rect##.top) /. font_metrics.row_height,
              ),
            col:
              Float.to_int(
                (target_x -. rect##.left) /. font_metrics.col_width,
              ),
          };
        inject(Update.PerformAction(Move(Target(target))));
      }),
    ],
    [Code.view(~font_metrics, ~sel_seg, ~unsel_seg, ~map, ~settings)]
    @ Deco.all(zipper, sel_seg)
    //@ [text(zipper |> Term.of_zipper |> Term.show_uexp)]
    @ [text(ci_is_wrapped)]
    @ [text(ci_text)]
    @ [br([])]
    @ [text("RES:" ++ res_text)],
  );
};

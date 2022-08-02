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

let ci_view = (zipper: Zipper.t, info_map) => {
  let index =
    switch (zipper |> Indicated.index) {
    | Some(index) => index
    | None => (-1)
    };
  let ci =
    switch (zipper |> Indicated.index) {
    | Some(index) => Id.Map.find_opt(index, info_map)
    | None => None
    };
  switch (ci) {
  | None => []
  | Some(ci) => [CursorInspector.view(index, ci)]
  };
};

let res_view = (term, info_map) => {
  let res_text =
    switch (term |> Elaborator.uexp_elab(info_map)) {
    | Elaborates(d, _, _) => get_result_str(d)
    | _ => "NO ELABORATION"
    };
  div([Attr.classes(["result"])], [text("RES:" ++ res_text)]);
};

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
  let term = zipper |> Term.of_zipper;
  let (_, _, info_map) = term |> Statics.uexp_to_info_map;
  //TODO(andrew): new div name/class
  div(
    [Attr.class_("code"), Attr.id("under-the-rail")],
    [Code.view(~font_metrics, ~sel_seg, ~unsel_seg, ~map, ~settings)]
    @ Deco.all(zipper, sel_seg)
    //@ [text(zipper |> Term.of_zipper |> Term.show_uexp)]
    @ [res_view(term, info_map)]
    @ ci_view(zipper, info_map),
  );
};

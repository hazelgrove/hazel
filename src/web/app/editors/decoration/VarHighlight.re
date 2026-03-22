open Util.WebUtil;
open Haz3lcore;

/* Variable binding/reference highlighting.
 *
 * Highlights related bindings and uses when the caret is on a variable
 * binding/reference (Phase 1) or the mouse hovers over one (Phase 2).
 * Supports variables, constructors, and type variables. */

/* Compute highlight IDs from an Info.t, excluding the source ID */
let ids_from_info =
    (~info_map: Language.Statics.Map.t, ci: Language.Info.t): list(Id.t) => {
  let current_id = Language.Info.id_of(ci);
  Language.Statics.Map.var_highlight_ids(info_map, ci)
  |> List.filter(id => !Id.equal(id, current_id));
};

/* Compute which IDs to highlight based on the currently indicated piece */
let compute_caret_ids =
    (~info_map: Language.Statics.Map.t, z: Zipper.t): list(Id.t) => {
  switch (Indicated.ci_of(z, info_map)) {
  | Some(ci) => ids_from_info(~info_map, ci)
  | None => []
  };
};

/* Compute which IDs to highlight based on hover target */
let compute_hover_ids =
    (~info_map: Language.Statics.Map.t, hover_id: Id.t): list(Id.t) => {
  switch (Id.Map.find_opt(hover_id, info_map)) {
  | Some(ci) => ids_from_info(~info_map, ci)
  | None => []
  };
};

/* Render a single highlight overlay for an ID using Measured position data */
let highlight_of_id =
    (
      ~measured: Measured.t,
      ~font_metrics: FontMetrics.t,
      ~clss: list(string),
      id: Id.t,
    )
    : list(Virtual_dom.Vdom.Node.t) => {
  switch (Measured.find_by_id(id, measured)) {
  | Some(measurement) =>
    let d = DecUtil.abs_dims(measurement);
    [
      Virtual_dom.Vdom.Node.div(
        ~attrs=[
          Virtual_dom.Vdom.Attr.classes(["var-highlight"] @ clss),
          Virtual_dom.Vdom.Attr.create(
            "style",
            Printf.sprintf(
              "position: absolute; left: %fpx; top: %fpx; width: %fpx; height: %fpx;",
              Float.of_int(d.left) *. font_metrics.col_width,
              Float.of_int(d.top) *. font_metrics.row_height,
              Float.of_int(d.width) *. font_metrics.col_width,
              Float.of_int(d.height) *. font_metrics.row_height,
            ),
          ),
        ],
        [],
      ),
    ]
  | None => []
  };
};

/* Main view function: renders variable highlight overlays.
 * Hover highlights take precedence over caret highlights when present. */
let view =
    (
      ~measured: Measured.t,
      ~font_metrics: FontMetrics.t,
      ~info_map: Language.Statics.Map.t,
      ~hover_id: option(Id.t),
      z: Zipper.t,
    )
    : Virtual_dom.Vdom.Node.t => {
  let (ids, clss) =
    switch (hover_id) {
    | Some(hid) =>
      let hover_ids = compute_hover_ids(~info_map, hid);
      if (hover_ids != []) {
        (hover_ids, ["hover"]);
      } else {
        (compute_caret_ids(~info_map, z), []);
      };
    | None => (compute_caret_ids(~info_map, z), [])
    };
  div_c(
    "var-highlights",
    List.concat_map(
      highlight_of_id(~measured, ~font_metrics, ~clss),
      ids,
    ),
  );
};

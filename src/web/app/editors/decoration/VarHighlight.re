open Util.WebUtil;
open Haz3lcore;

/* Variable binding/reference highlighting.
 *
 * When the caret is on a variable binding (pattern) or reference (expression),
 * highlights the related bindings and uses with an underline + subtle background. */

/* Compute which IDs to highlight based on the currently indicated piece */
let compute_highlight_ids =
    (
      ~info_map: Language.Statics.Map.t,
      z: Zipper.t,
    )
    : list(Id.t) => {
  switch (Indicated.ci_of(z, info_map)) {
  | Some(ci) =>
    let current_id = Language.Info.id_of(ci);
    Language.Statics.Map.var_highlight_ids(info_map, ci)
    |> List.filter(id => !Id.equal(id, current_id));
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

/* Main view function: renders all variable highlight overlays */
let view =
    (
      ~measured: Measured.t,
      ~font_metrics: FontMetrics.t,
      ~info_map: Language.Statics.Map.t,
      z: Zipper.t,
    )
    : Virtual_dom.Vdom.Node.t => {
  let ids = compute_highlight_ids(~info_map, z);
  div_c(
    "var-highlights",
    List.concat_map(
      highlight_of_id(~measured, ~font_metrics, ~clss=[]),
      ids,
    ),
  );
};

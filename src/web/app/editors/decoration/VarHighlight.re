open Util.WebUtil;
open Haz3lcore;

/* Variable binding/reference highlighting.
 *
 * Highlights related bindings and uses when the caret is on a variable
 * binding/reference, or when hovering over a variable. Supports variables,
 * constructors, and type variables. */

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

/* Compute which IDs to highlight based on a hovered piece */
let compute_hover_ids =
    (~info_map: Language.Statics.Map.t, hover_id: Id.t): list(Id.t) => {
  switch (Language.Statics.Map.lookup(hover_id, info_map)) {
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
    : list(Node.t) => {
  switch (Measured.find_by_id(id, measured)) {
  | Some(measurement) => [
      Node.div(
        ~attrs=[
          Attr.classes(["var-highlight", ...clss]),
          DecUtil.abs_style(~font_metrics, measurement),
        ],
        [],
      ),
    ]
  | None => []
  };
};

/* Main view function: renders variable highlight overlays.
 * Hover highlights take priority when present; caret is the fallback. */
let view =
    (
      ~measured: Measured.t,
      ~font_metrics: FontMetrics.t,
      ~info_map: Language.Statics.Map.t,
      ~hover_id: option(Id.t),
      z: Zipper.t,
    )
    : Node.t => {
  let (ids, clss) =
    switch (hover_id) {
    | Some(id) =>
      let ids = compute_hover_ids(~info_map, id);
      ids == [] ? (compute_caret_ids(~info_map, z), []) : (ids, ["hover"]);
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

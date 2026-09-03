open Util.WebUtil;
open Haz3lcore;

/* Variable binding/reference highlighting.
 *
 * Highlights related bindings and uses when the caret is on a variable
 * binding/reference. Supports variables, constructors, and type variables. */

/* Compute highlight IDs from an Info.t, excluding the source ID */
let ids_from_info =
    (~info_map: Language.Statics.Map.t, ci: Language.Info.t): list(Id.t) => {
  let current_id = Language.Info.id_of(ci);
  Language.Statics.Map.var_highlight_ids(info_map, ci)
  |> List.filter(id => !Id.equal(id, current_id));
};

/* Compute which IDs to highlight based on the currently indicated piece.
 * Suppressed while a user selection is active to avoid distracting flicker
 * as the caret sweeps across binders/references. */
let compute_caret_ids =
    (~info_map: Language.Statics.Map.t, z: Zipper.t): list(Id.t) => {
  switch (Indicated.ci_of(z, info_map)) {
  | _ when !Selection.is_empty(z.selection) => []
  | Some(ci) => ids_from_info(~info_map, ci)
  | None => []
  };
};

/* Render a single highlight overlay for an ID using Measured position data */
let highlight_of_id =
    (
      ~measured: Measured.t,
      ~font_metrics: FontMetrics.t,
      ~info_map: Language.Statics.Map.t,
      id: Id.t,
    )
    : list(Node.t) => {
  switch (Measured.find_by_id(id, measured)) {
  | Some(measurement) =>
    let sort_cls =
      switch (Language.Statics.Map.lookup(id, info_map)) {
      | Some(ci) => Sort.to_string(Language.Info.sort_of(ci))
      | None => "Any"
      };
    [
      Node.div(
        ~attrs=[
          Attr.classes(["var-highlight", sort_cls]),
          DecUtil.abs_style(~font_metrics, measurement),
        ],
        [],
      ),
    ];
  | None => []
  };
};

/* var_highlight_ids scans the info_map for the binding's references,
   and this view runs on every frame — during eval-stream bursts and
   caret holds that walk repeated per frame with unchanged inputs.
   Single-slot memo on (info_map identity, indicated id, selection
   emptiness); the slot pins one info_map generation, which current
   statics retains anyway. */
let caret_ids_memo:
  ref(option((Language.Statics.Map.t, option(Id.t), bool, list(Id.t)))) =
  ref(None);
let compute_caret_ids_cached =
    (~info_map: Language.Statics.Map.t, z: Zipper.t): list(Id.t) => {
  let sel_empty = Selection.is_empty(z.selection);
  let indicated = Indicated.index(z);
  switch (caret_ids_memo^) {
  | Some((m, i, se, ids))
      when m === info_map && i == indicated && se == sel_empty => ids
  | _ =>
    let ids = compute_caret_ids(~info_map, z);
    caret_ids_memo := Some((info_map, indicated, sel_empty, ids));
    ids;
  };
};

/* Main view function: renders variable highlight overlays. */
let view =
    (
      ~measured: Measured.t,
      ~font_metrics: FontMetrics.t,
      ~info_map: Language.Statics.Map.t,
      z: Zipper.t,
    )
    : Node.t => {
  let ids = compute_caret_ids_cached(~info_map, z);
  div_c(
    "var-highlights",
    List.concat_map(
      highlight_of_id(~measured, ~font_metrics, ~info_map),
      ids,
    ),
  );
};

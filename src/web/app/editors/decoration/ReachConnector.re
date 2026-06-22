open Util;
open Virtual_dom.Vdom;
open Haz3lcore;

/* When a group's section is hovered in the Reach sidebar
 * (settings.sidebar.reach.hovered_group), draw a line through that group's
 * reach points in the editor, in the group's color — so you can see at a glance
 * which points a merge group ties together. */

let px = (~font_metrics: FontMetrics.t, p: Point.t) => (
  float_of_int(p.col) *. font_metrics.col_width,
  /* vertically center on the point's row */
  (float_of_int(p.row) +. 0.5) *. font_metrics.row_height,
);

/* The anchor point for a reach point: the right edge of its underlying term,
 * where the offside decoration sits. */
let anchor = (id: Id.t, term_data: TermData.t, measured: Measured.t) =>
  switch (TermData.extreme_measures(id, term_data, measured)) {
  | Some((_l, r)) => Some(r)
  | None => None
  };

let member_points =
    (~group: int, ~term_data: TermData.t, ~measured: Measured.t, z: Zipper.t)
    : list(Point.t) =>
  z.refractors.manuals
  @ Id.Map.to_list(z.refractors.multis.ephemerals)
  |> List.filter_map(((id, entry: Refractors.entry)) =>
       switch (entry.kind) {
       | Reach when List.mem(group, ProjectorInfo.reach_groups_of(entry)) =>
         anchor(id, term_data, measured)
       | _ => None
       }
     )
  |> List.sort((a: Point.t, b) => compare((a.row, a.col), (b.row, b.col)));

let circle = (~color: string, ~font_metrics, p: Point.t): Node.t => {
  let (x, y) = px(~font_metrics, p);
  Node.create_svg(
    "circle",
    ~attrs=[
      Attr.create("cx", Printf.sprintf("%f", x)),
      Attr.create("cy", Printf.sprintf("%f", y)),
      Attr.create("r", "3"),
      Attr.create("style", "fill: " ++ color),
    ],
    [],
  );
};

/* Every merge group in use across all reach refractors. */
let groups_in_use = (z: Zipper.t): list(int) =>
  z.refractors.manuals
  @ Id.Map.to_list(z.refractors.multis.ephemerals)
  |> List.concat_map(((_, entry: Refractors.entry)) =>
       switch (entry.kind) {
       | Reach => ProjectorInfo.reach_groups_of(entry)
       | _ => []
       }
     )
  |> List.sort_uniq(compare);

/* The (always-rendered, hidden) connector SVG for one group. CSS hides it by
   default and shows it when it has the `active` class; ReachHover toggles that
   class imperatively on hover (no re-render), and we also add it here when the
   group is the hovered one so an unrelated redraw keeps it shown. */
let group_svg =
    (
      ~font_metrics: FontMetrics.t,
      ~measured: Measured.t,
      ~term_data: TermData.t,
      z: Zipper.t,
      g: int,
    )
    : option(Node.t) =>
  switch (member_points(~group=g, ~term_data, ~measured, z)) {
  | []
  | [_] => None /* nothing to connect with fewer than two points */
  | [first, ...rest] as pts =>
    let color = ReachProjView.group_color(g);
    let cmd = (p: Point.t) => {
      let (x, y) = px(~font_metrics, p);
      SvgUtil.Path.L({
        x,
        y,
      });
    };
    let (fx, fy) = px(~font_metrics, first);
    let path =
      SvgUtil.Path.view(
        ~attrs=[
          Attr.create(
            "style",
            /* Faint via stroke-opacity (not the SVG's opacity) so the dots,
               drawn separately below, stay full-strength. */
            "fill: none; stroke: "
            ++ color
            ++ "; stroke-width: 1.25px; stroke-opacity: 0.55;"
            ++ " stroke-linejoin: round; stroke-linecap: round;",
          ),
        ],
        [
          SvgUtil.Path.M({
            x: fx,
            y: fy,
          }),
          ...List.map(cmd, rest),
        ],
      );
    let active = ReachHover.hovered^ == Some(g) ? ["active"] : [];
    Some(
      Node.create_svg(
        "svg",
        ~attrs=[
          Attr.id(ReachHover.connector_id(g)),
          Attr.classes(["reach-connector", ...active]),
          Attr.create(
            "style",
            /* Clip to the code container: the line + dots are interior to the
               points, so nothing visible is lost, and this stops stroke/dots
               bleeding past the content edge from extending the scroll area. */
            "position: absolute; left: 0; top: 0; width: 100%; height: 100%;"
            ++ " overflow: hidden; pointer-events: none; z-index: 5;",
          ),
        ],
        [path, ...List.map(circle(~color, ~font_metrics), pts)],
      ),
    );
  };

/* All group connectors, rendered always (hidden until hovered). Visibility is
   driven by ReachHover via the `active` CSS class — no re-render on hover. */
let view =
    (
      ~font_metrics: FontMetrics.t,
      ~measured: Measured.t,
      ~term_data: TermData.t,
      z: Zipper.t,
    )
    : list(Node.t) =>
  groups_in_use(z)
  |> List.filter_map(group_svg(~font_metrics, ~measured, ~term_data, z));

open Util;
open Util.Web;
open Haz3lcorep;

/* Read-only code viewer, no interaction and no statics. All four
   functions do the same thing but take differently-typed inputs. */

let view =
    (
      type p',
      ~font_metrics: FontMetrics.t,
      ~secondary_icons,
      ~sort: Sort.t,
      ~measured,
      ~buffer_ids,
      ~segment: Haz3lcorep.Segment.t(p'),
      ~shape_map,
    )
    : Node.t => {
  module Text =
    Code.Text({
      type p = p';
      let map = measured;
      let shape_map = shape_map;
      let font_metrics = font_metrics;
      let secondary_icons = secondary_icons;
    });
  let code = Text.of_segment(buffer_ids, false, sort, segment);
  div_c("code", [span_c("code-text", code)]);
};

let view_segment =
    (
      type p,
      ~sort: Sort.t,
      ~shape_map: ProjectorShape.Map.t,
      segment: Segment.t(p),
    ) => {
  let measured = Measured.of_segment(segment, shape_map);
  let buffer_ids = [];
  view(~sort, ~measured, ~buffer_ids, ~segment, ~shape_map);
};

let view_editor =
    (type p, ~sort: Sort.t, editor: Haz3lcorep.Editor.t('p_k, p, 'p_a)) => {
  let syntax = Calc.get_saved_exc(editor.syntax);
  let measured = syntax.measured;
  let buffer_ids =
    Selection.is_buffer(Editor.Model.get_z(editor).selection)
      ? Calc.get_saved_exc(editor.selection_ids) : [];
  let segment = syntax.segment;
  let shape_map = syntax.shape_map;
  view(~sort, ~measured, ~buffer_ids, ~segment, ~shape_map);
};

let view_typ = (~settings, typ: Typ.t) => {
  let shape_map = ProjectorShape.Map.empty; // assume no projectors
  typ
  |> ExpToSegment.typ_to_segment(~settings)
  |> view_segment(~shape_map, ~sort=Typ);
};

let view_any = (~settings, ~font_metrics, ~secondary_icons, any: Any.t) => {
  let shape_map = ProjectorShape.Map.empty; // assume no projectors
  any
  |> ExpToSegment.any_to_segment(~settings)
  |> view_segment(~shape_map, ~sort=Any, ~font_metrics, ~secondary_icons);
};

open Util;
open WebUtil;

/* Read-only code viewer, no interaction and no statics. All four
   functions do the same thing but take differently-typed inputs. */

let view =
    (
      type p',
      ~background=false,
      ~font_metrics: Haz3lcorep.FontMetrics.t,
      ~secondary_icons,
      ~sort: Language.Sort.t,
      ~measured,
      ~buffer_ids,
      ~shape_map,
      segment: Haz3lcorep.Segment.t(p'),
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
  let backing =
    /* Add a fun background to your editor */
    background
      ? [
        Deco.quick_select_deco(~font_metrics, ~measured, ~shape_map, segment),
      ]
      : [];
  div_c("code", [span_c("code-text", code)] @ backing);
};

let view_segment =
    (
      type p,
      ~sort: Language.Sort.t,
      ~shape_map: Util.ProjectorShape.Map.t,
      segment: Haz3lcorep.Segment.t(p),
    ) => {
  let measured = Haz3lcorep.Measured.of_segment(segment, shape_map);
  let buffer_ids = [];
  view(~sort, ~measured, ~buffer_ids, ~shape_map, segment);
};

let view_editor =
    (
      type p,
      ~sort: Language.Sort.t,
      ~background=false,
      editor: Haz3lcorep.Editor.t('p_k, p, 'p_a),
    ) => {
  let syntax = Calc.get_saved_exc(editor.syntax);
  let measured = syntax.measured;
  let buffer_ids =
    Haz3lcorep.Selection.is_buffer(
      Haz3lcorep.Editor.Model.get_z(editor).selection,
    )
      ? Calc.get_saved_exc(editor.selection_ids) : [];
  let segment = syntax.segment;
  let shape_map = syntax.shape_map;
  view(~background, ~sort, ~measured, ~buffer_ids, ~shape_map, segment);
};

let view_typ = (~settings, typ: Language.Typ.t) => {
  let shape_map = ProjectorShape.Map.empty; // assume no projectors
  typ
  |> Haz3lcorep.ExpToSegment.typ_to_segment(~settings)
  |> view_segment(~shape_map, ~sort=Typ);
};

let view_any =
    (~font_metrics, ~secondary_icons, ~settings, any: Language.Any.t) => {
  any
  |> Haz3lcorep.ExpToSegment.any_to_segment(~settings)
  |> view_segment(
       ~shape_map=Id.Map.empty,
       ~sort=Any,
       ~font_metrics,
       ~secondary_icons,
     );
};

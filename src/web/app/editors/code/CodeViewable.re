open Util.WebUtil;
open Haz3lcore;

/* Read-only code viewer, no interaction and no statics. All four
   functions do the same thing but take differently-typed inputs. */

let view =
    (
      ~globals: Globals.t,
      ~sort: Sort.t,
      ~measured,
      ~buffer_ids,
      ~segment,
      ~shape_map,
    )
    : Node.t => {
  module Text =
    Code.Text({
      let map = measured;
      let settings = globals.settings;
      let shape_map = shape_map;
      let font_metrics = globals.font_metrics;
    });
  let code = Text.of_segment(buffer_ids, false, sort, segment);
  div_c("code", [span_c("code-text", code)]);
};

let view_segment =
    (
      ~globals: Globals.t,
      ~sort: Sort.t,
      ~shape_map: ProjectorCore.Shape.Map.t,
      segment: Segment.t,
    ) => {
  let measured = Measured.of_segment(segment, shape_map);
  let buffer_ids = [];
  view(~globals, ~sort, ~measured, ~buffer_ids, ~segment, ~shape_map);
};

let view_typ = (~globals: Globals.t, ~settings, typ: Language.Typ.t) => {
  let shape_map = ProjectorCore.Shape.Map.empty; // assume no projectors
  typ
  |> ExpToSegment.typ_to_segment(~settings)
  |> view_segment(~shape_map, ~globals, ~sort=Typ);
};

let view_any = (~globals: Globals.t, ~settings, any: Language.Any.t) => {
  any
  |> ExpToSegment.any_to_segment(~settings)
  |> view_segment(~globals, ~sort=Any);
};

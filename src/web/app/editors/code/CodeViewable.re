open Util.WebUtil;
open Haz3lcore;

/* Read-only code viewer, no interaction and no statics. All four
   functions do the same thing but take differently-typed inputs. */

let view =
    (
      ~globals: Globals.t,
      ~measured,
      ~term_data,
      ~buffer_ids,
      ~segment,
      ~shape_map,
      ~sort as _,
    )
    : Node.t => {
  let code =
    Code.view(
      ~measured,
      ~settings=globals.settings,
      ~shape_map,
      ~font_metrics=globals.font_metrics,
      ~term_data,
      ~buffer_ids,
      segment,
    );
  div_c("code", [span_c("code-text", code)]);
};

let view_segment =
    (
      ~globals: Globals.t,
      ~shape_map: ProjectorCore.Shape.Map.t,
      ~sort: Sort.t,
      segment: Segment.t,
    ) =>
  view(
    ~globals,
    ~measured=Measured.of_segment(segment, shape_map),
    ~term_data=Id.Map.empty, //TODO,
    ~buffer_ids=[],
    ~segment,
    ~sort,
    ~shape_map,
  );

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

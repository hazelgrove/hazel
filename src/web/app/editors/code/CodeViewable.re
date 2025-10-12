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

let view_segment = (~globals: Globals.t, segment: Segment.t) => {
  let shape_map = ProjectorCore.Shape.Map.empty; // assume no projectors
  let term_data = TermData.empty; //assume no indication/selection decoratinos
  view(
    ~globals,
    ~measured=Measured.of_segment(segment, shape_map),
    ~term_data,
    ~buffer_ids=[],
    ~segment,
    ~shape_map,
  );
};

let view_typ = (~globals: Globals.t, ~settings, typ: Language.Typ.t) =>
  typ |> ExpToSegment.typ_to_segment(~settings) |> view_segment(~globals);

let view_any = (~globals: Globals.t, ~settings, any: Language.Any.t) =>
  any |> ExpToSegment.any_to_segment(~settings) |> view_segment(~globals);

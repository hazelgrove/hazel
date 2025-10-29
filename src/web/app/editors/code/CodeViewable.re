open Util.WebUtil;
open Haz3lcore;

/* Read-only code viewer, no interaction and no statics. All four
   functions do the same thing but take differently-typed inputs. */

let view =
    (
      ~is_dynamic=(_: Id.t) => false,
      ~globals: Globals.t,
      ~measured,
      ~term_data,
      ~buffer_ids,
      ~segment,
      ~shape_map,
      (),
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
      ~is_dynamic,
      segment,
    );
  div_c("code", [span_c("code-text", code)]);
};

let view_segment =
    (~globals: Globals.t, ~is_dynamic=(_: Id.t) => false, segment: Segment.t) => {
  let shape_map = ProjectorCore.Shape.Map.empty; // assume no projectors
  let term_data = TermData.empty; //assume no indication/selection decoratinos
  view(
    ~globals,
    ~measured=Measured.of_segment(segment, shape_map),
    ~term_data,
    ~buffer_ids=[],
    ~segment,
    ~shape_map,
    ~is_dynamic,
    (),
  );
};

let view_typ =
    (
      ~globals: Globals.t,
      ~settings,
      ~is_dynamic=(_: Id.t) => false,
      typ: Language.Typ.t,
    ) =>
  typ
  |> ExpToSegment.typ_to_segment(~settings)
  |> view_segment(~globals, ~is_dynamic);

let view_any =
    (
      ~globals: Globals.t,
      ~settings,
      ~is_dynamic=(_: Id.t) => false,
      any: Language.Any.t,
    ) =>
  any
  |> ExpToSegment.any_to_segment(~settings)
  |> view_segment(~globals, ~is_dynamic);

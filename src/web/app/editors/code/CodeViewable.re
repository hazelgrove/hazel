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
      ~shape_map,
      ~refractor_shape_map,
      ~refine_sort: (Id.t, Sort.t) => Sort.t=(_, sort) => sort,
      segment,
    )
    : Node.t => {
  let code =
    Code.view(
      ~measured,
      ~settings=globals.settings,
      ~shape_map,
      ~refractor_shape_map,
      ~font_metrics=globals.font_metrics,
      ~term_data,
      ~refine_sort,
      ~buffer_ids,
      segment,
    );
  div_c("code", [span_c("code-text", code)]);
};

/* This view draws no projector layer, so a projector piece would come out as
   blank space -- the space reserved for a widget nobody paints. Replacing
   each with the syntax it wraps makes the assumption below true, and shows
   the text form of whatever the widget would have drawn. Without this a
   value containing a projector renders as nothing at all, which is how
   Fumola references went missing from probe rows. */
let unproject: Piece.t => Segment.t =
  fun
  | Projector(pr) => Piece.unparenthesize(pr.syntax)
  | p => [p];

let view_segment = (~globals: Globals.t, segment: Segment.t) => {
  let segment = ZipperBase.MapPiece.of_segment(unproject, segment);
  let shape_map = ProjectorCore.Shape.Map.empty; // no projectors, by construction above
  let refractor_shape_map = Id.Map.empty; //assume no refractors
  let term_data = TermData.empty; //assume no indication/selection decoratinos
  view(
    ~globals,
    ~measured=Measured.of_segment(segment, shape_map, refractor_shape_map),
    ~term_data,
    ~buffer_ids=[],
    ~shape_map,
    ~refractor_shape_map,
    segment,
  );
};

let view_typ = (~globals: Globals.t, ~settings, typ: Language.Typ.t) =>
  typ |> ExpToSegment.typ_to_segment(~settings) |> view_segment(~globals);

let view_any = (~globals: Globals.t, ~settings, any: Language.Any.t) =>
  any |> ExpToSegment.any_to_segment(~settings) |> view_segment(~globals);

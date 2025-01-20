open Util.Web;
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
      ~holes,
      ~shape_of_proj,
    )
    : Node.t => {
  module Text =
    Code.Text({
      let map = measured;
      let settings = globals.settings;
      let shape_of_proj = shape_of_proj;
    });
  let code = Text.of_segment(buffer_ids, false, sort, segment);
  let holes = List.map(Code.of_hole(~measured, ~globals), holes);
  div_c("code", [span_c("code-text", code), ...holes]);
};

// let view_editor =
//     (
//       ~globals: Globals.t,
//       ~sort: Sort.t,
//       {
//         state:
//           {
//             meta: {syntax: {measured, selection_ids, segment, holes, _}, _},
//             _,
//           },
//         _,
//       }: Editor.t,
//     )
//     : Node.t => {
//   view(
//     ~globals,
//     ~sort,
//     ~measured,
//     ~buffer_ids=selection_ids,
//     ~segment,
//     ~holes,
//   );
// };

let view_segment =
    (~globals: Globals.t, ~sort: Sort.t, ~shape_of_proj, segment: Segment.t) => {
  let measured = Measured.of_segment(segment, shape_of_proj);
  let buffer_ids = [];
  let holes = Segment.holes(segment);
  view(
    ~globals,
    ~sort,
    ~measured,
    ~buffer_ids,
    ~holes,
    ~segment,
    ~shape_of_proj,
  );
};

let view_exp =
    (~dynamics, ~globals: Globals.t, ~settings, ~info_map, exp: Exp.t) => {
  let shape_of_proj = ProjectorInfo.Shape.of_map(info_map, dynamics);
  exp
  |> ExpToSegment.exp_to_segment(~settings)
  |> view_segment(~shape_of_proj, ~globals, ~sort=Exp);
};

let view_typ = (~globals: Globals.t, ~settings, ~info_map, typ: Typ.t) => {
  let shape_of_proj =
    ProjectorInfo.Shape.of_map(info_map, Dynamics.Map.empty);
  typ
  |> ExpToSegment.typ_to_segment(~settings)
  |> view_segment(~shape_of_proj, ~globals, ~sort=Typ);
};

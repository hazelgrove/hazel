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
      ~info_map,
    )
    : Node.t => {
  module Text =
    Code.Text({
      let map = measured;
      let settings = globals.settings;
      let info_map = info_map;
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
    (~globals: Globals.t, ~sort: Sort.t, ~info_map, segment: Segment.t) => {
  let measured = Measured.of_segment(segment, info_map);
  let buffer_ids = [];
  let holes = Segment.holes(segment);
  view(~globals, ~sort, ~measured, ~buffer_ids, ~holes, ~segment, ~info_map);
};

let view_exp = (~globals: Globals.t, ~settings, exp: Exp.t) => {
  exp
  |> ExpToSegment.exp_to_segment(~settings)
  |> view_segment(~globals, ~sort=Exp);
};

let view_typ = (~globals: Globals.t, ~settings, typ: Typ.t) => {
  typ
  |> ExpToSegment.typ_to_segment(~settings)
  |> view_segment(~globals, ~sort=Typ);
};

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
      ~unselected,
      ~holes,
    )
    : Node.t => {
  module Text =
    Code.Text({
      let map = measured;
      let settings = globals.settings;
    });
  let code = Text.of_segment(buffer_ids, false, sort, unselected);
  let holes = List.map(Code.of_hole(~measured, ~globals), holes);
  div_c("code", [span_c("code-text", code), ...holes]);
};

let view_editor =
    (
      ~globals: Globals.t,
      ~sort: Sort.t,
      {state: {meta: {measured, buffer_ids, unselected, holes, _}, _}, _}: Editor.t,
    )
    : Node.t => {
  view(~globals, ~sort, ~measured, ~buffer_ids, ~unselected, ~holes);
};

let view_segment = (~globals: Globals.t, ~sort: Sort.t, unselected: Segment.t) => {
  let measured = Measured.of_segment(unselected);
  let buffer_ids = [];
  let holes = Segment.holes(unselected);
  view(~globals, ~sort, ~measured, ~buffer_ids, ~holes, ~unselected);
};

let view_exp = (~globals: Globals.t, ~inline: bool, exp: Exp.t) => {
  exp
  |> ExpToSegment.exp_to_segment(~inline)
  |> view_segment(~globals, ~sort=Exp);
};

let view_typ = (~globals: Globals.t, ~inline: bool, typ: Typ.t) => {
  typ
  |> ExpToSegment.typ_to_segment(~inline)
  |> view_segment(~globals, ~sort=Typ);
};

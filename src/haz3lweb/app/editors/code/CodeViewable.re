open Util.Web;
open Haz3lcore;

/* Read-only code viewer, no interaction and no statics. All four
   functions do the same thing but take differently-typed inputs. */

let view =
    (
      ~globals: Globals.t,
      ~sort: Sort.t,
      {state: {meta: {measured, buffer_ids, unselected, holes, _}, _}, _}: Editor.t,
    )
    : Node.t => {
  module Text =
    Code.Text({
      let map = measured;
      let settings = globals.settings;
    });
  let code = Text.of_segment(buffer_ids, false, sort, unselected);
  let holes = List.map(Code.of_hole(~measured, ~globals), holes);
  div_c("code", [Util.Web.span_c("code-text", code), ...holes]);
};

let view_segment = (~globals: Globals.t, ~sort: Sort.t, unselected: Segment.t) => {
  unselected
  |> Zipper.unzip
  |> Editor.init(~read_only=true)
  |> view(~globals, ~sort);
};

let view_exp = (~globals: Globals.t, ~inline: bool, exp: Exp.t) => {
  exp |> ExpToSegment.exp_to_editor(~inline) |> view(~globals, ~sort=Exp);
};

let view_typ = (~globals: Globals.t, ~inline: bool, typ: Typ.t) => {
  typ |> ExpToSegment.typ_to_editor(~inline) |> view(~globals, ~sort=Typ);
};

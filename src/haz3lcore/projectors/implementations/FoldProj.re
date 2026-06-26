open Util;
open ProjectorBase;

/* Fold projector logic: collapses the underlying syntax to a small
   glyph. The model and action types live at file level (outside the
   sealed module below) so that view backends (web: FoldProjView; tui:
   TermProjector.fold) can reuse them. */

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  [@default "⋱"]
  text: string,
  expanded: bool,
  always_render: bool,
};

let default: t = {
  text: "⋱",
  expanded: false,
  always_render: false,
};

let t_of_sexp = (sexp: Sexplib.Sexp.t): t =>
  switch (t_of_sexp(sexp)) {
  | exception _ => default
  | t => t
  };

[@deriving (show({with_path: false}), sexp, yojson)]
type fold_action =
  | Toggle;

module M: Projector with type model = t and type action = fold_action = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = t;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = fold_action;

  let init = _ => Some(default);

  let dynamics = false;
  let elaborate_syntax = false;

  let placeholder = (m, _) =>
    ProjectorCore.Shape.inline(m.text == "⋱" ? 2 : m.text |> String.length);
  let update = (m, _, _) => {
    ...m,
    expanded: !m.expanded,
  };
  let error = (_, _): option(ProjectorBase.error) => None;
  let initialize = None;
};

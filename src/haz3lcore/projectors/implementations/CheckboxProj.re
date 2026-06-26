open Util;
open ProjectorBase;

/* Checkbox projector logic: projects a boolean literal as a checkbox.
   Views live in the frontends (web: CheckboxProjView; tui:
   TermProjector.checkbox), reusing the helpers below. */

let bool_of = (any: Language.Any.t): option(bool) =>
  switch (any) {
  | Exp({term: Atom(Bool(b)), _}) => Some(b)
  | _ => None
  };

let get = (info: info): bool =>
  switch (
    info.syntax |> info.utility.seg_to_term |> OptUtil.and_then(bool_of)
  ) {
  | Some(b) => b
  | None => failwith("Checkbox: Get: not boolean literal")
  };

let toggle = (info: info): Base.segment =>
  switch (
    info.utility.lift_syntax(
      ~inline=true,
      fun
      | Exp({term: Atom(Bool(b)), _} as t) =>
        Exp({
          ...t,
          term: Atom(Bool(!b)),
        })
      | _ => failwith("Checkbox: Toggle: not boolean literal"),
      info.syntax,
    )
  ) {
  | Some(s) => s
  | None => failwith("Checkbox: Toggle: lift failed")
  };

let shape = ProjectorCore.Shape.inline(2);

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = unit;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;

  let init = (any: Language.Any.t) =>
    switch (bool_of(any)) {
    | Some(_) => Some()
    | None => None
    };

  let dynamics = false;
  let elaborate_syntax = false;
  let placeholder = (_, _) => shape;
  let update = (model, _, _) => model;
  let error = (_, _): option(ProjectorBase.error) => None;
  let initialize = None;
};

open Util;
open ProjectorBase;

/* Pure helpers are exposed at file level (outside the sealed module
   below) so that alternative view backends (e.g. the TUI) can reuse
   the projector's semantics without going through the Vdom view. */

let float_of = (any: Language.Any.t): option(float) =>
  switch (any) {
  | Exp({term: Atom(Float(f)), _}) => Some(f)
  | _ => None
  };

let get = (info: info): float =>
  switch (
    info.syntax |> info.utility.seg_to_term |> OptUtil.and_then(float_of)
  ) {
  | Some(f) => f
  | None => failwith("SliderF: Get: not float literal")
  };

let put = (info: info, v: string): Base.segment =>
  switch (
    info.utility.lift_syntax(
      ~inline=true,
      fun
      | Exp(t) =>
        Exp({
          ...t,
          term: Atom(Float(float_of_string(v))),
        })
      | _ => failwith("SliderF: Put: not float literal"),
      info.syntax,
    )
  ) {
  | Some(s) => s
  | None => failwith("SliderF: Put: lift failed")
  };

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = unit;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;

  let init = (any: Language.Any.t) =>
    switch (float_of(any)) {
    | Some(_) => Some()
    | None => None
    };

  let dynamics = false;
  let elaborate_syntax = false;
  let placeholder = (_, _) => ProjectorCore.Shape.inline(10);
  let update = (model, _, _) => model;
  let error = (_, _): option(ProjectorBase.error) => None;
  let initialize = None;
};

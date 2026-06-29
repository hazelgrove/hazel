open Util;
open ProjectorBase;

/* TextArea projector logic: projects a string literal as a
   multi-line text area. The view lives in the web frontend
   (TextAreaProjView), reusing the helpers below. */

let string_of = (any: Language.Any.t): option(string) =>
  switch (any) {
  | Exp({term: Atom(String(s)), _}) =>
    Some(StringUtil.unescape_linebreaks(s))
  | _ => None
  };

let get = (info: info): string =>
  switch (info.syntax |> info.utility.seg_to_term) {
  | Some(s) =>
    switch (string_of(s)) {
    | Some(s) => s
    | None => failwith("TextArea: get: Not string literal")
    }
  | None => failwith("TextArea: get: Not string literal")
  };

let put = (info, s: string): Base.segment =>
  switch (
    info.utility.lift_syntax(
      ~inline=true,
      fun
      | Exp(any) =>
        Exp({
          ...any,
          term: Atom(String(StringUtil.escape_linebreaks(s))),
        })
      | _any => failwith("TextArea: put: not string literal"),
      info.syntax,
    )
  ) {
  | Some(s) => s
  | None => failwith("TextArea: put: lift failed")
  };

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = unit;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;

  let init = (any: Language.Any.t) =>
    switch (string_of(any)) {
    | Some(_) => Some()
    | None => None
    };

  let dynamics = false;
  let elaborate_syntax = false;
  let placeholder = (_, info) => {
    let str = info |> get;
    ProjectorCore.Shape.{
      vertical: Block(StringUtil.num_linebreaks(str)),
      /* +2 for left and right padding */
      horizontal: 2 + StringUtil.max_line_width(str),
    };
  };
  let update = (model, _, _) => model;
  let error = (_, _): option(ProjectorBase.error) => None;
  let resolve = _ => None;
  let expand = (_, _) => None;
};

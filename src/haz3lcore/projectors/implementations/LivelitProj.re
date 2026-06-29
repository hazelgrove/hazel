open Util;
open ProjectorBase;
open Language;

/* Livelit projector logic: projects an application of a livelit name
   as the livelit's own GUI. The view (which invokes the livelit's view
   function from the typing context) lives in the web frontend
   (LivelitProjView), reusing the helpers below. */

let get_model = (info: info) =>
  switch (info.statics) {
  | Some(
      InfoExp({
        user_term:
          {term: Ap(_dir, {term: LivelitName(llname), _}, model), _},
        _,
      }),
    ) =>
    Some((llname, model))
  | _ => None
  };

let replace_model_term =
    (updated_model_term: TermBase.Exp.t, start_term: TermBase.Any.t)
    : TermBase.Any.t =>
  switch (start_term) {
  | Exp({term: Ap(Forward, name, _model), _} as rest) =>
    Exp({
      ...rest,
      term: Ap(Forward, name, updated_model_term),
    })
  | _ =>
    print_endline("Warning - LivelitProj.replace_model_term: not an Ap");
    start_term;
  };

module M: Projector = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model = unit;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action = unit;

  let init = (any: Language.Any.t) =>
    switch (any) {
    | Exp({term: Ap(_dir, {term: LivelitName(_), _}, _), _})
    | Exp({
        term: Parens({term: Ap(_dir, {term: LivelitName(_), _}, _), _}),
        _,
      }) =>
      Some()
    | _ => None
    };

  let placeholder = (_model, info) => {
    switch (get_model(info), info.statics) {
    | (Some((llname, _)), Some(InfoExp(exp))) =>
      /* Get the livelit size */
      switch (Ctx.lookup_livelit(exp.ctx, llname)) {
      | Some(ll) => ll.size
      | None =>
        /* Default size */
        ProjectorCore.Shape.inline(32)
      }
    | _ =>
      /* Default size */
      ProjectorCore.Shape.inline(32)
    };
  };

  let update = (_model, _info, action) =>
    switch (action) {
    | _ => print_endline("Warning - LivelitProj.update: No action")
    };

  let dynamics = false;
  let elaborate_syntax = false;
  let error = (_, _): option(ProjectorBase.error) => None;
  let resolve = _ => None;
  let expand = (_, _) => None;
};

open Util;
module Kind = ProjectorKind;

// ============================================================================
//     |  |  ___  __   ___     __   ___     __   __        __   __        __
//     |__| |__  |__) |__     |__) |__     |  \ |__)  /\  / _` /  \ |\ | /__`
//     |  | |___ |  \ |___    |__) |___    |__/ |  \ /~~\ \__> \__/ | \| .__/
// ============================================================================
//
//  This file handles dispatch to the various projectors. It does this using GADTs to
//  store different-typed models for different projectors. GADTs don't interact nicely with the
//  functors used in projectors, so these dispatch functions all look a lot more complicated than
//  they should be.
//
//  If you are here to add a new projector kind:
//
//       - Stay in the "Projector Kinds" section of this file.
//       - Add a case to each type / method, copying the others.
//
//  If you are here to add a new projector method:
//
//      - Stay in the "Projector Method Dispatch" section of this file.
//      - Copy a pre-existing method, I have annotated `calculate` with explanations
//        as it is the simplest example.

type package =
  | Pack(
      (module ProjectorInterface.PROJECTOR with
         type model' = 'm and type action' = 'a and type focus' = 'f),
      'm,
    )
    : package;

let dispatch =
    (
      type ed,
      ~editor_module as
        module Editor: EditorInterface.EDITOR with type model = ed,
      ~kind: Kind.t,
      f:
        (module ProjectorInterface.PROJECTOR with type editor_model = ed) => 'a,
    )
    : 'a => {
  switch (kind) {
  | Fold => f((module FoldProj.M(Editor)))
  | Slider => f((module SliderProj.M(Editor)))
  | SliderF => f((module SliderFProj.M(Editor)))
  | Pair => f((module PairProj.M(Editor)))
  | Checkbox => f((module CheckboxProj.M(Editor)))
  | Type => f((module TypeProj.M(Editor)))
  | Probe => f((module ProbeProj.M(Editor)))
  | Card => f((module CardProj.M(Editor)))
  | Livelit => f((module LivelitProj.M(Editor)))
  | TextArea => f((module TextAreaProj.M(Editor)))
  };
};

// ============================================================
// Pure Garbage (because the ppx deriver doesn't work on gadts)
// ============================================================

// Fold this module away and never worry about it again.

module PPXMethods = {
  let pp_package = (_f, _package: package) => {
    // Format.printf(f, model |> kind_of_model |> Kind.name);  // Note(matt): I tried to make this but it gnarly type errors
    failwith(
      "cannot print",
    );
  };

  let package_of_sexp =
      (
        type ed_m,
        ~editor_module: (module EditorInterface.EDITOR with type model = ed_m),
        sexp: Sexplib.Sexp.t,
      )
      : package =>
    switch (sexp) {
    | List([Atom(kind_string), m]) =>
      dispatch(
        ~editor_module,
        ~kind=Kind.of_name(kind_string),
        (module P: ProjectorInterface.PROJECTOR with type editor_model = ed_m) =>
        Pack((module P), P.model'_of_sexp(m))
      )
    | _ => failwith("Projector desearialization failed")
    };

  let sexp_of_package = (type ed_m, package: package): Sexplib.Sexp.t =>
    switch (package) {
    | Pack((module P), m) =>
      List([Atom(Kind.name(P.kind)), P.sexp_of_model'(m)])
    };

  let package_of_yojson =
      (type ed_m, ~editor_module, yojson: Yojson.Safe.t): package =>
    switch (yojson) {
    | `List([`String(kind_string), m]) =>
      dispatch(
        ~editor_module,
        ~kind=Kind.of_name(kind_string),
        (module P: ProjectorInterface.PROJECTOR with type editor_model = ed_m) =>
        Pack((module P), P.model'_of_yojson(m))
      )
    | _ => failwith("Projector desearialization failed")
    };

  let yojson_of_package = (type ed_m, package: package) =>
    switch (package) {
    | Pack((module P), m) =>
      `List([`String(Kind.name(P.kind)), P.yojson_of_model'(m)])
    };
};

include PPXMethods;

type model = {
  exp_cache: Calc.saved(Language.Any.t),
  package,
};

let model_of_sexp =
    (
      type ed_m,
      ~editor_module: (module EditorInterface.EDITOR with type model = ed_m),
      sexp: Sexplib.Sexp.t,
    )
    : model => {
  exp_cache: Calc.Pending,
  package: package_of_sexp(~editor_module, sexp),
};

let model_of_yojson =
    (
      type ed_m,
      ~editor_module: (module EditorInterface.EDITOR with type model = ed_m),
      yojson: Yojson.Safe.t,
    )
    : model => {
  exp_cache: Calc.Pending,
  package: package_of_yojson(~editor_module, yojson),
};

let sexp_of_model = (type ed_m, model: model): Sexplib.Sexp.t =>
  sexp_of_package(model.package);

let yojson_of_model = (type ed_m, model: model): Yojson.Safe.t =>
  yojson_of_package(model.package);

let pp_model = (_, _model: model) => failwith("pp_model not implemented");

type action = Yojson.Safe.t;
let action_of_yojson = Fun.id;
let yojson_of_action = Fun.id;
let action_of_sexp = (sexp: Sexplib.Sexp.t): action =>
  switch (sexp) {
  | Sexplib.Sexp.Atom(s) => Yojson.Safe.from_string(s)
  | _ => failwith("action_of_sexp: not an atom")
  };
let sexp_of_action = (a: action) =>
  Sexplib.Sexp.Atom(Yojson.Safe.to_string(a));

type focus = Yojson.Safe.t;
let focus_of_yojson = Fun.id;
let yojson_of_focus = Fun.id;
let focus_of_sexp = (sexp: Sexplib.Sexp.t): focus =>
  switch (sexp) {
  | Sexplib.Sexp.Atom(s) => Yojson.Safe.from_string(s)
  | _ => failwith("focus_of_sexp: not an atom")
  };
let sexp_of_focus = (f: focus) =>
  Sexplib.Sexp.Atom(Yojson.Safe.to_string(f));

let kind_of_model = (model: model): Kind.t =>
  switch (model.package) {
  | Pack((module P), _) => P.kind
  };

let pp_action = (_, _action: action) =>
  failwith("pp_action not implemented");
let pp_focus = (_, _focus: focus) => failwith("pp_focus not implemented");

let term_of_model = (model: model): Language.Any.t =>
  Calc.get_saved_exc(~print="Prohector", model.exp_cache);

// ============================================================
//                Projector Method Dispatch
// ============================================================
open OptUtil.Syntax;

let init =
    (
      type ed,
      ~editor_module: (module EditorInterface.EDITOR with type model = ed),
      kind: Kind.t,
      any: Language.Any.t,
      ed: unit => option(ed),
    )
    : option(model) => {
  let+ md =
    dispatch(
      ~editor_module,
      ~kind,
      ((module P)) => {
        let+ m = P.mk(any, ed);
        Pack((module P), m);
      },
    );
  {
    exp_cache: Calc.Pending,
    package: md,
  };
};

let update =
    (
      ~common: Common.t,
      ~sort: Sort.t,
      ~id: Id.t,
      action: action,
      model: model,
    )
    : model => {
  let Pack((module P), p_model) = model.package;
  let p_model =
    P.update(~common, ~sort, ~id, p_model, P.action'_of_yojson(action));
  {
    exp_cache: Calc.Pending,
    package: Pack((module P), p_model),
  };
};

let make_term =
    (~sort: Sort.t, model: model): (model, Calc.t(Language.Any.t)) => {
  let Pack((module P), p_model) = model.package;
  let (p_model, term) = P.mk_term(~sort, ~prev=model.exp_cache, p_model);
  (
    {
      exp_cache: Calc.save(term),
      package: Pack((module P), p_model),
    },
    term,
  );
};

let calculate = (~common: Common.t, model: model): model => {
  let Pack((module P), p_model) = model.package;
  let p_model = P.calculate(~common, p_model);
  {
    exp_cache: model.exp_cache,
    package: Pack((module P), p_model),
  };
};

let get_cursor_info =
    (
      ~common: Common.t,
      ~inject: action => Ui_effect.t(unit),
      ~read_only: bool,
      model: model,
      focus: focus,
    )
    : Cursor.t => {
  let Pack((module P), p_model) = model.package;
  P.get_cursor_info(
    ~common,
    ~inject=a => inject(P.yojson_of_action'(a)),
    ~read_only,
    p_model,
    P.focus'_of_yojson(focus),
  );
};

let placeholder =
    (type ed_m, type ed_a, type ed_f, ~common, p: Base.projector(model))
    : Util.ProjectorShape.t => {
  let Pack((module P), p_model) = p.model.package;
  P.placeholder(~common, ~id=p.id, p_model);
};

/* Route top-level metadata to the projector view function. */
let view =
    (
      ~common: Common.t,
      ~inject: action => Ui_effect.t(unit),
      ~escape: ProjectorInterface.external_action => Ui_effect.t(unit),
      ~take_focus: focus => Ui_effect.t(unit),
      ~focus: option(focus),
      ~info: ProjectorInterface.info,
      model: model,
    )
    : ProjectorInterface.View.t => {
  let Pack((module P), p_model) = model.package;
  P.view(
    ~common,
    ~inject=a => inject(P.yojson_of_action'(a)),
    ~escape,
    ~take_focus=f => take_focus(P.yojson_of_focus'(f)),
    ~focus=focus |> Option.map(f => P.focus'_of_yojson(f)),
    ~info,
    p_model,
  );
};

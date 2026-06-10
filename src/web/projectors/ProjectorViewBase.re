open Util;
open Virtual_dom.Vdom;
open Haz3lcore;
open ProjectorBase;

/* The web (Vdom) half of the projector API: each projector kind pairs
 * its core logic module (ProjectorBase.Projector, in haz3lcore) with a
 * view module here. The registry lives in ProjectorViews.re; core
 * consults frontend-installed focus behavior via
 * ProjectorBase.focusables (see install in ProjectorViews). */

module View = {
  /* A projector has an inline view, which replaces the underlying
   * syntax. Optionally, it may have an overlay view, which is shown
   * in the same place, but above most base editor decorations
   * including the inline views of all other projectors, and/or
   * an offside view, which is rendered at the end of the base
   * editor line containing the projector */
  type t = {
    inline: Node.t,
    overlay: option(Node.t),
    offside: option(Node.t),
    /* If true, the projector div gets the "error" class,
     * triggering the dashed red SVG border from proj-base.css */
    error: bool,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type status = {
    kind: ProjectorCore.Kind.t,
    sort: Sort.t, /* What sort does the parent editor attribute to the projector? */
    indication: option(Util.Direction.t), /* Is the parent editor caret adjacent? */
    selected: bool, /* Is the projector contained within a selection? */
    error: bool, /* Is there an error mark on the projector? */
    warning: bool /* Is there a warning mark on the projector? */
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type seg =
    (
      ~single_line: bool=?,
      ~background: bool=?,
      ~text_only: bool=?,
      Sort.t,
      list(syntax)
    ) =>
    Node.t;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type args('model, 'action) = {
    model: 'model,
    info,
    /* A callback for the projector's own actions */
    local: 'action => Ui_effect.t(unit),
    /* A callback for parent editor actions */
    parent: external_action => Ui_effect.t(unit),
    /* Creates a non-interactive embedded syntax view,
     * provided here to address a dependency cycle */
    view_seg: seg,
    /* Parent editor context on the projector */
    status,
    /* Core settings for feature flags */
    core_settings: Language.CoreSettings.t,
  };

  let mk = (~overlay=None, ~offside=None, ~error=false, inline) => {
    inline,
    overlay,
    offside,
    error,
  };
};

/* A web view for a projector kind, paired with its core logic module
 * (which provides the model/action types and their serialization) */
module type ProjectorView = {
  module L: Projector;
  let focusable: Focusable.t;
  let view: View.args(L.model, L.action) => View.t;
};

/* String-typed (serialized) interface used by the generic dispatch in
 * ProjectorView.re, mirroring ProjectorBase.Cooked for logic */
module type CookedView = {
  let focusable: Focusable.t;
  let view: View.args(string, string) => View.t;
};

module CookView = (V: ProjectorView) : CookedView => {
  let focusable = V.focusable;
  let view = (args: View.args(string, string)) =>
    V.view({
      model: args.model |> Sexplib.Sexp.of_string |> V.L.model_of_sexp,
      info: args.info,
      local: a =>
        args.local(a |> V.L.sexp_of_action |> Sexplib.Sexp.to_string),
      parent: args.parent,
      view_seg: args.view_seg,
      status: args.status,
      core_settings: args.core_settings,
    });
};

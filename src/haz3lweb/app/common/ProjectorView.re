open Haz3lcore;
open Virtual_dom.Vdom;
open Node;
open ProjectorBase;
open Projector;
open Util;
open Util.OptUtil.Syntax;
open Util.Web;

/* A friendly name for each projector. This is used
 * both for identifying a projector in the CSS and for
 * selecting projectors in the projector panel menu */
let name = (p: Base.kind): string =>
  switch (p) {
  | Fold => "fold"
  | Info => "type"
  | Probe => "probe"
  | Checkbox => "check"
  | Slider => "slider"
  | SliderF => "sliderf"
  | TextArea => "text"
  };

/* This must be updated and kept 1-to-1 with the above
 * name function in order to be able to select the
 * projector in the projector panel menu */
let of_name = (p: string): Base.kind =>
  switch (p) {
  | "fold" => Fold
  | "type" => Info
  | "probe" => Probe
  | "check" => Checkbox
  | "slider" => Slider
  | "sliderf" => SliderF
  | "text" => TextArea
  | _ => failwith("Unknown projector kind")
  };

/* Projectors get a default backing decoration similar
 * to token decorations. This can be made transparent
 * in the CSS if no backing is wanted */
let backing_deco =
    (~font_metrics: FontMetrics.t, ~measurement: Measured.measurement) =>
  PieceDec.relative_shard({
    font_metrics,
    measurement,
    tips: (Some(Convex), Some(Convex)),
  });

/* Adds attributes to a projector UI to support
 * custom styling when selected or indicated */
let status = (indicated: option(Direction.t), selected: bool, sort) =>
  [Sort.show(sort)]
  @ (selected ? ["selected"] : [])
  @ (
    switch (indicated) {
    | Some(d) => ["indicated", Direction.show(d)]
    | None => []
    }
  );

/* Wraps the view function for a projector, absolutely positioning
 * relative to the syntax, adding a default backing decoration, and
 * adding fallthrough handlers where appropriate*/
let view_wrapper =
    (
      ~inject: Action.t => Ui_effect.t(unit),
      ~font_metrics: FontMetrics.t,
      ~measurement: Measured.measurement,
      ~info: info,
      ~indication: option(Direction.t),
      ~selected: bool,
      p: Base.projector,
      views: list(Node.t),
    ) => {
  let sort =
    Option.map(Info.sort_of, info.statics) |> Option.value(~default=Sort.Exp);
  let focus = (id, _) =>
    Effect.(Many([Stop_propagation, inject(Project(Focus(id, None)))]));
  div(
    ~attrs=[
      Attr.classes(
        ["projector", name(p.kind)] @ status(indication, selected, sort),
      ),
      Attr.on_mousedown(focus(info.id)),
      DecUtil.abs_style(measurement, ~font_metrics),
    ],
    views,
  );
};

/* Dispatches projector external actions to editor-level actions */
let handle = (id, action: external_action): Action.project =>
  switch (action) {
  | Remove => Remove(id)
  | Escape(d) => Escape(id, d)
  | SetSyntax(f) => SetSyntax(id, f)
  };

/* Position in pixels for the position offset characters to the
 * right of the end of the row at measurement.origin. */
let offside =
    (
      ~offset: int,
      font_metrics: FontMetrics.t,
      measurement: Measured.measurement,
      measured: Measured.t,
    )
    : float =>
  font_metrics.col_width
  *. float_of_int(
       Measured.start_row_width(measurement, measured)
       + offset
       - measurement.origin.col,
     );

let offside_pos = offside_offset =>
  Attr.create(
    "style",
    Printf.sprintf("position: absolute; left: %fpx;", offside_offset),
  );

/* Gather utility functions/values to be passed to the projector.
 * See ProjectorBase.utility definition for more information */
let mk_utility = (globals: Globals.t): ProjectorBase.utility => {
  {
    font_metrics: globals.font_metrics,
    view_seg: (sort, seg) =>
      CodeViewable.view_segment(
        ~globals,
        ~sort,
        ~shape_of_proj=Projector.Shape.of_map_default,
        seg,
      ),
    exp_to_seg: exp =>
      exp
      |> DHExp.strip_casts
      |> ExpToSegment.exp_to_segment(
           ~settings={
             inline: false,
             fold_case_clauses: false,
             fold_fn_bodies: false,
             hide_fixpoints: false,
             fold_cast_types: false,
             show_filters: false,
           },
         ),
    seg_to_exp: seg => MakeTerm.go(seg).term,
  };
};

let mk_info =
    (
      id: Id.t,
      p: Piece.projector,
      ~cached_statics: CachedStatics.t,
      ~dynamics: Dynamics.Map.t,
    )
    : ProjectorBase.info => {
  id,
  syntax: p.syntax,
  statics: Statics.Map.lookup(id, cached_statics.info_map),
  dynamics: Dynamics.Map.lookup(id, dynamics),
};

/* Extracts projector-instance-specific metadata necessary to
 * render the view, instantiates appropriate action handlers,
 * renders the view, and then wraps it so as to position it
 * correctly with respect to the underyling editor */
let setup_view =
    (
      id: Id.t,
      ~cached_statics: CachedStatics.t,
      ~cached_syntax: Editor.CachedSyntax.t,
      ~dynamics,
      ~inject: Action.t => Ui_effect.t(unit),
      ~globals as {font_metrics, _} as globals: Globals.t,
      ~indication: option(Direction.t),
    )
    : option((Node.t, Node.t, option(Node.t))) => {
  let* p = Id.Map.find_opt(id, cached_syntax.projectors);
  let+ measurement = Measured.find_pr_opt(p, cached_syntax.measured);
  let (module P) = to_module(p.kind);
  let info = mk_info(id, p, ~cached_statics, ~dynamics);
  let utility = mk_utility(globals);
  let parent = a => inject(Project(handle(id, a)));
  let local = a =>
    inject(Project(SetModel(id, P.update(p.model, info, a))));
  let offside_pos =
    offside_pos(
      offside(~offset=4, font_metrics, measurement, cached_syntax.measured),
    );
  let wrapper =
    view_wrapper(
      ~inject,
      ~font_metrics,
      ~measurement,
      ~indication,
      ~info,
      ~selected=List.mem(id, cached_syntax.selection_ids),
      p,
    );
  let inline_view = P.view(p.model, info, ~local, ~parent, ~utility);
  let offside_view =
    Option.map(
      v =>
        div(
          ~attrs=[offside_pos],
          [v(p.model, info, ~local, ~parent, ~utility)],
        ),
      P.offside_view,
    );
  let overlay_view =
    Option.map(
      v => wrapper([v(p.model, info, ~local, ~parent, ~utility)]),
      P.overlay_view,
    );
  let underlay_view =
    switch (P.underlay_view) {
    | Some(v) => wrapper([v(p.model, info, ~utility)])
    | None => wrapper([backing_deco(~font_metrics, ~measurement)])
    };
  let combined_view = wrapper([inline_view] @ Option.to_list(offside_view));
  (underlay_view, combined_view, overlay_view);
};

/* Is the piece with id indicated? If so, where is it wrt the caret? */
let indication = (z, id) =>
  switch (Indicated.piece(z)) {
  | Some((p, d, _)) when Piece.id(p) == id => Some(Direction.toggle(d))
  | _ => None
  };

/* Returns a div containing all projector UIs, intended to
 * be absolutely positioned atop a rendered editor UI */
let all =
    (
      z,
      ~globals: Globals.t,
      ~cached_statics: CachedStatics.t,
      ~cached_syntax: Editor.CachedSyntax.t,
      ~dynamics: Dynamics.Map.t,
      ~inject,
    ) => {
  let mk_views = ((id: Id.t, _)) => {
    let indication = indication(z, id);
    setup_view(
      id,
      ~cached_statics,
      ~cached_syntax,
      ~dynamics,
      ~inject,
      ~globals,
      ~indication,
    );
  };
  let projector_ids = cached_syntax.projectors |> Id.Map.bindings |> List.rev;

  let (underlay_views, base_views, overlay_views) =
    projector_ids |> List.filter_map(mk_views) |> ListUtil.split3;
  let overlay_views = overlay_views |> List.filter_map(Fun.id);
  [
    div_c(
      "projectors",
      [
        div_c("base", base_views),
        div_c("underlays", underlay_views),
        div_c("overlays", overlay_views),
      ],
    ),
  ];
};

/* When the caret is directly adjacent to a projector, keyboard commands
 * can be overidden here. Right now, trying to move into the projector,
 * that is, pressing left when it's to the right or vice-versa, without
 * holding down a modifier, will give the projector focus (if its can_focus)
 * flag is set. Be conservative about these kind of overloads; you need
 * to consider how they interact with all the editor keyboard commands.
 * For example, without the modifiers check, this would break selection
 * around a projector. */
let key_handoff = (editor: Editor.t, key: Key.t): option(Action.project) =>
  switch (Indicated.projector(editor.state.zipper)) {
  | None => None
  | Some((id, p)) =>
    let* (_, d, _) = Indicated.piece(editor.state.zipper);
    let (module P) = to_module(p.kind);
    switch (key) {
    | {key, sys: _, shift: Up, meta: Up, ctrl: Up, alt: Up} when P.can_focus =>
      switch (key, d) {
      | (D("ArrowRight"), Right) => Some(Action.Focus(id, Some(Left)))
      | (D("ArrowLeft"), Left) => Some(Focus(id, Some(Right)))
      | _ => None
      }
    | _ => None
    };
  };

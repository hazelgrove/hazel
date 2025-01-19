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

let offside_wrapper =
    (font_metrics: FontMetrics.t, offside_base: int, v: Node.t) =>
  div(
    ~attrs=[
      Attr.create(
        "style",
        Printf.sprintf(
          "position: absolute; left: %fpx;",
          font_metrics.col_width *. float_of_int(offside_base),
        ),
      ),
    ],
    [v],
  );

/* Gather utility functions/values to be passed to the projector.
 * See ProjectorBase.utility definition for more information */
let mk_utility = (font_metrics: FontMetrics.t): ProjectorBase.utility => {
  let term_to_seg =
    ExpToSegment.any_to_pretty(
      ~settings={
        ...ExpToSegment.Settings.of_core(~inline=false, CoreSettings.off),
        show_unknown_as_hole: false,
      },
    );
  let seg_to_term = MakeTerm.any;
  let lift_syntax = (fn: Any.t => Any.t, piece: syntax): syntax =>
    switch ([piece] |> seg_to_term |> fn |> term_to_seg) {
    | [e] => e
    | seg =>
      let sort = Segment.sort_of(Segment.skel(seg), seg);
      switch (sort) {
      | Exp => Piece.mk_tile(Form.get("parens_exp"), [seg])
      | Pat => Piece.mk_tile(Form.get("parens_pat"), [seg])
      | Typ => Piece.mk_tile(Form.get("parens_typ"), [seg])
      | TPat
      | Rul
      | Any
      | Nul => failwith("Projector: lift_syntax")
      };
    };
  {
    view_seg: Code.simple_view(font_metrics),
    term_to_seg,
    seg_to_term,
    lift_syntax,
  };
};

let indication = (z, id) =>
  switch (Indicated.piece(z)) {
  | Some((p, d, _)) when Piece.id(p) == id => Some(Direction.toggle(d))
  | _ => None
  };

/* Find end of row offset position in grid units */
let offside_base =
    (~offset: int, measurement: Measured.measurement, measured: Measured.t)
    : int =>
  Measured.start_row_width(measurement, measured)
  + offset
  - measurement.origin.col;

type projector_data = {
  p: Piece.projector,
  indication: option(Direction.t),
  selected: bool,
  info: ProjectorBase.info,
  measurement: Measured.measurement,
  offside_base: int,
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

let collect_data =
    (cached_syntax: Editor.CachedSyntax.t, zipper, cached_statics, dynamics) => {
  let projector_ids = cached_syntax.projectors |> Id.Map.bindings |> List.rev;
  List.filter_map(
    ((id, _)) => {
      let* p = Id.Map.find_opt(id, cached_syntax.projectors);
      let+ measurement = Measured.find_pr_opt(p, cached_syntax.measured);
      {
        p,
        indication: indication(zipper, id),
        selected: List.mem(id, cached_syntax.selection_ids),
        measurement,
        info: mk_info(id, p, ~cached_statics, ~dynamics),
        offside_base:
          offside_base(~offset=4, measurement, cached_syntax.measured),
      };
    },
    projector_ids,
  );
};

/* Extracts projector-instance-specific metadata necessary to
 * render the view, instantiates appropriate action handlers,
 * renders the view, and then wraps it so as to position it
 * correctly with respect to the underyling editor */
let setup_view =
    (
      inject: Action.t => Ui_effect.t(unit),
      utility: ProjectorBase.utility,
      font_metrics: FontMetrics.t,
      {p, info, offside_base, indication, measurement, selected}: projector_data,
    )
    : (Node.t, Node.t, option(Node.t)) => {
  let (module P) = to_module(p.kind);
  let parent = a => inject(Project(handle(p.id, a)));
  let local = a =>
    inject(Project(SetModel(p.id, P.update(p.model, info, a))));
  let wrapper =
    view_wrapper(
      ~inject,
      ~font_metrics,
      ~measurement,
      ~indication,
      ~info,
      ~selected,
      p,
    );
  let inline_view = P.view(p.model, info, ~local, ~parent, ~utility);
  let offside_view =
    Option.map(
      v =>
        offside_wrapper(
          font_metrics,
          offside_base,
          v(p.model, info, ~local, ~parent, ~utility),
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
      inject: Action.t => Ui_effect.t(unit),
      utility: ProjectorBase.utility,
      font_metrics: FontMetrics.t,
      projector_data: list(projector_data),
    ) => {
  let (underlay_views, base_views, overlay_views) =
    projector_data
    |> List.map(setup_view(inject, utility, font_metrics))
    |> ListUtil.split3;
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

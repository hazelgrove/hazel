open Haz3lcore;
open Virtual_dom.Vdom;
open Node;
open ProjectorBase;
open Projector;
open Util;
open Util.OptUtil.Syntax;
open Util.Web;

type kind = Base.kind;

/* A friendly name for each projector. This is used
 * both for identifying a projector in the CSS and for
 * selecting projectors in the projector panel menu */
let name = (p: kind): string =>
  switch (p) {
  | Fold => "fold"
  | Info => "type"
  | Checkbox => "check"
  | Slider => "slide"
  | SliderF => "slidef"
  | TextArea => "text"
  };

/* This must be updated and kept 1-to-1 with the above
 * name function in order to be able to select the
 * projector in the projector panel menu */
let of_name = (p: string): kind =>
  switch (p) {
  | "fold" => Fold
  | "type" => Info
  | "check" => Checkbox
  | "slide" => Slider
  | "slidef" => SliderF
  | "text" => TextArea
  | _ => failwith("Unknown projector kind")
  };

/* Projectors get a default backing decoration similar
 * to token decorations. This can be made transparent
 * in the CSS if no backing is wanted */
let backing_deco =
    (
      ~font_metrics: FontMetrics.t,
      ~measurement: Measured.measurement,
      ~shape: shape,
    ) =>
  switch (shape) {
  | Inline(_)
  | Block(_) =>
    PieceDec.relative_shard({
      font_metrics,
      measurement,
      shapes: (Convex, Convex),
    })
  };

/* Adds attributes to a projector UI to support
 * custom styling when selected or indicated */
let status = (indicated: option(Direction.t), selected: bool, shape: shape) =>
  (selected ? ["selected"] : [])
  @ (
    switch (shape) {
    | Inline(_) => ["inline"]
    | Block(_) => ["block"]
    }
  )
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
      ~inject: UpdateAction.t => Ui_effect.t(unit),
      ~font_metrics: FontMetrics.t,
      ~measurement: Measured.measurement,
      ~info: info,
      ~indication: option(Direction.t),
      ~selected: bool,
      p: Base.projector,
      view: Node.t,
    ) => {
  let shape = Projector.shape(p, info);
  let focus = (id, _) =>
    Effect.(
      Many([
        Stop_propagation,
        inject(PerformAction(Project(Focus(id, None)))),
      ])
    );
  div(
    ~attrs=[
      Attr.classes(
        ["projector", name(p.kind)] @ status(indication, selected, shape),
      ),
      Attr.on_mousedown(focus(info.id)),
      DecUtil.abs_style(measurement, ~font_metrics),
    ],
    [view, backing_deco(~font_metrics, ~measurement, ~shape)],
  );
};

/* Dispatches projector external actions to editor-level actions */
let handle = (id, action: external_action): Action.project =>
  switch (action) {
  | Remove => Remove(id)
  | Escape(d) => Escape(id, d)
  | SetSyntax(f) => SetSyntax(id, f)
  };

/* Extracts projector-instance-specific metadata necessary to
 * render the view, instantiates appropriate action handlers,
 * renders the view, and then wraps it so as to position it
 * correctly with respect to the underyling editor */
let setup_view =
    (
      id: Id.t,
      ~meta: Editor.Meta.t,
      ~inject: UpdateAction.t => Ui_effect.t(unit),
      ~font_metrics,
      ~indication: option(Direction.t),
    )
    : option(Node.t) => {
  let* p = Id.Map.find_opt(id, meta.syntax.projectors);
  let* syntax = Some(p.syntax);
  let ci = Id.Map.find_opt(id, meta.statics.info_map);
  let info = {id, ci, syntax};
  let+ measurement = Measured.find_pr_opt(p, meta.syntax.measured);
  let (module P) = to_module(p.kind);
  let parent = a => inject(PerformAction(Project(handle(id, a))));
  let local = a =>
    inject(PerformAction(Project(SetModel(id, P.update(p.model, a)))));
  view_wrapper(
    ~inject,
    ~font_metrics,
    ~measurement,
    ~indication,
    ~info,
    ~selected=List.mem(id, meta.syntax.selection_ids),
    p,
    P.view(p.model, ~info, ~local, ~parent),
  );
};

let indication = (z, id) =>
  switch (Indicated.piece(z)) {
  | Some((p, d, _)) when Piece.id(p) == id => Some(Direction.toggle(d))
  | _ => None
  };

/* Returns a div containing all projector UIs, intended to
 * be absolutely positioned atop a rendered editor UI */
let all = (z, ~meta: Editor.Meta.t, ~inject, ~font_metrics) => {
  // print_endline(
  //   "cardinal: "
  //   ++ (meta.projected.projectors |> Id.Map.cardinal |> string_of_int),
  // );
  div_c(
    "projectors",
    List.filter_map(
      ((id, _)) => {
        //TODO(andrew): cleanup
        let indication = indication(z, id);
        setup_view(id, ~meta, ~inject, ~font_metrics, ~indication);
      },
      Id.Map.bindings(meta.syntax.projectors) |> List.rev,
    ),
  );
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
  switch (Editor.indicated_projector(editor)) {
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

/* The projector selection panel on the right of the bottom bar */
module Panel = {
  let option_view = (name, n) =>
    option(
      ~attrs=n == name ? [Attr.create("selected", "selected")] : [],
      [text(n)],
    );

  /* Decide which projectors are applicable based on the cursor info.
   * This is slightly inside-out as elsewhere it depends on the underlying
   * syntax, which is not easily available here */
  let applicable_projectors = (ci: Info.t): list(Base.kind) =>
    (
      switch (Info.cls_of(ci)) {
      | Exp(Bool)
      | Pat(Bool) => [Base.Checkbox]
      | Exp(Int)
      | Pat(Int) => [Slider]
      | Exp(Float)
      | Pat(Float) => [SliderF]
      | Exp(String)
      | Pat(String) => [TextArea]
      | _ => []
      }
    )
    @ [Base.Fold]
    @ (
      switch (ci) {
      | InfoExp(_)
      | InfoPat(_) => [(Info: Base.kind)]
      | _ => []
      }
    );

  let toggle_projector = (active, id, ci): Action.project =>
    active || applicable_projectors(ci) == []
      ? Remove(id) : SetIndicated(List.hd(applicable_projectors(ci)));

  let toggle_view = (~inject, ci, id, active: bool, might_project) =>
    div(
      ~attrs=[
        clss(
          ["toggle-switch"]
          @ (active ? ["active"] : [])
          @ (might_project ? [] : ["inactive"]),
        ),
        Attr.on_click(_ => inject(toggle_projector(active, id, ci))),
      ],
      [
        div(
          ~attrs=[clss(["toggle-knob"])],
          [
            Node.create(
              "img",
              ~attrs=[Attr.src("img/noun-fold-1593402.svg")],
              [],
            ),
          ],
        ),
      ],
    );

  let kind = (editor: Editor.t) => {
    let+ (_, p) = Editor.indicated_projector(editor);
    p.kind;
  };

  let id = (editor: Editor.t) => {
    switch (Editor.indicated_projector(editor)) {
    | Some((id, _)) => id
    | None => Id.invalid
    };
  };

  let currently_selected = editor =>
    option_view(
      switch (kind(editor)) {
      | None => "Fold"
      | Some(k) => name(k)
      },
    );

  let view = (~inject, editor: Editor.t, ci: Info.t) => {
    let might_project =
      switch (Indicated.piece''(editor.state.zipper)) {
      | Some((p, _, _)) => minimum_projection_condition(p)
      | None => false
      };
    div(
      ~attrs=[Attr.id("projectors")],
      [
        toggle_view(
          ~inject,
          ci,
          id(editor),
          kind(editor) != None,
          might_project,
        ),
        Node.select(
          ~attrs=[
            Attr.on_change((_, name) =>
              inject(SetIndicated(of_name(name)))
            ),
          ],
          (might_project ? applicable_projectors(ci) : [])
          |> List.map(name)
          |> List.map(currently_selected(editor)),
        ),
      ],
    );
  };
};

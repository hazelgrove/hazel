open Haz3lcore;
open Virtual_dom.Vdom;
open Node;
open Util;
open OptUtil.Syntax;
open Web;

/* This defines the projector selection menu/toggle at the bottom right */

module Applicable = {
  /* If there are applicable projectors, we distinguish the first
   * one, which will be the current active projector if the indicated
   * term is already projected */
  type t = option((ProjectorCore.kind, list(ProjectorCore.kind)));

  /* Determines what term to target for projection. This logic
   * should be kept in sync with the projector add/remove logic
   * in ProjectorPerform */
  let target_term = (cursor: Cursor.cursor(Editors.Update.t)) =>
    switch (cursor.selection) {
    | None
    | Some([]) =>
      switch (cursor.indicated_piece) {
      | Some(Projector({syntax, _})) =>
        MakeTerm.for_projection(Segment.unparenthesize_or_wrap(syntax))
      | _ =>
        let* info = cursor.info;
        Info.any_of(info);
      }
    | Some([Projector({syntax, _})]) =>
      MakeTerm.for_projection(Segment.unparenthesize_or_wrap(syntax))
    | Some(seg) => MakeTerm.for_projection(seg)
    };

  /* Is a projector of `kind` applicable to the target term? */
  let is_applicable =
      (cursor: Cursor.cursor(Editors.Update.t), kind: ProjectorCore.kind)
      : option(ProjectorCore.kind) => {
    let (module P) = ProjectorInit.to_module(kind);
    let* term = target_term(cursor);
    P.can_project(term) ? Some(kind) : None;
  };

  /* If the current indicated term is a projector, return its kind */
  let indicated_kind =
      (editor: option(Editor.t)): option(ProjectorCore.kind) => {
    let* editor = editor;
    let* (piece, _, _) = Indicated.for_index(editor.state.zipper);
    switch (piece) {
    | Projector({kind, _}) => Some(kind)
    | _ => None
    };
  };

  /* The string names of all projectors applicable to the currently
   * indicated syntax, with the currently applied projection (if any)
   * lifted to the top of the list */
  let lift_active_projector =
      (
        cursor: Cursor.cursor(Editors.Update.t),
        applicable_projectors: list(ProjectorCore.kind),
      )
      : list(ProjectorCore.kind) => {
    switch (indicated_kind(cursor.editor)) {
    | None => applicable_projectors
    | Some(k) => ListUtil.lift(k, applicable_projectors)
    };
  };

  let is_read_only = (cursor: Cursor.cursor(Editors.Update.t)): bool =>
    switch (cursor.editor) {
    | None => true
    | _ => cursor.editor_read_only
    };

  let projectors = (cursor): t =>
    if (is_read_only(cursor)) {
      None;
    } else {
      let list =
        ProjectorCore.projectors
        |> List.filter_map(is_applicable(cursor))
        |> lift_active_projector(cursor);
      switch (list) {
      | [] => None
      | [hd, ...tl] => Some((hd, tl))
      };
    };
};
let knob =
  div(
    ~attrs=[clss(["toggle-knob"])],
    [create("img", ~attrs=[Attr.src("img/noun-fold-1593402.svg")], [])],
  );

let toggle_view =
    (
      ~inject: Action.project => Ui_effect.t(unit),
      applicable_projectors: Applicable.t,
    ) =>
  switch (applicable_projectors) {
  | None => div(~attrs=[clss(["toggle-switch", "inactive"])], [knob])
  | Some((active, _)) =>
    div(
      ~attrs=[
        clss(["toggle-switch", "active"]),
        Attr.on_mousedown(_ => inject(SetIndicated(Specific(active)))),
      ],
      [knob],
    )
  };

let keyboard_shortcut_of = (kind: ProjectorCore.kind): string =>
  switch (kind) {
  | Fold => "Option-f"
  | Probe => "Option-v"
  | Info => "Option-t"
  | _ => "Option-l"
  };

/* A selection input for contetually applicable projectors */
let select_view =
    (
      ~inject: Action.project => Ui_effect.t(unit),
      applicable_projectors: Applicable.t,
    ) => {
  switch (applicable_projectors) {
  | None => select(~attrs=[Attr.id("projector-select")], [])
  | Some((active, rest)) =>
    select(
      ~attrs=[
        Attr.id("projector-select"),
        Attr.title(keyboard_shortcut_of(active)),
        Attr.on_change((_, name) => {
          let value = ProjectorView.name(active);
          JsUtil.set_select_value("projector-select", value);
          inject(SetIndicated(Specific(ProjectorView.of_name(name))));
        }),
        //Attr.string_property("value", value),
      ],
      [active, ...rest]
      |> List.map(k => option([text(ProjectorView.name(k))])),
    )
  };
};

let view = (~inject, cursor: Cursor.cursor(Editors.Update.t)) => {
  let applicable_projectors = Applicable.projectors(cursor);
  div(
    ~attrs=[Attr.id("projectors")],
    [select_view(~inject, applicable_projectors)]
    @ [toggle_view(~inject, applicable_projectors)],
  );
};

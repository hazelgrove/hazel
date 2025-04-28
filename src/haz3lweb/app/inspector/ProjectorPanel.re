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
  type t = option((ProjectorCore.Kind.t, list(ProjectorCore.Kind.t)));

  /* Determines what term to target for projection. This logic
   * should be kept in sync with the projector add/remove logic
   * in ProjectorPerform */
  let target_term = (cursor: Cursor.cursor(Editors.Update.t)) =>
    // TODO[Matt|Andrew] : Make the sort more general
    switch (cursor.selection) {
    | None
    | Some([]) =>
      switch (cursor.indicated_piece) {
      | Some(Projector(p)) => Some(Projectors.make_term(p.model, Exp))
      | _ =>
        let* info = cursor.info;
        Info.any_of(info);
      }
    | Some([Projector(p)]) => Some(Projectors.make_term(p.model, Exp))
    | Some(seg) =>
      Some(
        Editor.make_term(
          Exp,
          seg |> Zipper.unzip |> Editor.Model.mk(~sort=Exp),
        ),
      )
    };

  /* Is a projector of `kind` applicable to the target term? */
  let is_applicable =
      (cursor: Cursor.cursor(Editors.Update.t), kind: ProjectorCore.Kind.t)
      : option(ProjectorCore.Kind.t) => {
    let* term = target_term(cursor);
    let+ _ =
      Projectors.init(kind, term, () =>
        cursor.selection
        |> Option.map(Zipper.unzip)
        |> Option.map(Editor.Model.mk(~sort=Exp))
      );
    kind;
  };

  /* If the current indicated term is a projector, return its kind */
  let indicated_kind =
      (editor: option(Editor.t)): option(ProjectorCore.Kind.t) => {
    let* editor = editor;
    let* (piece, _, _) = Indicated.for_index(editor |> Editor.get_z);
    switch (piece) {
    | Projector(p) => Some(Projectors.kind_of_model(p.model))
    | _ => None
    };
  };

  /* The string names of all projectors applicable to the currently
   * indicated syntax, with the currently applied projection (if any)
   * lifted to the top of the list */
  let lift_active_projector =
      (
        cursor: Cursor.cursor(Editors.Update.t),
        applicable_projectors: list(ProjectorCore.Kind.t),
      )
      : list(ProjectorCore.Kind.t) => {
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
        ProjectorCore.Kind.projectors
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
      cursor: Cursor.cursor(Editors.Update.t),
    ) =>
  switch (applicable_projectors) {
  | None => div(~attrs=[clss(["toggle-switch", "inactive"])], [knob])
  | Some((active, _)) =>
    div(
      ~attrs=[
        clss(
          ["toggle-switch"]
          @ (
            Applicable.indicated_kind(cursor.editor) == None
              ? [] : ["active"]
          ),
        ),
        Attr.on_mousedown(_ => inject(SetIndicated(Specific(active)))),
      ],
      [knob],
    )
  };

let keyboard_shortcut_of = (kind: ProjectorCore.Kind.t): string =>
  switch (kind) {
  // | Fold => "Option-f"
  // | Probe => "Option-v"
  | Info => "Option-t"
  | Slider => "Option-l"
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
    let value = ProjectorCore.Kind.name(active);
    select(
      ~attrs=[
        Attr.id("projector-select"),
        Attr.title(keyboard_shortcut_of(active)),
        Attr.on_change((_, name) => {
          JsUtil.set_select_value("projector-select", value);
          inject(SetIndicated(Specific(ProjectorCore.Kind.of_name(name))));
        }),
      ],
      [active, ...rest]
      |> List.map(k =>
           option(
             ~attrs=[Attr.title(keyboard_shortcut_of(k))],
             [text(ProjectorCore.Kind.name(k))],
           )
         ),
    );
  };
};

let view = (~inject, cursor: Cursor.cursor(Editors.Update.t)) => {
  let applicable_projectors = Applicable.projectors(cursor);
  div(
    ~attrs=[Attr.id("projectors")],
    [select_view(~inject, applicable_projectors)]
    @ [toggle_view(~inject, applicable_projectors, cursor)],
  );
};

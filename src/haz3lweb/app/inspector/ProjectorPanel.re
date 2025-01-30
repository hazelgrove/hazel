open Haz3lcore;
open Virtual_dom.Vdom;
open Node;
open Util;
open OptUtil.Syntax;
open Web;

// The projector selection panel on the right of the bottom bar

let indicated_kind = (editor: option(Editor.t)) => {
  let* editor = editor;
  let+ (_, p) = Indicated.projector(editor.state.zipper);
  p.kind;
};

module Applicable = {
  /* Determines projectors applicable to indicated term */

  let is_indicated_piece_rule = (indicated_piece: option(Piece.t)) =>
    switch (indicated_piece) {
    | Some(Piece.Tile({label: ["|", "=>"], _})) => true
    | _ => false
    };
  let indicated_term =
      (cursor: Cursor.cursor(Editors.Update.t)): option(Term.Any.t) =>
    /* The condition on rule pieces here addresses an edge case where the
     * term selection logic considers rules to be terms but they are
     * nonconvex and currently cannot be projected. */
    is_indicated_piece_rule(cursor.indicated_piece)
      ? {
        let* info = cursor.info;
        let+ term = Info.any_of(info);
        term;
      }
      : {
        let* info = cursor.info;
        let+ term = Info.any_of(info);
        term;
      };

  /* Determines what term to target for projection. Ideally this would
   * use exactly the same logic as ProjectorPerform, which, in the event
   * there is no selection, tries to form one with Select.current_term,
   * and then proceeds in the same way as if there was an existing selection.
   * However, I haven't found a good way to call Select.current_term here
   * due to dependencies, so for now we individually handle an edge case */
  let target_term = (cursor: Cursor.cursor(Editors.Update.t)) =>
    switch (cursor.selection) {
    | None => indicated_term(cursor)
    | Some(selection) =>
      switch (selection) {
      | [] => indicated_term(cursor)
      | seg =>
        switch (MakeTerm.for_projection(seg)) {
        | None => None
        | Some(term) => Some(term)
        }
      }
    };

  /* Is a projector of `kind` applicable to the
   * currently selected or indicated term? */
  let is_applicable =
      (cursor: Cursor.cursor(Editors.Update.t), kind: ProjectorCore.kind)
      : option(ProjectorCore.kind) => {
    let (module P) = ProjectorInit.to_module(kind);
    let* piece = cursor.indicated_piece;
    let* term = target_term(cursor);
    P.can_project(piece, term) ? Some(kind) : None;
  };

  let lift = (str, strs) => List.cons(str, List.filter((!=)(str), strs));

  /* The string names of all projectors applicable to the currently
   * indicated syntax, with the currently applied projection (if any)
   * lifted to the top of the list */
  let lift_active_projector =
      (cursor: Cursor.cursor(Editors.Update.t), applicable_projectors) => {
    switch (indicated_kind(cursor.editor)) {
    | None => applicable_projectors
    | Some(k) => lift(k, applicable_projectors)
    };
  };

  let projectors = cursor =>
    ProjectorCore.projectors
    |> List.filter_map(is_applicable(cursor))
    |> lift_active_projector(cursor);
};

let toggle_projector = (active, applicable_projectors): Action.project =>
  active || applicable_projectors == []
    ? RemoveIndicated
    : SetIndicated(Specific(List.hd(applicable_projectors)));

let knob =
  div(
    ~attrs=[clss(["toggle-knob"])],
    [
      Node.create("img", ~attrs=[Attr.src("img/noun-fold-1593402.svg")], []),
    ],
  );
let toggle_view =
    (~inject, applicable_projectors, ~active: bool, ~might_project) =>
  div(
    ~attrs=[
      clss(
        ["toggle-switch"]
        @ (active ? ["active"] : [])
        @ (might_project && applicable_projectors != [] ? [] : ["inactive"]),
      ),
      Attr.on_mousedown(_ =>
        might_project
          ? inject(toggle_projector(active, applicable_projectors))
          : Effect.Ignore
      ),
    ],
    [knob],
  );

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
      applicable_projectors: list(ProjectorCore.kind),
    ) => {
  let applicable_projector_strings =
    List.map(ProjectorView.name, applicable_projectors);
  let value =
    switch (applicable_projector_strings) {
    | [] => ""
    | [hd, ..._] => hd
    };
  let title =
    switch (applicable_projectors) {
    | [] => ""
    | [hd, ..._] => keyboard_shortcut_of(hd)
    };
  Node.select(
    ~attrs=[
      Attr.title(title),
      Attr.on_change((_, name) =>
        inject(SetIndicated(Specific(ProjectorView.of_name(name))))
      ),
      Attr.string_property("value", value),
    ],
    applicable_projector_strings |> List.map(n => option([text(n)])),
  );
};

let might_project: Cursor.cursor(Editors.Update.t) => bool =
  cursor =>
    switch (cursor.editor) {
    | None => false
    | _ => !cursor.editor_read_only
    };

let view = (~inject, cursor: Cursor.cursor(Editors.Update.t)) => {
  let might_project = might_project(cursor);
  let active = indicated_kind(cursor.editor) != None;
  let applicable_projectors =
    might_project ? Applicable.projectors(cursor) : [];
  div(
    ~attrs=[Attr.id("projectors")],
    [select_view(~inject, applicable_projectors)]
    @ [toggle_view(~inject, ~active, ~might_project, applicable_projectors)],
  );
};

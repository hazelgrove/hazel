open Haz3lcore;
open Virtual_dom.Vdom;
open Node;
open Util.OptUtil.Syntax;
open Util.Web;

// The projector selection panel on the right of the bottom bar

let is_indicated_piece_rule = (indicated_piece: option(Piece.t)) =>
  switch (indicated_piece) {
  | Some(Piece.Tile({label: ["|", "=>"], _})) => true
  | _ => false
  };
let indicated_term = (cursor: Cursor.cursor(Editors.Update.t)) =>
  /* The condition on rule pieces here addresses an edge case where the
   * term selection logic considers rules to be terms but they are
   * nonconvex and currently cannot be projected */
  is_indicated_piece_rule(cursor.indicated_piece)
    ? Option.None : Util.OptUtil.and_then(Info.any_of, cursor.info);

/* Determines what term to target for projection. If there is a
 * selection, try to use that, otherwise, see what term is indicated,
 * according to the term selection logic  */
let target_term = (cursor: Cursor.cursor(Editors.Update.t)) =>
  switch (cursor.selection) {
  | None => indicated_term(cursor)
  | Some(selection) =>
    switch (selection) {
    | [] => indicated_term(cursor)
    | seg =>
      switch (MakeTerm.any(seg)) {
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

let applicable_projectors = (cursor: Cursor.cursor(Editors.Update.t)) =>
  ProjectorCore.projectors |> List.filter_map(is_applicable(cursor));

let kind = (editor: option(Editor.t)) => {
  let* editor = editor;
  let+ (_, p) = Indicated.projector(editor.state.zipper);
  p.kind;
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
    (
      ~inject,
      applicable_projectors,
      cursor: Cursor.cursor(Editors.Update.t),
      might_project,
    ) => {
  let active = kind(cursor.editor) != None;
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
};

let id = (editor: option(Editor.t)) => {
  {
    let* editor = editor;
    let+ (id, _) = Indicated.projector(editor.state.zipper);
    id;
  }
  |> Option.value(~default=Id.invalid);
};

let might_project: Cursor.cursor(Editors.Update.t) => bool =
  cursor =>
    switch (cursor.editor) {
    | _ when cursor.editor_read_only => false
    | None => false
    | Some(_) => true
    };

let lift = (str, strs) => List.cons(str, List.filter((!=)(str), strs));

/* The string names of all projectors applicable to the currently
 * indicated syntax, with the currently applied projection (if any)
 * lifted to the top of the list */
let applicable_projector_strings =
    (cursor: Cursor.cursor(Editors.Update.t), applicable_projectors) => {
  switch (kind(cursor.editor)) {
  | None => applicable_projectors
  | Some(k) => lift(k, applicable_projectors)
  };
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
      applicable_projectors: list(ProjectorCore.kind),
      cursor: Cursor.cursor(Editors.Update.t),
    ) => {
  let applicable_projectors =
    might_project(cursor)
      ? applicable_projector_strings(cursor, applicable_projectors) : [];
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

let toggle_view =
    (~inject, applicable_projectors, cursor: Cursor.cursor(Editors.Update.t)) =>
  toggle_view(~inject, applicable_projectors, cursor, might_project(cursor));

let view = (~inject, cursor: Cursor.cursor(Editors.Update.t)) => {
  let applicable_projectors = applicable_projectors(cursor);
  div(
    ~attrs=[Attr.id("projectors")],
    [select_view(~inject, applicable_projectors, cursor)]
    @ [toggle_view(~inject, applicable_projectors, cursor)],
  );
};

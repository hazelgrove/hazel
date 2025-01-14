open Haz3lcore;
open Virtual_dom.Vdom;
open Node;
open Projector;
open Util.OptUtil.Syntax;
open Util.Web;

// The projector selection panel on the right of the bottom bar

/* Decide which projectors are applicable based on the cursor info.
 * This is slightly inside-out as elsewhere it depends on the underlying
 * syntax, which is not easily available here */
let applicable_projectors: option(Info.t) => list(Base.kind) =
  fun
  | None => []
  | Some(ci) =>
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
      | InfoExp(_) => [Info, Probe]
      | InfoPat(_) => [Info, Probe]
      | _ => []
      }
    );

let toggle_projector = (active, id, ci: option(Info.t)): Action.project =>
  active || applicable_projectors(ci) == []
    ? Remove(id) : SetIndicated(List.hd(applicable_projectors(ci)));

let toggle_view =
    (~inject, ci: option(Info.t), id, active: bool, might_project) =>
  div(
    ~attrs=[
      clss(
        ["toggle-switch"]
        @ (active ? ["active"] : [])
        @ (might_project ? [] : ["inactive"]),
      ),
      Attr.on_mousedown(_ =>
        might_project
          ? inject(toggle_projector(active, id, ci)) : Effect.Ignore
      ),
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

let kind = (editor: option(Editor.t)) => {
  let* editor = editor;
  let+ (_, p) = Indicated.projector(editor.state.zipper);
  p.kind;
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
    | Some(editor) =>
      switch (Indicated.piece''(editor.state.zipper)) {
      | None => false
      | Some((p, _, _)) => minimum_projection_condition(p)
      }
    };

let lift = (str, strs) => List.cons(str, List.filter((!=)(str), strs));

/* The string names of all projectors applicable to the currently
 * indicated syntax, with the currently applied projection (if any)
 * lifted to the top of the list */
let applicable_projector_strings = (cursor: Cursor.cursor(Editors.Update.t)) => {
  let strs = applicable_projectors(cursor.info);
  switch (kind(cursor.editor)) {
  | None => strs
  | Some(k) => lift(k, strs)
  };
};

let keyboard_shortcut_of = (kind: Base.kind): string =>
  switch (kind) {
  | Fold => "Option-f"
  | Probe => "Option-v"
  | _ => ""
  };

/* A selection input for contetually applicable projectors */
let select_view =
    (
      ~inject: Action.project => Ui_effect.t(unit),
      cursor: Cursor.cursor(Editors.Update.t),
    ) => {
  let applicable_projectors =
    might_project(cursor) ? applicable_projector_strings(cursor) : [];
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
        inject(SetIndicated(ProjectorView.of_name(name)))
      ),
      Attr.string_property("value", value),
    ],
    applicable_projector_strings |> List.map(n => option([text(n)])),
  );
};

let toggle_view = (~inject, cursor: Cursor.cursor(Editors.Update.t)) =>
  toggle_view(
    ~inject,
    cursor.info,
    id(cursor.editor),
    kind(cursor.editor) != None,
    might_project(cursor),
  );

let view = (~inject, cursor: Cursor.cursor(Editors.Update.t)) => {
  div(
    ~attrs=[Attr.id("projectors")],
    [select_view(~inject, cursor)] @ [toggle_view(~inject, cursor)],
  );
};

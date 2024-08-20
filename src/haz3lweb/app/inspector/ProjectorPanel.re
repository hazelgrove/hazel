open Haz3lcore;
open Virtual_dom.Vdom;
open Node;
open Projector;
open Util.OptUtil.Syntax;
open Util.Web;

/* The projector selection panel on the right of the bottom bar */
let option_view = (name, n) =>
  option(
    ~attrs=n == name ? [Attr.create("selected", "selected")] : [],
    [text(n)],
  );

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
      | InfoExp(_)
      | InfoPat(_) => [(Info: Base.kind)]
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
  let+ (_, p) = Editor.Model.indicated_projector(editor);
  p.kind;
};

let id = (editor: option(Editor.t)) => {
  {
    let* editor = editor;
    let+ (id, _) = Editor.Model.indicated_projector(editor);
    id;
  }
  |> Option.value(~default=Id.invalid);
};

let might_project: option(Editor.t) => bool =
  fun
  | None => false
  | Some(editor) =>
    switch (Indicated.piece''(editor.state.zipper)) {
    | None => false
    | Some((p, _, _)) => minimum_projection_condition(p)
    };

let currently_selected = editor =>
  option_view(
    switch (kind(editor)) {
    | None => "Fold"
    | Some(k) => ProjectorView.name(k)
    },
  );

let view = (~inject, cursor: Cursor.cursor(Editors.Update.t)) => {
  let applicable_projectors = applicable_projectors(cursor.info);
  let should_show =
    might_project(cursor.editor) && applicable_projectors != [];
  let select_view =
    Node.select(
      ~attrs=[
        Attr.on_change((_, name) =>
          inject(Action.SetIndicated(ProjectorView.of_name(name)))
        ),
      ],
      (might_project(cursor.editor) ? applicable_projectors : [])
      |> List.map(ProjectorView.name)
      |> List.map(currently_selected(cursor.editor)),
    );
  let toggle_view =
    toggle_view(
      ~inject,
      cursor.info,
      id(cursor.editor),
      kind(cursor.editor) != None,
      might_project(cursor.editor),
    );
  div(
    ~attrs=[Attr.id("projectors")],
    (should_show ? [select_view] : []) @ [toggle_view],
  );
};

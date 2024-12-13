open Util;
open Haz3lcore;
open Web;

module Model = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type component = {
    id: Id.t,
    parent: option(Id.t),
    editor: Editor.t,
    kind: option(Base.kind),
    model: string,
  };
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {
    components: list(component),
    root_id: Id.t,
    statics: CachedStatics.t,
  };

  let mk = editor => {
    let id = Id.mk();
    {
      components: [{id, parent: None, editor, kind: None, model: ""}],
      root_id: id,
      statics: CachedStatics.empty,
    };
  };

  let get_component = (id, model) =>
    List.find(c => c.id == id, model.components);

  let set_component = (id, component, model) => {
    {
      ...model,
      components: List.map(c => c.id == id ? component : c, model.components),
    };
  };

  let add_component = (component, model) => {
    {...model, components: [component, ...model.components]};
  };
};

module Update = {
  open Updated;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type management =
    | Project({
        parent: Id.t,
        kind: Base.kind,
      }) /* Project syntax at caret of component with id */
    | Remove({child: Id.t})
    | Focus({
        child: Id.t,
        direction: option(Direction.t),
      })
    | Escape({
        child: Id.t,
        direction: Direction.t,
      });
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t =
    | Perform(Id.t, Action.t)
    | SetSyntax(Id.t, Piece.t)
    | SetModel(Id.t, string)
    | Manage(management);

  let perform = (~settings, action, model: Model.t, editor: Editor.t) =>
    Editor.Update.update(~settings, action, model.statics, editor)
    |> (
      fun
      | Ok(editor) => editor
      | Error(err) => raise(Action.Failure.Exception(err))
    )
    |> Updated.return(
         ~is_edit=Action.is_edit(action),
         ~recalculate=true,
         ~scroll_active={
           switch (action) {
           | Move(_)
           | Jump(_)
           | Select(Resize(_) | Term(_) | Smart(_) | Tile(_))
           | Destruct(_)
           | Insert(_)
           | Pick_up
           | Put_down
           | RotateBackpack
           | MoveToBackpackTarget(_)
           | Buffer(Set(_) | Accept | Clear)
           | Paste(_)
           | Copy
           | Cut
           | Reparse => true
           | Project(_)
           | Unselect(_)
           | Select(All) => false
           };
         },
       );

  let update = (~settings, action: t, model: Model.t): Updated.t(Model.t) => {
    switch (action) {
    | Perform(id, action) =>
      let component = Model.get_component(id, model);
      let* editor = perform(~settings, action, model, component.editor);
      let new_component = {...component, editor};
      Model.set_component(id, new_component, model);
    | SetModel(id, projector_model) =>
      let component = Model.get_component(id, model);
      let new_component = {...component, model: projector_model};
      Model.set_component(id, new_component, model) |> Updated.return;
    | Manage(Project({parent, kind})) =>
      let parent_component = Model.get_component(parent, model);
      let parent_z = parent_component.editor.state.zipper;
      switch (Indicated.for_index(parent_z)) {
      | None => raise(Action.Failure.Exception(Cant_project))
      | Some((piece, _d, _rel)) =>
        let (module P) = Projector.to_module(kind);
        let* parent_editor =
          perform(
            ~settings,
            Action.Project(SetIndicated(kind)),
            model,
            parent_component.editor,
          );
        let parent_component = {...parent_component, editor: parent_editor};
        let new_component: Model.component = {
          id: Piece.id(piece),
          parent: Some(parent),
          editor: [piece] |> Zipper.unzip |> Editor.Model.mk,
          kind: Some(kind),
          model: P.init,
        };
        model
        |> Model.add_component(new_component)
        |> Model.set_component(parent, parent_component);
      };
    | _ => model |> Updated.return_quiet
    };
  };
};

module Focus = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {component: Id.t};
};

module View = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type event =
    | Focus(Focus.t);

  let rec view_component =
          (
            ~globals: Globals.t,
            ~signal: event => Ui_effect.t(unit),
            ~inject: Update.t => Ui_effect.t(unit),
            ~selected: option(Focus.t),
            model: Model.t,
            component: Model.component,
          )
          : Node.t => {
    let f: Id.t => Node.t =
      id => {
        view_component(
          ~globals,
          ~signal,
          ~inject,
          ~selected,
          model,
          Model.get_component(id, model),
        );
      };
    switch (component.kind) {
    | Some(x) =>
      let (module P) = Projector.to_module(x);
      P.view(
        component.model,
        ~parent=
          fun
          | Remove => inject(Update.Manage(Remove({child: component.id})))
          | Escape(dir) =>
            inject(
              Update.Manage(Escape({child: component.id, direction: dir})),
            )
          | SetSyntax(syntax) =>
            inject(Update.SetSyntax(component.id, syntax)),
        ~local=x => inject(Update.SetModel(component.id, x)),
        ~info={
          id: component.id,
          ci: Id.Map.find_opt(component.id, model.statics.info_map),
          syntax: component.editor.state.zipper |> Zipper.zip |> List.hd,
        },
      );
    | None =>
      let edit_decos = {
        module Deco =
          Deco.Deco({
            let editor = component.editor;
            let globals = globals;
            let statics = model.statics;
          });
        Deco.editor(
          component.editor.state.zipper,
          selected == Some({component: component.id}),
        );
      };
      let view_wrapper =
          (
            ~font_metrics: FontMetrics.t,
            ~measurement: Measured.measurement,
            ~info: ProjectorBase.info,
            ~indication: option(Direction.t),
            ~selected: bool,
            p: Base.projector,
            view: Node.t,
          ) => {
        open Virtual_dom.Vdom;
        let shape = Projector.shape(p, info);
        Node.div(
          ~attrs=[
            Attr.classes(
              ["projector", ProjectorView.name(p.kind)]
              @ ProjectorView.status(indication, selected, shape),
            ),
            Attr.on_mousedown(_ => signal(Focus({component: info.id}))),
            DecUtil.abs_style(measurement, ~font_metrics),
          ],
          [
            view,
            ProjectorView.backing_deco(~font_metrics, ~measurement, ~shape),
          ],
        );
      };
      let setup_view =
          (
            id: Id.t,
            ~cached_statics: CachedStatics.t,
            ~cached_syntax: Editor.CachedSyntax.t,
            ~font_metrics,
            ~indication: option(Direction.t),
          )
          : option(Node.t) => {
        open OptUtil.Syntax;
        let* p = Id.Map.find_opt(id, cached_syntax.projectors);
        let* syntax = Some(p.syntax);
        let ci = Id.Map.find_opt(id, cached_statics.info_map);
        let info = ProjectorBase.{id, ci, syntax};
        let+ measurement = Measured.find_pr_opt(p, cached_syntax.measured);
        // let (module P) = Projector.to_module(p.kind);
        view_wrapper(
          ~font_metrics,
          ~measurement,
          ~indication,
          ~info,
          ~selected=List.mem(id, cached_syntax.selection_ids),
          p,
          f(id),
        );
      };
      let all =
          (
            z: Zipper.t,
            ~cached_statics: CachedStatics.t,
            ~cached_syntax: Editor.CachedSyntax.t,
            ~font_metrics,
          ) => {
        // print_endline(
        //   "cardinal: "
        //   ++ (meta.projected.projectors |> Id.Map.cardinal |> string_of_int),
        // );
        div_c(
          "projectors",
          List.filter_map(
            ((id, _)) => {
              let indication = ProjectorView.indication(z, id);
              setup_view(
                id,
                ~cached_statics,
                ~cached_syntax,
                ~font_metrics,
                ~indication,
              );
            },
            Id.Map.bindings(cached_syntax.projectors) |> List.rev,
          ),
        );
      };
      let projectors =
        all(
          component.editor.state.zipper,
          ~cached_statics=model.statics,
          ~cached_syntax=component.editor.syntax,
          ~font_metrics=globals.font_metrics,
        );
      let overlays = edit_decos @ [projectors];
      let code_view =
        CodeWithStatics.View.view(
          ~globals,
          ~overlays,
          ~sort=Exp,
          {editor: component.editor, statics: model.statics},
        );
      let mousedown_overlay =
        selected == Some({component: component.id}) && globals.mousedown
          ? [
            CodeEditable.View.mousedown_overlay(~globals, ~inject=x =>
              inject(Perform(component.id, x))
            ),
          ]
          : [];
      let on_mousedown =
        CodeEditable.View.mousedown_handler(
          ~globals,
          ~signal=(
            fun
            | MakeActive => Focus({component: component.id}) |> signal
          ),
          ~inject=x =>
          inject(Perform(component.id, x))
        );
      Node.div(
        ~attrs=[
          Virtual_dom.Vdom.Attr.classes(
            ["cell-item", "code-editor"]
            @ (
              selected == Some({component: component.id}) ? ["selected"] : []
            ),
          ),
          Virtual_dom.Vdom.Attr.on_mousedown(on_mousedown),
        ],
        mousedown_overlay @ [code_view],
      );
    };
  };

  let view =
      (
        ~globals: Globals.t,
        ~signal: event => Ui_effect.t(unit),
        ~inject: Update.t => Ui_effect.t(unit),
        ~selected: option(Focus.t),
        model: Model.t,
      ) =>
    view_component(
      ~globals: Globals.t,
      ~signal: event => Ui_effect.t(unit),
      ~inject: Update.t => Ui_effect.t(unit),
      ~selected: option(Focus.t),
      model,
      Model.get_component(model.root_id, model),
    );
};

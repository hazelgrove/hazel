open Util;
open Haz3lcore;

module Model = EditorManagerModel;

module Update = {
  open Updated;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type management =
    | Project({
        parent: Id.t,
        kind: ProjectorCore.Kind.t,
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
    | SetSyntax(Id.t, Segment.t)
    | SetModel(Id.t, string)
    | Undo(Id.t)
    | Redo(Id.t)
    | TAB(Id.t)
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
        let (module P) = ProjectorInit.to_module(kind);
        let new_id = Id.mk();
        let* parent_editor =
          perform(
            ~settings,
            Action.Project(SetIndicated(Specific(kind), new_id)),
            model,
            parent_component.editor,
          );
        let parent_component = {...parent_component, editor: parent_editor};
        let new_component: Model.component = {
          id: new_id,
          parent: Some(parent),
          //TODO(andrew): empty map below
          editor: [piece] |> Zipper.unzip |> Editor.Model.mk,
          kind: Some(kind),
          model: P.init,
        };
        let res =
          model
          |> Model.add_component(new_component)
          |> Model.set_component(parent, parent_component);
        print_endline("new component added. id: " ++ Id.str8(new_id));
        res;
      };
    | SetSyntax(id, syntax) =>
      //TODO(andrew): empty map below
      let new_editor = Zipper.unzip(syntax) |> Editor.Model.mk;
      let _ = print_endline("SETTING SYNTAX");
      let _ = print_endline("SETTING ID > " ++ Id.to_string(id));
      let _ =
        print_endline(
          "AVAILABLE IDs > "
          ++ (
            model.components
            |> List.map((c: Model.component) => Id.to_string(c.id))
            |> String.concat(", ")
          ),
        );
      model
      |> Model.set_component(
           id,
           {...Model.get_component(id, model), editor: new_editor},
         )
      |> Updated.return;
    | _ => model |> Updated.return_quiet // TODO: Delete
    };
  };

  let rec mk_term = (id: Id.t, model: Model.t) => {
    let component = Model.get_component(id, model);
    let from_segment = (segment: Segment.t) => {
      MakeTerm.go_em(~of_projector=mk_term(_, model), segment);
    };
    switch (component.kind) {
    | Some(kind) =>
      print_endline("MK_TERM in EditorManager");
      let (module P) = ProjectorInit.to_module(kind);
      P.mk_term(
        ~id,
        ~from_segment,
        ~segment=[component |> Model.piece_of_component],
      );
    | None =>
      print_endline("NONE in EditorManager");
      from_segment([component |> Model.piece_of_component]);
    };
  };

  let assemble = Model.assemble;
  //TODO(andrew):
  let sdfsdfds = components =>
    List.filter_map(EditorManagerModel.component_to_trad, components)
    |> List.map((g: ProjectorBase.trad) => (g.id, g))
    |> Id.Map.of_list;
  let calculate_syntax_cache =
      (
        ~settings,
        ~is_edited,
        statics: CachedStatics.t,
        dynamics,
        model: Model.t,
      ) => {
    ...model,
    components:
      List.map(
        (c: Model.component): Model.component =>
          {
            ...c,
            editor:
              Editor.Update.calculate(
                ~settings,
                ~is_edited,
                sdfsdfds(model.components),
                statics,
                dynamics,
                c.editor,
              ),
          },
        model.components,
      ),
  };

  let calculate = (~settings, ~is_edited, ~dynamics, ~stitch, model: Model.t) => {
    // let segment = assemble(model);
    // print_endline("assembled segment:" ++ Segment.show(segment));
    let term =
      switch (mk_term(model.root_id, model)) {
      | Exp(term) => term
      | t =>
        print_endline("TERM: " ++ Any.show(t));
        failwith("Not root?");
      };
    let statics = CachedStatics.init_from_term(~settings, term);
    let components =
      calculate_syntax_cache(~settings, ~is_edited, statics, dynamics, model).
        components;
    Model.{statics, components, root_id: model.root_id, dynamics};
  };
};

module Focus = {
  open Cursor;

  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {component: Id.t};

  let get_cursor_info = (~selection: t, model: Model.t): cursor(Update.t) => {
    let component = Model.get_component(selection.component, model);
    let info =
      Indicated.ci_of(component.editor.state.zipper, model.statics.info_map);
    {
      info,
      indicated_piece:
        Indicated.piece''(component.editor.state.zipper)
        |> Option.map(((p, _, _)) => p),
      selection: Some(component.editor.state.zipper.selection.content),
      selected_text:
        Some(
          () => Printer.to_string_selection(component.editor.state.zipper),
        ),
      editor: Some(component.editor),
      editor_read_only: false,
      editor_action: x => Some(Update.Perform(selection.component, x)),
      remove_projector:
        Option.map(
          x => Update.Manage(Remove({child: Info.id_of(x)})),
          info,
        ),
      add_projector: kind =>
        Some(Update.Manage(Project({parent: selection.component, kind}))),
      undo_action: Some(Undo(selection.component)),
      redo_action: Some(Redo(selection.component)),
      projectors: Some(component.editor.syntax.projectors), //TODO(andrew): make sure this routes
      of_projector: Some(id => Update.mk_term(id, model)),
    };
  };

  let handle_key_event =
      (~selection: t, ~event: Key.t, _: Model.t): option(Update.t) =>
    switch (event) {
    | {
        key: D("Z" | "z"),
        sys: Mac,
        shift: Down,
        meta: Down,
        ctrl: Up,
        alt: Up,
      }
    | {
        key: D("Z" | "z"),
        sys: PC,
        shift: Down,
        meta: Up,
        ctrl: Down,
        alt: Up,
      } =>
      Some(Update.Redo(selection.component))
    | {key: D("Z" | "z"), sys: Mac, shift: Up, meta: Down, ctrl: Up, alt: Up}
    | {key: D("Z" | "z"), sys: PC, shift: Up, meta: Up, ctrl: Down, alt: Up} =>
      Some(Update.Undo(selection.component))
    // TODO: Fix toggle
    // | {key: D("f"), sys: PC, shift: Up, meta: Up, ctrl: Up, alt: Down} =>
    //   Some(Project(ToggleIndicated(Fold, id)))
    // | {key: D("ƒ"), sys: Mac, shift: Up, meta: Up, ctrl: Up, alt: Down} =>
    //   /* Curly ƒ is what holding option turns f into on Mac */
    // Some(Project(ToggleIndicated(Fold, id)));
    | k =>
      Keyboard.handle_key_event(k)
      |> Option.map(x => Update.Perform(selection.component, x))
    };

  let jump_to_tile = (tile, model: Model.t): option((Update.t, t)) => {
    switch (
      List.find_opt(
        (c: Model.component) => Editor.Model.has_tile_id(c.editor, tile),
        model.components,
      )
    ) {
    | Some(component) =>
      Some((
        Update.Perform(component.id, Jump(TileId(tile))),
        {component: component.id},
      ))
    | None => None
    };
  };

  let default_selection = (model: Model.t): t => {
    {component: model.root_id};
  };
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
            ~overlays,
            ~dynamics: Dynamics.Map.t,
            model: Model.t,
            component: Model.component,
          )
          : ProjectorBase.View.t => {
    let projector_view: Id.t => ProjectorBase.View.t =
      id => {
        view_component(
          ~globals,
          ~signal,
          ~inject,
          ~selected,
          ~overlays=[],
          ~dynamics,
          model,
          Model.get_component(id, model),
        );
      };
    switch (component.kind) {
    | Some(kind) =>
      let syntax = Model.piece_of_component(component);
      let (module P) = ProjectorInit.to_module(kind);
      P.view(
        component.model,
        ~local=x => inject(Update.SetModel(component.id, x)),
        ProjectorInfo.mk_info(
          component.id,
          syntax,
          ~statics=model.statics.info_map,
          ~dynamics,
        ),
        ~parent=
          fun
          | Remove => inject(Update.Manage(Remove({child: component.id})))
          | Escape(dir) =>
            inject(
              Update.Manage(Escape({child: component.id, direction: dir})),
            )
          | SetSyntax(syntax) =>
            inject(Update.SetSyntax(component.id, syntax)),
        ~view_seg=
          (~background=?) =>
            ProjectorView.simple_code(~background?, globals.font_metrics),
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
      let projectors =
        ProjectorView.all(
          ~focus=signal(Focus({component: component.id})),
          projector_view,
          globals.font_metrics,
          ProjectorView.Model.mk(
            component.editor.syntax.projectors,
            component.editor.syntax.measured,
            component.editor.syntax.selection_ids,
            Indicated.piece(component.editor.state.zipper),
            model.statics.info_map,
            dynamics,
            selected == Some({component: component.id}),
          ),
        );
      let _ = CodeEditable.View.view;
      let overlays = overlays @ edit_decos @ projectors;
      let code_view =
        CodeWithStatics.View.view(
          ~globals,
          ~overlays,
          ~sort=Exp,
          {editor: component.editor, statics: model.statics, dynamics},
        );
      //TODO: weird wrapper
      Tylr(
        Virtual_dom.Vdom.Node.div(
          ~attrs=[
            Virtual_dom.Vdom.Attr.classes(
              ["cell-item", "code-editor"]
              @ (
                selected == Some({component: component.id})
                  ? ["selected"] : []
              ),
            ),
          ],
          [code_view],
        ),
      );
    };
  };

  let view =
      (
        ~globals: Globals.t,
        ~signal: event => Ui_effect.t(unit),
        ~inject: Update.t => Ui_effect.t(unit),
        ~selected: option(Focus.t),
        ~overlays,
        model: Model.t,
      ) =>
    view_component(
      ~globals: Globals.t,
      ~signal: event => Ui_effect.t(unit),
      ~inject: Update.t => Ui_effect.t(unit),
      ~selected: option(Focus.t),
      ~overlays,
      model,
      Model.get_component(model.root_id, model),
    );
};

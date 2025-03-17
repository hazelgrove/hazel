open Haz3lcore;
open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type component = {
  id: Id.t,
  parent: option(Id.t),
  editor: Editor.t,
  kind: option(ProjectorCore.Kind.t),
  model: string,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  components: list(component),
  root_id: Id.t,
  statics: CachedStatics.t,
  dynamics: Dynamics.Map.t,
};

let piece_of_component = (component: component): option(Piece.t) =>
  switch (component.editor.state.zipper |> Zipper.zip) {
  | [hd] => Some(hd)
  | seg =>
    //TODO: make less representable
    print_endline("piece_of_component: " ++ Segment.show(seg));

    prerr_endline("Assumption: zipper zips to singleton segment");
    None;
  };
let component_to_trad = (component: component): option(ProjectorBase.trad) =>
  //TODO(andrew): make the hurting stapp
  switch (component.kind) {
  | Some(kind) =>
    open OptUtil.Syntax;
    let+ piece = piece_of_component(component);
    ProjectorBase.{
      id: component.id,
      kind,
      model: component.model,
      syntax: piece,
    };
  | None => None
  };

let mk = editor => {
  let id = Id.mk();
  {
    components: [{id, parent: None, editor, kind: None, model: ""}],
    root_id: id,
    statics: CachedStatics.empty,
    dynamics: Dynamics.Map.empty,
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

let get_root_editor = model => {
  let root = List.find(c => c.id == model.root_id, model.components);
  root.editor;
};

[@deriving (show({with_path: false}), sexp, yojson)]
type persistent_component = {
  id: Id.t,
  parent: option(Id.t),
  editor: Editor.Model.persistent,
  kind: option(ProjectorCore.Kind.t),
  model: string,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type persistent = {
  components: list(persistent_component),
  root_id: Id.t,
};

let persist = (model: t) => {
  let components =
    List.map(
      (c: component) =>
        {
          id: c.id,
          parent: c.parent,
          editor: Editor.Model.persist(c.editor),
          kind: c.kind,
          model: c.model,
        },
      model.components,
    );
  {components, root_id: model.root_id};
};

let unpersist = (data: persistent): t => {
  let components =
    List.map(
      (c: persistent_component): component =>
        {
          id: c.id,
          parent: c.parent,
          editor: Editor.Model.unpersist(c.editor),
          kind: c.kind,
          model: c.model,
        },
      data.components,
    );
  {
    components,
    root_id: data.root_id,
    statics: CachedStatics.empty,
    dynamics: Dynamics.Map.empty,
  };
};

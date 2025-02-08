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

let segment_to_piece = (seg: Segment.t): Piece.t =>
  //TODO(andrew):............
  switch (seg) {
  | [] => failwith("EditorManager.Update.of_segment: empty segment")
  | [p] => p
  | [p, ..._] =>
    let sort = p |> Piece.sort |> fst;
    Piece.mk_tile(
      Form.mk(Form.ii, ["(", ")"], Mold.mk_op(sort, [sort])),
      [seg],
    );
  };

let component_to_piece = (component: component): Piece.t => {
  let editor = component.editor;
  let seg =
    Zipper.smart_seg(
      ~dump_backpack=true,
      ~erase_buffer=true,
      editor.state.zipper,
    );
  segment_to_piece(seg);
};

let assemble = (model: t): Segment.t => {
  let swap_out = (go, piece: Piece.t): Segment.t => {
    switch (piece) {
    | Projector(pr) => Piece.unparenthesize(go(pr.id))
    | _ => [piece]
    };
  };
  let rec go = (id: Id.t): Piece.t => {
    let seg = get_component(id, model) |> component_to_piece;
    ZipperBase.MapPiece.of_piece(swap_out(go), seg);
  };
  [go(model.root_id)];
};

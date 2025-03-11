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

let piece_of_component = (component: component): Piece.t =>
  switch (component.editor.state.zipper |> Zipper.zip) {
  | [hd] => hd
  | seg =>
    print_endline("piece_of_component: " ++ Segment.show(seg));
    //TODO: make less representable
    failwith("Assumption: zipper zips to singleton segment");
  };
let component_to_trad = (component: component): option(ProjectorBase.trad) =>
  switch (component.kind) {
  | Some(kind) =>
    Some({
      id: component.id,
      kind,
      model: component.model,
      syntax: piece_of_component(component),
    })
  | None =>
    prerr_endline("EditorManager.component_to_trad: None TODO");
    None;
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
  print_endline("assemble");
  let swap_out = (_go, piece: Piece.t): Segment.t => {
    print_endline("swap_out");
    switch (piece) {
    | Projector(pr) =>
      let component = get_component(pr.id, model);
      switch (component.kind) {
      | Some(kind) =>
        let (module P) = ProjectorInit.to_module(kind);
        let _should_instrument = P.dynamics;
        //TODO: this logic will leave it in for the printer; divide this fn
        // into a total stripper and partial stripper
        //TODO: make this actually work (instrument with... something)
        //should_instrument ? Piece.unparenthesize(P.go(pr.id)) : piece;
        let piece = component_to_piece(component);
        print_endline("piece: " ++ Piece.show(piece));
        //TODO(andrew): needs to recurse for general case...
        //Piece.unparenthesize(go(pr.id));
        //Piece.unparenthesize(piece);
        /* For now, creating parentheses with projector_id so
         * it shows up in the cursor inspector... need to consider
         * approaches here */
        let sort = Piece.sort(piece) |> fst;
        [
          Tile({
            id: component.id,
            label: ["(", ")"],
            mold: Mold.mk_op(sort, [sort]),
            shards: List.mapi((i, _) => i, ["(", ")"]),
            children: [Piece.unparenthesize(piece)],
          }),
        ];
      | None => failwith("EditorManager.assemble: None TODO")
      };
    | _ =>
      print_endline("piece not proj: " ++ Piece.show(piece));
      [piece];
    };
  };
  let rec go = (id: Id.t): Segment.t => {
    let seg = get_component(id, model) |> component_to_piece;
    print_endline("seg: " ++ Piece.show(seg));
    ZipperBase.MapPiece.of_piece(swap_out(go), seg);
  };
  go(model.root_id);
};

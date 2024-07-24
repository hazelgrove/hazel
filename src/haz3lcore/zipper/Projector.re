open ProjNew;

let seg_for_maketerm = (p: Base.projector): Base.segment => [p.syntax];

/* After adding a new projector module, add it here so that
 * it can be instantiated. The first-class module created by
 * this function must be reified whenever projector methods
 * are to be called; see `shape` below for an example */
let to_module = (kind: Base.kind): (module Cooked) =>
  switch (kind) {
  | Fold => (module Cook(FoldCore.M))
  | Info => (module Cook(InfoCore.M))
  | Slider => (module Cook(SliderCore.M))
  | SliderF => (module Cook(SliderFCore.M))
  | Checkbox => (module Cook(CheckboxCore.M))
  | TextArea => (module Cook(TextAreaCore.M))
  };

let shape = (p: Base.projector, info: info): shape => {
  let (module P) = to_module(p.kind);
  P.placeholder(p.model, info);
};

/* A projector is replaced by a placeholder in the underlying
 * editor for view purposes. This projector is an all-whitespace
 * monotile. Currently there is no explicit notion of placeholders
 * in the zipper; a tile consisting of any number of whitespaces
 * is considered a placeholder. This could be made more principled.
 * Note that a placeholder retains the UUID of the underlying. */
let placeholder_str = ({id, syntax, _} as p: Base.projector): string =>
  //TODO(andrew): pipe InfoMap to Measured/Code/Deco so can get it here i guess
  switch (shape(p, {id, syntax, ci: None})) {
  | Inline(width) => String.make(width, ' ')
  | Block({row, col}) => String.make(row - 1, '\n') ++ String.make(col, ' ')
  };

/* Currently projection is limited to convex pieces */
let minimum_projection_condition = (syntax: syntax): bool =>
  Piece.is_convex(syntax);

/* Add a new projector, gated on the predicated on the syntax */
let create = (kind: Base.kind, syntax: syntax): option((Base.kind, string)) => {
  let (module P) = to_module(kind);
  P.can_project(syntax) && minimum_projection_condition(syntax)
    ? Some((kind, P.init)) : None;
};

/* Returns the projector at the caret, if any */
let indicated = (z: ZipperBase.t) => {
  open Util.OptUtil.Syntax;
  let* id = Indicated.index(z);
  let* (p, _, _) = Indicated.piece(z);
  let+ projector =
    switch (p) {
    | Projector(pr) => Some(pr)
    | _ => None
    };
  (id, projector);
};

module MapPiece = {
  type updater = Piece.t => Piece.t;

  let rec of_segment = (f: updater, seg: Segment.t): Segment.t => {
    seg |> List.map(p => f(p)) |> List.map(of_piece(f));
  }
  and of_piece = (f: updater, piece: Piece.t): Piece.t => {
    switch (piece) {
    | Tile(t) => Tile(of_tile(f, t))
    | Grout(_)
    | Projector(_)
    | Secondary(_) => piece
    };
  }
  and of_tile = (f: updater, t: Tile.t): Tile.t => {
    {...t, children: List.map(of_segment(f), t.children)};
  };

  let of_siblings = (f: updater, sibs: Siblings.t): Siblings.t => (
    of_segment(f, fst(sibs)),
    of_segment(f, snd(sibs)),
  );

  let of_ancestor = (f: updater, ancestor: Ancestor.t): Ancestor.t => {
    {
      ...ancestor,
      children: (
        List.map(of_segment(f), fst(ancestor.children)),
        List.map(of_segment(f), snd(ancestor.children)),
      ),
    };
  };

  let of_generation =
      (f: updater, generation: Ancestors.generation): Ancestors.generation => (
    of_ancestor(f, fst(generation)),
    of_siblings(f, snd(generation)),
  );

  let of_ancestors = (f: updater, ancestors: Ancestors.t): Ancestors.t =>
    List.map(of_generation(f), ancestors);

  let of_selection = (f: updater, selection: Selection.t): Selection.t => {
    {...selection, content: of_segment(f, selection.content)};
  };

  let go = (f: updater, z: ZipperBase.t): ZipperBase.t => {
    ...z,
    selection: of_selection(f, z.selection),
    relatives: {
      ancestors: of_ancestors(f, z.relatives.ancestors),
      siblings: of_siblings(f, z.relatives.siblings),
    },
  };
};

//TODO(andrew): refine or remove
module SyntaxMap = {
  let projectors_map: ref(Id.Map.t(Base.projector)) = ref(Id.Map.empty);

  let placehold = (syntax: syntax) => {
    let id = Piece.id(syntax);
    switch (syntax) {
    | Projector(pr) =>
      projectors_map := Id.Map.add(id, pr, projectors_map^);
      syntax;
    | _ => syntax
    };
  };

  let go = (z: ZipperBase.t): Id.Map.t(Base.projector) => {
    projectors_map := Id.Map.empty;
    MapPiece.go(placehold, z) |> ignore;
    projectors_map^;
  };
};

/* Updates the underlying piece of syntax for a projector */
module Update = {
  let sib_has_id = (get, z: ZipperBase.t, id: Id.t): bool => {
    switch (z.relatives.siblings |> get) {
    | Some(l) => Piece.id(l) == id
    | _ => false
    };
  };

  let left_sib_has_id = sib_has_id(Siblings.left_neighbor);
  let right_sib_has_id = sib_has_id(Siblings.right_neighbor);

  let update_left_sib = (f: syntax => syntax, z: ZipperBase.t) => {
    let (l, r) = z.relatives.siblings;
    let sibs = (List.map(f, l), List.map(f, r));
    z |> ZipperBase.put_siblings(sibs);
  };

  let update_right_sib = (f: syntax => syntax, z: ZipperBase.t) => {
    let sibs =
      switch (z.relatives.siblings) {
      | (l, [hd, ...tl]) => (l, [f(hd), ...tl])
      | sibs => sibs
      };
    z |> ZipperBase.put_siblings(sibs);
  };

  let eff_update =
      (f: Piece.t => Piece.t, id: Id.t, z: ZipperBase.t): ZipperBase.t =>
    /* This applies the function to the piece in the zipper having id id, and
     * then replaces the id of the resulting piece with the idea of the old
     * piece, ensuring that the root id remains stable. This function assumes
     * the cursor is not inside the piece to be updated. This is optimized to
     * be O(1) when the piece is directly to the left or right of the cursor,
     * otherwise it is O(|zipper|) */
    if (left_sib_has_id(z, id)) {
      print_endline("Projector.Update.update: left_sib_has_id");
      update_left_sib(f, z);
    } else if (right_sib_has_id(z, id)) {
      print_endline("Projector.Update.update: right_sib_has_id");
      update_right_sib(f, z);
    } else {
      print_endline("Projector.Update.update: MapPiece.go");
      MapPiece.go(f, z);
    };

  let update_piece =
      (f: Base.projector => Base.projector, id: Id.t, syntax: syntax) =>
    switch (syntax) {
    | Projector(pr) when pr.id == id => Base.Projector(f(pr))
    | x => x
    };

  let init = (kind, id, syntax): option(Base.projector) => {
    let (module P) = to_module(kind);
    P.can_project(syntax) ? Some({id, kind, model: P.init, syntax}) : None;
  };

  let add_projector = (kind: Base.kind, id: Id.t, syntax: syntax) =>
    //TODO(andrew): same id still? make sure these aren't creating dupes somewhere
    switch (syntax) {
    | Projector(pr) when Piece.id(syntax) == id =>
      // Or Replace
      switch (init(kind, id, pr.syntax)) {
      | None => syntax
      | Some(pr) => Base.Projector(pr)
      }
    | syntax when Piece.id(syntax) == id =>
      switch (init(kind, id, syntax)) {
      | None => syntax
      | Some(pr) => Base.Projector(pr)
      }
    | x => x
    };

  let add_or_remove_projector = (kind: Base.kind, id: Id.t, syntax: syntax) =>
    //TODO(andrew): same id still? make sure these aren't creating dupes somewhere
    switch (syntax) {
    | Projector(pr) when Piece.id(syntax) == id => pr.syntax
    | syntax when Piece.id(syntax) == id =>
      switch (init(kind, id, syntax)) {
      | None => syntax
      | Some(pr) => Base.Projector(pr)
      }
    | x => x
    };

  let remove_projector = (id: Id.t, syntax: syntax) =>
    //TODO(andrew): same id still? make sure these aren't creating dupes somewhere
    switch (syntax) {
    | Projector(pr) when pr.id == id => pr.syntax
    | x => x
    };

  let update =
      (f: Base.projector => Base.projector, id: Id.t, z: ZipperBase.t)
      : ZipperBase.t =>
    eff_update(update_piece(f, id), id, z);

  let add = (k: Base.kind, id: Id.t, z: ZipperBase.t): ZipperBase.t =>
    eff_update(add_projector(k, id), id, z);
  let add_or_remove = (k: Base.kind, id: Id.t, z: ZipperBase.t): ZipperBase.t =>
    eff_update(add_or_remove_projector(k, id), id, z);

  let remove = (id: Id.t, z: ZipperBase.t): ZipperBase.t =>
    eff_update(remove_projector(id), id, z);

  let remove_all_projectors = (syntax: syntax) =>
    //TODO(andrew): avoid the need to remove all
    switch (syntax) {
    | Projector(pr) => pr.syntax
    | x => x
    };

  let remove_all = (z: ZipperBase.t): ZipperBase.t =>
    MapPiece.go(remove_all_projectors, z);
};

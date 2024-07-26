open ProjectorBase;

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
let placeholder = (p: Base.projector, ci: option(Info.t)): string =>
  //TODO(andrew): pipe InfoMap to Measured/Code/Deco so can get it here i guess
  switch (shape(p, {id: p.id, syntax: p.syntax, ci})) {
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

/* Updates the underlying piece of syntax for a projector */
module Update = {
  let update_piece =
      (f: Base.projector => Base.projector, id: Id.t, syntax: syntax) =>
    switch (syntax) {
    | Projector(pr) when pr.id == id => Base.Projector(f(pr))
    | x => x
    };

  let init = (kind: t, syntax: syntax): syntax => {
    /* We set the projector id equal to the Piece id for convienence
     * including cursor-info association. We maintain this invariant
     * when we update a projector's contained syntax */
    let (module P) = to_module(kind);
    switch (P.can_project(syntax)) {
    | false => syntax
    | true => Projector({id: Piece.id(syntax), kind, model: P.init, syntax})
    };
  };

  let add_projector = (kind: Base.kind, id: Id.t, syntax: syntax) =>
    switch (syntax) {
    | Projector(pr) when Piece.id(syntax) == id => init(kind, pr.syntax)
    | syntax when Piece.id(syntax) == id => init(kind, syntax)
    | x => x
    };

  let remove_projector = (id: Id.t, syntax: syntax) =>
    switch (syntax) {
    | Projector(pr) when pr.id == id => pr.syntax
    | x => x
    };

  let add_or_remove_projector = (kind: Base.kind, id: Id.t, syntax: syntax) =>
    switch (syntax) {
    | Projector(pr) when Piece.id(syntax) == id => pr.syntax
    | syntax when Piece.id(syntax) == id => init(kind, syntax)
    | x => x
    };

  let remove_any_projector = (syntax: syntax) =>
    switch (syntax) {
    | Projector(pr) => pr.syntax
    | x => x
    };

  let update =
      (f: Base.projector => Base.projector, id: Id.t, z: ZipperBase.t)
      : ZipperBase.t =>
    ZipperBase.MapPiece.fast_local(update_piece(f, id), id, z);

  let add = (k: Base.kind, id: Id.t, z: ZipperBase.t): ZipperBase.t =>
    ZipperBase.MapPiece.fast_local(add_projector(k, id), id, z);

  let add_or_remove = (k: Base.kind, id: Id.t, z: ZipperBase.t): ZipperBase.t =>
    ZipperBase.MapPiece.fast_local(add_or_remove_projector(k, id), id, z);

  let remove = (id: Id.t, z: ZipperBase.t): ZipperBase.t =>
    ZipperBase.MapPiece.fast_local(remove_projector(id), id, z);

  let remove_all = (z: ZipperBase.t): ZipperBase.t =>
    ZipperBase.MapPiece.go(remove_any_projector, z);
};

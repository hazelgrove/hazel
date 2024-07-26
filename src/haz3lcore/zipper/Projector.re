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

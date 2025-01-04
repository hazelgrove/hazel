open ProjectorBase;

/* After adding a new projector module, add it here so that
 * it can be instantiated. The first-class module created by
 * this function must be reified whenever projector methods
 * are to be called; see `shape` below for an example */
let to_module = (kind: Base.kind): (module Cooked) =>
  switch (kind) {
  | Fold => (module Cook(FoldProj.M))
  | Info => (module Cook(InfoProj.M))
  | Probe => (module Cook(ProbeProj.M))
  | Slider => (module Cook(SliderProj.M))
  | SliderF => (module Cook(SliderFProj.M))
  | Checkbox => (module Cook(CheckboxProj.M))
  | TextArea => (module Cook(TextAreaProj.M))
  | Card => (module Cook(CardProj.M))
  };

/* Currently projection is limited to convex pieces */
let minimum_projection_condition = (syntax: syntax): bool =>
  Piece.is_convex(syntax);

let init = (kind: t, syntax: syntax): syntax => {
  /* We set the projector id equal to the Piece id for convienence
   * including cursor-info association. We maintain this invariant
   * when we update a projector's contained syntax */
  let (module P) = to_module(kind);
  switch (P.can_project(syntax) && minimum_projection_condition(syntax)) {
  | false => syntax
  | true => Projector({id: Piece.id(syntax), kind, model: P.init, syntax})
  };
};

let init_from_str = (kind: t, syntax: syntax, model_str: string): syntax => {
  let (module P) = to_module(kind);
  switch (P.can_project(syntax) && minimum_projection_condition(syntax)) {
  | false => syntax
  | true => Projector({id: Piece.id(syntax), kind, model: model_str, syntax})
  };
};

let shape = (p: Base.projector, info: info): shape => {
  let (module P) = to_module(p.kind);
  P.placeholder(p.model, info);
};

let shape_of_proj =
    (statics: Statics.Map.t, dynamics: Dynamics.Map.t, p: Base.projector)
    : shape => {
  let statics = Statics.Map.lookup(p.id, statics);
  let dynamics = Dynamics.Map.lookup(p.id, dynamics);
  shape(p, {id: p.id, syntax: p.syntax, statics, dynamics});
};

let token_of_shape = (shape: shape): string =>
  switch (shape.vertical) {
  | Inline
  | Tab(_) => String.make(shape.horizontal, ' ')
  | Block(num_lb) =>
    String.make(num_lb, '\n') ++ String.make(shape.horizontal, ' ')
  };

/* Returns a token consisting of whitespace (possibly including linebreaks)
 * representing the space to leave for the projector in the underlying code view */
let _token_of_proj =
    (statics: Statics.Map.t, dynamics: Dynamics.Map.t, p: Base.projector) =>
  token_of_shape(shape_of_proj(statics, dynamics, p));

open ProjectorBase;

/* After adding a new projector module, add it here so that
 * it can be instantiated. The first-class module created by
 * this function must be reified whenever projector methods
 * are to be called; see `shape` below for an example */
let to_module = (kind: ProjectorCore.kind): (module Cooked) =>
  switch (kind) {
  | Fold => (module Cook(FoldProj.M))
  | Info => (module Cook(TypeProj.M))
  | Probe => (module Cook(ProbeProj.M))
  | Slider => (module Cook(SliderProj.M))
  | SliderF => (module Cook(SliderFProj.M))
  | Checkbox => (module Cook(CheckboxProj.M))
  | TextArea => (module Cook(TextAreaProj.M))
  | Card => (module Cook(CardProj.M))
  };

let init =
    (kind: ProjectorCore.kind, syntax: syntax, any: Term.Any.t)
    : option(syntax) => {
  /* We set the projector id equal to the Piece id for convienence
   * including cursor-info association. We maintain this invariant
   * when we update a projector's contained syntax */
  let (module P) = to_module(kind);
  switch (P.can_project(syntax, any)) {
  | false => None
  | true =>
    Some(Projector({id: Piece.id(syntax), kind, model: P.init, syntax}))
  };
};

let init_or_noop =
    (kind: ProjectorCore.kind, syntax: syntax, any: Term.Any.t): syntax =>
  switch (init(kind, syntax, any)) {
  | Some(pr) => pr
  | None => syntax
  };

let init_or_noop_from_str =
    (
      kind: ProjectorCore.kind,
      syntax: syntax,
      any: Term.Any.t,
      model_str: string,
    )
    : syntax => {
  let (module P) = to_module(kind);
  switch (P.can_project(syntax, any)) {
  | false => syntax
  | true => Projector({id: Piece.id(syntax), kind, model: model_str, syntax})
  };
};

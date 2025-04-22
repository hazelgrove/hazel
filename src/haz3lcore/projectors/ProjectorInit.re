open ProjectorBase;

/* After adding a new projector module, add it here so that
 * it can be instantiated. The first-class module created by
 * this function must be reified whenever projector methods
 * are to be called; see `shape` below for an example */
let to_module =
    (type a, kind: ProjectorCore.Kind.gadt(a))
    : (module Projector with type model = a) =>
  switch (kind) {
  | Fold => (module FoldProj.M)
  | Info => (module TypeProj.M)
  | Probe => (module ProbeProj.M)
  | Slider => (module SliderProj.M)
  | SliderF => (module SliderFProj.M)
  | Checkbox => (module CheckboxProj.M)
  | TextArea => (module TextAreaProj.M)
  | Card => (module CardProj.M)
  };

let init = (kind: ProjectorCore.Kind.t, any: Term.Any.t): option('p) => {
  open ProjectorCore.Kind;
  let.gadt W(kind_gadt) = kind;
  let (module P) = to_module(kind_gadt);
  switch (P.init(any)) {
  | None => None
  | Some(model) => Some(ProjectorCore.V(kind_gadt, model))
  };
};

let make_term = (V(k, m): ProjectorCore.model, exp): Any.t => {
  let (module P) = to_module(k);
  P.mk_term(m, exp);
};

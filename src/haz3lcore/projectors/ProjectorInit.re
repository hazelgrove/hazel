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

let init =
    (kind: ProjectorCore.Kind.t, syntax: syntax, any: Term.Any.t)
    : option(syntax) => {
  open ProjectorCore.Kind;
  let.gadt W(kind_gadt) = kind;
  let (module P) = to_module(kind_gadt);
  switch (P.init(any)) {
  | None => None
  | Some(model) =>
    Some(Base.Projector(Base.mk_projector(syntax, V(kind_gadt, model))))
  };
};

let init_or_noop =
    (kind: ProjectorCore.Kind.t, syntax: syntax, any: Term.Any.t): syntax =>
  switch (init(kind, syntax, any)) {
  | Some(pr) => pr
  | None => syntax
  };

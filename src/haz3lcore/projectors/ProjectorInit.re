open ProjectorBase;

/* After adding a new projector module, add it here so that
 * it can be instantiated. The first-class module created by
 * this function must be reified whenever projector methods
 * are to be called; see `shape` below for an example */
let to_module =
    (
      type ed,
      type model,
      type action,
      kind: ProjectorCore.Kind.gadt(model, action, ed),
    )
    : ProjectorBase.methods(model, action, ed) =>
  switch (kind) {
  // | Fold => FoldProj.methods
  | Info => TypeProj.methods
  // | Probe => ProbeProj.methods
  // | Checkbox => CheckboxProj.methods
  | Slider => SliderProj.methods
  // | SliderF => SliderFProj.methods
  // | Card => CardProj.methods
  // | TextArea => TextAreaProj.methods
  };

let init =
    (type ed, kind: ProjectorCore.Kind.t, any: Term.Any.t, ed: unit => ed)
    : option(ProjectorCore.model(ed)) => {
  open ProjectorCore.Kind;
  let.gadt W(kind_gadt) = kind;
  let methods = to_module(kind_gadt);
  switch (methods.init(any, ed)) {
  | None => None
  | Some(model) => Some(ProjectorCore.V(kind_gadt, model))
  };
};

let make_term = (~term_of_ed, V(k, m): ProjectorCore.model('ed)) => {
  let methods = to_module(k);
  methods.mk_term(~term_of_ed, _, m);
};

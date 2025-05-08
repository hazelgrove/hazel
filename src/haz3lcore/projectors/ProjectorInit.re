open ProjectorBase;

/* After adding a new projector module, add it here so that
 * it can be instantiated. The first-class module created by
 * this function must be reified whenever projector methods
 * are to be called; see `shape` below for an example */
let to_module =
    (
      type ed_m,
      type ed_a,
      type model,
      type action,
      kind: ProjectorCore.Kind.gadt(model, action, ed_m, ed_a),
    )
    : ProjectorBase.methods(model, action, ed_m, ed_a) =>
  switch (kind) {
  // | Fold => FoldProj.methods
  | Info => TypeProj.methods
  | Pair => PairProj.methods
  // | Probe => ProbeProj.methods
  // | Checkbox => CheckboxProj.methods
  | Slider => SliderProj.methods
  // | SliderF => SliderFProj.methods
  // | Card => CardProj.methods
  // | TextArea => TextAreaProj.methods
  };

let init =
    (
      type ed,
      type ed_a,
      kind: ProjectorCore.Kind.t,
      any: Term.Any.t,
      ed: unit => option(ed),
    )
    : option(ProjectorCore.model(ed, ed_a)) => {
  open ProjectorCore.Kind;
  let.gadt W(kind_gadt) = kind;
  let methods = to_module(kind_gadt);
  switch (methods.init(any, ed)) {
  | None => None
  | Some(model) => Some(ProjectorCore.V(kind_gadt, model))
  };
};

let make_term = (~term_of_ed, V(k, m): ProjectorCore.model('ed, 'ed_a)) => {
  let methods = to_module(k);
  methods.mk_term(~term_of_ed, _, m);
};

let focusable_of_model = (V(k, _): ProjectorCore.model('ed, 'ed_a)) => {
  let methods = to_module(k);
  methods.focusable;
};

let focusable_of_kind = (k: ProjectorCore.Kind.t) => {
  open ProjectorCore.Kind;
  let.gadt W(kind_gadt) = k;
  let methods = to_module(kind_gadt);
  methods.focusable;
};

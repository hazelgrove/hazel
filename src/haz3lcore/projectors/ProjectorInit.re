open ProjectorBase;

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
  let methods = ProjectorCore.to_module(kind_gadt);
  switch (methods.init(any, ed)) {
  | None => None
  | Some(model) => Some(ProjectorCore.V(kind_gadt, model))
  };
};

let make_term = (~term_of_ed, V(k, m): ProjectorCore.model('ed, 'ed_a)) => {
  let methods = ProjectorCore.to_module(k);
  methods.mk_term(~term_of_ed, _, m);
};

let focusable_of_model = (V(k, _): ProjectorCore.model('ed, 'ed_a)) => {
  let methods = ProjectorCore.to_module(k);
  methods.focusable;
};

let focusable_of_kind = (k: ProjectorCore.Kind.t) => {
  open ProjectorCore.Kind;
  let.gadt W(kind_gadt) = k;
  let methods = ProjectorCore.to_module(kind_gadt);
  methods.focusable;
};

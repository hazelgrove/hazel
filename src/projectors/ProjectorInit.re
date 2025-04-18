open ProjectorBase;

module Init = (Syntax: Syntax) => {
  /* After adding a new projector module, add it here so that
   * it can be instantiated. The first-class module created by
   * this function must be reified whenever projector methods
   * are to be called; see `shape` below for an example */
  let to_module =
      (type a, kind: ProjectorCore.Kind.gadt(a))
      : (module ProjectorInstance with type model = a) => {
    switch (kind) {
    | Fold => (module FoldProj.Make(Syntax))
    | Info => (module TypeProj.Make(Syntax))
    | Probe => (module ProbeProj.Make(Syntax))
    | Slider => (module SliderProj.Make(Syntax))
    | SliderF => (module SliderFProj.Make(Syntax))
    | Checkbox => (module CheckboxProj.Make(Syntax))
    | TextArea => (module TextAreaProj.Make(Syntax))
    | Card => (module CardProj.Make(Syntax))
    };
  };

  let init =
      (kind: ProjectorCore.Kind.t, syntax: syntax, any: Semantics.Any.t)
      : option(syntax) => {
    open ProjectorCore.Kind;
    let.gadt W(kind_gadt) = kind;
    let (module P) = to_module(kind_gadt);
    switch (P.init(any)) {
    | None => None
    | Some(model) =>
      Some(
        Base.Projector(ProjectorCore.mk(kind, syntax, V(kind_gadt, model))),
      )
    };
  };

  let init_or_noop =
      (kind: ProjectorCore.Kind.t, syntax: syntax, any: Semantics.Any.t)
      : syntax =>
    switch (init(kind, syntax, any)) {
    | Some(pr) => pr
    | None => syntax
    };
};

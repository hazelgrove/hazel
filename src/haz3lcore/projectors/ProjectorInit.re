open ProjectorBase;

/* After adding a new projector module, add it here so that
 * it can be instantiated. The first-class module created by
 * this function must be reified whenever projector methods
 * are to be called; see `shape` below for an example */
let to_module = (kind: ProjectorCore.Kind.t): (module Cooked) =>
  switch (kind) {
  | Fold => (module Cook(FoldProj.M))
  | Statics => (module Cook(TypeProj.M))
  | Probe => (module Cook(ProbeProj.M))
  | Slider => (module Cook(SliderProj.M))
  | SliderF => (module Cook(SliderFProj.M))
  | Checkbox => (module Cook(CheckboxProj.M))
  | TextArea => (module Cook(TextAreaProj.M))
  | Livelit => (module Cook(LivelitProj.M))
  | Card => (module Cook(CardProj.M))
  | Table => (module Cook(TableProj.M))
  | Csv => (module Cook(CSVProjector.M))
  };

/* Projectors store their underlying syntax as a term (Any.t). `init` wraps
 * the term in a Projector piece; `init_or_noop` falls back to the provided
 * piece (the un-projected syntax) when the projector declines the term. */
let init = (kind: ProjectorCore.Kind.t, any: Language.Any.t): option(syntax) => {
  let (module P) = to_module(kind);
  switch (P.init(any)) {
  | None => None
  | Some(model) => Some(Projector(ProjectorCore.mk(kind, any, model)))
  };
};

let init_or_noop =
    (kind: ProjectorCore.Kind.t, fallback: syntax, any: Language.Any.t)
    : syntax =>
  switch (init(kind, any)) {
  | Some(pr) => pr
  | None => fallback
  };

let init_or_noop_from_str =
    (
      kind: ProjectorCore.Kind.t,
      fallback: syntax,
      any: Language.Any.t,
      model_str: string,
    )
    : syntax => {
  let (module P) = to_module(kind);
  switch (P.init(any)) {
  | None => fallback
  | Some(_) => Projector(ProjectorCore.mk(kind, any, model_str))
  };
};

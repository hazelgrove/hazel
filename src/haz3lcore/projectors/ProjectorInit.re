open ProjectorBase;

/* After adding a new projector module, add it here so that
 * it can be instantiated. The first-class module created by
 * this function must be reified whenever projector methods
 * are to be called; see `shape` below for an example */
let to_module = (kind: ProjectorCore.Kind.t): (module Cooked) =>
  switch (kind) {
  | Fold => (module Cook(FoldProj.M))
  | Statics => (module Cook(TypeProj.M))
  | Player => (module Cook(PlayerProj.M))
  | Probe => (module Cook(ProbeProj.M))
  | Slider => (module Cook(SliderProj.M))
  | SliderF => (module Cook(SliderFProj.M))
  | Knob => (module Cook(KnobProj.M))
  | Checkbox => (module Cook(CheckboxProj.M))
  | TextArea => (module Cook(TextAreaProj.M))
  | Livelit => (module Cook(LivelitProj.M))
  | Card => (module Cook(CardProj.M))
  | Csv => (module Cook(CSVProjector.M))
  | NotePicker => (module Cook(NotePickerProj.M))
  | RhythmGrid => (module Cook(RhythmGridProj.M))
  | XYPad => (module Cook(XYPadProj.M))
  };

let init =
    (kind: ProjectorCore.Kind.t, syntax: syntax, any: Language.Any.t)
    : option(syntax) => {
  let (module P) = to_module(kind);
  switch (P.init(any)) {
  | None => None
  | Some(model) => Some(Projector(ProjectorCore.mk(kind, syntax, model)))
  };
};

let init_or_noop =
    (kind: ProjectorCore.Kind.t, syntax: syntax, any: Language.Any.t): syntax =>
  switch (init(kind, syntax, any)) {
  | Some(pr) => pr
  | None => syntax
  };

let init_or_noop_from_str =
    (
      kind: ProjectorCore.Kind.t,
      syntax: syntax,
      any: Language.Any.t,
      model_str: string,
    )
    : syntax => {
  let (module P) = to_module(kind);
  switch (P.init(any)) {
  | None => syntax
  | Some(_) => Projector(ProjectorCore.mk(kind, syntax, model_str))
  };
};

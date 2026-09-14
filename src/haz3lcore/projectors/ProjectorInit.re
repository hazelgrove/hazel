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
  | HTML => (module Cook(HTMLProj.M))
  | Table => (module Cook(TableProj.M))
  | Csv => (module Cook(CSVProjector.M))
  | Automerge => (module Cook(AutomergeProj.M))
  | AutomergeWriteBack => (module Cook(AutomergeWriteBackProj.M))
  };

/* Projectors probed automatically so `info.dynamics` carries the live value
 * of the syntax they replace. Was `Projector.dynamics` until dev's #2522
 * dropped that (unread) flag; re-homed as a kind predicate, matching
 * hazel-html's `wants_dynamics`. */
let wants_dynamics = (kind: ProjectorCore.Kind.t): bool =>
  switch (kind) {
  /* HTML: the projector's own sample stream is what `live_value` reads.
     Livelit: the view fold-in (Statics' Projector case) samples the live
     HTML of a user-defined livelit at this projector's id.
     AutomergeWriteBack: writes the live value of the syntax it replaces
     back to the document. */
  | HTML
  | Livelit
  | AutomergeWriteBack => true
  | Fold
  | Statics
  | Probe
  | Slider
  | SliderF
  | Checkbox
  | TextArea
  | Card
  | Table
  | Csv
  | Automerge => false
  };

let init =
    (
      kind: ProjectorCore.Kind.t,
      syntax: syntax,
      ~placement=ProjectorCore.Placement.Inline,
      any: Language.Any.t,
    )
    : option(syntax) => {
  let (module P) = to_module(kind);
  switch (P.init(any)) {
  | None => None
  | Some(model) =>
    Some(Projector(ProjectorCore.mk(~placement, kind, syntax, model)))
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

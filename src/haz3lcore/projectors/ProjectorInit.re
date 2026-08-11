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

/* Printer for Term init overrides, injected by ProjectorPerform at
 * module initialization. Resolving a Term override requires
 * ExpToSegment, but this module cannot depend on ExpToSegment: it is
 * reachable from ExpToSegment via MakeTerm -> ... -> Refractors ->
 * ProjectorInit. ProjectorPerform sits above both and registers the
 * real printer (the same conversion SetTerm uses, reusing splices
 * from the original syntax by id). If unregistered, Term overrides
 * degrade to keeping the selected syntax. */
let term_printer:
  ref((~original_syntax: Base.segment, Language.Any.t) => Base.segment) =
  ref((~original_syntax, _term) => original_syntax);

/* Resolve an init-returned syntax override against the selected
 * syntax: Term overrides are printed to a segment, Syntax overrides
 * are installed directly, and None keeps the selection. */
let resolve_override =
    (syntax: syntax, override: option(init_override)): syntax =>
  switch (override) {
  | None => syntax
  | Some(Syntax(seg)) => seg
  | Some(Term(term)) => term_printer^(~original_syntax=syntax, term)
  };

/* Construct a Projector piece wrapping the given syntax segment.
 * The projector's [init] may optionally return a replacement for the
 * underlying syntax (e.g. to wrap list items in splices); see
 * ProjectorBase.init_override. */
let init =
    (kind: ProjectorCore.Kind.t, syntax: syntax, any: Language.Any.t)
    : option(Base.piece) => {
  let (module P) = to_module(kind);
  switch (P.init(any, syntax)) {
  | None => None
  | Some((model, override)) =>
    let syntax = resolve_override(syntax, override);
    Some(Projector(ProjectorCore.mk(kind, syntax, model)));
  };
};

/* Like [init], but falls back to wrapping the syntax in no projector
 * (returning the original syntax as a segment) when init declines. */
let init_or_noop =
    (kind: ProjectorCore.Kind.t, syntax: syntax, any: Language.Any.t)
    : Base.segment =>
  switch (init(kind, syntax, any)) {
  | Some(pr) => [pr]
  | None => syntax
  };

let init_or_noop_from_str =
    (
      kind: ProjectorCore.Kind.t,
      syntax: syntax,
      any: Language.Any.t,
      model_str: string,
    )
    : Base.segment => {
  let (module P) = to_module(kind);
  switch (P.init(any, syntax)) {
  | None => syntax
  | Some((_, override)) =>
    let syntax = resolve_override(syntax, override);
    [Projector(ProjectorCore.mk(kind, syntax, model_str))];
  };
};

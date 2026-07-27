open ProjectorBase;

module Model = ProjectorCore.Model;

/* Dispatch from a projector kind or model to its implementation. This is
 * the only module that knows the full list, so adding a projector means
 * adding a case to each switch below; the compiler finds them all.
 *
 * Members that need the model switch on Model.t rather than on Kind.t,
 * which picks the implementation and unpacks its model in one step and so
 * has no impossible cases to raise on. Members that don't (see `statics`)
 * still switch on Kind.t, since their callers have no model yet. */

/* ProbeProj's model is the one whose payload isn't in the tagged union: it
 * wraps a RichProbe.packed_model, an existential resolved through the
 * RichProbeRegistry, which src/language can't name. So it crosses the
 * boundary as a sexp string; see ProjectorCore.Model.Probe.
 *
 * Probe is a refractor, so its models live in the side table rather than
 * in segments, and these parses stay off the placeholder/error path that
 * motivated typing the other models. */
let probe_in = (s: string): ProbeProj.probe_model =>
  switch (Sexplib.Sexp.of_string(s)) {
  | sexp => ProbeProj.probe_model_of_sexp(sexp)
  | exception _ => {active_renderer: None}
  };

let probe_out = (m: ProbeProj.probe_model): Model.t =>
  Model.Probe(m |> ProbeProj.sexp_of_probe_model |> Sexplib.Sexp.to_string);

let statics = (kind: ProjectorCore.Kind.t): (module Static) =>
  switch (kind) {
  | Fold => (module FoldProj.M)
  | Statics => (module TypeProj.M)
  | Probe => (module ProbeProj.M)
  | Slider => (module SliderProj.M)
  | SliderF => (module SliderFProj.M)
  | Checkbox => (module CheckboxProj.M)
  | TextArea => (module TextAreaProj.M)
  | Livelit => (module LivelitProj.M)
  | Card => (module CardProj.M)
  | Table => (module TableProj.M)
  | Csv => (module CSVProjector.M)
  };

/* None if the projector declines to handle this term */
let init_model =
    (kind: ProjectorCore.Kind.t, any: Language.Any.t): option(Model.t) =>
  switch (kind) {
  | Fold => FoldProj.M.init(any) |> Option.map(m => Model.Fold(m))
  | Statics => TypeProj.M.init(any) |> Option.map(m => Model.Statics(m))
  | Probe => ProbeProj.M.init(any) |> Option.map(probe_out)
  | Slider => SliderProj.M.init(any) |> Option.map(() => Model.Slider)
  | SliderF => SliderFProj.M.init(any) |> Option.map(() => Model.SliderF)
  | Checkbox => CheckboxProj.M.init(any) |> Option.map(() => Model.Checkbox)
  | TextArea => TextAreaProj.M.init(any) |> Option.map(() => Model.TextArea)
  | Livelit => LivelitProj.M.init(any) |> Option.map(() => Model.Livelit)
  | Card => CardProj.M.init(any) |> Option.map(m => Model.Card(m))
  | Table => TableProj.M.init(any) |> Option.map(() => Model.Table)
  | Csv => CSVProjector.M.init(any) |> Option.map(m => Model.Csv(m))
  };

let placeholder = (model: Model.t, info: info): ProjectorCore.Shape.t =>
  switch (model) {
  | Fold(m) => FoldProj.M.placeholder(m, info)
  | Statics(m) => TypeProj.M.placeholder(m, info)
  | Probe(s) => ProbeProj.M.placeholder(probe_in(s), info)
  | Slider => SliderProj.M.placeholder((), info)
  | SliderF => SliderFProj.M.placeholder((), info)
  | Checkbox => CheckboxProj.M.placeholder((), info)
  | TextArea => TextAreaProj.M.placeholder((), info)
  | Livelit => LivelitProj.M.placeholder((), info)
  | Card(m) => CardProj.M.placeholder(m, info)
  | Table => TableProj.M.placeholder((), info)
  | Csv(m) => CSVProjector.M.placeholder(m, info)
  };

let error = (model: Model.t, info: info): option(ProjectorBase.error) =>
  switch (model) {
  | Fold(m) => FoldProj.M.error(m, info)
  | Statics(m) => TypeProj.M.error(m, info)
  | Probe(s) => ProbeProj.M.error(probe_in(s), info)
  | Slider => SliderProj.M.error((), info)
  | SliderF => SliderFProj.M.error((), info)
  | Checkbox => CheckboxProj.M.error((), info)
  | TextArea => TextAreaProj.M.error((), info)
  | Livelit => LivelitProj.M.error((), info)
  | Card(m) => CardProj.M.error(m, info)
  | Table => TableProj.M.error((), info)
  | Csv(m) => CSVProjector.M.error(m, info)
  };

/* Rebuilds View.args at the projector's own model and action types, and
 * runs its `update` inside `local` so callers only ever see a new Model.t.
 * Exists so `view` below doesn't repeat the record for each projector. */
let adapt =
    (
      ~view: View.args('model, 'action) => View.t,
      ~update: ('model, info, 'action) => 'model,
      ~inj: 'model => Model.t,
      model: 'model,
      args: view_args,
    )
    : View.t =>
  view({
    model,
    info: args.info,
    local: a => args.local(inj(update(model, args.info, a))),
    parent: args.parent,
    view_seg: args.view_seg,
    status: args.status,
    core_settings: args.core_settings,
  });

let view = (args: view_args): View.t =>
  switch (args.model) {
  | Fold(m) =>
    adapt(
      ~view=FoldProj.M.view,
      ~update=FoldProj.M.update,
      ~inj=m => Model.Fold(m),
      m,
      args,
    )
  | Statics(m) =>
    adapt(
      ~view=TypeProj.M.view,
      ~update=TypeProj.M.update,
      ~inj=m => Model.Statics(m),
      m,
      args,
    )
  | Probe(s) =>
    adapt(
      ~view=ProbeProj.M.view,
      ~update=ProbeProj.M.update,
      ~inj=probe_out,
      probe_in(s),
      args,
    )
  | Slider =>
    adapt(
      ~view=SliderProj.M.view,
      ~update=SliderProj.M.update,
      ~inj=() => Model.Slider,
      (),
      args,
    )
  | SliderF =>
    adapt(
      ~view=SliderFProj.M.view,
      ~update=SliderFProj.M.update,
      ~inj=() => Model.SliderF,
      (),
      args,
    )
  | Checkbox =>
    adapt(
      ~view=CheckboxProj.M.view,
      ~update=CheckboxProj.M.update,
      ~inj=() => Model.Checkbox,
      (),
      args,
    )
  | TextArea =>
    adapt(
      ~view=TextAreaProj.M.view,
      ~update=TextAreaProj.M.update,
      ~inj=() => Model.TextArea,
      (),
      args,
    )
  | Livelit =>
    adapt(
      ~view=LivelitProj.M.view,
      ~update=LivelitProj.M.update,
      ~inj=() => Model.Livelit,
      (),
      args,
    )
  | Card(m) =>
    adapt(
      ~view=CardProj.M.view,
      ~update=CardProj.M.update,
      ~inj=m => Model.Card(m),
      m,
      args,
    )
  | Table =>
    adapt(
      ~view=TableProj.M.view,
      ~update=TableProj.M.update,
      ~inj=() => Model.Table,
      (),
      args,
    )
  | Csv(m) =>
    adapt(
      ~view=CSVProjector.M.view,
      ~update=CSVProjector.M.update,
      ~inj=m => Model.Csv(m),
      m,
      args,
    )
  };

let init =
    (kind: ProjectorCore.Kind.t, syntax: syntax, any: Language.Any.t)
    : option(syntax) =>
  init_model(kind, any)
  |> Option.map(model => Base.Projector(ProjectorCore.mk(syntax, model)));

let init_or_noop =
    (kind: ProjectorCore.Kind.t, syntax: syntax, any: Language.Any.t): syntax =>
  switch (init(kind, syntax, any)) {
  | Some(pr) => pr
  | None => syntax
  };

/* Like init_or_noop, but with a model supplied by the caller. `any` is
 * still consulted so the projector can decline the term. */
let init_or_noop_with_model =
    (syntax: syntax, any: Language.Any.t, model: Model.t): syntax =>
  switch (init_model(Model.kind(model), any)) {
  | None => syntax
  | Some(_) => Base.Projector(ProjectorCore.mk(syntax, model))
  };

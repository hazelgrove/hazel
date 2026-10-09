open Virtual_dom.Vdom;
open ProjectorBase;
open Language;

module Sexp = Sexplib.Sexp;

/* How the sample menu's "View as" list names a view: as it is written
   (a livelit's `^name`, set in the code font) or as prose (Table). */
[@deriving show({with_path: false})]
type view_label = {
  name: string,
  code: bool,
};

/* Where a rendering is drawn: offside (a sample chip at the end of the
   line) or in the probe's drawer. Livelit views are told (see
   UserLivelit.place). */
[@deriving (show({with_path: false}), sexp, yojson)]
type place = UserLivelit.place;

/* A rich probe renderer: a domain-specific view of probed values.
   - value: the parsed representation; `parse` succeeding means the
     renderer can show the expression.
   - model: UI state of the rendering's controls, persisted with the probe.
   - action: events that update the model. A rendering can also request
     editor-level effects (syntax edits, focus) through its `~parent`
     callback (external_action). */
module type RichProbe = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type value;

  let update: (model, action) => model;
  /* Parse an expression into its domain-specific value representation.
     This extracts the structured data needed for interactive visualization.
     ~statics is the probed expression's info (type + context), for
     renderers that apply by TYPE rather than by value shape. */
  let parse: (~statics: option(Info.t), Sort.t, Exp.t) => option(value);
  /* `parse` for the view a model selects: also takes a view offered only
     on request (`on_request`), which `parse` never finds */
  let parse_chosen:
    (~statics: option(Info.t), Sort.t, Exp.t, model) => option(value);
  /* The views offered for a value only on request, in the "View as" list,
     never by an automatic pick: the livelits whose type merely fits */
  let on_request: (~statics: option(Info.t), Sort.t, Exp.t) => list(model);
  /* Whether a parsed value is positive evidence that this renderer should
     be picked AUTOMATICALLY (auto-rich embeds, wells). Explicit picks ignore
     it. Lets a renderer decline vacuous matches (an empty list parses as an
     empty card hand) or opt out of auto-selection altogether (tables). */
  let auto_applies: value => bool;
  /* Initialize the probe's state from a parsed value. Assumes value is valid. */
  let init: value => model;
  /* Default state independent of any sample value — the model a
     text-level renderer selection (`^^probe@<id>`) starts with. */
  let empty: model;

  /* Height in editor rows when the rendering replaces the sample view in
     the drawer, so the framework can reserve the right number of lines. */
  let drawer_rows: (model, value) => int;

  /* The views this renderer offers for a value, as the models that select
     them, in the order an automatic pick prefers them (its pick is the
     first). Most renderers offer one; the livelit renderer offers every
     livelit whose view renders the value. */
  let views: value => list(model);
  /* The name of the view a model selects for a value */
  let label: (model, value) => view_label;

  let badge: Node.t;

  let render:
    (
      ~info: info,
      ~exp: Exp.t,
      ~value: value,
      ~view_seg: (Sort.t, Segment.t) => Node.t,
      ~model: model,
      ~local: action => Ui_effect.t(unit),
      ~parent: external_action => Ui_effect.t(unit),
      ~sort: Sort.t,
      ~place: place,
      unit
    ) =>
    Node.t;
};

/* Existential packs for renderer state.
 *
 * Each pack carries:
 *   - a string id, stable across persistence, used to dispatch through the registry
 *   - a Type.Id.t witness, fresh per pack_renderer call, used at runtime to
 *     recover the concrete type without Obj.magic
 *   - the value itself
 *
 * The Type.Id.t cannot be serialized (it's only meaningful within one process),
 * so on deserialization the renderer's currently-registered Type.Id is substituted
 * via the registry. */
type packed_model =
  | PModel(string, Type.Id.t('m), 'm): packed_model;

type packed_action =
  | PAction(string, Type.Id.t('a), 'a): packed_action;

type packed_renderer = {
  id: string,
  can_handle: (~statics: option(Info.t), Sort.t, Exp.t) => bool,
  /* can_handle AND the renderer's auto_applies — the predicate every
     automatic renderer pick goes through */
  auto_applies: (~statics: option(Info.t), Sort.t, Exp.t) => bool,
  init_model:
    (~statics: option(Info.t), Sort.t, Exp.t) => option(packed_model),
  empty_model: packed_model,
  update_model: (packed_model, packed_action) => packed_model,
  /* whether the view a model selects draws a value: can_handle, for a
     chosen view (which may be one offered only on request) */
  handles: (packed_model, ~statics: option(Info.t), Sort.t, Exp.t) => bool,
  /* ~model: the view chosen for the probe; None for an automatic pick */
  drawer_rows:
    (
      ~statics: option(Info.t),
      ~model: option(packed_model),
      Sort.t,
      Exp.t
    ) =>
    option(int),
  /* the views offered for a value, each named and with its model */
  views:
    (~statics: option(Info.t), Sort.t, Exp.t) =>
    list((view_label, packed_model)),
  /* ...and those offered only on request */
  on_request:
    (~statics: option(Info.t), Sort.t, Exp.t) =>
    list((view_label, packed_model)),
  /* the name of the view a model selects for a value */
  label:
    (packed_model, ~statics: option(Info.t), Sort.t, Exp.t) =>
    option(view_label),
  render_model:
    (
      packed_model,
      ~info: info,
      ~exp: Exp.t,
      ~view_seg: (Sort.t, Segment.t) => Node.t,
      ~local: packed_action => Ui_effect.t(unit),
      ~parent: external_action => Ui_effect.t(unit),
      ~sort: Sort.t,
      ~place: place,
      unit
    ) =>
    option(Node.t),
  /* Payload (de)serializers — used by RichProbeRegistry's top-level
   * packed_*_of_sexp/yojson dispatchers to encode/decode the body for
   * *this* renderer. They expect the packed value to belong to this
   * renderer; mismatches yield empty/null bodies (encode). */
  sexp_of_model_payload: packed_model => Sexp.t,
  model_payload_of_sexp: Sexp.t => packed_model,
  yojson_of_model_payload: packed_model => Yojson.Safe.t,
  model_payload_of_yojson: Yojson.Safe.t => packed_model,
  sexp_of_action_payload: packed_action => Sexp.t,
  action_payload_of_sexp: Sexp.t => packed_action,
  yojson_of_action_payload: packed_action => Yojson.Safe.t,
  action_payload_of_yojson: Yojson.Safe.t => packed_action,
  badge: Node.t,
};

let renderer_id_of_model = (PModel(rid, _, _): packed_model): string => rid;
let renderer_id_of_action = (PAction(rid, _, _): packed_action): string => rid;

/* Pack a RichProbe module into a packed_renderer. Allocates fresh Type.Id
 * witnesses for the model and action types and binds them in the closures
 * below; cast functions use Type.Id.provably_equal to recover the concrete
 * types safely (no Obj.magic). */
let pack_renderer =
    (
      type m,
      type a,
      type v,
      module_impl: (module RichProbe with
                      type model = m and type action = a and type value = v),
      id: string,
    )
    : packed_renderer => {
  module R = (val module_impl);
  let model_id: Type.Id.t(m) = Type.Id.make();
  let action_id: Type.Id.t(a) = Type.Id.make();
  let cast_model = (pm: packed_model): option(m) =>
    switch (pm) {
    | PModel(_, other, m) =>
      switch (Type.Id.provably_equal(other, model_id)) {
      | Some(Type.Equal) => Some(m)
      | None => None
      }
    };
  let cast_action = (pa: packed_action): option(a) =>
    switch (pa) {
    | PAction(_, other, a) =>
      switch (Type.Id.provably_equal(other, action_id)) {
      | Some(Type.Equal) => Some(a)
      | None => None
      }
    };
  {
    id,
    can_handle: (~statics, sort, exp) =>
      Option.is_some(R.parse(~statics, sort, exp)),
    auto_applies: (~statics, sort, exp) =>
      switch (R.parse(~statics, sort, exp)) {
      | Some(v) => R.auto_applies(v)
      | None => false
      },
    init_model: (~statics, sort, exp) =>
      R.parse(~statics, sort, exp)
      |> Option.map(v => PModel(id, model_id, R.init(v))),
    empty_model: PModel(id, model_id, R.empty),
    handles: (pm, ~statics, sort, exp) =>
      switch (cast_model(pm)) {
      | Some(m) => Option.is_some(R.parse_chosen(~statics, sort, exp, m))
      | None => false
      },
    drawer_rows: (~statics, ~model, sort, exp) =>
      switch (Option.bind(model, cast_model)) {
      | Some(m) =>
        R.parse_chosen(~statics, sort, exp, m)
        |> Option.map(v => R.drawer_rows(m, v))
      | None =>
        R.parse(~statics, sort, exp)
        |> Option.map(v => R.drawer_rows(R.init(v), v))
      },
    views: (~statics, sort, exp) =>
      switch (R.parse(~statics, sort, exp)) {
      | Some(v) =>
        List.map(
          m => (R.label(m, v), PModel(id, model_id, m)),
          R.views(v),
        )
      | None => []
      },
    on_request: (~statics, sort, exp) =>
      List.filter_map(
        m =>
          R.parse_chosen(~statics, sort, exp, m)
          |> Option.map(v => (R.label(m, v), PModel(id, model_id, m))),
        R.on_request(~statics, sort, exp),
      ),
    label: (pm, ~statics, sort, exp) =>
      switch (cast_model(pm)) {
      | Some(m) =>
        R.parse_chosen(~statics, sort, exp, m)
        |> Option.map(v => R.label(m, v))
      | None => None
      },
    update_model: (pm, pa) =>
      switch (cast_model(pm), cast_action(pa)) {
      | (Some(m), Some(a)) => PModel(id, model_id, R.update(m, a))
      | _ => pm
      },
    render_model:
      (pm, ~info, ~exp, ~view_seg, ~local, ~parent, ~sort, ~place, ()) =>
      switch (
        Option.bind(cast_model(pm), m =>
          R.parse_chosen(~statics=info.statics, sort, exp, m)
          |> Option.map(v => (m, v))
        )
      ) {
      | Some((m, value)) =>
        Some(
          R.render(
            ~info,
            ~exp,
            ~value,
            ~view_seg,
            ~model=m,
            ~local=a => local(PAction(id, action_id, a)),
            ~parent,
            ~sort,
            ~place,
            (),
          ),
        )
      | None => None
      },
    sexp_of_model_payload: pm =>
      switch (cast_model(pm)) {
      | Some(m) => R.sexp_of_model(m)
      | None => Sexp.List([])
      },
    model_payload_of_sexp: sexp =>
      PModel(id, model_id, R.model_of_sexp(sexp)),
    yojson_of_model_payload: pm =>
      switch (cast_model(pm)) {
      | Some(m) => R.yojson_of_model(m)
      | None => `Null
      },
    model_payload_of_yojson: j => PModel(id, model_id, R.model_of_yojson(j)),
    sexp_of_action_payload: pa =>
      switch (cast_action(pa)) {
      | Some(a) => R.sexp_of_action(a)
      | None => Sexp.List([])
      },
    action_payload_of_sexp: sexp =>
      PAction(id, action_id, R.action_of_sexp(sexp)),
    yojson_of_action_payload: pa =>
      switch (cast_action(pa)) {
      | Some(a) => R.yojson_of_action(a)
      | None => `Null
      },
    action_payload_of_yojson: j =>
      PAction(id, action_id, R.action_of_yojson(j)),
    badge: R.badge,
  };
};

/* show derivers for ppx_deriving.show compat. The payload itself isn't
 * inspected — only the renderer id is printed. */
let pp_packed_model = (fmt, PModel(rid, _, _): packed_model) =>
  Format.fprintf(fmt, "<packed_model:%s>", rid);
let show_packed_model = (pm: packed_model): string =>
  Format.asprintf("%a", pp_packed_model, pm);

let pp_packed_action = (fmt, PAction(rid, _, _): packed_action) =>
  Format.fprintf(fmt, "<packed_action:%s>", rid);
let show_packed_action = (pa: packed_action): string =>
  Format.asprintf("%a", pp_packed_action, pa);

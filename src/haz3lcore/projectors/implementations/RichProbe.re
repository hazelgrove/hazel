open Virtual_dom.Vdom;
open ProjectorBase;
open Language;

module Sexp = Sexplib.Sexp;

/* Signature for domain-specific representations with clear parsing and rendering phases.
      Each RichProbe module handles a specific visualization of syntax elements.

      - 'value': The abstract data type representing the parsed internal representation of the probed value.
        This is parsed and it's presence signifies the ability to visualize the expression.

      - 'model': The UI state for the probe's interactive elements and controls.
        Stores user inputs, selected options, and transient state.

      - 'action': Events that can change the probe's model, like user interactions
        (button clicks, input changes) or system updates. This can also be used to update the syntax/
   */
module type RichProbe = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type model;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type action;
  [@deriving (show({with_path: false}), sexp, yojson)]
  type value;

  let update: (model, action) => model;
  /* Parse an expression into its domain-specific value representation.
     This extracts the structured data needed for interactive visualization. */
  let parse: (Sort.t, Exp.t) => option(value);
  /* Initialize the probe's state from a parsed value. Assumes value is valid. */
  let init: value => model;

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
  can_handle: (Sort.t, Exp.t) => bool,
  init_model: (Sort.t, Exp.t) => option(packed_model),
  update_model: (packed_model, packed_action) => packed_model,
  render_model:
    (
      packed_model,
      ~info: info,
      ~exp: Exp.t,
      ~view_seg: (Sort.t, Segment.t) => Node.t,
      ~local: packed_action => Ui_effect.t(unit),
      ~parent: external_action => Ui_effect.t(unit),
      ~sort: Sort.t,
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
    can_handle: (sort, exp) => Option.is_some(R.parse(sort, exp)),
    init_model: (sort, exp) =>
      R.parse(sort, exp) |> Option.map(v => PModel(id, model_id, R.init(v))),
    update_model: (pm, pa) =>
      switch (cast_model(pm), cast_action(pa)) {
      | (Some(m), Some(a)) => PModel(id, model_id, R.update(m, a))
      | _ => pm
      },
    render_model: (pm, ~info, ~exp, ~view_seg, ~local, ~parent, ~sort, ()) =>
      switch (cast_model(pm), R.parse(sort, exp)) {
      | (Some(m), Some(value)) =>
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
            (),
          ),
        )
      | _ => None
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

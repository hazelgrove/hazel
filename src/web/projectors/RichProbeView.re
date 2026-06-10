open Virtual_dom.Vdom;
open Haz3lcore;
open ProjectorBase;
open Language;
open RichProbe;

/* The web (Vdom) half of the rich-probe API: each renderer pairs its
 * core logic module (RichProbe.RichProbe, in haz3lcore — parsing, state,
 * serialization) with a view module here (badge + render). The pairs are
 * registered in RichProbeViewRegistry, mirroring the core registry in
 * RichProbeRegistry. */

module type RichProbeView = {
  type model;
  type action;
  type value;

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

/* A renderer's core pack (id, can_handle, init_model, update_model,
 * payload serializers) paired with its web view (badge, render). */
type packed_view = {
  core: packed_renderer,
  badge: Node.t,
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
};

/* Pair a core RichProbe module with its view module.
 *
 * The packed_model/packed_action existentials carry a Type.Id witness
 * that is private to the core registry's pack_renderer closures, so the
 * view side can't cast them directly. Instead the typed model/action are
 * recovered through the registered renderer's payload (de)serializers —
 * the same path persistence takes — which re-bind to the live witness.
 * Models here are small transient UI state, so the round-trip is cheap. */
let pack_view =
    (
      type m,
      type a,
      type v,
      core_impl: (module RichProbe.RichProbe with
                    type model = m and type action = a and type value = v),
      view_impl: (module RichProbeView with
                    type model = m and type action = a and type value = v),
      id: string,
    )
    : packed_view => {
  module C = (val core_impl);
  module V = (val view_impl);
  let core =
    switch (RichProbeRegistry.find(id)) {
    | Some(r) => r
    | None =>
      failwith("RichProbeView.pack_view: renderer not registered: " ++ id)
    };
  let unpack_model = (pm: packed_model): option(m) =>
    renderer_id_of_model(pm) == id
      ? Some(core.sexp_of_model_payload(pm) |> C.model_of_sexp) : None;
  let pack_action = (a: a): packed_action =>
    core.action_payload_of_sexp(C.sexp_of_action(a));
  {
    core,
    badge: V.badge,
    render_model: (pm, ~info, ~exp, ~view_seg, ~local, ~parent, ~sort, ()) =>
      switch (unpack_model(pm), C.parse(sort, exp)) {
      | (Some(model), Some(value)) =>
        Some(
          V.render(
            ~info,
            ~exp,
            ~value,
            ~view_seg,
            ~model,
            ~local=a => local(pack_action(a)),
            ~parent,
            ~sort,
            (),
          ),
        )
      | _ => None
      },
  };
};

/* Registry of available rich-probe renderers, plus id-keyed (de)serialization
 * dispatch for the existential `packed_model` / `packed_action` types.
 *
 * Lives outside RichProbe.re to avoid a circular dependency: concrete
 * renderers depend on RichProbe.RichProbe, so RichProbe can't see them.
 *
 * To add a new renderer: implement RichProbe.RichProbe and add one entry
 * to `renderers` below. */

open RichProbe;
module Sexp = Sexplib.Sexp;

let renderers: list(packed_renderer) = [
  pack_renderer((module TableRenderer), "table"),
  pack_renderer((module HtmlRenderer), "html"),
];

let find = (id: string): option(packed_renderer) =>
  List.find_opt((r: packed_renderer) => r.id == id, renderers);

exception Unknown_renderer(string);

/* These names are picked up by ppx_sexp_conv / ppx_yojson_conv at the
 * deriving site (probe_model, ProbeProj.action) when RichProbeRegistry
 * is opened. */

let sexp_of_packed_model = (pm: packed_model): Sexp.t => {
  let rid = renderer_id_of_model(pm);
  switch (find(rid)) {
  | Some(r) => Sexp.List([Sexp.Atom(rid), r.sexp_of_model_payload(pm)])
  | None => Sexp.List([Sexp.Atom(rid)])
  };
};

let packed_model_of_sexp = (sexp: Sexp.t): packed_model =>
  switch (sexp) {
  | Sexp.List([Sexp.Atom(rid), body]) =>
    switch (find(rid)) {
    | Some(r) => r.model_payload_of_sexp(body)
    | None => raise(Unknown_renderer(rid))
    }
  | _ => failwith("packed_model_of_sexp: malformed sexp")
  };

let yojson_of_packed_model = (pm: packed_model): Yojson.Safe.t => {
  let rid = renderer_id_of_model(pm);
  switch (find(rid)) {
  | Some(r) => `List([`String(rid), r.yojson_of_model_payload(pm)])
  | None => `List([`String(rid)])
  };
};

let packed_model_of_yojson = (j: Yojson.Safe.t): packed_model =>
  switch (j) {
  | `List([`String(rid), body]) =>
    switch (find(rid)) {
    | Some(r) => r.model_payload_of_yojson(body)
    | None => raise(Unknown_renderer(rid))
    }
  | _ => failwith("packed_model_of_yojson: malformed json")
  };

let sexp_of_packed_action = (pa: packed_action): Sexp.t => {
  let rid = renderer_id_of_action(pa);
  switch (find(rid)) {
  | Some(r) => Sexp.List([Sexp.Atom(rid), r.sexp_of_action_payload(pa)])
  | None => Sexp.List([Sexp.Atom(rid)])
  };
};

let packed_action_of_sexp = (sexp: Sexp.t): packed_action =>
  switch (sexp) {
  | Sexp.List([Sexp.Atom(rid), body]) =>
    switch (find(rid)) {
    | Some(r) => r.action_payload_of_sexp(body)
    | None => raise(Unknown_renderer(rid))
    }
  | _ => failwith("packed_action_of_sexp: malformed sexp")
  };

let yojson_of_packed_action = (pa: packed_action): Yojson.Safe.t => {
  let rid = renderer_id_of_action(pa);
  switch (find(rid)) {
  | Some(r) => `List([`String(rid), r.yojson_of_action_payload(pa)])
  | None => `List([`String(rid)])
  };
};

let packed_action_of_yojson = (j: Yojson.Safe.t): packed_action =>
  switch (j) {
  | `List([`String(rid), body]) =>
    switch (find(rid)) {
    | Some(r) => r.action_payload_of_yojson(body)
    | None => raise(Unknown_renderer(rid))
    }
  | _ => failwith("packed_action_of_yojson: malformed json")
  };

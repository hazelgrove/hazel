open Haz3lcore;

/* Debounced, id-keyed re-run shared by the Z3-backed refractors (Reach and
 * TestGen). Those refractors bypass the projector view cache and re-render
 * every frame, so each point's view watches its own constraint "signature"
 * (the SMT script) and re-runs `run` a short while after the signature last
 * changed — i.e. once edits settle — instead of solving on every keystroke.
 * Identical scripts are cached downstream (Z3Wasm), so unchanged points are
 * free. Each refractor kind makes its own instance (`make`) so points of
 * different kinds that share a node id can't collide. */

type t = {
  timers: Hashtbl.t(Id.t, Js_of_ocaml.Dom_html.timeout_id),
  last_sigs: Hashtbl.t(Id.t, string),
};

let default_ms = 400.0;

let make = (): t => {
  timers: Hashtbl.create(64),
  last_sigs: Hashtbl.create(64),
};

/* Re-run `run` ~`default_ms` after this point's signature settles. A no-op
 * while the signature is unchanged, so storing results (which never changes the
 * signature) can't retrigger a solve. */
let tick = (d: t, ~id: Id.t, ~sig_: string, ~run: unit => unit): unit =>
  if (Hashtbl.find_opt(d.last_sigs, id) != Some(sig_)) {
    Hashtbl.replace(d.last_sigs, id, sig_);
    switch (Hashtbl.find_opt(d.timers, id)) {
    | Some(t) => Js_of_ocaml.Dom_html.window##clearTimeout(t)
    | None => ()
    };
    let t =
      Js_of_ocaml.Dom_html.window##setTimeout(
        Js_of_ocaml.Js.wrap_callback(() => {
          Hashtbl.remove(d.timers, id);
          run();
        }),
        default_ms,
      );
    Hashtbl.replace(d.timers, id, t);
  };

open Util;
open Js_of_ocaml;

/* Which livelit views the eval worker runs to Html (^ticket(18)): the
   costly ones on the screen. Matthew's defaults, 2026-10-16: only the
   costly livelits' views move to the worker, since a cheap view runs here
   faster than the worker's round trip (with every view there, Kids'
   Choice's release came about 45 ms later), and the request names the ones
   on the screen.

   The page times each view it runs (LivelitProj.live_html, by the sample's
   id, which is its projector's); one that took [costly_ms] or more is
   costly from then on. A view the worker ran comes back as Pure(html),
   which runs here at once and is not timed, so a view keeps the cost it
   had while the worker runs it. */
let costly_ms = 8.0;

let cost: Hashtbl.t(Id.t, float) = Hashtbl.create(64);

let note = (id: Id.t, ms: float): unit => Hashtbl.replace(cost, id, ms);

/* the projector's element (LivelitProj gives it the id Id.cls(id)) meets
   the window; with no page (the tests, the worker), every view counts */
let on_screen = (id: Id.t): bool =>
  try(
    switch (
      Js.Opt.to_option(
        Dom_html.document##getElementById(Js.string(Id.cls(id))),
      )
    ) {
    | None => false
    | Some(el) =>
      let r = el##getBoundingClientRect;
      let h = Dom_html.window##.innerHeight;
      let w = Dom_html.window##.innerWidth;
      r##.bottom > 0.
      &&
      r##.top < float_of_int(h)
      &&
      r##.right > 0.
      &&
      r##.left < float_of_int(w);
    }
  ) {
  | _ => true
  };

/* the views for the worker to run: the costly ones on the screen */
let ids = (): list(Id.t) =>
  Hashtbl.fold(
    (id, ms, acc) => ms >= costly_ms && on_screen(id) ? [id, ...acc] : acc,
    cost,
    [],
  );

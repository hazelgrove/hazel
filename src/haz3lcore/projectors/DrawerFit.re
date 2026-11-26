open Util;
open Poly;

/* Drawers whose content the web layer renders (a probe's stepper, a
   theorem's proof). The rows each one measured once rendered, by drawer
   id, catch what a count from the model misses, and a drawer reserves
   the larger, so it never runs into the code below. A change bumps
   [layout], which CachedSyntax checks, so the editor re-lays out. */

let layout = ref(0);

let measured: Stdlib.Hashtbl.t(Id.t, int) = Stdlib.Hashtbl.create(8);

let rows = (id: Id.t, computed: int): int =>
  max(
    computed,
    Stdlib.Hashtbl.find_opt(measured, id) |> Option.value(~default=1),
  );

/* a drawer opens to its whole length below its line */
let shape = (id: Id.t, computed: int): ProjectorShape.t => {
  horizontal: 0,
  vertical: Tab(rows(id, computed)),
};

/* true when it changed */
let set_measured = (id: Id.t, n: int): bool =>
  if (Poly.equal(Stdlib.Hashtbl.find_opt(measured, id), Some(n))) {
    false;
  } else {
    Stdlib.Hashtbl.replace(measured, id, n);
    layout := layout^ + 1;
    true;
  };

/* a new stepper starts from its own count, not the last one's height */
let forget = (id: Id.t): unit => Stdlib.Hashtbl.remove(measured, id);

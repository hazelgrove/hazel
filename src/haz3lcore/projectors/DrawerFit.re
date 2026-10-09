open Util;

/* Drawers whose content the web layer renders (a probe's stepper, a
   theorem's proof). The rows each one measured once rendered, by drawer
   id, catch what a count from the model misses, and a drawer reserves
   the larger, so it never runs into the code below. A change bumps
   [layout], which CachedSyntax checks, so the editor re-lays out. */

let layout = ref(0);

let measured: Hashtbl.t(Id.t, int) = Hashtbl.create(8);

let rows = (id: Id.t, computed: int): int =>
  max(computed, Hashtbl.find_opt(measured, id) |> Option.value(~default=1));

/* true when it changed */
let set_measured = (id: Id.t, n: int): bool =>
  if (Hashtbl.find_opt(measured, id) == Some(n)) {
    false;
  } else {
    Hashtbl.replace(measured, id, n);
    layout := layout^ + 1;
    true;
  };

/* a new stepper starts from its own count, not the last one's height */
let forget = (id: Id.t): unit => Hashtbl.remove(measured, id);

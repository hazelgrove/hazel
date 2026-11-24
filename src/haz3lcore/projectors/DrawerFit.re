open Util;

/* Drawers whose content the web layer renders (a probe's stepper, a
   theorem's proof), by drawer id. Each reserves the larger of the rows
   counted from its model and the rows it measured once rendered, which
   catch what the count misses, so it never runs into the code below. A
   change bumps [layout], which CachedSyntax checks, so the editor re-lays
   out. */

let layout = ref(0);
let relayout = () => layout := layout^ + 1;

let computed: ref(Id.Map.t(int)) = ref(Id.Map.empty);
let measured: ref(Id.Map.t(int)) = ref(Id.Map.empty);

let rows = (id: Id.t): int => {
  let find = table =>
    Id.Map.find_opt(id, table^) |> Option.value(~default=1);
  max(find(computed), find(measured));
};

/* a drawer opens to its whole length below its line */
let shape = (id: Id.t): ProjectorShape.t => {
  horizontal: 0,
  vertical: Tab(rows(id)),
};

/* true when the drawer's rows changed */
let set = (table, id: Id.t, n: int): bool => {
  let before = rows(id);
  table := Id.Map.add(id, n, table^);
  if (rows(id) == before) {
    false;
  } else {
    relayout();
    true;
  };
};
let set_computed = set(computed);
let set_measured = set(measured);

/* a new stepper starts from its own count, not the last one's height */
let forget = (id: Id.t): unit => {
  computed := Id.Map.remove(id, computed^);
  measured := Id.Map.remove(id, measured^);
};

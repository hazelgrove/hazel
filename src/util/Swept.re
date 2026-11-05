/* a bounded table for per-item results: at the bound, entries no recent
   pass touched go (superseded versions of edited items), and a table
   still half full doubles its bound, so a live set past it (a huge
   program, many editors) can't empty the table every pass */
type t('k, 'v) = {
  tbl: Hashtbl.t('k, ('v, ref(int))),
  mutable bound: int,
  mutable pass: int,
};
let mk = (~bound=4096, ()): t(_) => {
  tbl: Hashtbl.create(256),
  bound,
  pass: 0,
};
/* one pass over an editor's items; several editors take turns */
let recent = 64;
let next_pass = (t: t(_)): unit => t.pass = t.pass + 1;
let find_opt = (t: t('k, 'v), k: 'k): option('v) =>
  switch (Hashtbl.find_opt(t.tbl, k)) {
  | Some((v, seen)) =>
    seen := t.pass;
    Some(v);
  | None => None
  };
let replace = (t: t('k, 'v), k: 'k, v: 'v): unit => {
  if (Hashtbl.length(t.tbl) >= t.bound) {
    Hashtbl.filter_map_inplace(
      (_, (_, seen) as e) => seen^ > t.pass - recent ? Some(e) : None,
      t.tbl,
    );
    if (2 * Hashtbl.length(t.tbl) >= t.bound) {
      t.bound = 2 * t.bound;
    };
  };
  Hashtbl.replace(t.tbl, k, (v, ref(t.pass)));
};

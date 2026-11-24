open Poly;
/* a memo table that forgets: [tick] starts a pass, and every [every]
   passes it drops the entries no lookup reached in the last [ttl] */
type t('k, 'v) = {
  tbl: Stdlib.Hashtbl.t('k, ('v, ref(int))),
  every: int,
  ttl: int,
  mutable now: int,
};

let mk = (~size: int, ~every: int, ~ttl: int): t('k, 'v) => {
  tbl: Stdlib.Hashtbl.create(size),
  every,
  ttl,
  now: 0,
};

let tick = (t: t('k, 'v)): unit => {
  t.now = t.now + 1;
  if (t.now mod t.every == 0) {
    Stdlib.Hashtbl.filter_map_inplace(
      (_, (_, seen) as e) => seen^ < t.now - t.ttl ? None : Some(e),
      t.tbl,
    );
  };
};

/* a lookup reaches the entry even if the caller then replaces it */
let find_opt = (t: t('k, 'v), k: 'k): option('v) =>
  switch (Stdlib.Hashtbl.find_opt(t.tbl, k)) {
  | Some((v, seen)) =>
    seen := t.now;
    Some(v);
  | None => None
  };

let replace = (t: t('k, 'v), k: 'k, v: 'v): unit =>
  Stdlib.Hashtbl.replace(t.tbl, k, (v, ref(t.now)));

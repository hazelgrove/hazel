open Sexplib.Std;

/* Zippered finite map over nats, used with Z expressions
 * i.e. there is a selected element of type Z and the rest is a int map of type A */
[@deriving sexp]
type t('a, 'z) = (IntMap.t('a), (int, 'z));
let make = (m: IntMap.t('a), (n, _) as nz: (int, 'z)): option(t('a, 'z)) =>
  switch (IntMap.lookup(m, n)) {
  | Some(_) => None
  | None => Some((m, nz))
  };
let erase = (zmap: t('a, 'z), erase: 'z => 'a) => {
  let (map', (n, z)) = zmap;
  IntMap.insert_or_update(map', (n, erase(z)));
};
let prj_map = ((map, _): t('a, 'z)): IntMap.t('a) => map;
let prj_z_kv = (zmap: t('a, 'z)): (int, 'z) => {
  let (_, nz) = zmap;
  nz;
};
let prj_z_v = (zmap: t('a, 'z)): 'z => {
  let (_, (_, z)) = zmap;
  z;
};

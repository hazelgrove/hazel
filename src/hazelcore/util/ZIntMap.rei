/* Zippered finite map over nats, used with Z expressions
 * i.e. there is a selected element of type Z and the rest is a int map of type A */
[@deriving (sexp, show)]
type t('a, 'z) = (IntMap.t('a), (int, 'z));
let mk: (IntMap.t('a), (int, 'z)) => option(t('a, 'z));
let erase: (t('a, 'z), 'z => 'a) => IntMap.t('a);
let prj_map: t('a, 'z) => IntMap.t('a);
let prj_z_kv: t('a, 'z) => (int, 'z);
let prj_z_v: t('a, 'z) => 'z;

[@deriving sexp]
type t('z, 'a) = (list('a), 'z, list('a));

let singleton: 'z => t('z, 'a);

let join: t('a, 'a) => list('a);

let replace_z: ('z, t('z, 'a)) => t('z, 'a);

let map_z: ('z1 => 'z2, t('z1, 'x)) => t('z2, 'x);

let map: ('y => 'z, 'a => 'b, t('y, 'a)) => t('z, 'b);

let mapi: ((int, 'y) => 'z, (int, 'a) => 'b, t('y, 'a)) => t('z, 'b);

let optmap_z: ('z1 => option('z2), t('z1, 'a)) => option(t('z2, 'a));

let prj: t('z, 'a) => (list('a), 'z, list('a));

let prj_prefix: t('z, 'a) => list('a);

let prj_z: t('z, 'a) => 'z;

let prj_suffix: t('z, 'a) => list('a);

let prefix_length: t('z, 'a) => int;

let suffix_length: t('z, 'a) => int;

let length: t('z, 'a) => int;

let erase: (t('z, 'a), 'z => 'a) => list('a);

let shift_next: t('a, 'a) => option(t('a, 'a));

let shift_prev: t('a, 'a) => option(t('a, 'a));

let shift_end: t('a, 'a) => t('a, 'a);

let shift_begin: t('a, 'a) => t('a, 'a);

let shift_to: (int, t('a, 'a)) => option(t('a, 'a));

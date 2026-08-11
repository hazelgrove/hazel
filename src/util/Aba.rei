[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t('a, 'b) = (list('a), list('b));
let mk: (list('a), list('b)) => t('a, 'b);
let first_a: t('a, 'b) => 'a;
let last_a: t('a, 'b) => 'a;
let rev: ('a => 'a, 'b => 'b, t('a, 'b)) => t('a, 'b);
let cons: ('a, 'b, t('a, 'b)) => t('a, 'b);
let get_as: t('a, 'b) => list('a);
let get_bs: t('a, 'b) => list('b);
let hd: t('a, 'b) => 'a;
let aba_triples: t('a, 'b) => list(('a, 'b, 'a));
let map_a: ('a => 'c, t('a, 'b)) => t('c, 'b);
let map_abas: ((('a, 'b, 'a)) => 'c, t('a, 'b)) => t('a, 'c);
let trim: t('a, 'b) => option(('a, t('b, 'a), 'a));
let split: ('c => Either.t('a, 'b), list('c)) => t(list('a), 'b);
let join: ('a => 'c, 'b => 'c, t('a, 'b)) => list('c);
let fold_left: ('a => 'acc, ('acc, 'b, 'a) => 'acc, t('a, 'b)) => 'acc;
let fold_right: (('a, 'b, 'c) => 'c, 'a => 'c, t('a, 'b)) => 'c;

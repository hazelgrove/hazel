include Monads.MONAD with type t('a) := option('a);

let map2: (('a, 'b) => 'c, option('a), option('b)) => option('c);

let get: (unit => 'a, option('a)) => 'a;
let get_or_fail: (string, option('a)) => 'a;
let get_or_raise: (exn, option('a)) => 'a;

let and_then: ('a => option('b), option('a)) => option('b);

let filter: ('a => bool, option('a)) => option('a);

let sequence: list(option('a)) => option(list('a));

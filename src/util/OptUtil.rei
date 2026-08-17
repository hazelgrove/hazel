let get: (unit => 'a, option('a)) => 'a;
let get_or_fail: (string, option('a)) => 'a;
let get_or_raise: (exn, option('a)) => 'a;
let map2: (('a, 'b) => 'c, option('a), option('b)) => option('c);
let some_if: (bool, 'a) => option('a);
let zip: (option('a), option('b)) => option(('a, 'b));
let unzip: option(('a, 'b)) => (option('a), option('b));
let traverse: ('a => option('b), list('a)) => option(list('b));
let sequence: list(option('a)) => option(list('a));
let and_then: ('a => option('b), option('a)) => option('b);
let replace: ('a => option('a), 'a) => 'a;
let fold_left_opt:
  (('acc, 'a) => option('acc), list('a), 'acc) => option('acc);
let filter: ('a => bool, option('a)) => option('a);
let value_exn: (~none: exn, option('a)) => 'a;
module Syntax: {
  let ( let* ): (option('a), 'a => option('b)) => option('b);
  let (let+): (option('a), 'a => 'b) => option('b);
  let (and+): (option('a), option('b)) => option(('a, 'b));
};

let map: ('a => 'b, option('a)) => option('b);
let pure_bind: (option('a), 'a => 'b) => option('b);
let bind: (option('a), 'a => option('b)) => option('b);
let and_then: ('a => option('b), option('a)) => option('b);
let guard: bool => option(unit);
let sequence: list(option('a)) => option(list('a));
let with_default: ('a, option('a)) => 'a;
let sequence_fst: ((option('a), 'b)) => option(('a, 'b));
let sequence_snd: (('a, option('b))) => option(('a, 'b));
let filter: ('a => bool, option('a)) => option('a);

module Syntax: {
  let (let+): (option('a), 'a => 'b) => option('b);
  let ( let* ): (option('a), 'a => option('b)) => option('b);
};

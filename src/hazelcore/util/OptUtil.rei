let map2: (('a, 'b) => 'c, option('a), option('b)) => option('c);

let get: (unit => 'a, option('a)) => 'a;

let sequence: list(option('a)) => option(list('a));

module Let_syntax: {
  let map: (option('a), ~f: 'a => 'b) => option('b);
  let bind: (option('a), ~f: 'a => option('b)) => option('b);
};

module Syntax: {
  let (let+): (option('a), 'a => 'b) => option('b);
  let ( let* ): (option('a), 'a => option('b)) => option('b);
};

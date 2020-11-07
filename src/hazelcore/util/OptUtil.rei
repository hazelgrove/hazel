let map2: (('a, 'b) => 'c, option('a), option('b)) => option('c);

let get: (unit => 'a, option('a)) => 'a;

let and_then: ('a => option('b), option('a)) => option('b);

let filter: ('a => bool, option('a)) => option('a);

let sequence: list(option('a)) => option(list('a));

module Let_syntax: {
  let map: (option('a), ~f: 'a => 'b) => option('b);
  let bind: (option('a), ~f: 'a => option('b)) => option('b);
};

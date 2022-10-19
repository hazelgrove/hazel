let map: ('a => 'b, result('a, 'e)) => result('b, 'e);
let pure_bind: (result('a, 'e), 'a => 'b) => result('b, 'e);
let bind: (result('a, 'e), 'a => result('b, 'e)) => result('b, 'e);
let and_then: ('a => result('b, 'e), result('a, 'e)) => result('b, 'e);
let guard: ('e, bool) => result(unit, 'e);
let sequence: list(result('a, 'e)) => result(list('a), 'e);
let to_option: result('a, 'e) => option('a);
let with_default: ('a, result('a, 'e)) => 'a;
let unwrap: ('a => 'b, 'e => 'b, result('a, 'e)) => 'b;

module Syntax: {
  let (let+): (result('a, 'e), 'a => 'b) => result('b, 'e);
  let ( let* ): (result('a, 'e), 'a => result('b, 'e)) => result('b, 'e);
};

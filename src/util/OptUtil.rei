/* Option helpers that Stdlib.Option lacks. */

let get_or_fail: (string, option('a)) => 'a;
let get_or_raise: (exn, option('a)) => 'a;
let unzip: option(('a, 'b)) => (option('a), option('b));
/** `None` if the function gives `None` for any element. */
let traverse: ('a => option('b), list('a)) => option(list('b));
/** The function's result, or the argument unchanged where it gives `None`. */
let replace: ('a => option('a), 'a) => 'a;
let fold_left_opt:
  (('acc, 'a) => option('acc), list('a), 'acc) => option('acc);
module Syntax: {
  let ( let* ): (option('a), 'a => option('b)) => option('b);
  let (let+): (option('a), 'a => 'b) => option('b);
  let (and+): (option('a), option('b)) => option(('a, 'b));
};

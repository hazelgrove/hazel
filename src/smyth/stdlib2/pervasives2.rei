let identity: 'a => 'a;

let (<<): ('b => 'c, 'a => 'b, 'a) => 'c;
let (>>): ('a => 'b, 'b => 'c, 'a) => 'c;

let curry: ((('a, 'b)) => 'c, 'a, 'b) => 'c;
let uncurry: (('a, 'b) => 'c, ('a, 'b)) => 'c;

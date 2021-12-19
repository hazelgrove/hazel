let swap: (('a, 'b)) => ('b, 'a);

let flip: (('a, 'b) => 'c, 'b, 'a) => 'c;

let map2: ('x => 'y, ('x, 'x)) => ('y, 'y);

let uncurry: (('a, 'b) => 'c, ('a, 'b)) => 'c;
let curry: ((('a, 'b)) => 'c, 'a, 'b) => 'c;

let uncurry3: (('a, 'b, 'c) => 'd, ('a, 'b, 'c)) => 'd;
let curry3: ((('a, 'b, 'c)) => 'd, 'a, 'b, 'c) => 'd;

let bimap: ('a => 'b, 'c => 'd, ('a, 'c)) => ('b, 'd);

let map_left: ('a => 'b, ('a, 'c)) => ('b, 'c);
let map_right: ('b => 'c, ('a, 'b)) => ('a, 'c);

let apply_left: ('a => 'b, ('a, 'c)) => 'b;
let apply_right: ('a => 'b, ('c, 'a)) => 'b;

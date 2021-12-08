let swap: (('a, 'b)) => ('b, 'a);

let map2: ('x => 'y, ('x, 'x)) => ('y, 'y);

let bimap: ('a => 'b, 'c => 'd, ('a, 'c)) => ('b, 'd);

let uncurry: (('a, 'b) => 'c, ('a, 'b)) => 'c;

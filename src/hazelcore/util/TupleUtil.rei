let swap: (('a, 'b)) => ('b, 'a);

let map2: ('x => 'y, ('x, 'x)) => ('y, 'y);

let uncurry: (('a, 'b) => 'c, ('a, 'b)) => 'c;

let bimap: ('a => 'b, 'c => 'd, ('a, 'c)) => ('b, 'd);

let map_left: ('a => 'b, ('a, 'c)) => ('b, 'c);

let map_right: ('b => 'c, ('a, 'b)) => ('a, 'c);

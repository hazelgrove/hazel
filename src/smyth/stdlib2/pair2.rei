let pair: ('a, 'b) => ('a, 'b);
let map_fst: ('a => 'c, ('a, 'b)) => ('c, 'b);
let map_snd: ('b => 'c, ('a, 'b)) => ('a, 'c);
let lift_snd_result: (('a, result('b, 'e))) => result(('a, 'b), 'e);

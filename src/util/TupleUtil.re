let swap = ((a, b)) => (b, a);

let map2 = (f, (a, b)) => (f(a), f(b));

let map3 = (f, (a, b, c)) => (f(a), f(b), f(c));

let bimap_r = (f: 'b => 'c, (a: 'a, b: 'b)): ('a, 'c) => (a, f(b));

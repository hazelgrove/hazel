let swap = ((x, y)) => (y, x);

let flip = (f, x1, x2) => f(x2, x1);

let map2 = (f, (x1, x2)) => (f(x1), f(x2));

let uncurry = (f, (x1, x2)) => f(x1, x2);
let curry = (f, x1, x2) => f((x1, x2));

let uncurry3 = (f, (x1, x2, x3)) => f(x1, x2, x3);
let curry3 = (f, x1, x2, x3) => f((x1, x2, x3));

let bimap = (f, g, (x1, x2)) => (f(x1), g(x2));

let map_left = (f, (x1, x2)) => (f(x1), x2);
let map_right = (f, (x1, x2)) => (x1, f(x2));

let apply_left = (f, (x, _)) => f(x);
let apply_right = (f, (_, x)) => f(x);

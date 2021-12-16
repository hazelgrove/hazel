let swap = ((x, y)) => (y, x);

let map2 = (f, (x1, x2)) => (f(x1), f(x2));

let uncurry = (f, (x1, x2)) => f(x1, x2);

let bimap = (f, g, (x1, x2)) => (f(x1), g(x2));

let map_left = (f, (x1, x2)) => (f(x1), x2);

let map_right = (f, (x1, x2)) => (x1, f(x2));

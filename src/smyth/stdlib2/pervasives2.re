let identity = x => x;

let (<<) = (f, g, x) => f(g(x));

let (>>) = (f, g, x) => g(f(x));

let curry = (f, x, y) => f((x, y));

let uncurry = (f, (x, y)) => f(x, y);

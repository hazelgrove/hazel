let uncurry = (f, (a, b)) => f(a, b);

let curry = (f, a, b) => f((a, b));

let swap = ((a, b)) => (b, a);

let pair = (a, b) => (a, b);

let double = a => (a, a);

let apply = ((f, g), (a, b)) => (f(a), g(b));

let map2 = a => apply(double(a));

let map3 = (f, (a, b, c)) => (f(a), f(b), f(c));

let map2_bin = f => Fun.compose(apply, map2(f));
let apply_bin = fg => Fun.compose(apply, apply(fg));

let rec repeat = (n: int, f: 'x => 'x, x: 'x): 'x =>
  n <= 0 ? x : repeat(n - 1, f, f(x));

let force_opt = (f: 'x => option('y), x: 'x): 'y =>
  switch (f(x)) {
  | None => failwith("FunUtil.force_opt")
  | Some(y) => y
  };

let curry2 = (f, x, y) => f((x, y));

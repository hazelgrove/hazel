let force_opt = opt =>
  switch (opt) {
  | Some(x) => x
  | _ => assert(false)
  };

let compose = (f, g, x) => f(g(x));

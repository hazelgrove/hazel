let pair = (x, y) => (x, y);

let map_fst = (f, (x, y)) => (f(x), y);

let map_snd = (f, (x, y)) => (x, f(y));

let lift_snd_result = ((x, r)) =>
  switch (r) {
  | Ok(y) => Ok((x, y))
  | Error(e) => Error(e)
  };

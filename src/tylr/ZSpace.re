type t = (int, Space.t);

let empty = (0, Space.empty);

let is_extreme = (d: Dir.t, (z, s): t) =>
  d == L && z == 0 || d == R && z == Space.length(s);

let extreme_z = ((z, s): t): option(Dir.t) =>
  if (z == 0) {
    Some(L);
  } else if (z == Space.length(s)) {
    Some(R);
  } else {
    None;
  };

let move = (d: Dir.t, (z, s): t): option(t) =>
  switch (d) {
  | L when z > 0 => Some((z - 1, s))
  | R when z < Space.length(s) => Some((z + 1, s))
  | _ => None
  };

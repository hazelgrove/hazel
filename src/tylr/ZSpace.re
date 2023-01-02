type t = (int, Space.t);

let empty = (0, Space.empty);

let move = (d: Dir.t, (z, s): t): option(t) =>
  switch (d) {
  | L when z > 1 => Some((z - 1, s))
  | R when z < List.length(s) - 1 => Some((z + 1, s))
  | _ => None
  };
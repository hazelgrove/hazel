type t = (Piece.Z.t, Piece.t);

let zip: t => Piece.t = snd;

let move = (d: Dir.t, (z, p): t): option(t) =>
  switch (d) {
  | L when z > 0 => Some((z - 1, p))
  | R when z < Piece.length(p) => Some((z + 1, p))
  | _ => None
  };

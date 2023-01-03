type t = (Chain.Z.t, Chain.t);

let zpiece = ((z, c): t) => {
  let (p_idx, z) = z;
  let p = List.nth(Aba.get_bs(c), p_idx);
  (z, p);
};

let split = (((p_idx, z), c): t): (Chain.t, ZPiece.t, Chain.t) =>
  failwith("todo");

let move = (d: Dir.t, ((p, _), c) as zc: t): option(t) =>
  ZPiece.move(d, zpiece(zc)) |> Option.map(((z, _)) => ((p, z), c));

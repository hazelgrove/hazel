type t =
  | Typ(Term_typ.tile)
  | Pat(Term_pat.tile)
  | Exp(Term_exp.tile);

let get = (get_typ, get_pat, get_exp) =>
  fun
  | Typ(tile) => get_typ(tile)
  | Pat(tile) => get_pat(tile)
  | Exp(tile) => get_exp(tile);

let enter = (_, _): (Shard.t, AltListFrame.aframe(Shard.t, Term.t)) =>

type t = Tile_base.t(op, pre, post, bin) // individual tile
and s = (Skel.t, list(t)) // sequence of tiles with associated skeleton
and op =
  | OpHole(MetaVar.t) // expression operand hole
  | OpText(OpText.t) // handles variables, constant literals
  | ListNil // []
  | Paren(s) // (e)
  | Fun(Tile_pat.s, s)
  | Inj(InjSide.t, t)
  | Case(s, rules)
and rules = list(rule)
and rule = (Tile_pat.s, Tile_exp.s)
and pre =
  | Let(Tile_pat.s, s)
and post =
  | Ap(Tile_exp.s)
and bin =
  | BinHole(MetaVar.t)
  | BinText(BinText.t);
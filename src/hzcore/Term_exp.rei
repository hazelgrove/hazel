type t = Term_base.t(tile)
and tile = Tile_base.t(op, pre, post, bin) // individual tile
// TODO add grout to list(t)
// maybe just list(grout_or(t))
and op =
  | OpHole(MetaVar.t) // expression operand hole
  | OpText(OpText_exp.t) // handles variables, constant literals
  // TODO start working in list literals
  | ListNil // []
  | Paren(t) // (e)
  | Inj(InjSide.t, t)
  | Case(t, rules)
and rules = list(rule)
and rule = (Term_pat.s, t)
and pre =
  | Fun(Term_pat.t)
  | Let(Term_pat.t, t)
and post =
  | Ap(s)
and bin =
  | BinHole(MetaVar.t)
  | BinText(BinText_exp.t(Operators_Exp.t));

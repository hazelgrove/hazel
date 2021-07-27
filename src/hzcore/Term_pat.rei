type t = Term_base.t(tile)
and tile = Tile_base.t(op, pre, post, bin)
and op =
  | OpHole(MetaVar.t)
  | OpText(OpText_pat.t)
  // TODO list lit
  | ListNil
  | Paren(t)
  | Inj(InjSide.t, t)
and pre = unit // void
and post =
  | TypeAnn(Term_typ.t)
and bin =
  | BinHole(MetaVar.t)
  | BinText(BinText_pat.t);

exception Void_pre;

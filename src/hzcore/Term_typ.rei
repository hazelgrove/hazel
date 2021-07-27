type t = Term_base.t(tile)
and tile = Tile_base.t(op, pre, post, bin)
and op =
  | OpHole
  | OpText(OpText_typ.t)
  | Paren(t)
  | List(t)
and pre = unit // void
and post = unit // void
and bin =
  | BinHole
  | BinText(BinText_typ.t);

exception Void_pre;
exception Void_post;

type targets = {
  primary: TileId.t,
  secondary: list(TileId.t),
};
type mod =
  | Delete(TileId.t, list(TileId.t))
  | Construct(TileId.t)
  | Modify_text(TileId.t)
  | Expand_keyword(TileId.t) 
  | Insert_case_rule({
      case: TileId.t,

    })
  | Remove_case_rule({
      case: TileId.t,
      rule: int,
      pat: list(TileId.t),
    })
  | Complete_tuple({
      hd: TileId.t,
      commas: list(TileId.t),
    });
type t =
  | Move(Path.t)
  | Modify(mod, targets);
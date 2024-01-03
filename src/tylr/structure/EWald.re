include Wald;
type t = Wald.t(Piece.t, ECell.t);

let bake = Wald.rev_map(EToken.bake, ECell.bake);

// let clear = ESlot.clear_wald;

module Marked = {
  type t = Wald.t(Piece.t, EMeld.Marked.t);
};

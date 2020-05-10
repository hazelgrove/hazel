open Sexplib.Std;

[@deriving sexp]
type t = {
  title: string,
  cards: list(CardInfo.t),
};

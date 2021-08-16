open Sexplib.Std;

[@deriving (sexp, show)]
type t = {
  title: string,
  cards: list(CardInfo.t),
};

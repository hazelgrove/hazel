open Sexplib.Std;

module Ident0 = {
  [@deriving (sexp, eq, ord)]
  type t = string;

  let v = str => str;
  let of_string = v;
  let to_string = ident => ident;

  let length = String.length;

  let concat = (ident, ident') => ident ++ ident';
  let join = (ident, ident') => ident ++ "." ++ ident';
};

include Ident0;

module Map = Util.MapSexp.Make(Ident0);

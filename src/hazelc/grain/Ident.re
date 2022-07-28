open Sexplib.Std;

module Inner: {
  [@deriving sexp]
  type t;

  let v: string => t;
  let to_string: t => string;
  let of_string: string => t;
  let map: (string => string, t) => t;

  let concat: (t, t) => t;
  let join: (t, t) => t;
} = {
  [@deriving sexp]
  type t = string;

  let v = str => str;
  let to_string = ident => ident;
  let of_string = str => str;
  let map = (f, ident) => ident |> to_string |> f |> of_string;

  let concat = (ident, ident') => ident ++ ident';
  let join = (ident, ident') => ident ++ "." ++ ident';
};

[@deriving sexp]
type t = Inner.t;

let v = Inner.v;
let to_string = Inner.to_string;
let of_string = Inner.of_string;
let map = Inner.map;

let concat = Inner.concat;
let join = Inner.join;

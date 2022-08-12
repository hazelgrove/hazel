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
module Set = Util.SetSexp.Make(Ident0);

module NumberedGen = {
  [@deriving sexp]
  type t = {
    prefix: Ident0.t,
    named_delim: Ident0.t,
    count: int,
  };

  let init = (~prefix=Ident0.v("t"), ~named_delim=Ident0.v("_"), ()) => {
    prefix,
    named_delim,
    count: 0,
  };

  let next = ({prefix, count, _} as t_gen) => {
    let name = Ident0.concat(prefix, Ident0.v(string_of_int(count)));
    (name, {...t_gen, count: count + 1});
  };

  let next_named = (x, {named_delim, _} as t_gen) => {
    let (suffix, t_gen) = next(t_gen);
    let name = Ident0.concat(Ident0.concat(x, named_delim), suffix);
    (name, t_gen);
  };
};

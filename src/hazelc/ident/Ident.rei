module Ident0: {
  [@deriving (sexp, eq, ord)]
  type t;

  let v: string => t;
  let of_string: string => t;
  let to_string: t => string;

  let length: t => int;

  let concat: (t, t) => t;
  let join: (t, t) => t;
};

include  (module type of Ident0) with type t = Ident0.t;

module Map: (module type of Util.MapSexp.Make(Ident0));
module Set: (module type of Util.SetSexp.Make(Ident0));

module NumberedGen: {
  /**
    The type for the numbered identifier generator.
   */
  [@deriving sexp]
  type t;

  /**
    [init ~prefix ~named_delim ()] is a numbered identifier generator.
   */
  let init: (~prefix: Ident0.t=?, ~named_delim: Ident0.t=?, unit) => t;

  /**
    [next gen] is an identifier prefixed by [~prefix] (which was given at
    initialization).
   */
  let next: t => (Ident0.t, t);

  /**
    [next_named x gen] is an identifier of the format [x] [~named_delim] [(next
    gen)], with spaces removed.
   */
  let next_named: (Ident0.t, t) => (Ident0.t, t);
};

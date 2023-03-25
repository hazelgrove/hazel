open Util;

// todo: consider just reusing Tile.t
[@deriving (show({with_path: false}), sexp, yojson)]
type t = {proto: Proto.t};
let mk = mold => {proto: Proto.Grout.mk(mold)};

// todo: remove sort parameter and always make min sort
let mk_operand = (sort: Sort.o) => mk(Mold.mk(sort, Prec.max));
let mk_prefix = (~r=?, sort, prec) => mk(Mold.mk_prefix(sort, prec, ~r?));
let mk_postfix = (~l=?, sort, prec) => mk(Mold.mk_postfix(~l?, sort, prec));
let mk_infix = (sort, prec) => mk(Mold.mk_infix(sort, prec));

// todo: incorporate unique filling
let length = _ => 1;

// let has_sugg = g => g.sugg != "";
// let is_hole = g => !has_sugg(g);

// let suggestion = g =>
//   switch (LangUtil.tokens_of_mold(g.mold)) {
//   | [Const(t)] when Mold.must_match(L, g.mold) || Mold.must_match(R, g.mold) => t
//   | _ => ""
//   };

let uncons_char = (g: t) =>
  Proto.uncons_char(g.proto) |> Option.map(((l, r)) => (mk(l), mk(r)));
let unsnoc_char = (g: t) =>
  Proto.unsnoc_char(g.proto) |> Option.map(((l, r)) => (mk(l), mk(r)));

let zip = (l, r) => mk(Proto.zip(l, r));
let unzip = (n, g): Either.t(Dir.t, (t, t)) =>
  switch (n) {
  | 0 => L(L)
  // todo: unicode length
  | _ when n >= String.length(g.filled) => L(R)
  | _ =>
    let (l, r) = Token.split(n, g.filled);
    R(({...g, filled: l}, {...g, filled: r}));
  };

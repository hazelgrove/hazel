open Sexplib.Std;
open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  id: Id.t,
  mold: Mold.t,
  prefix: string,
};

let mk = (~id=?, ~prefix="", mold) => {
  let id = id |> OptUtil.get(() => Id.Gen.next());
  {id, mold, prefix};
};

let mk_convex = (~id=?, sort: Sort.o) => mk(~id?, Mold.mk(sort, Prec.max));

let mk_concave = (~id=?, l: Mold.t, r: Mold.t) => {
  let sort = Sort.lca(l.sort, r.sort);
  let prec = min(l.prec, r.prec);
  mk(~id?, Mold.mk_infix(sort, prec));
};

// todo: incorporate unique filling
let length = _ => 1;

let suggestion = g =>
  switch (LangUtil.tokens_of_mold(g.mold)) {
  | [Const(t)] when Mold.must_match(L, g.mold) || Mold.must_match(R, g.mold) => t
  | _ => ""
  };

let uncons_char = (g: t): option((t, t)) =>
  StringUtil.uncons(g.prefix)
  |> Option.map(((hd, tl)) => ({...g, prefix: hd}, {...g, prefix: tl}));
let unsnoc_char = (g: t): option((t, t)) =>
  StringUtil.unsnoc(g.prefix)
  |> Option.map(((tl, hd)) => ({...g, prefix: tl}, {...g, prefix: hd}));

let unzip = (n, g): Either.t(Dir.t, (t, t)) =>
  switch (n) {
  | 0 => L(L)
  // todo: unicode length
  | _ when n > String.length(g.prefix) => L(R)
  | _ =>
    let (l, r) = Token.split(n, g.prefix);
    R(({...g, prefix: l}, {...g, prefix: r}));
  };

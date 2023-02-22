open Util;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  id: Id.t,
  mold: Mold.t,
  sugg: Token.t,
  fill: Token.t,
};

let mk = (~id=?, ~sugg="", ~fill="", mold) => {
  let id = id |> OptUtil.get(() => Id.Gen.next());
  {id, mold, sugg, fill};
};

let mk_convex = (~id=?, sort: Sort.o) => mk(~id?, Mold.mk(sort, Prec.max));

let mk_concave = (~id=?, sort, prec) => mk(~id?, Mold.mk_infix(sort, prec));

// todo: incorporate unique filling
let length = _ => 1;

let has_sugg = g => g.sugg != "";

// let suggestion = g =>
//   switch (LangUtil.tokens_of_mold(g.mold)) {
//   | [Const(t)] when Mold.must_match(L, g.mold) || Mold.must_match(R, g.mold) => t
//   | _ => ""
//   };

let uncons_char = (g: t): option((t, t)) =>
  StringUtil.uncons(g.fill)
  |> OptUtil.and_then(((hd, tl)) =>
       tl == "" ? None : Some(({...g, fill: hd}, {...g, fill: tl}))
     );
let unsnoc_char = (g: t): option((t, t)) =>
  StringUtil.unsnoc(g.fill)
  |> OptUtil.and_then(((tl, hd)) =>
       tl == "" ? None : Some(({...g, fill: tl}, {...g, fill: hd}))
     );

let zip = (l, r): option(t) =>
  Id.eq(l.id, r.id) ? Some({...l, fill: l.fill ++ r.fill}) : None;

let unzip = (n, g): Either.t(Dir.t, (t, t)) =>
  switch (n) {
  | 0 => L(L)
  // todo: unicode length
  | _ when n > String.length(g.fill) => L(R)
  | _ =>
    let (l, r) = Token.split(n, g.fill);
    R(({...g, fill: l}, {...g, fill: r}));
  };

open Sexplib.Std;
open Util;

[@deriving (show({with_path: false}), sexp, yojson, ord)]
type t('a) = list(RFrame.t('a));

let empty: t(_) = [];

let push_s = (~onto: Dir.t, s: Regex.s(_), ctx: t(_)): t(_) =>
  switch (s) {
  | [] => ctx
  | [_, ..._] as s =>
    switch (onto, ctx) {
    | (L, [Seq_(ls, rs), ...ctx]) => [Seq_(s @ ls, rs), ...ctx]
    | (R, [Seq_(ls, rs), ...ctx]) => [Seq_(ls, s @ rs), ...ctx]
    | (L, _) => [Seq_(s, []), ...ctx]
    | (R, _) => [Seq_([], s), ...ctx]
    }
  };
let push = (~onto, r) => push_s(~onto, Regex.flatten(r));

let nullable = (side: Dir.t) => List.for_all(RFrame.nullable(side));

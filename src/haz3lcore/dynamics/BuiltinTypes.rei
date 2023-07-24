[@deriving (show({with_path: false}), sexp, yojson)]
type t = VarMap.t_(Typ.t);

let ctx: (t, Ctx.t) => Ctx.t;

let using: (Var.t, Typ.t, t) => t;

module Pervasives: {let builtin_types: t;};

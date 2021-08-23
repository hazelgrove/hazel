[@deriving sexp]
type mode =
  | Syn
  | Ana(Type.t)
  | Fn_pos;

[@deriving sexp]
type t = {
  ctx: Ctx.t,
  mode,
};

let assigned_hole_type: t => Type.t;

let inconsistent: (Type.t, t) => bool;

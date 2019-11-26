[@deriving sexp]
type t =
  | Hole(MetaVar.t)
  | Var(Var.t);

/* TODO (charles for david): are there any child indices for a tpat? What's a child index? */
let child_indices = _ => [];

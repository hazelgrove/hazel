include (module type of Ident.Map);

/**
  Context of variable remappings (e.g. x -> t124_x).
 */
[@deriving sexp]
type t = Ident.Map.t(Ident.t);

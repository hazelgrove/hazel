module HoleReason = {
  [@deriving sexp]
  type t =
    | Unbound
    | Reserved;
};

[@deriving sexp]
type t =
  | NotInTyVarHole
  | InHole(HoleReason.t, MetaVar.t);

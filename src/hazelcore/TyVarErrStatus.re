module HoleReason = {
  [@deriving sexp]
  type t =
    | Unbound
    | Reserved
    | InvalidName;
};

[@deriving sexp]
type t =
  | NotInTyVarHole
  | InHole(HoleReason.t, MetaVar.t);

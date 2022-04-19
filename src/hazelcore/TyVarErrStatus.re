module HoleReason = {
  [@deriving sexp]
  type t =
    | Unbound
    | Reserved
    | InvalidName;
};

[@deriving sexp]
type t =
  | NotInTyVarHole(Index.t(Index.abs))
  | InHole(HoleReason.t, MetaVar.t);

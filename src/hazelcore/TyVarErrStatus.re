module HoleReason = {
  [@deriving sexp]
  type t =
    | Unbound
    | Reserved
    | InvalidText;
};

[@deriving sexp]
type t =
  | NotInTyVarHole
  | InHole(HoleReason.t, MetaVar.t);

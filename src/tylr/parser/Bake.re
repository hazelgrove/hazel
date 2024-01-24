open Util;

type t = Chain.t(Rel.t(Cell.t, Cell.t), Token.t);

let height = Chain.length;

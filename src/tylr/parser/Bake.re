open Util;

type t = Chain.t(Cell.Base.t(Walk.Stride.t), Token.t);

let height = Chain.length;

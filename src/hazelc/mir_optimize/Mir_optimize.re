open Sexplib.Std;

open Mir_anf;

[@deriving sexp]
type opts = unit;

let passes = [];

let optimize = (~opts, block: block): block => {
  List.fold_left((block, pass) => pass(opts, block), block, passes);
};

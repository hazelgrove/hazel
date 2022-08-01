/**
  Re-export label library.
 */
module Label_ = Label;

/**
  Re-export anf types.
 */
module Typ = Typ;
[@deriving sexp]
type typ = Typ.t;

module Complete = Complete;
module Label = AnfLabel;

include  (module type of Anf) with type block = Anf.block;

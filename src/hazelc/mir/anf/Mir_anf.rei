/**
  Re-export label library.
 */
module Label_ = Label;

/**
  Re-export anf types.
 */
module Completeness = Completeness;
module Label = AnfLabel;

include  (module type of Anf) with type prog = Anf.prog;

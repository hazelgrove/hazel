/**
  Re-export label library.
 */
module Label_ = Label;

/**
  Re-export anf types.
 */
module Complete = Complete;
module Label = AnfLabel;

include  (module type of Anf) with type block = Anf.block;

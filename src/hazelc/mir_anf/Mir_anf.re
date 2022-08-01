module Label_ = Label;

module Typ = Typ;
[@deriving sexp]
type typ = Typ.t;

module Complete = Complete;
module Label = AnfLabel;
include Anf;

module Typ = Typ;
[@deriving sexp]
type typ = Typ.t;

module Complete = Complete;

module Pat = Pat;
[@deriving sexp]
type pat = Pat.t;

module Sigma = Sigma;

module Delta = Delta;
[@deriving sexp]
type delta = Delta.t;

module Syn = Syn;
let syn: (TypContext.t, Delta.t, Anf.block) => Syn.syn_result;

[@deriving sexp]
type syn_types = Syn.syn_types;
[@deriving sexp]
type syn_ok = Syn.syn_ok;
[@deriving sexp]
type syn_error = Syn.syn_error;
[@deriving sexp]
type syn_result = Syn.syn_result;

include  (module type of Anf) with type block = Anf.block;

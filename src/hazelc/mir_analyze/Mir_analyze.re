module Completes = Completes;
[@deriving sexp]
type completes = Completes.t;

module CompletesAnalysis = CompletesAnalysis;
[@deriving sexp]
type completes_level = CompletesAnalysis.level;
[@deriving sexp]
type completes_opts = CompletesAnalysis.opts;

[@deriving sexp]
type opts = {completes: completes_opts};

let completes_analyze = CompletesAnalysis.analyze;

open Mir_anf;

module CompletesAnalysis = CompletesAnalysis;
[@deriving sexp]
type completes_level = CompletesAnalysis.level;
[@deriving sexp]
type completes_opts = CompletesAnalysis.opts;

[@deriving sexp]
type opts = {completes: completes_opts};

let completes_analyze:
  (~opts: completes_opts, block) => (Complete.t, Completes.t);

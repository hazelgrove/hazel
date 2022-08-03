open Mir_anf;

[@deriving sexp]
type level =
  | NoAnalysis
  | LocalAnalysis
  | GlobalAnalysis;

[@deriving sexp]
type opts = {level};

let no_analyze = _block => (
  Complete.IndeterminatelyIncomplete,
  Completes.empty,
);
let local_analyze = LocalCompletesAnalysis.analyze;
let global_analyze = LocalCompletesAnalysis.analyze;

let analyze = (~opts, block) => {
  switch (opts.level) {
  | NoAnalysis => block |> no_analyze
  | LocalAnalysis => block |> local_analyze
  | GlobalAnalysis => block |> global_analyze
  };
};

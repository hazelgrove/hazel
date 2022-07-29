[@deriving sexp]
type level =
  | NoAnalysis
  | LocalAnalysis
  | GlobalAnalysis;

[@deriving sexp]
type opts = {level};

let no_analyze = prog => prog;
let local_analyze = LocalIndetAnalysis.analyze;
let global_analyze = LocalIndetAnalysis.analyze;

let analyze = (~opts, prog) => {
  switch (opts.level) {
  | NoAnalysis => prog |> no_analyze
  | LocalAnalysis => prog |> local_analyze
  | GlobalAnalysis => prog |> global_analyze
  };
};

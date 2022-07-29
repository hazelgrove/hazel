[@deriving sexp]
type level =
  | NoAnalysis
  | LocalAnalysis
  | GlobalAnalysis;

[@deriving sexp]
type opts = {level};

let no_analyze = block => block;
let local_analyze = LocalIndetAnalysis.analyze;
let global_analyze = LocalIndetAnalysis.analyze;

let analyze = (~opts, block) => {
  switch (opts.level) {
  | NoAnalysis => block |> no_analyze
  | LocalAnalysis => block |> local_analyze
  | GlobalAnalysis => block |> global_analyze
  };
};

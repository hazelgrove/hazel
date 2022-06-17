[@deriving sexp]
type level =
  | NoAnalysis
  | LocalAnalysis
  | GlobalAnalysis;

[@deriving sexp]
type opts = {level};

let analyze = (~opts, prog) => {
  switch (opts.level) {
  | NoAnalysis => prog
  | LocalAnalysis => prog |> LocalIndetAnalysis.analyze
  | GlobalAnalysis =>
    prog |> LocalIndetAnalysis.analyze |> LamIndetAnalysis.analyze
  };
};

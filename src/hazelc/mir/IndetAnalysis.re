[@deriving sexp]
type level =
  | NoAnalysis
  | LocalAnalysis;

[@deriving sexp]
type opts = {level};

let analyze = (~opts, prog) => {
  switch (opts.level) {
  | NoAnalysis => prog
  | LocalAnalysis => LocalIndetAnalysis.analyze(prog)
  };
};

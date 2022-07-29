module IndetAnalysis = IndetAnalysis;

open Mir_anf;

[@deriving sexp]
type opts = {indet_analysis: IndetAnalysis.opts};

let passes = [opts => IndetAnalysis.analyze(~opts=opts.indet_analysis)];

let optimize = (~opts, prog: prog): prog => {
  List.fold_left((prog, pass) => pass(opts, prog), prog, passes);
};

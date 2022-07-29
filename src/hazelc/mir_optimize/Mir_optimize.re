module IndetAnalysis = IndetAnalysis;

open Mir_anf;

[@deriving sexp]
type opts = {indet_analysis: IndetAnalysis.opts};

let passes = [opts => IndetAnalysis.analyze(~opts=opts.indet_analysis)];

let optimize = (~opts, block: block): block => {
  List.fold_left((block, pass) => pass(opts, block), block, passes);
};

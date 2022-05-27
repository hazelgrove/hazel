[@deriving sexp]
type opts = {indet_analysis: IndetAnalysis.opts};

let passes = [opts => IndetAnalysis.analyze(~opts=opts.indet_analysis)];

let optimize = (~opts, prog: Anf.prog): Anf.prog =>
  List.fold_left((prog, pass) => pass(opts, prog), prog, passes);

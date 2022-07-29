module Linearize = Linearize;

module Optimize = Optimize;
module IndetAnalysis = Optimize.IndetAnalysis;

module Label = Label;
module Completeness = Completeness;

module Anf = Anf;
module AnfLabel = AnfLabel;
include (module type of Anf);

let optimize: (~opts: Optimize.opts, prog) => prog;

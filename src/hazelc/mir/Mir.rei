module Linearize = Linearize;

module Optimize = Optimize;
module IndetAnalysis = Optimize.IndetAnalysis;

module Label = Label;
module Completeness = Completeness;

module Anf = Anf;
include (module type of Anf);

let optimize: (~opts: Optimize.opts, prog) => prog;

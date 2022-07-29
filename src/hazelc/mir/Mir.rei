module Linearize = Linearize;

module Optimize = Optimize;
module IndetAnalysis = Optimize.IndetAnalysis;

module Anf = Anf;

let optimize: (~opts: Optimize.opts, Anf.prog) => Anf.prog;

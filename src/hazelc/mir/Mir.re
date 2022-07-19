module Linearize = Linearize;

module Optimize = Optimize;
module IndetAnalysis = Optimize.IndetAnalysis;

module Label = Label;
module Completeness = Completeness;

module Anf = Anf;
include Anf;

let optimize = Optimize.optimize;

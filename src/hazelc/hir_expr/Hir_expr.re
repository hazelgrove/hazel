module Typ = Typ;
[@deriving (sexp, eq, ord)]
type typ = Typ.t;

module TypContext = TypContext;
[@deriving sexp]
type typ_context = TypContext.t;

module Expr = Expr;
[@deriving sexp]
type expr = Expr.t;
[@deriving sexp]
type case = Expr.case;
[@deriving sexp]
type rule = Expr.rule;

module Sigma = Sigma;
[@deriving sexp]
type sigma = Sigma.t;

module Pat = Pat;
[@deriving sexp]
type pat = Pat.t;

module Delta = Delta;
[@deriving sexp]
type delta = Delta.t;

module FreshLabels = FreshLabels;

module Syn = Syn;
let syn = Syn.syn;

[@deriving sexp]
type syn_types = Syn.syn_types;
[@deriving sexp]
type syn_ok = Syn.syn_ok;
[@deriving sexp]
type syn_error = Syn.syn_error;
[@deriving sexp]
type syn_result = Syn.syn_result;

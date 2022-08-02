module Typ = Typ;
[@deriving sexp]
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

module Pat = Pat;
[@deriving sexp]
type pat = Pat.t;

module Delta = Delta;

module Syn = Syn;
let syn = Syn.syn;

[@deriving sexp]
type types = Syn.types;
[@deriving sexp]
type syn_ok = Syn.syn_ok;
[@deriving sexp]
type syn_error = Syn.syn_error;
[@deriving sexp]
type syn_result = Syn.syn_result;

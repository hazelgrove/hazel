open Util;
open Grammar;
[@deriving (show({with_path: false}), sexp, yojson)]
type any_t = Grammar.any_t(unit);
[@deriving (show({with_path: false}), sexp, yojson)]
type exp_t = Grammar.exp_t(unit);
[@deriving (show({with_path: false}), sexp, yojson)]
type exp_term = Grammar.exp_term(unit);
[@deriving (show({with_path: false}), sexp, yojson)]
type pat_t = Grammar.pat_t(unit);
[@deriving (show({with_path: false}), sexp, yojson)]
type pat_term = Grammar.pat_term(unit);
[@deriving (show({with_path: false}), sexp, yojson)]
type typ_t = Grammar.typ_t(unit);
[@deriving (show({with_path: false}), sexp, yojson)]
type typ_term = Grammar.typ_term(unit);
[@deriving (show({with_path: false}), sexp, yojson)]
type tpat_t = Grammar.tpat_t(unit);
[@deriving (show({with_path: false}), sexp, yojson)]
type tpat_term = Grammar.tpat_term(unit);
[@deriving (show({with_path: false}), sexp, yojson)]
type rul_t = Grammar.rul_t(unit);
[@deriving (show({with_path: false}), sexp, yojson)]
type rul_term = Grammar.rul_term(unit);
[@deriving (show({with_path: false}), sexp, yojson)]
type environment_t = Grammar.environment_t(unit);
[@deriving (show({with_path: false}), sexp, yojson)]
type closure_environment_t = Grammar.closure_environment_t(unit);
[@deriving (show({with_path: false}), sexp, yojson)]
type stepper_filter_kind_t = Grammar.stepper_filter_kind_t(unit);
[@deriving (show({with_path: false}), sexp, yojson)]
type type_hole = Grammar.type_hole(unit);
[@deriving (show({with_path: false}), sexp, yojson)]
type type_provenance = Grammar.type_provenance(unit);
[@deriving (show({with_path: false}), sexp, yojson)]
type filter = Grammar.filter(unit);
[@deriving (show({with_path: false}), sexp, yojson)]
type deferral_position_t = Grammar.deferral_position_t;

// Trying to add helper functions to build terms
let lift: 'a => Annotated.t('a, unit) = term => {term, annotation: ()};

let invalid: string => exp_t = s => lift(Invalid(s): exp_term);
let empty_hole: exp_t = lift(EmptyHole: exp_term);
let multi_hole: list(any_t) => exp_t = l => lift(MultiHole(l): exp_term);
let dynamic_error_hole: (exp_t, InvalidOperationError.t) => exp_t =
  (e, err) => lift(DynamicErrorHole(e, err): exp_term);
let failed_cast: (exp_t, typ_t, typ_t) => exp_t =
  (e, t1, t2) => lift(FailedCast(e, t1, t2): exp_term);

let deferral: deferral_position_t => exp_t =
  d => lift(Deferral(d): exp_term);
let undefined: exp_t = lift(Undefined: exp_term);
let bool_lit: bool => exp_t = b => lift(Bool(b): exp_term);
let int_lit: int => exp_t = i => lift(Int(i): exp_term);
let float_lit: float => exp_t = f => lift(Float(f): exp_term);
let string_lit: string => exp_t = s => lift(String(s): exp_term);
let list_lit: list(exp_t) => exp_t = l => lift(ListLit(l): exp_term);
let constructor: (string, typ_t) => exp_t =
  (s, t) => lift(Constructor(s, t): exp_term);
let fun_: (pat_t, exp_t, option(typ_t), option(Var.t)) => exp_t =
  (p, e, t, n) => lift(Fun(p, e, t, n): exp_term);
let bin_op = (op, e1, e2) => lift(BinOp(op, e1, e2): exp_term);

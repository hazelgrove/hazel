[@deriving sexp]
type instance_report = (DHExp.t, TestStatus.t);

[@deriving sexp]
type report = (KeywordID.t, list(instance_report));

[@deriving sexp]
type t = list(report);

let empty: t;

let lookup: (KeywordID.t, t) => option(list(instance_report));
let lookup_and_join: (KeywordID.t, t) => TestStatus.t;

let extend: ((KeywordID.t, instance_report), t) => t;

let count: t => int;
let count_status: (TestStatus.t, t) => int;

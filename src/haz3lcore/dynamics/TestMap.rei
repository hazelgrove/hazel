[@deriving (show({with_path: false}), sexp, yojson)]
type instance_report = (DHExp.t, TestStatus.t);

let joint_status: list(instance_report) => TestStatus.t;

[@deriving (show({with_path: false}), sexp, yojson)]
type report = (Id.t, list(instance_report));

[@deriving (show({with_path: false}), sexp, yojson)]
type t = list(report);

let empty: t;

let lookup: (Id.t, t) => option(list(instance_report));
let lookup_and_join: (Id.t, t) => TestStatus.t;

let extend: ((Id.t, instance_report), t) => t;

let count: t => int;
let count_status: (TestStatus.t, t) => int;

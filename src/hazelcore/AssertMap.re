open Sexplib.Std;

[@deriving sexp]
type assert_instance_report = (DHExp.t, AssertStatus.t);

[@deriving sexp]
type assert_report = (KeywordID.t, list(assert_instance_report));

[@deriving sexp]
type t = list(assert_report);
let empty: t = [];

let lookup = List.assoc_opt;

let extend =
    ((id, report): (KeywordID.t, assert_instance_report), assert_map: t): t => {
  switch (List.assoc_opt(id, assert_map)) {
  | Some(a) => [(id, a @ [report]), ...assert_map]
  | None => [(id, [report]), ...assert_map]
  };
};

let join_statuses: list(assert_instance_report) => AssertStatus.t =
  reports => AssertStatus.join_all(List.map(snd, reports));

let lookup_and_join = (n: int, assert_map: t): AssertStatus.t =>
  switch (lookup(n, assert_map)) {
  | None => Indet
  | Some(reports) => join_statuses(reports)
  };

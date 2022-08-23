open Sexplib.Std;

[@deriving sexp]
type test_instance_report = (DHExp.t, TestStatus.t, Environment.t, DHExp.t);

[@deriving sexp]
type test_report = (KeywordID.t, list(test_instance_report));

[@deriving sexp]
type t = list(test_report);
let empty: t = [];

let lookup = List.assoc_opt;

let extend =
    ((id, report): (KeywordID.t, test_instance_report), test_map: t): t => {
  switch (List.assoc_opt(id, test_map)) {
  | Some(a) => [(id, a @ [report]), ...List.remove_assoc(id, test_map)]
  | None => [(id, [report]), ...test_map]
  };
};

let joint_status: list(test_instance_report) => TestStatus.t =
  reports => TestStatus.join_all(List.map(((_, s, _, _)) => s, reports));

let lookup_and_join = (n: int, test_map: t): TestStatus.t =>
  switch (lookup(n, test_map)) {
  | None => Indet
  | Some(reports) => joint_status(reports)
  };

let count: t => int = List.length;

let count_status: (TestStatus.t, t) => int =
  (status, test_map) =>
    List.filter(
      ((_, instances)) => status == joint_status(instances),
      test_map,
    )
    |> List.length;

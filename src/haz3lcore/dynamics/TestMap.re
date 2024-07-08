open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;

/* FIXME: Make more obvious names. */
[@deriving (show({with_path: false}), sexp, yojson)]
type instance_report = (DHExp.t, TestStatus.t);

let joint_status: list(instance_report) => TestStatus.t =
  reports => TestStatus.join_all(List.map(snd, reports));

[@deriving (show({with_path: false}), sexp, yojson)]
type report = (KeywordID.t, list(instance_report));

[@deriving (show({with_path: false}), sexp, yojson)]
type t = list(report);
let empty: t = [];

let lookup = List.assoc_opt;

let lookup_and_join = (n, test_map) =>
  switch (lookup(n, test_map)) {
  | None => TestStatus.Indet
  | Some(reports) => joint_status(reports)
  };

let extend = ((id, report), test_map) => {
  switch (List.assoc_opt(id, test_map)) {
  | Some(a) => List.remove_assoc(id, test_map) @ [(id, a @ [report])]
  | None => test_map @ [(id, [report])]
  };
};

let count = List.length;

let count_status = (status, test_map) =>
  List.filter(
    ((_, instances)) => status == joint_status(instances),
    test_map,
  )
  |> List.length;

open Util;

/* FIXME: Make more obvious names. */
[@deriving (show({with_path: false}), sexp, yojson)]
type instance_report = {
  exp: DHExp.t,
  status: TestStatus.t,
  hint: string,
};

let get_status: instance_report => TestStatus.t = report => report.status;

let joint_status: list(instance_report) => TestStatus.t =
  reports => TestStatus.join_all(List.map(~f=get_status, reports));

let get_hint: instance_report => string = report => report.hint;

let joint_hints: list(instance_report) => list(string) =
  reports => List.map(~f=get_hint, reports);

[@deriving (show({with_path: false}), sexp, yojson)]
type report = (Id.t, list(instance_report));

[@deriving (show({with_path: false}), sexp, yojson)]
type t = list(report);

let hints: list(report) => list(string) =
  reports => joint_hints(List.concat(List.map(~f=snd, reports)));

let empty: t = [];

let lookup = (id, test_map) =>
  List.Assoc.find(test_map, id, ~equal=Id.equal);

let extend = ((id, report), test_map) => {
  switch (List.Assoc.find(test_map, id, ~equal=Id.equal)) {
  | Some(a) =>
    List.Assoc.remove(test_map, id, ~equal=Id.equal) @ [(id, a @ [report])]
  | None => test_map @ [(id, [report])]
  };
};

let count = List.length;

let count_status = (status, test_map) =>
  List.filter(
    ~f=((_, instances)) => Poly.equal(status, joint_status(instances)),
    test_map,
  )
  |> List.length;

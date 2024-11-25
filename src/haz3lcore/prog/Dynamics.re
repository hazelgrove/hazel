open Util;

module Info = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {vals: list(DHExp.t)};

  let first_val = (di: t): option(DHExp.t) => ListUtil.hd_opt(di.vals);
};

module Map = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {test_map: TestMap.t};
  let empty: t = {test_map: TestMap.empty};

  let mk = (test_map: TestMap.t): t => {test_map: test_map};

  let lookup = (id: Id.t, dm: t): option(Info.t) =>
    switch (TestMap.lookup(id, dm.test_map)) {
    | None => None
    | Some(vals) => Some({vals: List.map(fst, vals)})
    };
};

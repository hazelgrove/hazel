open Util;

module Probe = {
  module Info = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = DHExp.t;
  };

  module Map = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = Id.Map.t(list(Info.t));

    let empty = Id.Map.empty;
    let lookup = Id.Map.find_opt;

    let extend = (id, report, map: t) =>
      Id.Map.update(
        id,
        opt =>
          switch (opt) {
          | Some(a) => Some(a @ [report])
          | None => Some([report])
          },
        map,
      );
  };

  let extend = ((id, report), test_map) => {
    switch (List.assoc_opt(id, test_map)) {
    | Some(a) => List.remove_assoc(id, test_map) @ [(id, a @ [report])]
    | None => test_map @ [(id, [report])]
    };
  };
};

module Info = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {vals: list(Probe.Info.t)};
};

module Map = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = {probes: Probe.Map.t};
  let empty: t = {probes: Probe.Map.empty};

  let mk = (probes: Probe.Map.t): t => {probes: probes};

  let lookup = (id: Id.t, dm: t): option(Info.t) =>
    switch (Probe.Map.lookup(id, dm.probes)) {
    | None => None
    | Some(vals) => Some({vals: vals})
    };
};

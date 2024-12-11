open Util;

module Probe = {
  let instrument =
      (m: Statics.Map.t, id: Id.t, probe_tag: Probe.tag): Probe.tag =>
    switch (probe_tag) {
    | Paren => Paren
    | Probe(_) =>
      Probe({
        refs: Statics.Map.refs_in(m, id),
        stem: Statics.Map.enclosing_abstractions(m, id),
      })
    };

  module Env = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type raw =
      | Opaque
      | Val(DHExp.t);

    [@deriving (show({with_path: false}), sexp, yojson)]
    type entry = {
      name: string,
      id: Id.t,
      raw,
    };

    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = list(entry);

    let to_raw = (d: DHExp.t) =>
      switch (d.term) {
      | Fun(_)
      | FixF(_) => Opaque
      | _ => Val(d)
      };

    let mk_entry = (env, {name, id, _}: Binding.t) =>
      switch (ClosureEnvironment.lookup(env, name)) {
      | Some(d) =>
        let raw =
          d
          |> DHExp.strip_casts
          |> Exp.substitute_closures(ClosureEnvironment.map_of(env))
          |> to_raw;
        {name, id, raw};
      | None => failwith("Probe: variable not found in environment")
      };

    let mk = (env: ClosureEnvironment.t, refs: Binding.s) =>
      List.map(mk_entry(env), refs);
  };

  module Info = {
    [@deriving (show({with_path: false}), sexp, yojson)]
    type t = {
      value: DHExp.t,
      env: Env.t,
      stack: Probe.stack,
    };

    let mk = (value: DHExp.t, env: ClosureEnvironment.t, pr: Probe.t) => {
      value,
      stack: ClosureEnvironment.stack_of(env),
      env: Env.mk(env, pr.refs),
    };
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

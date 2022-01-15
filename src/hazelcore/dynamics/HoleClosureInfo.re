open Sexplib.Std;

module EnvironmentMap = {
  include IntMap;
};

module HoleClosure = {
  [@deriving sexp]
  type t = (MetaVar.t, MetaVarInst.t);
};

/*  Each hole maps to (
            a mapping from closure numbers to (
                hole closure number,
                hole closure environment,
                list of parent hole closures
            )
        )

        This replaces HoleInstanceInfo.t
    */
[@deriving sexp]
type t =
  MetaVarMap.t(
    EnvironmentMap.t((int, Environment.t, list(HoleClosure.t))),
  );

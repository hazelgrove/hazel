open Sexplib.Std;

[@deriving sexp]
type t = MetaVarMap.t(IntMap.t((int, EvalEnv.t)));

let empty: t = IntMap.empty;

/* TODO: rename to find_opt_hole_closure */
let mem_hole_closure =
    (hcc: t, u: MetaVar.t, env: EvalEnv.t): option((int, EvalEnv.t)) => {
  switch (EvalEnv.id_of_evalenv(env)) {
  | Some(ei) =>
    switch (MetaVarMap.find_opt(u, hcc)) {
    | Some(hole_closures) => IntMap.find_opt(ei, hole_closures)
    | None => None
    }
  | None => None
  };
};

/* TODO: rename to install_hole_closure */
let get_hole_closure_id = (hcc: t, u: MetaVar.t, env: EvalEnv.t): (t, int) => {
  switch (EvalEnv.id_of_evalenv(env)) {
  | Some(ei) =>
    switch (MetaVarMap.find_opt(u, hcc)) {
    | Some(hole_closures) =>
      switch (IntMap.find_opt(ei, hole_closures)) {
      | Some((ci, _)) => (hcc, ci)
      | None =>
        let ci = IntMap.cardinal(hole_closures);
        (
          MetaVarMap.add(u, IntMap.add(ei, (ci, env), hole_closures), hcc),
          ci,
        );
      }
    | None => (
        MetaVarMap.add(u, MetaVarMap.singleton(ei, (0, env)), hcc),
        0,
      )
    }
  | None => raise(EvalEnv.InvalidEvalEnvType)
  };
};

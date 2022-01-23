/* TODO: annotate */
[@deriving sexp]
type t = MetaVarMap.t(IntMap.t((int, EvalEnv.t)));
let empty: t;
let mem_hole_closure: (t, MetaVar.t, EvalEnv.t) => option((int, EvalEnv.t));
let get_hole_closure_id: (t, MetaVar.t, EvalEnv.t) => (t, int);

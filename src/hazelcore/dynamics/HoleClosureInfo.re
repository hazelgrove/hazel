open Sexplib.Std;

[@deriving sexp]
type t = MetaVarMap.t(list(EvalEnv.t));

let empty: t = MetaVarMap.empty;

let num_unique_hcs = (hci: t, u: MetaVar.t): int => {
  switch (hci |> MetaVarMap.find_opt(u)) {
  | Some(hcs) => hcs |> List.length
  | None => 0
  };
};

let find_hc_opt =
    (hci: t, u: MetaVar.t, i: HoleClosureId.t): option(EvalEnv.t) => {
  switch (hci |> MetaVarMap.find_opt(u)) {
  | Some(hcs) => List.nth_opt(hcs, i)
  | None => None
  };
};

open Sexplib.Std;

[@deriving sexp]
type t = MetaVarMap.t(list((ClosureEnvironment.t, HoleInstanceParents.t)));

let empty: t = MetaVarMap.empty;

let num_unique_hcs = (hci: t, u: MetaVar.t): int => {
  switch (hci |> MetaVarMap.find_opt(u)) {
  | Some(hcs) => hcs |> List.length
  | None => 0
  };
};

let find_hc_opt =
    (hci: t, u: MetaVar.t, i: HoleInstanceId.t)
    : option((ClosureEnvironment.t, HoleInstanceParents.t)) => {
  switch (hci |> MetaVarMap.find_opt(u)) {
  | Some(hcs) => List.nth_opt(hcs, i)
  | None => None
  };
};

/* Add a parent to a hole. Assumes both the parent and the hole exist
   in the HoleInstanceInfo_.t. */
let add_parent =
    ((u, i): HoleInstance.t, parent: HoleInstanceParents.t_, hci: t): t => {
  let u_hole_closures = hci |> MetaVarMap.find(u);
  hci
  |> MetaVarMap.add(
       u,
       u_hole_closures
       |> List.mapi((i', (env, parents)) =>
            if (i' == i) {
              (env, parent |> HoleInstanceParents.add_parent(parents));
            } else {
              (env, parents);
            }
          ),
     );
};

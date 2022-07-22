open Sexplib.Std;

[@deriving sexp]
type t = MetaVarMap.t(list((ClosureEnvironment.t, HoleInstanceParents.t)));

let empty: t = MetaVarMap.empty;

let num_instances = (hii: t, u: MetaVar.t): int =>
  hii
  |> MetaVarMap.find_opt(u)
  |> Option.map(his => List.length(his))
  |> Option.value(~default=0);

let find_instance =
    (hci: t, u: MetaVar.t, i: HoleInstanceId.t)
    : option((ClosureEnvironment.t, HoleInstanceParents.t)) => {
  switch (hci |> MetaVarMap.find_opt(u)) {
  | Some(hcs) => List.nth_opt(hcs, i)
  | None => None
  };
};

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

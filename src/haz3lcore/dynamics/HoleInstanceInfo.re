open Sexplib.Std;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = MetaVarMap.t(list((ClosureEnvironment.t, HoleInstanceParents.t)));

let empty: t = MetaVarMap.empty;

let num_instances = (hii: t, u: MetaVar.t): int =>
  hii
  |> MetaVarMap.find_opt(u)
  |> Option.map(his => List.length(his))
  |> Option.value(~default=0);

let find_instance =
    (hii: t, u: MetaVar.t, i: HoleInstanceId.t)
    : option((ClosureEnvironment.t, HoleInstanceParents.t)) => {
  switch (hii |> MetaVarMap.find_opt(u)) {
  | Some(his) => List.nth_opt(his, i)
  | None => None
  };
};

let add_parent =
    ((u, i): HoleInstance.t, parent: HoleInstanceParents.t_, hii: t): t => {
  let u_instances = hii |> MetaVarMap.find(u);
  hii
  |> MetaVarMap.add(
       u,
       u_instances
       |> List.mapi((i', (env, parents)) =>
            if (i' == i) {
              (env, parent |> HoleInstanceParents.add_parent(parents));
            } else {
              (env, parents);
            }
          ),
     );
};

let fast_equal = (hii1: t, hii2: t): bool => {
  let fast_equal_his = (his1, his2) =>
    List.equal(
      ((sigma1, _), (sigma2, _)) =>
        ClosureEnvironment.id_equal(sigma1, sigma2)
        /* Check that variable mappings in ClosureEnvironment are equal */
        && List.equal(
             ((x1, d1), (x2, d2)) => x1 == x2 && DHExp.fast_equal(d1, d2),
             ClosureEnvironment.to_list(sigma1),
             ClosureEnvironment.to_list(sigma2),
           ),
      his1,
      his2,
    );
  MetaVarMap.equal(fast_equal_his, hii1, hii2);
};

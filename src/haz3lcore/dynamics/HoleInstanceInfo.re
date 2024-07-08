open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;

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

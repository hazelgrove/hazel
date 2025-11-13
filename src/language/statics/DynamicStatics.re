open Util;

module Map = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type entry = {
    exps: list(Exp.t),
    ty_envs: list(Environment.t(Typ.t)),
  };
  [@deriving (show({with_path: false}), sexp, yojson)]
  type t = Id.Map.t(entry); // Probably put these in one list

  let empty = Id.Map.empty;
  let lookup = Id.Map.find_opt;
};

let dynamic_type_env = (ctx, dynamics_exp) =>
  Option.map((de: Map.entry) => de.ty_envs, dynamics_exp)
  |> Option.map(envs => {
       module StringSet = Set.Make(String);

       let dedup_seq = seq => {
         let seen = ref(StringSet.empty);
         Seq.filter(
           x =>
             if (StringSet.mem(x, seen^)) {
               false;
             } else {
               seen := StringSet.add(x, seen^);
               true;
             },
           seq,
         );
       };

       let vars =
         envs
         |> List.to_seq
         |> Seq.concat_map(e => Environment.to_bindings(e) |> List.to_seq)
         |> Seq.map(fst)
         |> dedup_seq;

       let vars_with_joined_types =
         vars
         |> Seq.map(var => {
              let tys =
                envs
                |> List.filter_map(env =>
                     switch (Environment.lookup(env, var)) {
                     | Some(ty) => Some(ty)
                     | None => None
                     }
                   );
              (
                var,
                Typ.meet_all(ctx, tys)
                |> Option.value(~default=Unknown(Internal) |> Typ.temp),
              );
            })
         |> List.of_seq;
       Environment.of_bindings(vars_with_joined_types);
     });

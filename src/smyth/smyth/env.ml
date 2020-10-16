open Lang

let empty : env = ([], [])

let all_res : env -> (string * res) list = fun (rs, _) -> rs

let all_type : env -> (string * typ) list = fun (_, ts) -> ts

let concat : env list -> env =
 fun envs ->
  let rss, tss = List.split envs in
  (List.concat rss, List.concat tss)

let add_res : string * res -> env -> env = fun b (rs, ts) -> (b :: rs, ts)

let concat_res : (string * res) list -> env -> env =
 fun b (rs, ts) -> (b @ rs, ts)

let add_type : string * typ -> env -> env = fun b (rs, ts) -> (rs, b :: ts)

let concat_type : (string * typ) list -> env -> env =
 fun b (rs, ts) -> (rs, b @ ts)

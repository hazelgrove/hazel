open Lang

let empty : type_ctx = ([], [])

let all_type : type_ctx -> type_binding list = fun (ts, _) -> ts

let all_poly : type_ctx -> string list = fun (_, ps) -> ps

let concat : type_ctx list -> type_ctx =
 fun gammas ->
  let tss, pss = List.split gammas in
  (List.concat tss, List.concat pss)

let add_type : type_binding -> type_ctx -> type_ctx =
 fun b (ts, ps) -> (b :: ts, ps)

let concat_type : type_binding list -> type_ctx -> type_ctx =
 fun bs (ts, ps) -> (bs @ ts, ps)

let add_poly : poly_binding -> type_ctx -> type_ctx =
 fun b (ts, ps) -> (ts, b :: ps)

let concat_poly : poly_binding list -> type_ctx -> type_ctx =
 fun bs (ts, ps) -> (ts, bs @ ps)

let peel_type : type_ctx -> (type_binding * type_ctx) option =
 fun (ts, ps) ->
  match ts with head :: tail -> Some (head, (tail, ps)) | [] -> None

let names : type_ctx -> string list = fun (ts, ps) -> List.map fst ts @ ps

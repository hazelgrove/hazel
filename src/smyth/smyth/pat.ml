open Lang

let rec syntactically_equal : pat -> pat -> bool =
 fun p1 p2 ->
  match (p1, p2) with
  | PVar x1, PVar x2 -> String.equal x1 x2
  | PTuple ps1, PTuple ps2 ->
      Int.equal (List.length ps1) (List.length ps2)
      && List.for_all2 syntactically_equal ps1 ps2
  | PWildcard, PWildcard -> true
  | _ -> false

let rec bind_res : pat -> res -> env option =
 fun p r ->
  match p with
  | PVar x -> Some (Env.add_res (x, r) Env.empty)
  | PTuple ps -> (
    match r with
    | RTuple rs ->
        if Int.equal (List.length ps) (List.length rs) then
          List.map2 bind_res ps rs |> Option2.sequence
          |> Option2.map Env.concat
        else None
    | _ ->
        let len = List.length ps in
        ps
        |> List.mapi (fun i_ p -> bind_res p (RProj (len, i_ + 1, r)))
        |> Option2.sequence |> Option2.map Env.concat )
  | PWildcard -> Some Env.empty

let bind_rec_name_res : string option -> res -> env =
 fun rec_name_opt r ->
  match rec_name_opt with
  | Some rec_name -> Env.add_res (rec_name, r) Env.empty
  | None -> Env.empty

let rec bind_typ : bind_spec -> pat -> typ -> type_ctx option =
 fun bind_spec p tau ->
  match p with
  | PVar x -> Some (Type_ctx.add_type (x, (tau, bind_spec)) Type_ctx.empty)
  | PTuple ps -> (
    match tau with
    | TTuple taus ->
        if Int.equal (List.length ps) (List.length taus) then
          List.map2 (bind_typ bind_spec) ps taus
          |> Option2.sequence
          |> Option2.map Type_ctx.concat
        else None
    | _ -> None )
  | PWildcard -> Some Type_ctx.empty

let bind_rec_name_typ : string option -> typ -> type_ctx =
 fun rec_name_opt tau ->
  match rec_name_opt with
  | Some rec_name ->
      Type_ctx.add_type (rec_name, (tau, Rec rec_name)) Type_ctx.empty
  | None -> Type_ctx.empty

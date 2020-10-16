open Lang

let rec final r = determinate r || indeterminate r

and final_env (env : env) : bool =
  env |> Env.all_res |> List.for_all (fun (_x, e) -> final e)

and determinate r =
  match r with
  | RFix (env, _, _, _) -> final_env env
  | RTuple comps -> List.for_all final comps
  | RCtor (_, arg) -> final arg
  | _ -> false

and indeterminate r =
  match r with
  | RHole (env, _) -> final_env env
  | RApp (r1, RARes r2) -> indeterminate r1 && final r2
  | RApp (r1, RAType _) -> indeterminate r1
  | RProj (_, _, arg) -> indeterminate arg
  | RCase (env, scrutinee, _) -> final_env env && indeterminate scrutinee
  | _ -> false

let rec to_value r =
  match r with
  | RTuple comps ->
      comps |> List.map to_value |> Option2.sequence
      |> Option2.map (fun vcomps -> VTuple vcomps)
  | RCtor (name, arg) ->
      Option2.map (fun v -> VCtor (name, v)) (to_value arg)
  | _ -> None

let rec from_value v =
  match v with
  | VTuple comps -> RTuple (List.map from_value comps)
  | VCtor (name, v_arg) -> RCtor (name, from_value v_arg)

let rec consistent r1 r2 =
  if r1 = r2 then Some []
  else
    match (r1, r2) with
    | RTuple comps1, RTuple comps2 ->
        if List.length comps1 <> List.length comps2 then None
        else
          List.map2 consistent comps1 comps2
          |> Option2.sequence |> Option2.map List.concat
    | RCtor (name1, arg1), RCtor (name2, arg2) ->
        if String.equal name1 name2 then consistent arg1 arg2 else None
    | _ -> (
      match to_value r1 with
      | Some v1 -> Some [(r2, v1)]
      | None -> (
        match to_value r2 with Some v2 -> Some [(r1, v2)] | None -> None ) )

module HTyp =
 struct
  (* types with holes *)
  type t =
  | Hole
  | Unit
  | Num
  | Bool
  | Arrow of t * t
  | Prod of t * t
  | Sum of t * t
  | List of t

  (* eqity *)
  let rec eq ty1 ty2 =
    match (ty1, ty2) with
    | (Hole, Hole) -> true
    | (Hole, _) -> false
    | (Unit, Unit) -> true
    | (Unit, _) -> false
    | (Num, Num) -> true
    | (Num, _) -> false
    | (Bool, Bool) -> true
    | (Bool, _) -> false
    | (Arrow (ty1, ty2), Arrow (ty1', ty2')) ->
      (eq ty1 ty1') && (eq ty2 ty2')
    | (Arrow (_, _), _) -> false
    | (Prod (ty1, ty2), Prod (ty1', ty2')) ->
      (eq ty1 ty1') && (eq ty2 ty2')
    | (Prod (_, _), _) -> false
    | (Sum (ty1, ty2), Sum (ty1', ty2')) ->
      (eq ty1 ty1') && (eq ty2 ty2')
    | (Sum (_, _), _) -> false
    | (List ty, List ty') ->
      eq ty ty'
    | (List _, _) -> false

  (* type consistency *)
  let rec consistent x y =
    match (x, y) with
    | (Hole, _)
    | (_, Hole) ->  true
    | (Unit, Unit) -> true
    | (Unit, _) -> false
    | (Num, Num) -> true
    | (Num, _) -> false
    | (Bool, Bool) -> true
    | (Bool, _) -> false
    | (Arrow (ty1, ty2), Arrow (ty1', ty2'))
    | (Prod (ty1, ty2), Prod (ty1', ty2'))
    | (Sum (ty1, ty2), Sum (ty1', ty2')) ->
      (consistent ty1 ty1') && (consistent ty2 ty2')
    | (Arrow (_, _), _) -> false
    | (Prod (_, _), _) -> false
    | (Sum (_, _), _) -> false
    | (List ty, List ty') ->
      consistent ty ty'
    | (List _, _) -> false

  let inconsistent ty1 ty2 =
    negb (consistent ty1 ty2)

  (* matched arrow types *)
  let matched_arrow = function
  | Hole -> Some (Hole,Hole)
  | Arrow (ty1, ty2) -> Some (ty1,ty2)
  | _ -> None

  let has_matched_arrow = function
  | Hole -> true
  | Arrow (_, _) -> true
  | _ -> false

  (* matched product types *)
  let matched_prod = function
  | Hole -> Some (Hole,Hole)
  | Prod (ty1, ty2) -> Some (ty1,ty2)
  | _ -> None

  let has_matched_prod = function
  | Hole -> true
  | Prod (_, _) -> true
  | _ -> false

  let rec get_tuple ty1 ty2 = match ty2 with
  | Prod (ty21, ty22) -> ty1::(get_tuple ty21 ty22)
  | _ -> ty1::(ty2::[])

  let rec make_tuple = function
  | [ty1; ty2] -> Prod (ty1, ty2)
  | [ty1] -> ty1
  | ty1 :: tys ->
    let ty2 = make_tuple tys in
    Prod (ty1, ty2)
  | [] -> Unit

  let rec zip_with_skels skels types =
    match (skels, types) with
    | ([], []) -> ([], [])
    | (skel::skels, ty::tys) ->
      let (tail, remainder) = zip_with_skels skels tys in
      ((skel, ty)::tail, remainder)
    | (_::_, []) -> ([], skels)
    | ([], _::_) -> ([], [])

  (* matched sum types *)
  let matched_sum = function
  | Hole -> Some (Hole,Hole)
  | Sum (tyL, tyR) -> Some (tyL,tyR)
  | _ -> None

  let has_matched_sum = function
  | Hole -> true
  | Sum (_, _) -> true
  | _ -> false

  (* matched list types *)
  let matched_list = function
  | Hole -> Some Hole
  | List ty -> Some ty
  | _ -> None

  let has_matched_list = function
  | Hole -> true
  | List _ -> true
  | _ -> false

  (* complete (i.e. does not have any holes) *)
  let rec complete = function
  | Hole -> false
  | Unit -> true
  | Num -> true
  | Bool -> true
  | Arrow (ty1, ty2) -> if complete ty1 then complete ty2 else false
  | Prod (ty1, ty2) -> if complete ty1 then complete ty2 else false
  | Sum (ty1, ty2) -> if complete ty1 then complete ty2 else false
  | List ty -> complete ty

  let rec join ty1 ty2 =
    match (ty1, ty2) with
    | (_, Hole) -> Some ty1
    | (Hole, _) -> Some ty2
    | (Unit, Unit) -> Some ty1
    | (Unit, _) -> None
    | (Num, Num) -> Some ty1
    | (Num, _) -> None
    | (Bool, Bool) -> Some ty1
    | (Bool, _) -> None
    | (Arrow (ty1, ty2), Arrow (ty1', ty2')) ->
      match (join ty1 ty1', join ty2 ty2') with
      | (Some ty1, Some ty2) -> Some (Arrow (ty1, ty2))
      | _ -> None
    | (Arrow (_, _), _) -> None
    | (Prod (ty1, ty2), Prod (ty1', ty2')) ->
      match (join ty1 ty1', join ty2 ty2') with
      | (Some ty1, Some ty2) -> Some (Prod (ty1, ty2))
      | _ -> None
    | (Prod (_, _), _) -> None
    | (Sum (ty1, ty2), Sum (ty1', ty2')) ->
      match (join ty1 ty1', join ty2 ty2') with
      | (Some ty1, Some ty2) -> Some (Sum (ty1, ty2))
      | _ -> None
    | (Sum (_, _), _) -> None
    | (List ty, List ty') ->
      match join ty ty' with
      | Some ty -> Some (List ty)
      | None -> None
    | (List ty, _) -> None
 end

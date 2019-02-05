type 'a t_ = (Var.t * 'a) list

let empty =
  []

let is_empty = function
| [] -> true
| _::_ -> false

let rec drop ctx x =
  match ctx with
  | [] -> ctx
  | (y,elt)::ctx' ->
    if Var.eq x y then ctx' else (y,elt)::(drop ctx' x)

let extend ctx xa =
  let x,elt = xa in
  xa::(drop ctx x)

let union ctx1 ctx2 =
  List.fold_left extend ctx2 ctx1

let rec lookup ctx x =
  match ctx with
  | [] -> None
  | (y,elt)::ctx' ->
    if Var.eq x y then Some elt else lookup ctx' x

let contains ctx x =
  match lookup ctx x with
  | Some _ -> true
  | None -> false

let map f xs =
  List.map
    (fun xa -> let x,_ = xa in x,(f xa))
    xs

let rec length = function
| [] -> 0
| _::ctx' -> 1 + (length ctx')

let to_list ctx = ctx

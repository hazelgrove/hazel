module Util =
 struct

  (* Section ListUtil *)

  let rec update_nth n xs f =
    match (n, xs) with
    | (_, []) -> []
    | (0, x::xs) -> (f x)::xs
    | (n, x::xs) -> x::(update_nth (n-1) xs f)

  let rec _findmapi i xs f =
    match xs with
    | [] -> None
    | x::xs ->
      match f i x with
      | Some b -> Some b
      | None -> _findmapi (i+1) xs f)

  let findmapi xs f = _findmapi 0 xs f

  let rec zip_eq xs ys =
    match (xs, ys) with
    | ([], []) => Some []
    | (x::xs, y::ys) =>
      match zip_eq xs ys with
      | None => None
      | Some tail => Some ((x,y)::tail)
    | (_::_, []) => None
    | ([], _::_) => None

  let rec unzip xs =
    match xs with
    | [] -> [],[]
    | (x,y)::xys ->
      let xs,ys = unzip xys in
      (x::xs),(y::ys)

  (* End ListUtil *)

  (* Section StringUtil *)

  let str_eqb = String.equal

  let char_le_b ch1 ch2 =
    leb (nat_of_ascii ch1) (nat_of_ascii ch2)

  let char_eq_b ch1 ch2 =
    eqb (nat_of_ascii ch1) (nat_of_ascii ch2)

  let char_in_range_b ch s e =
    if char_le_b s ch then char_le_b ch e else false

  module NatMap =
   struct
    type 'a t = (int * 'a) list

    let empty = []

    let extend delta x = x::delta

    let rec drop delta n =
      match delta with
      | [] -> None
      | (y,a)::delta' ->
        if eqb n y then Some (delta',a) else drop delta' n

    let union = app

    let rec lookup delta x =
      match delta with
      | [] -> None
      | (y,a)::delta' ->
        if eqb x y then Some a else lookup delta' x

    let rec insert_or_update delta x =
      let u,a = x in
      match delta with
      | [] -> x::delta
      | (u',a')::delta' ->
        if eqb u u'
        then (u',a)::delta'
        else (u',a')::(insert_or_update delta' x)

    let rec insert_or_map delta u a0 f =
      match delta with
      | [] -> let a0 = a0 () in a0,((u,a0)::delta)
      | (u'a)::delta' ->
        if eqb u u'
        then let a' = f a in a',((u',a')::delta')
        else let a',delta'' = insert_or_map delta' u a0 f in
             a',((u',a)::delta'')

    let rec map f = function
    | [] -> []
    | (u,a)::delta' -> (u,(f a))::(map f delta')

    let rec update_with f u delta u_nil =
      match delta with
      | [] -> u_nil,delta
      | (u',a)::delta' ->
        if eqb u u'
        then let a' = f a in a',((u',a')::delta')
        else let a',delta'' = update_with f u delta' u_nil in
             a',((u',a)::delta'')

    let length = length

    let to_list delta = delta

    let fold delta f b =
      fold_left f delta b
   end
 end

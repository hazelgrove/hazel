type nat = int

(* Section ListUtil *)

let rec update_nth n xs f =
  begin match (n, xs) with
  | (_, []) -> []
  | (0, x::xs) -> (f x)::xs
  | (n, x::xs) -> x::(update_nth (n-1) xs f)
  end

let rec _findmapi i xs f =
  begin match xs with
  | [] -> None
  | x::xs ->
    begin match f i x with
    | Some b -> Some b
    | None -> _findmapi (i+1) xs f
    end
  end

let findmapi xs f = _findmapi 0 xs f

let rec zip_eq xs ys =
  begin match (xs, ys) with
  | ([], []) -> Some []
  | (x::xs, y::ys) ->
    begin match zip_eq xs ys with
    | None -> None
    | Some tail -> Some ((x,y)::tail)
    end
  | (_::_, []) -> None
  | ([], _::_) -> None
  end

let rec unzip xs =
  begin match xs with
  | [] -> [],[]
  | (x,y)::xys ->
    let xs,ys = unzip xys in
    (x::xs),(y::ys)
  end

(* End ListUtil *)

module ZList = struct
  type ('z, 'a) t = 'a list * 'z * 'a list

  let singleton (z : 'z) : ('z, 'a) t =
    ([], z, [])

  let rec split_at (n : nat) (xs : 'a list) : (('a, 'a) t) option =
    begin match (n, xs) with
    | (_, []) -> None
    | (0, x :: xs) ->
      let prefix = [] in
      let suffix = xs in
      Some (prefix, x, suffix)
    | (_, x :: xs) ->
      let n' = n - 1 in
      begin match split_at n' xs with
      | None -> None
      | Some (prefix, z, suffix) ->
        let prefix' = x :: prefix in
        Some (prefix', z, suffix)
      end
    end

  let rec replace_z
    (zs : ('z, 'a) t)
    (z : 'z)
    : ('z, 'a) t =
      let (prefix, _, suffix) = zs in
      (prefix, z, suffix)

  let optmap_z
    (f : 'z1 -> ('z2 option))
    (zs : ('z1, 'a) t)
    : ('z2, 'a) t option =
      let (prefix, z, suffix) = zs in
      begin match f z with
      | None -> None
      | Some z' -> Some (prefix, z', suffix)
      end

  let prj_prefix (zxs : ('z, 'a) t) : 'a list =
    let (prefix, _, _) = zxs in prefix

  let prefix_length (zxs : ('z, 'a) t) : nat =
    List.length (prj_prefix zxs)

  let prj_z (zxs : ('z, 'a) t) : 'z =
    let (_, z, _) = zxs in z

  let prj_suffix (zxs : ('z, 'a) t) : 'a list =
    let (_, _, suffix) = zxs in suffix

  let erase (xs : ('z, 'a) t) (erase_z : 'z -> 'a) =
    let (prefix, z, suffix) = xs in
    let a = erase_z z in
    prefix @ (a :: suffix)
end

(* Section StringUtil *)

let str_eqb = String.equal

let char_le_b ch1 ch2 = (Char.code ch1) < (Char.code ch2)

let char_eq_b ch1 ch2 = (Char.code ch1) = (Char.code ch2)

let char_in_range_b ch s e =
  if char_le_b s ch then char_le_b ch e else false

module NatMap =
 struct
  type 'a t = (int * 'a) list

  let empty = []

  let extend delta x = x::delta

  let rec drop delta n =
    begin match delta with
    | [] -> None
    | (y,a)::delta' ->
      if n == y then Some (delta',a) else drop delta' n
    end

  let union = List.append

  let rec lookup delta x =
    begin match delta with
    | [] -> None
    | (y,a)::delta' ->
      if x = y then Some a else lookup delta' x
    end

  let rec insert_or_update delta x =
    let u,a = x in
    begin match delta with
    | [] -> x::delta
    | (u',a')::delta' ->
      if u = u'
      then (u',a)::delta'
      else (u',a')::(insert_or_update delta' x)
    end

  let rec insert_or_map delta u a0 f =
    begin match delta with
    | [] -> let a0 = a0 () in a0,((u,a0)::delta)
    | (u',a)::delta' ->
      if u == u'
      then let a' = f a in a',((u',a')::delta')
      else let a',delta'' = insert_or_map delta' u a0 f in
           a',((u',a)::delta'')
    end

  let rec map f = function
  | [] -> []
  | (u,a)::delta' -> (u,(f a))::(map f delta')

  let rec update_with f u delta u_nil =
    begin match delta with
    | [] -> u_nil,delta
    | (u',a)::delta' ->
      if u = u'
      then let a' = f a in a',((u',a')::delta')
      else let a',delta'' = update_with f u delta' u_nil in
           a',((u',a)::delta'')
    end

  let length = List.length

  let to_list delta = delta

  let fold delta f b =
    List.fold_left f delta b
 end

(* Zippered finite map over nats, used with Z expressions
 * i.e. there is a selected element of type Z and the rest is a nat map of type A *)
module ZNatMap =
  struct
    type ('a, 'z) t = 'a NatMap.t * (nat * 'z)
    let make (m : 'a NatMap.t) ((n, z) as nz : nat * 'z) : (('a, 'z) t) option =
      begin match NatMap.lookup m n with
      | Some _ -> None
      | None -> Some (m, nz)
      end
  end

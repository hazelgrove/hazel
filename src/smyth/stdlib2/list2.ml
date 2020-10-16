open Pervasives2

let pure_bind xs f = List.map f xs

let pure x = [x]

let bind xs f = List.map f xs |> List.concat

let concat_map f xs = bind xs f

let maximum = function
  | [] -> None
  | head :: tail -> Some (List.fold_left max head tail)

let repeat n x =
  let rec helper k acc = if k <= 0 then acc else helper (k - 1) (x :: acc) in
  helper n []

let sequence mxs =
  List.fold_right
    (fun xs acc -> bind xs @@ fun x -> pure_bind acc @@ fun ys -> x :: ys)
    mxs [[]]

let filter_somes xs = List.filter_map Fun.id xs

let intersperse sep xs =
  let rec helper acc = function
    | [] -> List.rev acc
    | [x] -> List.rev (x :: acc)
    | head :: tail -> helper (sep :: head :: acc) tail
  in
  helper [] xs

let range ~low ~high = ListLabels.init ~len:(high - low + 1) ~f:(( + ) low)

let remove_first y xs =
  let rec helper acc = function
    | [] -> List.rev acc
    | head :: tail ->
        if head = y then List.rev_append acc tail
        else helper (head :: acc) tail
  in
  helper [] xs

let permutations ys =
  (* Source: https://stackoverflow.com/a/40099411 *)
  let rec permutations' xs =
    if xs = [] then [[]]
    else
      bind xs
      @@ fun x ->
      bind (permutations' (remove_first x xs))
      @@ fun permutation -> [x :: permutation]
  in
  List.sort_uniq compare (permutations' ys)

let map3 f xs1 xs2 xs3 =
  List.map2 (fun (x1, x2) x3 -> f x1 x2 x3) (List.combine xs1 xs2) xs3

let hd_opt xs = match xs with [] -> None | head :: _ -> Some head

let tl_opt xs = match xs with [] -> None | _ :: tail -> Some tail

let uncons xs = match xs with [] -> None | head :: tail -> Some (head, tail)

let is_empty xs = match xs with [] -> true | _ :: _ -> false

let rec transpose xss =
  if List.for_all is_empty xss then []
  else
    List.filter_map hd_opt xss
    :: transpose (List.map (tl_opt >> Option2.with_default []) xss)

let collapse_equal xs =
  match xs with
  | [] -> None
  | head :: tail ->
      if List.for_all (fun x -> x = head) tail then Some head else None

let index_left xs = List.mapi (fun i x -> (i, x)) xs

let index_right xs = List.mapi (fun i x -> (x, i)) xs

let rec find_map f xs =
  match xs with
  | [] -> None
  | head :: tail -> (
    match f head with Some x -> Some x | None -> find_map f tail )

let sum xs = List.fold_left ( + ) 0 xs

let fsum xs = List.fold_left ( +. ) 0.0 xs

let average xs =
  let len = List.length xs in
  if Int.equal len 0 then None else Some (fsum xs /. float_of_int len)

let take n xs =
  let rec helper acc n xs =
    if n <= 0 then List.rev acc
    else
      match xs with
      | [] -> List.rev acc
      | head :: tail -> helper (head :: acc) (n - 1) tail
  in
  helper [] n xs

let rec drop n xs =
  if n <= 0 then xs
  else match xs with [] -> [] | _ :: tail -> drop (n - 1) tail

let cartesian_product xs ys =
  concat_map (fun x -> List.map (fun y -> (x, y)) ys) xs

let count pred xs =
  let rec helper acc = function
    | [] -> acc
    | head :: tail -> helper (if pred head then acc + 1 else acc) tail
  in
  helper 0 xs

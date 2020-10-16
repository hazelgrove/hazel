let map f = function Some x -> Some (f x) | None -> None

let pure_bind ox f = map f ox

let bind ox f = match ox with Some x -> f x | None -> None

let and_then f ox = bind ox f

let guard b = if b then Some () else None

let sequence xs =
  let rec helper acc = function
    | [] -> Some (List.rev acc)
    | head :: tail -> (
      match head with Some x -> helper (x :: acc) tail | None -> None )
  in
  helper [] xs

let with_default default ox = match ox with Some x -> x | None -> default

let sequence_fst (ox, y) =
  match ox with Some x -> Some (x, y) | None -> None

let sequence_snd (x, oy) =
  match oy with Some y -> Some (x, y) | None -> None

let filter pred ox =
  match ox with Some x -> if pred x then Some x else None | None -> None

module Syntax = struct
  let ( let+ ) = pure_bind

  let ( let* ) = bind
end

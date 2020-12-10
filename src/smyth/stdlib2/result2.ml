let map f = function Ok x -> Ok (f x) | Error e -> Error e

let pure_bind x f = map f x

let bind rx f = match rx with Ok x -> f x | Error e -> Error e

let and_then f rx = bind rx f

let guard e b = if b then Ok () else Error e

let sequence xs =
  let rec helper acc = function
    | [] -> Ok (List.rev acc)
    | head :: tail -> (
      match head with Ok x -> helper (x :: acc) tail | Error e -> Error e )
  in
  helper [] xs

let to_option r = match r with Ok x -> Some x | Error _ -> None

let with_default default r = match r with Ok x -> x | Error _ -> default

let unwrap f g r = match r with Ok x -> f x | Error y -> g y

module Syntax = struct
  let ( let+ ) = pure_bind

  let ( let* ) = bind
end

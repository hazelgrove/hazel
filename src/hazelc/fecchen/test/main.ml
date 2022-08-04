module Query = struct
  type 'd t = A : int t | B : int t | C : int t

  let equal : type a b. a t -> b t -> (a t, b t) Gadt.Eq.t option =
   fun x x' ->
    match (x, x') with
    | A, A -> Some Refl
    | B, B -> Some Refl
    | C, C -> Some Refl
    | _, _ -> None
end

module F = Fecchen.Make (Query)

let rules F.{ fetch; pure; io = _ } q =
  let open Query in
  let open F.Syntax in
  match q with
  | A ->
      print_endline "fetching A";
      pure 10
  | B ->
      print_endline "fetching B";
      fetch A >>= fun n -> pure (n + 10)
  | C ->
      print_endline "fetching C";
      fetch B >>= fun n -> pure (n + 10)

let () = F.run rules Query.C |> string_of_int |> print_endline

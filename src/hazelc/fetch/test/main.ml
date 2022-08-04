module Query = struct
  type 'd t = A : int t | B : int t | C : int t | D : int t

  let equal : type a b. a t -> b t -> (a t, b t) Gadt.Eq.t option =
   fun x x' ->
    match (x, x') with
    | A, A -> Some Refl
    | B, B -> Some Refl
    | C, C -> Some Refl
    | D, D -> Some Refl
    | _, _ -> None
end

module F = Fetch.Make (Query)

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
      fetch B >>= fun n ->
      fetch A >>= fun n' -> pure (n + n')
  | D ->
      print_endline "fetching D";
      fetch C >>= fun n -> pure (n + 10)

let () = F.run_with_memo rules Query.D |> string_of_int |> print_endline

open Lang

let rec nat : exp -> int option = function
  | ECtor ("S", [], arg) -> Option.map (( + ) 1) (nat arg)
  | ECtor ("Z", [], ETuple []) -> Some 0
  | _ -> None

let listt : exp -> (exp list * typ list) option =
  let rec helper expected_opt = function
    | ECtor ("Cons", type_args, ETuple [head; tail]) -> (
        let good () =
          Option.map
            (fun (es, taus) -> (head :: es, taus))
            (helper expected_opt tail)
        in
        match expected_opt with
        | Some expected ->
            if Type.equal (TTuple expected) (TTuple type_args) then good ()
            else None
        | None -> good () )
    | ECtor ("Nil", type_args, ETuple []) -> (
        let good () = Some ([], type_args) in
        match expected_opt with
        | Some expected ->
            if Type.equal (TTuple expected) (TTuple type_args) then good ()
            else None
        | None -> good () )
    | _ -> None
  in
  helper None

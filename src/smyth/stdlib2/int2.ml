(* Source: https://crypto.stanford.edu/~blynn/haskell/count.html *)
let rec partition ~n ~k =
  if n = 0 && k = 0 then [[]]
  else if n <= 0 || k <= 0 then []
  else
    let option1 =
      partition ~n:(n - 1) ~k:(k - 1) |> List.map (fun parts -> parts @ [1])
    in
    let option2 = partition ~n:(n - k) ~k |> List.map (List.map (( + ) 1)) in
    option1 @ option2

let partition_permutations ~n ~k =
  partition ~n ~k |> List2.concat_map List2.permutations

(* Source: https://stackoverflow.com/a/16950740/ *)
let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n ->
      let b = pow a (n / 2) in
      b * b * if n mod 2 = 0 then 1 else a

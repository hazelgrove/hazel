type nat = int

type 'op t =
| Placeholder of nat
| BinOp of err_status * 'op * 'op t * 'op t

let rec leftmost_op = function
| Placeholder _ -> None
| BinOp (_, op, skel1, _) ->
  (match leftmost_op skel1 with
   | Some op1 -> Some op1
   | None -> Some op)

let rec rightmost_op = function
| Placeholder _ -> None
| BinOp (_, op, _, skel2) ->
  (match rightmost_op skel2 with
   | Some op1 -> Some op1
   | None -> Some op)

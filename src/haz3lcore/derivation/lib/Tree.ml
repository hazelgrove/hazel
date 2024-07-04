type 'a p = Node of 'a * 'a p list
type pos = Value | Children of int * pos

let value (Node (v, _)) = v
let children (Node (_, c)) = c
let hd_children t = t |> children |> List.hd
let tl_children t = t |> children |> List.tl

let rec nth_node (Node (v, c)) = function
  | Value -> Node (v, c)
  | Children (i, pos) -> pos |> nth_node (List.nth c i)

let nth_node_opt t pos = try Some (nth_node t pos) with Failure _ -> None
let nth t pos = nth_node t pos |> value
let nth_opt t pos = try Some (nth t pos) with Failure _ -> None
let init f = Node (f (), [])
let rec flatten (Node (v, c)) = v :: (c |> List.map flatten |> List.concat)

let rec equal eq (Node (v1, c1)) (Node (v2, c2)) =
  eq v1 v2 && List.equal (equal eq) c1 c2

let equal_struct n1 n2 = equal (fun _ _ -> true) n1 n2

let rec combine (Node (v1, c1), Node (v2, c2)) =
  Node ((v1, v2), List.combine c1 c2 |> List.map combine)

let rec map f (Node (v, c)) = Node (f v, c |> List.map (map f))

let mapi f =
  let rec aux f acc_pos (Node (v, c)) =
    Node
      ( v |> f (acc_pos Value),
        c |> List.mapi (fun i -> aux f (fun pos -> acc_pos (Children (i, pos))))
      )
  in
  aux f Fun.id

let rec fold_deep f (Node (v, c)) = f v (c |> List.map (fold_deep f))
let fold_right f n = n |> flatten |> List.fold_right f
let fold_left f init n = n |> flatten |> List.fold_left f init

let rec exists f (Node (v, c)) =
  f v || c |> List.exists (fun node -> exists f node)

let rec for_all f (Node (v, c)) =
  f v && c |> List.for_all (fun node -> for_all f node)

let flatten_pos t = t |> mapi (fun pos _ -> pos) |> flatten

let exists_pos t pos =
  try nth_node t pos |> Fun.const true with Failure _ -> false
(* let rec map_nth_node f ((Node (v,c))) =
     function
     | Value  -> f ((Node (v, c)))
     | ((Children (i,pos))) ->
         ((Node
             (v, (c |> (ListUtil.map_nth i (fun t  -> map_nth_node f t pos)))))
         )
   let map_nth f =
     map_nth_node
       (fun ((Node (v,c)))  ->
          ((Node ((f v), c))))
   let put_nth_node t' = map_nth_node (fun _  -> t')
   let put_nth v' = map_nth (fun _  -> v') *)

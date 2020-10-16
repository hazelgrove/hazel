type 'a t = Leaf | Node of 'a t * 'a * 'a t

let rec binsert : 'a -> 'a t -> 'a t =
 fun y tree ->
  match tree with
  | Leaf -> Node (Leaf, y, Leaf)
  | Node (left, x, right) ->
      if y == x then tree
      else if y < x then Node (binsert y left, x, right)
      else (* y > x *)
        Node (left, x, binsert y right)

let rec pre_order : 'a t -> 'a list =
 fun tree ->
  match tree with
  | Leaf -> []
  | Node (left, x, right) -> [x] @ pre_order left @ pre_order right

let rec in_order : 'a t -> 'a list =
 fun tree ->
  match tree with
  | Leaf -> []
  | Node (left, x, right) -> in_order left @ [x] @ in_order right

let rec post_order : 'a t -> 'a list =
 fun tree ->
  match tree with
  | Leaf -> []
  | Node (left, x, right) -> post_order left @ post_order right @ [x]

let rec count_leaves : 'a t -> int =
 fun tree ->
  match tree with
  | Leaf -> 1
  | Node (left, _, right) -> count_leaves left + count_leaves right

let rec count_nodes : 'a t -> int =
 fun tree ->
  match tree with
  | Leaf -> 0
  | Node (left, _, right) -> 1 + count_nodes left + count_nodes right

let rec map : ('a -> 'b) -> 'a t -> 'b t =
 fun f tree ->
  match tree with
  | Leaf -> Leaf
  | Node (left, x, right) -> Node (map f left, f x, map f right)

let rec count_nodes_at_level : int -> 'a t -> int =
 fun level tree ->
  if level < 0 then 0
  else
    match tree with
    | Leaf -> 0
    | Node (left, _, right) ->
        if level = 0 then 1
        else
          count_nodes_at_level (level - 1) left
          + count_nodes_at_level (level - 1) right

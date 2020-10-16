type 'a t = Leaf | Node of 'a t * 'a * 'a t

(* Assumes tree is a binary search tree *)
val binsert : 'a -> 'a t -> 'a t

val pre_order : 'a t -> 'a list

val in_order : 'a t -> 'a list

val post_order : 'a t -> 'a list

val count_leaves : 'a t -> int

val count_nodes : 'a t -> int

val map : ('a -> 'b) -> 'a t -> 'b t

val count_nodes_at_level : int -> 'a t -> int
